//// TODO: Make this a good example of Gleam code
//// This is _very_ naive. Please don't use as inspiration :)
//// It just listens for events from the barnacle, and prints them to stdout.

import argv
import barnacle
import barnacle_demo/node_list
import gleam/bytes_builder
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/node
import gleam/erlang/os
import gleam/erlang/process.{type Selector, type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/element.{element}
import lustre/element/html.{html}
import lustre/server_component
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}
import pprint
import youid/uuid

const barnacle_poll_interval = 15_000

const listener_poll_interval = 1000

const default_port = 8080

pub fn main() {
  // Parse the port to run on from arguments
  let port = case argv.load().arguments {
    [port, ..] -> {
      case int.parse(port) {
        Ok(port) -> port
        Error(_) -> default_port
      }
    }
    _ -> default_port
  }

  // Get the basename of the current node
  let assert Ok(basename) = barnacle.get_node_basename(node.self())
  let listener = process.new_subject()

  // Get the Fly app name from the environment
  let assert Ok(app_name) = os.get_env("FLY_APP_NAME")

  let barnacle =
    // Use internal DNS to connect to the nodes
    barnacle.dns(basename, app_name <> ".internal")
    |> barnacle.with_listener(listener)
    |> barnacle.with_poll_interval(barnacle_poll_interval)

  let assert Ok(barnacle_subject) = barnacle.start(barnacle)

  let app = node_list.app()
  let assert Ok(node_list) = lustre.start_actor(app, barnacle_subject)

  let assert Ok(_) = run_server(port, node_list)
  listen_and_publish(listener, node_list)
}

// ----- Server component updater ----- //

type NodeList =
  Subject(lustre.Action(node_list.Msg, lustre.ServerComponent))

fn listen_and_publish(
  listener: Subject(barnacle.BarnacleResponse(a)),
  node_list: NodeList,
) {
  case process.receive(listener, listener_poll_interval) {
    Ok(barnacle.RefreshResponse(Ok(nodes))) -> {
      io.println("Connected to nodes:\n" <> pprint.format(nodes))
    }
    Ok(barnacle.RefreshResponse(Error(err))) -> {
      io.println("Error refreshing nodes:\n" <> pprint.format(err))
    }
    // We don't really care about anything else for this demo
    _ -> Nil
  }

  process.send(
    node_list,
    lustre.dispatch(node_list.UpdateNodes(
      [node.self(), ..node.visible()]
      |> io.debug
      |> list.map(fn(node) { node |> node.to_atom |> atom.to_string })
      |> list.sort(string.compare),
    )),
  )

  listen_and_publish(listener, node_list)
}

// ----- Mist server ----- //

fn run_server(port: Int, node_list: NodeList) {
  fn(req: Request(Connection)) -> Response(ResponseData) {
    case request.path_segments(req) {
      ["node_list"] ->
        mist.websocket(
          request: req,
          on_init: socket_init(_, node_list),
          on_close: socket_close,
          handler: socket_update,
        )

      ["lustre-server-component.mjs"] -> {
        let assert Ok(priv) = erlang.priv_directory("lustre")
        let path = priv <> "/static/lustre-server-component.mjs"

        mist.send_file(path, offset: 0, limit: None)
        |> result.map(fn(script) {
          response.new(200)
          |> response.prepend_header("content-type", "application/javascript")
          |> response.set_body(script)
        })
        |> result.lazy_unwrap(fn() {
          response.new(404)
          |> response.set_body(mist.Bytes(bytes_builder.new()))
        })
      }

      _ ->
        response.new(200)
        |> response.prepend_header("content-type", "text/html")
        |> response.set_body(
          html([], [
            html.head([], [
              html.script(
                [
                  attribute.type_("module"),
                  attribute.src("/lustre-server-component.mjs"),
                ],
                "",
              ),
            ]),
            html.body([], [
              html.h1([], [html.text("Connected Nodes")]),
              element(
                "lustre-server-component",
                [server_component.route("/node_list")],
                [],
              ),
            ]),
          ])
          |> element.to_document_string_builder
          |> bytes_builder.from_string_builder
          |> mist.Bytes,
        )
    }
  }
  |> mist.new
  |> mist.bind("0.0.0.0")
  |> mist.port(port)
  |> mist.start_http
}

type State =
  #(String, NodeList)

fn socket_init(
  _conn: WebsocketConnection,
  node_list: NodeList,
) -> #(State, Option(Selector(lustre.Patch(node_list.Msg)))) {
  let self = process.new_subject()
  let id = uuid.v4() |> uuid.to_string

  process.send(node_list, server_component.subscribe(id, process.send(self, _)))

  #(
    #(id, node_list),
    Some(process.selecting(process.new_selector(), self, fn(a) { a })),
  )
}

fn socket_update(
  state: State,
  conn: WebsocketConnection,
  msg: WebsocketMessage(lustre.Patch(node_list.Msg)),
) {
  case msg {
    mist.Text(json) -> {
      let action = json.decode(json, server_component.decode_action)

      case action {
        Ok(action) -> process.send(state.1, action)
        Error(_) -> Nil
      }

      actor.continue(state)
    }

    mist.Binary(_) -> actor.continue(state)
    mist.Custom(patch) -> {
      let assert Ok(_) =
        patch
        |> server_component.encode_patch
        |> json.to_string
        |> mist.send_text_frame(conn, _)

      actor.continue(state)
    }
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
  }
}

fn socket_close(state: State) {
  process.send(state.1, server_component.unsubscribe(state.0))
}
