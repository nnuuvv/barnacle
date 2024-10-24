import barnacle
import gleam/erlang/atom
import gleam/erlang/node
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/string
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event

pub fn app() {
  lustre.simple(init, update, view)
}

pub opaque type Model(a) {
  Model(barnacle_subject: Subject(barnacle.Message(a)), nodes: List(String))
}

fn init(barnacle_subject: Subject(barnacle.Message(a))) -> Model(a) {
  Model(barnacle_subject, [node.self() |> node.to_atom |> atom.to_string])
}

pub type Msg {
  UpdateNodes(List(String))
  DisconnectCurrentNode
}

@external(erlang, "barnacle_demo_ffi", "disconnect_from_node")
fn disconnect_node(node: atom.Atom) -> Result(node.Node, Nil)

fn update(model: Model(a), msg: Msg) -> Model(a) {
  case msg {
    UpdateNodes(nodes) ->
      Model(..model, nodes: nodes |> list.sort(string.compare))
    DisconnectCurrentNode -> {
      let self = node.self() |> node.to_atom |> atom.to_string
      list.each(model.nodes, fn(node) {
        case node == self {
          True -> Nil
          False -> {
            let _ = disconnect_node(node |> atom.create_from_string)
            Nil
          }
        }
      })

      Model(..model, nodes: [self])
    }
  }
}

fn view(model: Model(a)) -> element.Element(Msg) {
  html.div([attribute.style([#("max-width", "100ch")])], [
    html.ul(
      [],
      list.map(model.nodes, fn(node) { html.li([], [html.text(node)]) }),
    ),
    html.p([], [
      html.text(
        "Open the site in a couple of browsers, check you're connected to different nodes, then click the button to disconnect the current node.",
      ),
    ]),
    html.p([], [
      html.text("Current node: "),
      html.span([attribute.style([#("font-weight", "bold")])], [
        html.text(node.self() |> node.to_atom |> atom.to_string),
      ]),
    ]),
    html.button(
      [
        event.on_click(DisconnectCurrentNode),
        ..case list.length(model.nodes) > 1 {
          True -> []
          False -> [attribute.disabled(True)]
        }
      ],
      [html.text("Disconnect current node")],
    ),
  ])
}
