import barnacle/internal/dns
import barnacle/internal/local_epmd
import gleam/erlang/atom
import gleam/erlang/node
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervisor
import gleam/result
import gleam/set

// ----- Setup functions ----- //

pub opaque type Barnacle(error) {
  Barnacle(
    name: Option(atom.Atom),
    strategy: Strategy(error),
    poll_interval: Int,
    listener: Option(Subject(BarnacleResponse(error))),
  )
}

fn default_barnacle() -> Barnacle(error) {
  Barnacle(
    name: None,
    strategy: default_strategy(),
    poll_interval: 5000,
    listener: None,
  )
}

pub fn with_name(barnacle: Barnacle(error), name: String) -> Barnacle(error) {
  Barnacle(..barnacle, name: Some(name |> atom.create_from_string))
}

pub fn with_poll_interval(
  barnacle: Barnacle(error),
  poll_interval: Int,
) -> Barnacle(error) {
  Barnacle(..barnacle, poll_interval: poll_interval)
}

pub fn with_listener(
  barnacle: Barnacle(error),
  listener: Subject(BarnacleResponse(error)),
) -> Barnacle(error) {
  Barnacle(..barnacle, listener: Some(listener))
}

// ----- Strategies ----- //

pub opaque type Strategy(error) {
  Strategy(
    discover_nodes: fn() -> Result(List(atom.Atom), error),
    connect_nodes: fn(List(atom.Atom)) ->
      Result(List(atom.Atom), List(#(atom.Atom, node.ConnectError))),
    disconnect_nodes: fn(List(atom.Atom)) ->
      Result(List(atom.Atom), List(#(atom.Atom, NodeDisconnectError))),
    list_nodes: fn() -> Result(List(atom.Atom), error),
  )
}

pub type NodeDisconnectError {
  FailedToDisconnect
  LocalNodeIsNotAlive
}

fn default_strategy() -> Strategy(error) {
  Strategy(
    discover_nodes: fn() { Ok([]) },
    connect_nodes:,
    disconnect_nodes:,
    list_nodes:,
  )
}

pub fn new_strategy(
  discover_nodes: fn() -> Result(List(atom.Atom), error),
) -> Strategy(error) {
  Strategy(..default_strategy(), discover_nodes:)
}

pub fn with_connect_nodes_function(
  strategy: Strategy(error),
  connect_nodes: fn(List(atom.Atom)) ->
    Result(List(atom.Atom), List(#(atom.Atom, node.ConnectError))),
) -> Strategy(error) {
  Strategy(..strategy, connect_nodes:)
}

pub fn with_disconnect_nodes_function(
  strategy: Strategy(error),
  disconnect_nodes: fn(List(atom.Atom)) ->
    Result(List(atom.Atom), List(#(atom.Atom, NodeDisconnectError))),
) -> Strategy(error) {
  Strategy(..strategy, disconnect_nodes:)
}

pub fn with_list_nodes_function(
  strategy: Strategy(error),
  list_nodes: fn() -> Result(List(atom.Atom), error),
) -> Strategy(error) {
  Strategy(..strategy, list_nodes:)
}

// ----- Default Strategy ----- //

@external(erlang, "barnacle_ffi", "disconnect_from_node")
fn disconnect_node(node: atom.Atom) -> Result(node.Node, NodeDisconnectError)

fn list_nodes() -> Result(List(atom.Atom), error) {
  [node.self(), ..node.visible()]
  |> list.map(node.to_atom)
  |> Ok
}

fn connect_nodes(nodes: List(atom.Atom)) {
  nodes
  |> list.map(fn(node) {
    case node.connect(node) {
      Ok(_) -> Ok(node)
      Error(err) -> Error(#(node, err))
    }
  })
  |> result_apply
}

fn disconnect_nodes(nodes: List(atom.Atom)) {
  nodes
  |> list.map(fn(node) {
    case disconnect_node(node) {
      Ok(_) -> Ok(node)
      Error(err) -> Error(#(node, err))
    }
  })
  |> result_apply
}

// ----- Built-in Strategies ----- //

pub fn custom(strategy: Strategy(error)) -> Barnacle(error) {
  Barnacle(..default_barnacle(), strategy:)
}

pub fn local_epmd() -> Barnacle(Nil) {
  Barnacle(
    ..default_barnacle(),
    strategy: Strategy(
      ..default_strategy(),
      discover_nodes: local_epmd.discover_nodes,
    ),
  )
}

pub fn epmd(nodes: List(atom.Atom)) -> Barnacle(Nil) {
  Barnacle(
    ..default_barnacle(),
    strategy: Strategy(..default_strategy(), discover_nodes: fn() { Ok(nodes) }),
  )
}

pub fn dns(basename: String, hostname_query: String) -> Barnacle(Nil) {
  Barnacle(
    ..default_barnacle(),
    strategy: Strategy(
      ..default_strategy(),
      discover_nodes: fn() { dns.discover_nodes(basename, hostname_query) },
    ),
  )
}

// ----- Actor ----- //

pub type RefreshResult(error) =
  Result(List(atom.Atom), RefreshError(error))

pub opaque type Message(error) {
  Refresh(return: Option(Subject(RefreshResult(error))))
  Stop(return: Option(Subject(Nil)))
  Shutdown(return: Option(Subject(Nil)))
}

pub type BarnacleResponse(error) {
  RefreshResponse(RefreshResult(error))
  StopResponse(Nil)
  ShutdownResponse(Nil)
}

type State(error) {
  State(
    self: Subject(Message(error)),
    barnacle: Barnacle(error),
    timer: Option(process.Timer),
  )
}

pub fn start(barnacle: Barnacle(error)) {
  barnacle
  |> spec(None)
  |> actor.start_spec
}

pub fn child_spec(
  barnacle: Barnacle(error),
  parent: Subject(Subject(Message(error))),
) {
  supervisor.worker(fn(_) {
    barnacle
    |> spec(Some(parent))
    |> actor.start_spec
  })
}

pub fn refresh(
  subject: Subject(Message(error)),
  return: Option(Subject(RefreshResult(error))),
) {
  process.send(subject, Refresh(return))
}

pub fn stop(subject: Subject(Message(error)), return: Option(Subject(Nil))) {
  process.send(subject, Stop(return))
}

pub fn shutdown(subject: Subject(Message(error)), return: Option(Subject(Nil))) {
  process.send(subject, Shutdown(return))
}

pub type RefreshError(error) {
  StrategyError(error)
  ConnectError(List(#(atom.Atom, node.ConnectError)))
  DisconnectError(List(#(atom.Atom, NodeDisconnectError)))
}

fn handle_message(message: Message(error), state: State(error)) {
  let State(self:, barnacle:, timer:) = state
  case message {
    Refresh(return) -> {
      let refresh_result = refresh_nodes(barnacle)

      cancel_timer(timer)
      let timer =
        process.send_after(self, barnacle.poll_interval, Refresh(None))

      send_response(return, refresh_result)
      send_response(barnacle.listener, RefreshResponse(refresh_result))
      actor.continue(State(..state, timer: Some(timer)))
    }
    Stop(return) -> {
      cancel_timer(timer)
      send_response(return, Nil)
      send_response(barnacle.listener, StopResponse(Nil))
      actor.continue(State(..state, timer: None))
    }
    Shutdown(return) -> {
      cancel_timer(timer)
      send_response(return, Nil)
      send_response(barnacle.listener, ShutdownResponse(Nil))
      actor.Stop(process.Normal)
    }
  }
}

fn spec(
  barnacle: Barnacle(error),
  parent: Option(Subject(Subject(Message(error)))),
) {
  actor.Spec(init_timeout: 10_000, loop: handle_message, init: fn() {
    option.map(barnacle.name, process.register(process.self(), _))
    let self = process.new_subject()
    let selector =
      process.new_selector()
      |> process.selecting(self, function.identity)

    option.map(parent, process.send(_, self))

    let timer = process.send_after(self, barnacle.poll_interval, Refresh(None))

    actor.Ready(
      selector: selector,
      state: State(self:, barnacle:, timer: Some(timer)),
    )
  })
}

fn send_response(maybe_client: Option(Subject(a)), response: a) -> Nil {
  option.map(maybe_client, process.send(_, response))
  Nil
}

fn cancel_timer(timer: Option(process.Timer)) -> Nil {
  option.map(timer, process.cancel_timer)
  Nil
}

fn refresh_nodes(barnacle: Barnacle(error)) -> RefreshResult(error) {
  use available_nodes_list <- result.try(
    barnacle.strategy.discover_nodes()
    |> result.map_error(StrategyError),
  )

  use current_nodes_list <- result.try(
    barnacle.strategy.list_nodes()
    |> result.map_error(StrategyError),
  )

  let self = node.self() |> node.to_atom

  let available_nodes =
    available_nodes_list
    |> set.from_list
    |> set.delete(self)

  let current_nodes =
    current_nodes_list
    |> set.from_list
    |> set.delete(self)

  let nodes_to_add = set.difference(available_nodes, current_nodes)
  let nodes_to_remove = set.difference(current_nodes, available_nodes)

  let connect_results =
    barnacle.strategy.connect_nodes(nodes_to_add |> set.to_list)
  use _ <- result.try(connect_results |> result.map_error(ConnectError))

  let disconnect_results =
    barnacle.strategy.disconnect_nodes(nodes_to_remove |> set.to_list)
  use _ <- result.try(disconnect_results |> result.map_error(DisconnectError))

  barnacle.strategy.list_nodes()
  |> result.map_error(StrategyError)
}

// ----- Utils ----- //

fn result_apply(results: List(Result(a, b))) -> Result(List(a), List(b)) {
  case result.partition(results) {
    #(vals, []) -> Ok(vals)
    #(_, errs) -> Error(errs)
  }
}
