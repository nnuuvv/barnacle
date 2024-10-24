-module(barnacle_demo_ffi).

-export([disconnect_from_node/1]).

disconnect_from_node(Node) ->
  case disconnect_node(Node) of
    true ->
      {ok, Node};
    false ->
      {error, failed_to_disconnect};
    ignored ->
      {error, local_node_is_not_alive}
  end.

