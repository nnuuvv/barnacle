-module(barnacle_ffi).

-export([list_local_nodes/0, disconnect_from_node/1]).

list_local_nodes() ->
  case erl_epmd:names() of
    {error, address} ->
      {error, nil};
    {ok, Names} ->
      {ok, [list_to_binary(Name) || {Name, _} <- Names]}
  end.

disconnect_from_node(Node) ->
  case disconnect_node(Node) of
    true ->
      {ok, Node};
    false ->
      {error, failed_to_disconnect};
    ignored ->
      {error, local_node_is_not_alive}
  end.
