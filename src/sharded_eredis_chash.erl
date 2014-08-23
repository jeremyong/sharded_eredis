%%%-------------------------------------------------------------------
%%% @author Jeremy Ong <jeremy@playmesh.com>
%%% @copyright (C) 2012, PlayMesh, Inc.
%%%-------------------------------------------------------------------

-module(sharded_eredis_chash).

-export([lookup/1, create_ring/1, store_ring/0]).

%% @doc The consistent hash ring spans from 0 to 2^160 - 1.
-define(RINGUPPER, trunc(math:pow(2,160) - 1)).

-type cnode() :: term().
-type cring() :: [{integer(), cnode()}].


-spec lookup(term()) -> cnode().
lookup(Key) ->
    Ring = case application:get_env(sharded_eredis, ring) of
               undefined ->
                   store_ring();
               {ok, Ring1} ->
                   Ring1
           end,
    Hash = hash(Key),
    {{_, Node}, {_, Max}} =
        lists:foldl(fun({C, CNode}, {{L,LNode}, {U,UNode}}) ->
                            case C =< Hash of
                                true ->
                                    case C > L of
                                        true ->
                                            {{C, CNode}, {U, UNode}};
                                        false ->
                                            {{L, LNode}, {U, UNode}}
                                    end;
                                false ->
                                    case C > U of
                                        true ->
                                            {{L, LNode}, {C, CNode}};
                                        false ->
                                            {{L, LNode}, {U, UNode}}
                                    end
                            end
                    end,
                    {{-1, undefined}, {-1, undefined}},
                    Ring),
    case Node of
        undefined ->
            Max;
        _ ->
            Node
    end.

-spec store_ring() ->
    cring().
store_ring() ->
    {ok, Pools} = application:get_env(sharded_eredis, pools),
    {Nodes, _} = lists:unzip(Pools),
    Ring = sharded_eredis_chash:create_ring(Nodes),
    ok = application:set_env(sharded_eredis, ring, Ring),
    Ring.

-spec create_ring([term()]) -> cring().
create_ring(Shards) ->
    NumShards = length(Shards),
    Interval = ?RINGUPPER div NumShards,
    Points = [ X * Interval || X <- lists:seq(0, NumShards-1) ],
    lists:zip(Points, Shards).

-spec hash(binary() | list()) -> integer().
hash(Key) when is_binary(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, Key),
    Hash;
hash(Key) when is_atom(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, list_to_binary(atom_to_list(Key))),
    Hash;
hash(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, list_to_binary(Key)),
    Hash.
