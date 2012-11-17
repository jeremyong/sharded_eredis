-module(eredis_pool_chash).

-export([lookup/1, create_ring/1]).

%% @doc The consistent hash ring spans from 0 to 2^160 - 1.
-define(RINGUPPER, trunc(math:pow(2,160) - 1)).

-type cnode() :: term().
-type cring() :: [{integer(), cnode()}].

-spec lookup(term()) -> cnode().
lookup(Key) ->
    Ring = application:get_env(eredis_pool, ring),
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

-spec create_ring([term()]) -> cring().
create_ring(Shards) ->
    NumShards = length(Shards),
    Interval = ?RINGUPPER div NumShards,
    Points = [ X * Interval || X <- lists:seq(0, NumShards-1) ],
    lists:zip(Points, Shards).

-spec hash(binary() | list()) -> integer().
hash(Key) when is_binary(Key) ->
    <<Hash:160/integer>> = crypto:sha(Key),
    Hash;
hash(Key) ->
    <<Hash:160/integer>> = crypto:sha(list_to_binary(Key)),
    Hash.
