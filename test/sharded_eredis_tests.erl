-module(sharded_eredis_tests).

-include_lib("eunit/include/eunit.hrl").

-import(eredis, [create_multibulk/1]).

-define(Setup, fun() -> application:start(sharded_eredis)  end).
-define(Clearnup, fun(_) -> application:stop(sharded_eredis)  end).

basic_test_() ->
    {inparallel,

     {setup, ?Setup, ?Clearnup,
      [

       { "get and set",
         fun() ->
                 ?assertMatch({ok, _}, sharded_eredis:q(["DEL", foo1])),

                 ?assertEqual({ok, undefined}, 
                              sharded_eredis:q(["GET", foo1])),

                 ?assertEqual({ok, <<"OK">>}, 
                              sharded_eredis:q(["SET", foo1, bar])),

                 ?assertEqual({ok, <<"bar">>}, 
                              sharded_eredis:q(["GET", foo1]))
         end
        },

       { "delete test",
         fun() ->
                 ?assertMatch({ok, _}, sharded_eredis:q(["DEL", foo2])),

                 ?assertEqual({ok, <<"OK">>}, 
                              sharded_eredis:q(["SET", foo2, bar])),

                 ?assertEqual({ok, <<"1">>}, 
                              sharded_eredis:q(["DEL", foo2])),

                 ?assertEqual({ok, undefined}, 
                              sharded_eredis:q(["GET", foo2]))
         end
        },

       { "mset and mget",
         fun() ->
                 Keys = lists:seq(1, 1000),

                 ?assertMatch({ok, _}, sharded_eredis:q(["DEL" | Keys])),

                 KeyValuePairs = [[K, K*2] || K <- Keys],
                 ExpectedResult = 
                     [list_to_binary(integer_to_list(K * 2)) || K <- Keys],

                 ?assertEqual({ok, <<"OK">>}, 
                              sharded_eredis:q(["MSET" | lists:flatten(KeyValuePairs)])),

                 ?assertEqual({ok, ExpectedResult}, 
                              sharded_eredis:q(["MGET" | Keys])),

                 ?assertMatch({ok, _}, sharded_eredis:q(["DEL" | Keys]))
         end
        }

      ]
     }
    }.

