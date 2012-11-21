-module(sharded_eredis_tests).

-include_lib("eunit/include/eunit.hrl").

-import(eredis, [create_multibulk/1]).

-define(Setup, fun() -> application:start(sharded_eredis)  end).
-define(Cleanup, fun(_) -> application:stop(sharded_eredis)  end).

basic_test_() ->
    {inparallel,

     {setup, ?Setup, ?Cleanup,
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
        }

      ]
     }
    }.

