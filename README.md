# sharded_eredis

Copyright (c) 2012 PlayMesh, Inc.

**sharded_eredis** is collection of pools of Redis clients, using eredis
and poolboy. Each pool points to a different shard.

This project modifies the
[original](https://github.com/hiroeorz/eredis_pool) repository by including a
consistent hashing library to simplify presharding, suggested by
antirez. Read the original blogpost
[here](http://oldblog.antirez.com/post/redis-presharding.html).

The name has been changed so as not to induce confusion for those
originally using hiroeorz's sharded_eredis; the functionality has changed
significantly.

eredis:
https://github.com/wooga/eredis

poolboy:
https://github.com/devinus/poolboy

## Caveats!!

This library uses Redis but in a distributed fashion. Multi-object
operations are no longer guaranteed to be atomic and are not supported
by this library. Examples include multisets, multigets,
source-destination operations, and
multi-object transactions. Transactions technically still work if all
keys involved in the transaction are on the same machine but the Redis
transaction was not built to be distributed.

## Setup

- git clone git://github.com/jeremyong/sharded_eredis.git
- cd eredis_pool
- make get-deps
- make

## Testing

make test

## Config

Add new pools. This configuration specifies 4 shards.

```erlang
  {env, [
          {global_or_local, local}
          {pools, [
                   {pool0, [
                              {size, 10},
                              {max_overflow, 20}
                              {host, "127.0.0.1"},
                              {port, 6378}
                             ]},
                   {pool1, [
                              {size, 10},
                              {max_overflow, 20},
                              {host, "127.0.0.1"},
                              {port, 6380}
                             ]},
                   {pool2, [
                              {size, 10},
                              {max_overflow, 20},
                              {host, "127.0.0.1"},
                              {port, 6381},
                             ]}
                   {pool3, [
                              {size, 10},
                              {max_overflow, 20},
                              {host, "127.0.0.1"},
                              {port, 6382},
                             ]}
                  ]}
        ]}
```

You can include this using the erlang `-config` option.

The `global_or_local` option is used to determine whether the pools
are registered as local or global. Personally, I start this
application on every instance and run pools on each instance locally
to minimize network latency.


## Examples

application start.
```erlang
 sharded_eredis:start().
 ok
```

key-value set and get
```erlang
 sharded_eredis:q(["SET", "foo", "bar"]).
 {ok,<<"OK">>}
 
 sharded_eredis:q(["GET", "foo"]).       
 {ok,<<"bar">>}
```

Note that the name of the pool does not need to be supplied with each
query (in contrast to prior behavior). The library will automatically determine which pool associated
to the correct shard should be invoked.
 
The Redis documentation can be referred to [here](http://redis.io/commands).
