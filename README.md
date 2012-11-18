# sharded_eredis

**sharded_eredis** is collection of pools of Redis clients, using eredis
and poolboy. Each pool points to a different shard.

This fork modifies the
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

## Setup

- git clone git://github.com/jeremyong/sharded_eredis.git
- cd eredis_pool
- make get-deps
- make

## Testing

make check

## Settings

edit src/sharded_eredis.app.src

```erlang
 {application, sharded_eredis,
  [
   {description, ""},
   {vsn, "1"},
   {registered, []},
   {applications, [
                   kernel,
                   stdlib
                  ]},
   {mod, { sharded_eredis_app, []}},
   {env, [
           {pools, [
                    {default, [
                               {size, 10},
                               {max_overflow, 20}
                              ]}
                   ]}
         ]}
  ]}.
```

Add new pools. This configuration specifies 4 shards.

```erlang
  {env, [
          {pools, [
                   {pool0, [
                              {size, 10},
                              {max_overflow, 20}
                              {host, "127.0.0.1"},
                              {port, 6378}
                             ]},
                   {pool1, [
                              {size, 30},
                              {max_overflow, 20},
                              {host, "127.0.0.1"},
                              {port, 6379}
                             ]},
                   {pool2, [
                              {size, 20},
                              {max_overflow, 20},
                              {host, "127.0.0.1"},
                              {port, 6380},
                             ]}
                   {pool3, [
                              {size, 20},
                              {max_overflow, 20},
                              {host, "127.0.0.1"},
                              {port, 6380},
                             ]}
                  ]}
        ]}
```


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
query. The library will automatically determine which pool associated
to the correct shard should be invoked.
 
The Redis documentation can be referred to [here](http://redis.io/commands).
