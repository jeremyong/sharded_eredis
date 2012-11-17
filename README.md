# eredis_pool

eredis_pool is Pool of Redis clients, using eredis and poolboy.

This fork modifies the
[original](https://github.com/hiroeorz/eredis_pool) by including a
consistent hashing library to simplify presharding, suggested by
antirez. Read the original blogpost
[here](http://oldblog.antirez.com/post/redis-presharding.html).

eredis:
https://github.com/wooga/eredis

poolboy:
https://github.com/devinus/poolboy

## Setup

- git clone git://github.com/jeremyong/eredis_pool.git
- cd eredis_pool
- make get-deps
- make

## Testing

make check

## Settings

edit src/eredis_pool.app.src

```erlang
 {application, eredis_pool,
  [
   {description, ""},
   {vsn, "1"},
   {registered, []},
   {applications, [
                   kernel,
                   stdlib
                  ]},
   {mod, { eredis_pool_app, []}},
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
 eredis_pool:start().
 ok
```

key-value set and get
```erlang
 eredis_pool:q(["SET", "foo", "bar"]).
 {ok,<<"OK">>}
 
 eredis_pool:q(["GET", "foo"]).       
 {ok,<<"bar">>}
```
 
Other commands are here.
http://redis.io/commands
