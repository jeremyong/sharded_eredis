

#Module sharded_eredis#
* [Function Index](#index)
* [Function Details](#functions)


Copyright (c) (C) 2012, PlayMesh, Inc.
-------------------------------------------------------------------

__Authors:__ Jeremy Ong ([`jeremy@playmesh.com`](mailto:jeremy@playmesh.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#q-1">q/1</a></td><td>Query the redis server using eredis.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td></td></tr><tr><td valign="top"><a href="#q2-2">q2/2</a></td><td>q2 is similar to q but allows the user to specify the node.</td></tr><tr><td valign="top"><a href="#q2-3">q2/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#transaction-2">transaction/2</a></td><td>Supply a key and a function to perform a transaction.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="q-1"></a>

###q/1##


<pre>q(Command::iolist()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
<br></br>


Query the redis server using eredis. Automatically hashes
the key and selects the correct shard<a name="q-2"></a>

###q/2##


<pre>q(Command::iolist(), Timeout::integer()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
<br></br>


<a name="q2-2"></a>

###q2/2##


<pre>q2(Node::term(), Command::iolist()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
<br></br>


q2 is similar to q but allows the user to specify the node<a name="q2-3"></a>

###q2/3##


<pre>q2(Node::term(), Command::iolist(), Timeout::integer()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
<br></br>


<a name="start-0"></a>

###start/0##


`start() -> any()`

<a name="stop-0"></a>

###stop/0##


`stop() -> any()`

<a name="transaction-2"></a>

###transaction/2##


`transaction(Key, Fun) -> any()`

Supply a key and a function to perform a transaction.
It is NOT CHECKED but assumed that the function only performs operations
on that key or keys sharing the same node as that key.