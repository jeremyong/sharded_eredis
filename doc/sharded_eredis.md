

#Module sharded_eredis#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.

Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)), Jeremy Ong ([`jeremy@playmesh.com`](mailto:jeremy@playmesh.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#q-1">q/1</a></td><td>
Executes the given command in the specified connection.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#transaction-2">transaction/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="q-1"></a>

###q/1##


<pre>q(Command::iolist()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
<br></br>



Executes the given command in the specified connection. The
command must be a valid Redis command and may contain arbitrary
data which will be converted to binaries. The returned values will
always be binaries.<a name="q-2"></a>

###q/2##


<pre>q(Command::iolist(), Timeout::integer()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
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

