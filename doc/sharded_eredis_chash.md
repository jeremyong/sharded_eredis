

#Module sharded_eredis_chash#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Copyright (c) (C) 2012, PlayMesh, Inc.
-------------------------------------------------------------------

__Authors:__ Jeremy Ong ([`jeremy@playmesh.com`](mailto:jeremy@playmesh.com)).
<a name="types"></a>

##Data Types##




###<a name="type-cnode">cnode()</a>##



<pre>cnode() = term()</pre>



###<a name="type-cring">cring()</a>##



<pre>cring() = [{integer(), <a href="#type-cnode">cnode()</a>}]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_ring-1">create_ring/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td>The consistent hash ring spans from 0 to 2^160 - 1.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="create_ring-1"></a>

###create_ring/1##


<pre>create_ring(Shards::[term()]) -> <a href="#type-cring">cring()</a></pre>
<br></br>


<a name="lookup-1"></a>

###lookup/1##


<pre>lookup(Key::term()) -> <a href="#type-cnode">cnode()</a></pre>
<br></br>


The consistent hash ring spans from 0 to 2^160 - 1.