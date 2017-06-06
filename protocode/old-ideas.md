;; oldish ideas

Space Graph
-----------
Scene graphs suck, just store spaces. Also use system to heavily
optimize transform lookups
Nodes are not stored in the graph AT ALL

Spaces
------
- space is defined in terms of transform
- transform cached as matrix
- created from any transform whether point and quat or matrix
- structure is: id, transform, dead-flag
- spaces have parents by being in the space-tree, a space with not
parent is in :model space
- model space is singular. It is not the position of a node. Nodes
could create their own spaces
- if required, this is what camera do, can also be used for hierarchical nodes

Space Tree
----------
- Hierarchy of spaces
- SpaceNode is struct of space and list of children
- Symbiotic with space and api treats both as one thing
- Means getting/cloning a space doesnt show it's children, that is
seperate query.
  also keeps children out of transform-lookup...is that good?

Add New Space
-------------
the transform from parent to child is cached immediately as well
transforms from any fundamental
parent spaces.

Space Update
------------
Called on spacenode,
- changes transform in space.
- walks marking child spaces with :dirty flag
- how does this relate to fundamental nodes and transform caching?


Fundamental Spaces
------------------
Spaces with index below some n are fundamental
They can be looked up directly in dedicated pool...how does this
affect transform lookup?

Transform Cache Lookup
----------------------
- space objs in cache node shared with space tree
- take first 8 bits of :from index and :to index
- smaller is first n steps, larger is second n-1 steps (n depends of depth etc)
  this gives us one pool containing cached results for both
directions, last n is lookup-flag.
  cache :to->:from :from->:to. This is for locality as both dirty together
- cache-node is :from-space :to-space :to-transform, :from-transform
(inverse) & :call-count.
  Inverse is cached less optimistically
- :call-count is visit count since the TGC (transform garbage
collector) was last here.


TGC - transform garbage collector
---------------------------------
- incrementally walks cache tree (expected one pool/block per frame)
- incrementally bubble-sorts pool/s, one pass
- deletes any node where (= :call-count 0)


SPC - space garbage collector
-----------------------------
- incrementally removes dead spaces


Unknowns/Experiments
--------------------
- currently is required to free the space resource, can we use weak
pointers? what is the performance hit?
