# crdt

CRDTs for port79.

## Observed-Remove MultiMap

### "Specification"

Following the pattern of Shapiro et al.'s process of building a data type from
its sequential specification:

`M = #{K #_> #{V V' V'' …}}`
  
#### Sequential spec

```
{} (add K V) {K #_> #{V}}
{K #_> #{V}} (add K V') {K #_> #{V V'}}
{K #_> #{V V'}} (lookup K) #{V V'}
{K #_> #{V}} (remove K) {}
```

#### Concurrent operations and options

`{… …} (add K V) || (remove K) {?? ??}`

##### How to choose between concurrent add and remove?

* <s>linearizable</s>
* error state?
* LWW?
* **add wins?**
* remove wins?

`{… …} (add K V) || (add K V') {?? ??}`

##### How to choose between concurrent adds with same key?

* <s>linearizable</s>
* error state?
* LWW?
* **multimap**
  * <s>All values are MV-registers</s>
  * map effectively a set of key+tag, each with a payload V
* _V- or K-specific join fn?_ (which can be any of the above, or other) (maybe
  later)

### Implementation thoughts

Should allow us to model damn near anything.

Atomic unit similar to Datomic datoms, called _tuples_ for now:

`[entity-id attribute value t]`

TODO: do we have a better term than "entity"?

* entity-ids need to be UUIDs, _always_.  There's no coordinator here, so new
  entity identifiers must be able to be safely generated anytime, anywhere.
(Note that entity-ids are listed using numerals and other non-unique values in
examples below just for the sake of readability.)
* attributes can be anything; really, they're attribute keys, especially if one
  wants to characterize each entity as a sub-multimap of the global multimap.
* values can be anything
* `t` needs to be more than a timestamp (and obviously can't be a transaction
  id/number), and more than the unique tag described in the implemention of
Observed-Remove sets. (Recall above; a map is "effectively a set of key+tag,
each with a payload V".)  Likely needs to be a reference to another entity, so
we can track:
  * authored timestamp
  * system timestamp (i.e. when it hits our servers, vs. when it was authored
    in an offline environment)
  * other write metadata

Filtering on `t` is what will allow us to trivially obtain consistent snapshots
— for limited sets of entities, or for the entire map.

Again riffing on "a map is effectively a set of key+tag, each with a payload
V", each tuple can be characterized as a map entry like so:

`[[eid a] [v t]]`

…especially insofar as we want to be able to efficiently look up the value of a
particular attribute of a particular entity, and we want to be able to easily
characterize discrete changes to an entity (i.e. we don't want to try to
construct a join operation over map values).

Now, how the tuple(s) are arranged in storage is entirely an implementation
detail for each backend.

#### Updates

Updates to an existing key should be a combination remove/add (using identical
timestamps so as to make it effectively atomic).

#### Deletes

We want to preserve history, but ensure that deletes are properly taken into
consideration during query.

Potential approaches:

###### Use a tombstone: same eid, same a, same value (TODO uh, that's gonna get pricey for large values), but include a `:delete` entry in the t.

```
[1 :length 5 {:time 1354222006831 :actor "service handle name" ...}]
[1 :length 5 {:time 1354222009983 :actor "..."}]
```

###### _Update_ the tuple's t with the same sort of information that otherwise appears in t, but tweaked so as to indicate the different operation.  e.g.:

```
[1 :length 5 {:time 1354222006831 :actor "service handle name"
              :deleted/time 135422200998 :deleted/actor "..."}]
```

The latter nicely eliminates the need to:

1. write a new tuple (including a potentially bulky value)
2. compensate for the deletion "tombstone" at query time (just check a value in
   a tuple's t rather than removing a tuple from an in-process query result)

However, it:

1. blows away the tuple's immutability, and therefore cache-friendly nature
2. is a dirty stinking hack :-P

###### Clever alternative

What if t was really just a (unique) "tag", and the information associated with
it was stored elsewhere?  Then we could:

1. write a "tombstone" tuple _without a value_ (cheap!)
2. modeling would be friendlier: no needing to hack a distinction between
   add/remove t data

However:

1. a join (or equivalent) would be needed to pair up t data with its
   corresponding tuple

### What is `t`!?

Nothing here makes it clear.  Gotta 'decomplect' (ugh).

* a unique tag necessary to implement Observed-Remove semantics
* a system timestamp associated with the operation
* a userland timestamp associated with the operation (this is a concession to
  the reality that changes might be made by an offline user long before it
trickles into the database)  (TODO this is screwy; what happens when there's
multiple databases, run by others, and two databases are merged?  This is
either a problem, or a premature optimization for a time where there will be >
1 datastores.)
* a bag of attributes associated with the _operation_, not the tuple itself.
  e.g.:
  * instigating user/handle/"agent" (for automated/programmatic operations)
  * auditing info
  * etc. etc etc

Tuples: `[timestamp tag e? a v?]`

```
(if e
  (if v
    :attribute-assertion
    :attribute-removal)
  (do (assert v) :operation-metadata))  
```

Operation meta: `[timestamp tag nil a v]`

(The particular layout of tuple entries is storage-specific; shown here with:

1. Optional `v` entry at the end, and
2. Timestamp/tag pair prefixing the "actual" tuple data

because:

1. Makes schema/queryies for e.g. postgres implementation obvious
2. when representing tuples/meta as vectors/arrays, can efficiently map index
   => entry
3. keeping timestamp/tag first allows for easy consistent snapshotting and
   range-based time "querying" (just a sorted set))

Example addition, with meta, and removal, with meta:

```
[1354222006831 "tUUID1" "eUUID" :tag :p]
[1354222006831 "tUUID1" nil :user "cemerick"]
[1354222006831 "tUUID1" nil :source "interactive"]
[1354222006831 "tUUID1" nil :app "wiki app"]
[135422200998 "tUUID1" "eUUID" :tag]   ;; this is the 'remove' operation/tuple: no _v_alue
[135422200998 "tUUID1" nil :user "cemerick"]
[135422200998 "tUUID1" nil :source "auto-reformatter"]
[135422200998 "tUUID1" nil :app "wiki app"]
```

Tuple values are not defined for deletes _only_.  The absence of a value in a
tuple is what marks it as a remove/delete.

Operation metadata can never be "deleted" (paired with tombstones), so it
doesn't need its own tags.

The "eid" for operation metadata tuples is compound because we need to use the
same tag when deleting a tuple.  It's the timestamp that distinguishes between
meta on an add operation and meta on a remove operation.

### Multi-datastore capability

i.e. for when the core of this is open sourced, and everyone can run a
foundational port79 instance?

Almost surely a premature notion.  But, right away, a clear issue:  how to
reconcile the potential merging of two CmRDTs?  Can an op log be repeated, e.g.
just send the tuples from one instance to another?

(holy shit, we don't need a CmRDT given tuples...just propagate them in order
of system timestamp?!  I suppose that would cause all sorts of difficulty if
the destination instance was accepting operations at the same time.  Just make
the replication a synchronous operation?  Have I been designing a CvRDT all
this time and didn't realize it?!)

TODO: review the Shapiro paper that goes into detail on CvRDT vs. CmRDT.

### Modeling flexibility

#### Graphs

```
         P
     ___/|\___
  br/    |    \span
         | 
       strong

[ts tag 1 :tag :p]
[ts tag 1 :ref/html 2]
[ts tag 1 :ref/html 3]
[ts tag 1 :ref/html 4]
[ts tag 2 :tag :br]
[ts tag 2 :rank 0.0]
[ts tag 3 :tag :span]
[ts tag 3 :rank 1.0]
[ts tag 4 :tag :strong]
[ts tag 4 :rank 0.5]
```

#### Dense tabular data

```
    A  B  C
   ---------
1 | x  y  z
2 | a  b  c
3 | i  j  k

[ts tag m #{1 A} x]
[ts tag n :schema q]
```

(i.e. `:schema` could point to a row- or column- (or even cell-) based schema
needed by programs consuming the data)

An optimization for truly large datasets would be to store only a reference to
an efficient "external" datastore (maybe Cassandra would be right here?).

#### Maps

It's a multimap already; just need to be able to enforce a single-value
constraint per key (or deterministically choose a single value given concurrent
additions of different values).

#### Sets

The values in multimaps are sets.  Done.

#### Binary

Depending on the backend, large binaries are just values, wrapped in a
streaming interface or delay.

#### RDF

What does it mean for this datastore to interoperate or be backwards-compatible
with existing RDF datastores?

### Implementation: Queries

Referring to the small graph example above:

###### Attribute lookup

Given a `[eid a]` pair (`[1 :tag]`), give me its value (`#{:p}`)

###### Entity attributes

Given an `eid` (`1`), give me all of its attribute names (`#{:tag :children}).

###### Entity lookup

Given an `eid` (`1`), give me all of its attributes as a map (`{:tag #{:p}
:children #{2 3}}`).

###### Recursive walk

Given an `eid` (`1`) and a set of keys to traverse (`#{:children}`), give me:

* the eids of all transitively-referenced entities (`#{2 3}`), OR
* a composite entity, rooted at the entity with the eid specified in the query,
  with referenced entities replacing their references (`{:tag #{:p} :children #{{:tag :br} {:tag :span}}}`)

#### General principles

* All queries must be parameterizable by time, default being "now".
* Any complete entities retrieved must contain their eid under a special key
  e.g. `:db/id #db/id "...UUID..."`

### Future considerations

* optional consistency constraints
  * per-key
  * per-value-type
  * scoped within/below values accessible under a particular key
* multiple backends suitable for geographical distribution and low latency
  (Riak!)

### Storage backend notes

Regardless of the backend(s) used, the _model_ has to be the same.  A port79
instance running on riak should interoperate / replicate with another running
on postgres, etc.

#### Options

* postgres
  * all the query you could ever need
  * fast and efficient
  * good hosting options
  * shardable and easily staffed if it becomes a long-term solution
  * simply not distributed => high latency for anyone distant
* Riak
  * Lots of potential here: multi-datacenter replication, 2i, lucene search,
    link walking, riak_pipe, riak CS for large binaries, more
  * no starter hosting options (@riak\_on is working on it...?)
  * No clear recursive query options, comparable to recursive CTEs in PG.  
* redis?
* voldemort?
* cassandra/hbase/...   — way too complicated to start with

#### Unworkable

* couchdb
  * impossible to produce even the most trivial of aggregates from views
    (largely due to `reduce_limit`, but the problem — an empty object is
returned when e.g. 50 values are in a map — appears to occur even when that is
set to false)
  * json documents + always-HTTP => fat
* dynamodb
  * Can only query by primary key
* mongo: gtfo
* graph databases (e.g. neo4j, orient, etc) seem ill-suited; though we have
  references and such, and modeling graphs will be a primary use case:
  * entities are most naturally vertices, but representing change over time in
    graph databases is exceedingly cumbersome and inefficient
  * graph databases do not generally appear to be shardable, never mind fully
    distributed vs. e.g. Riak
  * storing dense data will always be inefficient in all possible ways
* datomic
  * awesome, but enforces linearizability of _all_ operations; definitionally
    impossible to distribute or scale horizontally

## License

Copyright © 2012 TODO

