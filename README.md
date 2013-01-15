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

Atomic unit similar to Datomic datoms, called _units_ for now:

`[entity-id attribute value tag]`

TODO: the terminology around unit/tag/entity/attribute/value is still weak

This is a "unit":

```
[#entity "cea77880-6610..."
 :name
 "port79"
 #tag "a5cde-45cc-..."]
```

It consists of four parts, `[e a v tag]`:

1. an **entity id**, a unique token associated with a logical
   entity/identity.  Each entity can have many attributes.  (Alternative names:
entity subject item name element cell)
2. an **attribute key**, a value identifying the attribute
   stored by this unit.  Multiple units can provide values for the same
entity/attribute pair, making for natural sets and multimaps.  (Alterantive
names: predicate field slot bit cell register part feature aspect)
3. a **value**, the value for this unit (Alternative names: value
   object datum bit)
4. a **tag**, a unique token associated _only_ with the operation that created
   the unit

* `tag` identifies a single write (which may convey many units, including
  "meta" units describing metadata about the write itself; at a minimum write
metadata _always_ includes a timestamp indicating when the write occurred) 
* `e` and `tag` need to be globally unique, _always_.  (This implies the use of
  UUIDs, but does not require it, as long as the identifiers used are globally
unique; in particular, different encodings of the same notion are reasonable).
There's no coordinator here, so new entity and operation identifiers must be
able to be safely generated anytime, anywhere.  (Note that entity-ids are
listed using numerals and other non-unique values in examples below just for
the sake of readability.)
* attributes can be anything; really, they're attribute keys, especially if one
  wants to characterize each entity as a sub-multimap of the global multimap.
* values can be anything

Filtering on `time` is what will allow us to trivially obtain consistent snapshots
— for limited sets of entities, or for the entire map.

Again riffing on "a map is effectively a set of key+tag, each with a payload
V", each tuple can be characterized as a map entry like so:

`[[eid a] [v [time tag]]]`

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

Tuples: `[e a v e']`

```
(if e
  (if v
    :attribute-assertion
    :attribute-removal)
  (do (assert v) :operation-metadata))  
```

Operation meta: `[e' a v timestamp e']`

Tuple removal/retraction: `[e a v e'' e']`

Example addition, with meta, and removal, with meta:

```
["eUUID" :tag :p "tUUID1"]
["tUUID1" :user "cemerick" "tUUID1"]
["tUUID1" :source "interactive" "tUUID1"]
["tUUID1" :app "wiki app" "tUUID1"]
["tUUID1" :time 1354222006831 "tUUID1"]
;; this is the 'remove' operation/tuple, naming the value and the write that produced it
;; this gives us Observed-Remove semantics for value removal
["eUUID" :tag :p "tUUID2" "tUUID1"]
["tUUID2" :user "cemerick" "tUUID2"]
["tUUID2" :source "auto-reformatter" "tUUID2"]
["tUUID2" :app "wiki app" "tUUID2"]
["tUUID2" :time 135422200998 "tUUID2"]
```

The #tombstone pair literal containing the `[tag value]` of the value to be removed
is what indicates a remove.

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

### TODO "Provenance" / authorship verification

If distributed replicas are to be managed by many parties that don't
necessarily trust each other, then a method must be devised to identify trusted
data vs. untrusted data.  Further, it should be possible for a datastore to
maintain parallel (potentially contradictory) sets of data related to
particular entity(ies), authored by different people/organizations/data
sources.

Git's use of SHAs to maintain a verifiable audit trail of sorts should be
looked at.  Combined with signing of updates in some capacity, that should
allow distributed replicas to consume units written elsewhere with as little or
as much trust as they determine is appropriate.

Good overview of how git hashes data, combines references to object SHAs with
their filenames in trees:

  http://www-cs-students.stanford.edu/~blynn/gitmagic/ch08.html

If each write contains a hash of its contents (in the write metadata,
presumably? But surely the write metadata must also be included in the
calculation of the write's hash?), that includes by reference a hash of any
prior write, then any downstream replica would be able to verify the write as
complete (or not).  Pair this with signing of the hash and/or the contents of
the write, and a replica could verify that a write is intact/untampered-with. 

### Indexing

It is not clear how to determine what indexes to maintain.

As a stupid first step, Datomic's indexes include:

* :eavt
* :aevt
* :avet contains datoms for attributes where :db/index = true.
* :vaet contains datoms for attributes of :db.type/ref

We don't have any notion of :db/index, so #3 can be ignored for now.

`:vaet` makes sense to support efficient reference lookups for tree/graph traversal (in this case, finding referrers, not referents).

Why no `:t*`?

### Query

* [magic
  sets](https://encrypted.google.com/search?hl=en&q=%22magic%20sets%22%20datalog);
I'm not sure what I've implemented so far, but it's surely not thoroughly
efficient or well-grounded
* [implementing datalog with
  core.logic](http://martinsprogrammingblog.blogspot.co.uk/2012/07/replicating-datomicdatalog-queries-with.html)
— very simple alternative to a "proper" datalog implementation, if necessary

[e  a  v  tag]
[e  a  v  tag?]
[e  a  v? tag]
[e  a  v? tag?]
[e  a? v  tag]
[e  a? v  tag?]
[e  a? v? tag]
[e  a? v? tag?]
[e? a  v  tag]
[e? a  v  tag?]
[e? a  v? tag]
[e? a  v? tag?]
[e? a? v  tag]
[e? a? v  tag?]
[e? a? v? tag]
[e? a? v? tag?]

----

Exchange with Marc Shapiro:

> My thoughts so far are along the lines of using something like a Merkle tree
> to determine when the state corresponding to a particular update has been
> completely received — at which point, the CRDT's API can then safely include
> that state in queries, etc.  I see that Riak already uses Merkle trees during
> read-repair and (apparently, it's not open source) their multi-datacenter
> replication; this makes me think I'm sniffing on the right track, even though
> Riak's usage of Merkle trees won't be helpful to me at an 'application'
> level.
> 
> Something along these lines may be desirable anyway, in service of ensuring
> data integrity and detecting tampering when replicating with untrusted
> parties or over untrusted channels.

It's an interesting thought.  I'm not sure Merkle tree is the right approach,
because of concurrent updates.  You may want to look at this publication about
signing updates in the presence of concurrent updates in CRDTs:
http://www.loria.fr/~ignatcla/pmwiki/pmwiki.php/Main/PublicationsByYear?action=bibentry&bibfile=ref.bib&bibref=TruongGroup12

(Title: Authenticating Operation-based History in Collaborative Systems,
 Hien Thi Thu Truong)

### Modeling flexibility

#### Composites / entity references

```
;; (At time 1234:)
{:a :x :b {:c :y :db/id 2} :db/id 1}


[1234 "tag1" 1 :a :x]
[1234 "tag1" 1 :b #db/ref[2]]
[1234 "tag1" 2 :c :y]
```

or, if we want to refer to a particular version of an entity:

```
;; (At time 1235:)
{:c :z :db/id 2}
{:a :x :b {:c :y :db/id 2} :db/id 1}

[1235 "tag2" 2 :c :z]
[1235 "tag2" 1 :b #db/ref[2 1234]]
```

`#db/ref[2 1234]` being a tagged literal, indicating a reference to entity `2`
at time `1234` (the time component being optional, in case a reference should
track downstream changes).  The write tag could even be included there, which
would lock a reference to a particular revision of a unit (thus excluding any
that match the entity and time constraints but were concurrently written on
another replica).

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

