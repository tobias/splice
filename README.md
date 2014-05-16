# Splice

Like the elephant, Splice is many things, depending on where your context and
points of contact:

* a masterless, distributed database
* an Observed-Remove multimap CRDT that presents / implements query facilities
  via a datalog, supporting storage via ...
* a medium for reifying operations into computable values that may be
  replicated over arbitrary topologies and mechanisms to satisfy the
  communication requirements of globally distributed computational applications.

This is the _reference implementation_ of Splice, written in portable Clojure /
ClojureScript for use in any environment that can run a Java or JavaScript VM.

## Mise en garde

_Use may cause personal injury._

First, yes, I'm aware that parts of this may seem very crazy.

While this project is one result of a lot of research and work, it is incredibly
alpha, not production-ready, and absolutely everything is subject to
change. Those familiar with CRDT implementation strategies may be confused by
certain design decisions made so far; again, everything is subject to change,
but I think it's safe to assume that the intended primary field of use for
splice is different than other (most?) implementations.

_This README both overstates and has lagged behind the current state of the
implementation._ This isn't how I like to present projects, but getting things
out into the sunlight despite shortcomings has become a necessity.

Finally, this is just one part (foundational) of Quilt. Much more to come.

With those caveats, check it out.

### Contributing

If you see something wrong with Splice, _please only file an issue for
now_. Pull requests will be accepted after due consideration for and against
having a CLA for Quilt Project subprojects.

## "Installation"

No releases of splice have been made yet. This is intentional. Please feel free
to clone the repo and play around.

## Usage



## Data model

Splice is designed to support many different shapes of data, corresponding to
datatypes that you may be familiar with in various programming languages:

* maps (a.k.a. hashes)
* sets
* graphs
* tables / matrices
* sequences

All of these are modeled in terms of _entities_. Entities are multimaps, each of
which is identified by a _globally-unique_ identifier, called an _eid_.

At the lowest level, each attribute/value entry in each entity is encoded as a
_tuple_, the base unit of data in Splice:

```
[eid attribute value write remove-write]
```

The first four components of Splice tuples in the vector representation are
mandatory; others are optional:

* `eid` is the _globally-unique_ identifier of an entity, i.e. UUIDs.
* `attribute` is an entry key that identifies a particular pair within an
  entity.  Attributes may be any scalar or sequence containing scalars, though
  the most common attributes are keywords.
* `value` may be any scalar, including references.  `nil` is not allowed, except
  in the case of remove tuples; see ["Removals"](#removals) for details.
* `write` is an eid, an implicit reference to the entity that represents the
  write of which a given tuple was a part: tuples may only be added as part of a
  write, which may contain many tuples, and which is the unit of replication and
  distribution.  Each write entity carries metadata about the write.
* Optional components:
  * `remove-write` must be an eid, an implicit reference to an write's entity.
    This correspondence indicates the removal of the attribute/value entry
    represented by the matching tuple.  See ["Removals"](#removals) for details.

(Note that throughout this document, eids are represented as keywords prefixed
with `e` or `w`
[indicating whether the eid in question names a write entity or a "regular" entity],
just for the sake of readability.)

Splice tuples may be represented as a vector or as a map:

```
[:e83 :name "Splice" :w55]
```

is the same as:

```
{:eid :e83
 :a :name
 :v "Splice"
 :write :w55}
```

The latter is somewhat friendlier to the human eye, especially when many of the
optional slots are populated; the former is simply a defined total ordering of
the attributes of a tuple.  When represented as a map, any undefined optional
components of a tuple are presumed to be `nil`; when represented as a vector,
undefined components must be explicitly included as `nil` as necessary in order
to maintain the positions of other defined components in the vector.

### Attributes

Attributes are generally keywords, though there are legitimate uses for numbers
and vectors of both, too.

Re: keywords, need to define:

* which are system-reserved (either all non-namespaced keywords, or all those
  under a particular namespace, or those that adhere to a particular convention
  [e.g. name starting with `_` or `-`?])
  * defaulting to `db`-namespaced keywords right now, though I'm not fond of
    aping Datomic there
* which imply local-only, unreplicated entries

### Value types

Any value type supported by [Sedan](https://bitbucket.com/cemerick/sedan) may be
used as a tuple value.

Note that it is considered good splice modeling to use only scalar values.
Directly encoding aggregates (e.g. vectors) as tuple values has three
significant drawbacks:

* it becomes impossible to query for / access / modify the members of those
aggregates (e.g. the elements in a vector) individually
* consequently, you end up needing to encode/decode the entire aggregate when
  updating / accessing it, implying a potentially significant performance
  penalty
* It is impossible for multiple actors to modify parts of the aggregate
  independently within their local Splice replicas, and have those modifications
  replicate and join with other modifications without conflict.  (i.e. Splice is
  a CRDT, but you can work around / sabotage its conflict-resolution semantics via
  ill-considered use of aggregate tuple values.)

Note that all of the above also applies to any scalar that is decomposable and
may be concurrently changed piece-wise.  e.g. storing the content of a document
as a single string is just as bad as storing it as a single vector of
characters, since different actors cannot sanely make concurrent changes to the
document without coordination or consensus.

See the [modeling](#modeling) section of this document for details on how to
represent aggregates within Splice efficiently and without sabotaging the
"conflict-free" semantics it provides.

#### References

TODO (implemented but undocumented)

Need distinction between inclusive references and simple links

### <a name="removals"></a> Removals

Each tuple represents either the addition or removal of a single attribute/value
entry within the multimap entity identified by its `eid`.  e.g. here is a tuple
representing the `[:name "Jane"]` entry in entity `:e3` that was included in
write `:w4`:

`[:e3 :name "Jane" :w4]`

To remove that entry from `:e3`, another tuple would need to be written that
identifies the same `eid`, `attribute`, and `value`, but has a `remove-write`
component equal to the `write` component of the entry to be removed.  This tuple
satisfies these constraints; when its write is applied to a Splice store that
contains the "Jane" entry, that entry will be removed (it happens to have been a
part of write `:w90`):

`[:e3 :name nil :w90 :w4]`

Note that the same entry may have been added by multiple writes (typically
corresponding to concurrent activity by multiple actors affecting disparate
Splice databases that were actively or later replicated), e.g.:

```
[:e3 :name "Jane" :w4]
[:e3 :name "Jane" :w16]
[:e3 :name "Jane" :w82]
```

In order to logically remove a given entry, one must remove all known tuples
that assert that entry. (This particular semantic is called "observed remove" in
the CRDT literature [where the `write` eid is the "tag"], and is one strategy to
cope with concurrent, uncoordinated changes.)  So, given the above set of tuples
observed to assert the "Jane" entry, one must write out a corresponding set of
remove tuples in order to fully remove it:

```
[:e3 :name "Jane" :w90 :w4]
[:e3 :name "Jane" :w90 :w16]
[:e3 :name "Jane" :w90 :w82]
```

(Replication protocols may batch these to minimize transmission overhead, but
all three tuples must be materialized w.r.t. query at destination replicas.)
 
The "Jane" tuple could later (or concurrently) be re-added to the `:e3` entity;
removing it would again require writing a remove tuple matching the addition
tuple as described earlier.

It is nonsensical to remove a removal tuple; doing so is effectively a no-op
(though it is possible that one will be able to query to find a tuple
attempting to remove a removal tuple).  To revert a removal, one must
write a new tuple fully representing the desired entry(ies).

#### Removals, history, and immutability

Note that "removal" is strictly a logical, non-destructive operation.  Removed
entries are not erased or deleted from the database; they are simply no longer
taken into account when determining the _current_ state of the affected
entities.  In addition, removed entries may be readily accessed by queries that
explicitly walk backwards in history, or those applied to snapshots of the
database that don't include removal tuples.

TODO retention policy, see bakery.md

## Writes and replication

A _write_ is a collection of tuples that are applied to a replica atomically.
Each write contains tuples defining an entity representing metadata about the
write itself, e.g. wall-clock times (when the write was originally made, when it
was replicated locally, etc), optional additional causality information,
indications of who/what was responsible for the write, as well as arbitrary
application-specific metadata.  The eid of the write entity must be in the
`write` slot of each tuple in the write, including the tuples that define the
write entity.

_Replication_ is the (potentially bidirectional) transmission of
tuples from one store to another, called _replicas_.  The objective is to bring
the "destination" replica's cumulative state up to date with that of the
"source" replica's, subject to particulars of replication policy, access
control, and optimizations that do not affect the semantics of e.g. query. (Note
that replication policies may not cause a violation in the atomic write
invariant, i.e. access control policies may not cause only part of a write to be
replicated.)

Splice does not (yet) define its own replication algorithms or transports
(though it does implement a few); the intention is that any suitable replication
mechanism may be used (including those provided by
tuple backends themselves).  The only requirement is that Splice replication
must obey these invariants:

* Writes must be replicated and applied atomically; i.e. at no point should only
part of a write affect/inform a query.
* Writes must be applied to the destination replica in the same order that
  they were applied to the source replica.  (This implies a partial order among all
  writes in the system, and implicitly satisfies the weak causality required by
  OR-sets per Shapiro et al., as well as the causal delivery requirement of
  "operation-based" CRDTs generally. Though, practically speaking, even if a
  write that contained a remove tuple was applied to a replica prior to the
  write that contained the tuple to be removed, all contemplated query systems
  would be unaffected by the violation of causality: until a remove tuple's
  complement arrives, it is logically inert w.r.t. the set invariants.)

Write entities must contain the following attributes:

* wall-clock times
  * original write `:db/otime`
  * local replication (not replicated!)
* Reference to the identity of the writer
* Number of tuples in the write (could be used to enforce atomicity of write
  application if tuple backend's "native", non-tuple-aware replication is being
  used)?
* Later (and optional):
  * cryptographic signature of the write (hard in JS environments)
  * explicit cross-actor causality information
  * markers / signature from transaction / consensus services indicating
    acceptance of a write within a serializable history

### What is replicated where, when, and how is it retained?

(This is all prospective.)

A taxonomy of replication and tuple storage concepts:

* an _origin_ is the replica where a write is originally made, either by a human
  or an automated actor.  It should be incredibly rare for an origin to not
  retain every tuple that was originally written to it (subject to e.g. purging
  and local operational considerations around GC of tuples older than `T`).
* _filters_ are criteria (presumably implemented as a query) that limits the scope of what one
  replica delivers to another during replication
* Tuple storage comes in two flavours, _durable_ and _caching_:

  * Durable storage is where the replica aims to retain received
    tuples permanently (subject to purging and transient semantics).
  * Caching storage is where the replica retains tuples only to optimize the
    responsiveness of local queries.

  Which mix of storage a replica uses is a local policy decision, but there are
  some general expectations:

  * Origin replicas should keep their original writes' tuples in durable storage
  *
* _Peering_ is where a replica aims to collect all tuples from its
  counterpart(s) into durable storage
* _Tethering_ is where a replica performs replication solely in response to
  query / immediate activity, primarily utilizing caching storage

Formally, there is little to distinguish one replica from another: any replica
can peer with another given sufficient resources, and any replica can tether to
another if it has permission to utilize the latter's resources to assist in
querying.  However, there is likely to be an obvious topological pattern, where
very large nodes (just a logical abstraction over very large fleets of hardware)
provide authoritative, long-term durable storage; these will peer most
frequently.  The least-connected members of the topology will likely be tethered
to one or more of these large peering nodes, though it is possible that nodes
around the periphery of the graph will also peer from time to time, often with
significant filtering.   

     +----------------+       +------------------+
     |                |Peering|                  |
     |                +------->                  |
     |                <-------+                  |
     |                |       |                  |
     +-^---------^--^-+       +-^---------------^+
       |         |  |           |               | 
       |Tethering|  +---------+ |               |
       |         |            | |               |
  +----V+     +--v---+      +-v-v---+         +-v----+ 
  |     |     |      |      |       <---------+      |
  |     |     |      |      |       +--------->      |
  +-----+     +------+      +-------+ Peering +------+ 


## Snapshotting

Filtering on write time(s) (there are many to choose from, potentially) allows
queries to operate on snapshots of database from different timelines.

Multiple definitions of "consistency" exist given Splice's distribution model,
each one governed by a different basis of time (three potential ones listed
here, there may be more):

1. when writes were applied to the local replica
2. when writes were _originally_ made (this will be identical to #1
  for writes made locally, as opposed to those that are replicated in, which
  will always have replication times that come after their original write time)
3. when writes are marked as "accepted" by an external consistency service,
  e.g. as part of a proposed then accepted transaction

Which mode one uses as the basis of queries defines the stability of results
over time.  e.g. mode 3 and mode 1 (if incoming replication is disabled, or if
queries are constructed to ignore writes replicated in from elsewhere) will
always present consistent histories; meanwhile, as writes are replicated in,
mode 2 will present histories that change based on the relative clock skew of
remote actors replicating in near real-time, the simple passage of time between
original timestamping of writes to "offline" replicas and the replication of
those writes elsewhere, and even "malicious" timestamping of original writes.

### <a name="modeling"></a> Modeling flexibility

Anything that can be modeled in terms of OR-sets should work well.

#### Sets

The values in multimaps are sets.

#### Maps

It's a multimap already, which is sufficient / superior for a vast subset of
applications.

Enforcing a single-value constraint per attribute (or deterministically choosing
a single value given concurrent additions of different values) would require a
transaction / consensus mechanism.

#### PN-Counters

`write` "tags" are not sufficient to represent sources of increments and
decrements to PN-Counters.  Each tuple's value needs to differ in order to
preserve each "operation" given OR-set join semantics.  i.e., this doesn't work:

```
[:e1 :cnt 1 :w1]
[:e1 :cnt 1 :w2]
[:e1 :cnt -1 :w3]
[:e1 :cnt 1 :w4]
```

All of the duplicate values (`1` or `:inc`, or whatever you want to use to
represent increments and decrements) will be joined away; writes 2 and 4 would
have no effect on query.  This works, though:

```
[:e1 :cnt ["foo" 1] :w1]
[:e1 :cnt ["bar" 1] :w2]
[:e1 :cnt ["baz" -1] :w3]
[:e1 :cnt ["chi" 1] :w4]
```

Each of the strings in the value vectors should be globally-unique.  This can be
reasonably queried to yield all values participating in the counter at
`[:e1 :cnt]`, which has a value of `3` given the above.

#### Composites / entity references

(This is obsolete given much more recent work on entity references, fences.)

```
{:a :x :b {:c :y :db/id 2} :db/id 1}


[:e1 :a :x]
[:e1 :b #db/ref[:e2]]
[:e2 :c :y :wx]
[:wx :ts/local 1234 :wx]
```

or, if we want to refer to a particular version of an entity:

```
;; (At time 1235:)
{:c :z :db/id 2}
{:a :x :b {:c :y :db/id 2} :db/id 1}

[:e1 :a :x]
[:e1 :b #db/ref[:e2 :wy]]
[:e2 :c :y :wx]
[:wx :ts/local 1234 :wx]
[:e2 :c :z :wy]
[:wy :ts/local 1235 :wy]

```

`#db/ref[2 1234]` being a tagged literal, indicating a reference to entity `:e2`
at time `1234` (the time component being optional, in case a reference should
track downstream changes).  The write tag could even be included there, which
would lock a reference to a particular revision of a tuple (thus excluding any
that match the entity and time constraints but were concurrently written on
another replica).

#### (Partially-ordered) sequences

TODO (implemented, not documented)

#### Graphs

```
       fence
  +----+   +-----+
  |              |
  |      P       |
  |  ___/|\___   |
  br/    |    \span
         |
       strong

; ignoring write tags here
[1 :tag :p]
[1 :ref/html 2]
[1 :ref/html 3]
[1 :ref/html 4]
[2 :tag :br]
[2 :rank 0.0]
[3 :tag :span]
[3 :rank 1.0]
[4 :tag :strong]
[4 :rank 0.5]
[5 :fence true]
[5 :start 2]
[5 :end 3]
```

#### Dense tabular data

```
    A  B  C
   ---------
1 | x  y  z
2 | a  b  c
3 | i  j  k

[ts tag m [1 'A] x]
[ts tag n :schema q]
```

(i.e. `:schema` could point to a row- or column- (or even cell-) based schema
needed by programs consuming the data)

TODO the vector of [row number, column name] as tuple attribute is intuitive,
and enforces total order, but means that column and row insertions are not
possible given concurrent edits (and are a nasty business even with a single
actor).  Just how painful would using ranks be here for real work/data?

#### Binary

Binary data stored in Splice should be chunked into reasonable sizes (so as to
keep each tuple to a polite size), and gathered into logical blobs by
partially-ordered references (which, for all practical scenarios, are surely
made totally-ordered since the originator of a blob creates it all at once, and
can assert the ordering of each chunk; can't think of any use case where
_chunks_ are going to be added concurrently by uncoordinated actors).  Blobs are
thus very readily removed; each replica can make a local policy decision re: how
aggressively to replicate / retain the corresponding chunks.  Conversely, chunk
tuples should probably never be removed (if only because of the size cost,
though that is probably just advisory), leaving chunks totally immutable (even
logically).  In any case, default replication policy should probably _skip_
binary-valued tuples, potentially triggering replication of them from upstream
replica(s) only when references to them are de-referenced.

#### What doesn't splice readily support?

* "intuitional" counters
* registers beyond LWW and MV varieties
* Anything that isn't amenable to local and remote convergence semantics of CRDTs
* Anything else requiring PLOP semantics
* ...?

All of the above could be achieved/supported via consensus mechanisms across splice replicas / actors TBD.

### Query (high-level walkthrough)

#### Tuple scan

Let's assume covering indices stored in an SSTable

Assuming covering indices, this is straightforward:

`subseq` (or equivalent for the particular backend/storage being used) with
desired beginning and ending tuples. If you only have one end of the range, and
results should be open on the other, a "top" or "bottom" tuple needs to be
constructed, e.g. "ZZZZZZZZZZZZZZZZZZZZZZZZ" when used as an end bound if the
data in question were characterized as a string. (Hopefully the representation
in question is good enough to provide a dedicated top and bottom value so such
hacks aren't required; sedan fits the bill here.)

Encoding/decoding boundaries denoted by '@':

```
    +------------------+               +----------------------+
    |                  |               |   Durable SSTable    |
    |     "Apps"       |               |----------------------|
    |                  +----------+    |                      |
    +--^+--------------+          |    |   +------------+     |
       @|                         +----@---> +-----------+    |
       ||    +---------------+         |   | | +-----------+  |
       ||    |               <---------|   | | |           |  |
       ||    | Tuple scan(s) |         |   +-|-|  Tuples   |  |
       ||    |               +---+     |     +-|           |  |
       ||    +-----+------+--+   |     |       +-----------+  |
       ||          |      |      |     |                      |
       |@          |      |      |     +-----------^----------+
 +-----+v----------v---+  |      |                 |
 |                     |  |      +-------------+   |
 |      "Query"        |  |                    |   |
 |  Joins, @predicates,|  |                    |   |
 |  @expressions,      |  |                    |   |
 |  @aggregations      |--+------              |   |
 |                     |  |     |              |   |
 +------------+^-------+  |     |              |   |
              ||          |     |              |   |
              ||          |     |              |   |
    +---------v+----------v-+   |            +-v---+-------+
    |                       |   +------------>             |  Replica-1
 ~~~|      Tethering        |~~~~~~~~~~~~~~~~|   Peering   |~~~~~~~~~~~~~~
    |                       | Replica-3   ~  |             |  Replica-2
    +---------+^------------+             ~  +----+^-------+
              ||                          ~       ||
              ||                          ~       ||
              ||                          ~       ||
              v|                          ~       v|
```

For many (most?) replication scenarios, tuples will not need to be decoded at
all. Only if the replication criteria requires runtime predicate filtering
would decoding be neccessary to yield query results that inform replication.

#### "Full" queries



### Implementation: Queries

Referring to the small graph example above:

###### Attribute lookup

Given a `[eid a]` pair (`[1 :tag]`), give me its value (`#{:p}`)

###### Entity attributes

Given an `eid` (`1`), give me all of its attribute names (`#{:tag :children}`).

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

## "Formal" "Specification"

_(Part of my earliest thinking through CRDTs and the shape of the data model. May
not be relevant / representative anymore.)_

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

## TODO

### Design

* Various semantics around consistent snapshot, causality, and (partial)
  ordering of updates is all hand-wavy.  In particular:
  * What does it mean for a reference value to refer to HEAD or latest? Write
    timestamps are straightforward, but surely run into all the usual BS with
    wall clocks, and get really strange given long-lived partitions.  What
    happens when a reference is tied to a particular rev of an entity, which
    itself refers to other entities in a floating manner?
  * To what extent should causality be encoded in splice to start? OR-set
    semantics are necessarily causal for removes, but nothing else.  Is that
    sufficient for a large enough set of possible applications?
  * What does "consistent snapshot" mean in the context of floating revs?
* Identity
  * scheme for uniquely identifying writers
  * scheme for uniquely identifying humans in the system?
  * maybe "strings" is as far as we can go at this level, let identity
    definition / verification be just another service?
* Trust
  * writes, once written, can't be tampered with; requires signing of writes in
    a verifiable way
  * malicious actors replicating entirely spoofed writes WTF
  * eids need to be globally unique; but, what happens when a poorly-implemented
    app starts spitting out eids that aren't unique?  Similar but potentially
    even more costly would be entity modifications that are replicated by a
    malicious actor.  Do we need...?:

    * mechanisms for backing out writes from suspect replicas (what happens if
    * blackballing particular replicas that are known to be a source of bad
      tuples (similar to spam blacklists)
    * signing writes in a verifiable way (so problematic writes can be tracked
      to their authors, thus providing disincentive)

    None of this is a problem to start, when (a) the number of authoritative
    sources of tuples is very low, and (b) the software that is producing tuples
    is produced by a small number of organizations.  It's an issue when
    untrusted actors attempt to co-replicate, without any trusted authority in
    the middle / arbitrating.  "Just use UUIDs" isn't a plan; think Globally.
* Efficient reactive query is a must: esp. if replication is to be driven off of
  access control properties, ensuring that a query can emit a result immediately
  upon receiving e.g. the final missing tuple is critical compared to the
  fallback prospect of re-running each "reactive" query after each write
  completes.  More generally, reactive query is needed by any kind of autonomous
  tool / agent.
* Datalog / query:
  * scan range queries are OK, but just a quarter of the solution to the problem
    of leveraging indexes to drive ordering of results:
	* All of the runtime query logic uses clojure.set operations, which all
      operate on "rels", sets of maps.  Fine enough, but all ordering from
      underlying indexes is lost in the process.  We'll probably need to
      reimplement key clojure.set operations so they'll work with sequences
      and/or channels, and/or use
      [insertion-order-preserving set impls](https://github.com/flatland/ordered).
	  * this is not to say that duplicates should be allowed in results, just
        that any deduplication needs to occur without affecting order, so a
        mechanism like `distinct` or similar.
	* ordering of results needs to be made explicit (perhaps with planning-time
      checks to ensure that user-requested ordering doesn't conflict with
      scan-range match clauses?...maybe not)
	  
* How should data purging / truncation be supported?  Contemplated supporting it
  directly in the data model, rejected it (see below), but a mechanism for
  e.g. purging data prior to X for entities Y for attributes Z should be
  supported.  Challenges include:
  * How does this affect local query (i.e. the data is gone, but should
    queries/apps be able to detect this)?
  * How does this manifest itself mechanically? Policy, management operation, or
    other, or both?

### Implementation(s)

* (partially) ordered data
  * Rank strings work, but do not support any notion of either unique
    disambiguators or statistically unique midpoints (so e.g. concurrent edits
    at the same "position" in some text by multiple actors will yield
    interleaved content).  Lots of wiggle-room for addressing that, will get
    back to it when it matters more.

## Design decision non sequiturs

* Struggled a while thinking about how to best represent remove operations.  The
  challenge is that a tuple representing a removal/retraction needs to
  identify the existing tuple whose value it is affecting (effectively
  shadowing it at read/query time, though an implementation might actually
  modify the removed tuple to indicate this so that it doesn't have to do
  subqueries to determine whether tuples that match query clauses have actually
  been removed or not).  There is the further question of how to do this in a
  way that leads to natural and efficient query.
  
  There are 3.5 approaches AFAICT:

  1. A remove tuple must include all components of the tuple to be removed. This
  is the most direct transliteration of the specification of OR-sets, but forces
  a potentially large cost for removals, since removal tuples have to re-state
  the full eid/a/v/write to identify the tuple to be removed.
  2. A remove tuple could include only the _hash_ of the value of the tuple to be
    removed.  This caps the size of the remove tuple, but complicates: who does
    the hashing, when, is the value hash stored with the original tuple or
    (re)calculated at each replica, what encoding of values is to be used as the
    basis for a hash (if sedan, then that would be forever tying splice to
    a _specific revision_ of sedan, inextricably so).
  3. Tuples each get _their own_ globally-unique ID (`tid`).  This makes it easy
  to name existing tuples, but may actually yield more waste than pushing around
  removed values, since removals are likely to be a minority case, but this
  approach would require all tuples ever to carry a tid in the off chance that
  they'll eventually be removed.
    * Alternatively, tuples each get their own _optional_ ID.  i.e. when the
  value in a tuple is larger than a potential tuple ID, then the latter is
  added.  If it's not, then a removal tuple has to restate the value itself.
    * Alternatively alternatively, tuple IDs can be very small: they can be any
      scalar unique within the write they were a part of (just sequentially
      numbered would work, since each write originates from a single source).
      The identifier for each tuple is then compound, `[write tid]`.  All
      tuples already have to identify the write eid (since that's the tag for
      the entry, in OR-set terms), so adding a (small, much smaller than a UUID)
      might be an acceptable cost vs. treble damages on removals if values need
      to be restated.

  Option 1 requires the re-statement of tuple values upon remove, at least when
  replicating (whether a particular replica implements remove by storing the
  tuple as transmitted or not is an implementation / optimization detail); this
  could be very expensive when removing entries with large values.  Option 2 is
  stupid-complicated, as is option 3.1.  Option 3.2 was briefly selected because
  it imposes a minimial cost (to all tuples, unfortunately) in order to avoid
  potential treble damages just because a user wanted to remove an entry with a
  large value (that just multiply given multiple tuples encoding the same
  logical entry).  However, it was eventually abandoned because it carries its
  own complexity: tuples end up being identifiable in two different ways (`eavw`
  as well as `w / tid`), and the use of the latter for removes implied a join
  for _every_ tuple in order to determine if it was removed or not (something
  that perhaps could be optimized away given suitably clever tuple storage
  and/or query impl, but I don't want to require "clever" at this point).  Thus,
  option 1 is selected; generality > efficiency, at least for now.
* Contemplated attempting to incorporate purging/excision into the tuple
  representation as a particular type of removal with specific operational
  (side-effecting) semantics, but abandoned the notion. The decision to require
  inclusion of values in removes (described above) made representing purges that
  much more difficult (since including the value to be purged in the purging
  tuple is pointless).  More importantly, insofar as splice datasets are
  replicated, the elimination / destruction of historical data would have always
  been advisory. If a tuple has not been replicated elsewhere, and one's local
  replica is configured to do so, then one can have confidence that history has
  actually been purged; given other circumstances, one is relying upon the good
  faith of prior replication participants.  Thus, purging can be performed as a
  purely local operation of a particular splice implementation, but isn't
  encoded in the data model at all. Implementations that provide a purge
  operation/API should write a tuple/entity representing the purge (so it's
  queryable, particular representation TBD), but that data should be considered
  local-only, and not subject to replication by default.
* Decided against storing binary data elsewhere (e.g. S3 or similar), and
addressing it via references.  Not providing a solution for binary data would
imply expecting/enforcing particular semantics (i.e. immutable chunks/blobs) on
the accompanying blobstore(s) (which would always be a point of confusion /
contention), and necessitate the entire topology either maintaining a separate
replication mechanism for binary data, or having to be online in order to access
referenced binary data (again, from e.g. S3).  _Maybe_ the former is actually
reasonable (and effectively true given implicitly different default replication
policy for binary-valued tuples), but we can consider it an optimization at the
moment, not an inherent data modeling question.
* "Distributed query" (i.e. scanning for tuples both locally and across N
  replicas the local is tethered to) is a no-go for now.  The complexity and
  costs for doing this are just impossible to contemplate right now, and it's
  not a must-have for the first spikes of the first apps.  Someday.  For now,
  all queries either run locally, or maybe could be sent to tethered replicas
  for execution there.
* `nil` is disallowed as a tuple value, except for removal tuples, where it is
  effectively standing in for "undefined"

## Related work

* CRDTs, writ large
  *
    ["A comprehensive study of Convergent and Commutative Replicated Data Types"](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf)
* Datomic, data modeling and programming model /ht's
* Linda / TupleSpaces
  *
  ["Linda in Context"](http://www.ece.rutgers.edu/~parashar/Classes/ece451-566/slides/carriero.pdf):
  the first few pages are a cogent outline of what computation should be, modulo
  some flawed premises (in particular, binding a particular model of computation
  up with the particulars of the tuple space data model, and the implicit
  selection of coordinated/exclusive writes via blocking `read`, etc)
* Operational Transforms
* So, so much more. At some point, developing a full and proper splice
  bibliography will be mandatory, credit where credit is due, etc.

## Thanks

## License

Copyright © 2012-* Chas Emerick.

(Sane OSS license TBD.)
