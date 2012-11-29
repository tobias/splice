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
````

#### Concurrent operations and options

`{… …} (add K V) || (remove K) {?? ??}`

##### How to choose between concurrent add and remove?

* <s>linearizable</s>
* error state?
* LWW?
* **add wins?**
* remove wins?

{… …} (add K V) || (add K V') {?? ??}

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

Atomic unit similar to Datomic datoms:

`[[entity-id key] value] t`

(Entity UUIDs are listed using numerals and other non-unique values in examples
below just for the sake of readability.)

`t` needs to be more than a timestamp, and the unique tag described to
implement Observed-Remove sets. (Recall above; a map is "effectively a set of
key+tag, each with a payload V".)  Likely needs to be a reference to another
entity, so we can track:

* authored timestamp
* system timestamp (i.e. when it hits our servers, vs. when it was authored in
  an offline environment)
* other write metadata

Not sure yet where `t` (or the reference to the above data) fits into the
datum. 

### Graphs

```
     P
    / \
  br   span

[[1 :tag] :p] t
[[1 :children] 2] t
[[1 :children] 3] t
[[2 :tag] :br] t
[[3 :tag] :span] t
```

### Dense tabular data

```
    A  B  C
   ---------
1 | x  y  z
2 | a  b  c
3 | i  j  k

[[∂ #{1 A}] x] t
[[p :schema] ∆] t
```

(i.e. `:schema` could point to a row- or column- (or even cell-) based schema
needed by programs consuming the data)

### Maps

It's a multimap already; just need to be able to enforce a single-value
constraint per key (or deterministically choose a single value given concurrent
additions of different values).

### Sets

The values in multimaps are sets.  Done.



### Binary

Depending on the backend, large binaries are just values, wrapped in a
streaming interface or delay.

### Future considerations

* optional consistency constraints
  * per-key
  * per-value-type
  * scoped within/below values accessible under a particular key
* multiple backends suitable for geographical distribution and low latency (Riak!)


## License

Copyright © 2012 FIXME

