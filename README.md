# mset: an OCaml library for small multisets

This module implements a persistent data structure for multisets using
bitmaps, for multisets small enough to fit in a single machine
integer.

The universe (i.e. the elements that can be stored in the multiset
and, for each, its maximal multiplicity) has to be provided
upfront.

## Example

For instance,
```
  module MS = (val chars ['a',12; 'b',42; 'c',27])
```
provides you with a module `MS` that implements multisets containing
at most 12 occurrences of the element `'a'`, at most 42 occurrences of
the element `'b'`, and at most 27 occurrences of the element `'c'`.

Internally, the data type `MS.t` is implemented as a bitset of type
`int`, yet behind an abstract data type. The API is defensive:
functions over multisets fail if they are given elements not belonging
to the universe (i.e. a character other than `'a'`, `'b'`, and `'c'`),
or if the capacity of an element is exceeded.

Elements are not limited to characters. A
[functor](https://backtracking.github.io/mset/mset/Mset/Make/index.html)
allows you to build multisets for any type of elements.

## Documentation

For more details, have a look at the
[documentation](https://backtracking.github.io/mset/mset/Mset/index.html).
