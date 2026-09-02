# netaddr

NETADDR is a zero dependency Common Lisp library for manipulating IP addresses,
subnets, ranges, and sets. It is inspired by its namesake library in Python,
[netaddr](https://github.com/netaddr/netaddr). Tests pass on SBCL, ECL,
ABCL, CCL, and LispWorks. NETADDR supports/provides:

* Datatypes for IPv4 and IPv6 addresses, subnets, and ranges.
* Shorthand syntax for the above with a reader macro `#i`. See the [IP
  Syntax](#IP-syntax) section for details.
* Helper lookup functions for RFC reserved space, e.g., `PRIVATE?`, `RESERVED?`, and
  `PUBLIC?`.
* An `IP-SET` data structure for working with sets of addresses, subnets, and
  ranges. See `MAKE-IP-SET`.
* Set operations on the above like union, intersection, difference, and
  symmetric difference.
* Membership checks of IPs against subnets, ranges, and sets using `CONTAINS?`.

## Class Hierarchy

```
                           ┌ ─ ─ ─ ┐
                   ┌───────   IP+   ────────┐
                   │       └ ─ ─ ─ ┘        │
                   │                        │
                   │                        │
                   │                        │
                   ▼                        ▼
               ┌ ─ ─ ─ ┐              ┌──────────┐
                IP-LIKE ◀─ ─ set of─ ─│  IP-SET  │
               └ ─ ─ ─ ┘              └──────────┘
                   │
                   ├──────┐
      ┌────────────┘      ▼
      │               ┌ ─ ─ ─ ┐
      │             ┌─ IP-PAIR ──┐
      ▼             ▼ └ ─ ─ ─ ┘  ▼
┌──────────┐  ┌──────────┐ ┌──────────┐
│IP-ADDRESS│  │IP-NETWORK│ │ IP-RANGE │
└──────────┘  └──────────┘ └──────────┘
```

Users of this library will only instantiate the leaf classes in the tree above,
using their respective `MAKE-*` functions, or in the case of the three that
inherit from `IP-LIKE`, the short-hand `#i` notation. `IP-SET`s are comprised of
a set of `IP-LIKE`s. Most operations will expect either `IP-LIKE`s as arguments
and/or `IP+`s. For example, `CONTAINS?` takes an `IP+` as its first argument and
an `IP-LIKE` as its second argument because:

* An `IP-ADDRESS` `CONTAINS?` itself.
* An `IP-NETWORK` and an `IP-RANGE` `CONTAINS?` themselves, any subset of those
  networks or ranges, and any `IP-ADDRESS` that is a member of the network or
  range.
* An `IP-SET` `CONTAINS?` any of its member `IP-LIKE`s, and so on. When it
  does, the most specific member containing the argument is returned, so
  `CONTAINS?` on an `IP-SET` is also a longest prefix match; `LONGEST-MATCH` is
  the same operation under a name that makes that intent clear.

`IP-SET`s index their members lazily on first query, so membership tests and
longest prefix matches cost O(log n) regardless of how many networks a set
holds, and the set theoretic operations only examine members that actually
overlap. Mutating a set (`ADD!`, `ADDNEW!`, `SUB!`) maintains the index
incrementally.

## Equality

There are two equality operators for `IP+` subclasses:

* `IP-EQUAL` (aliased to `IP=`)
* `IP-EQUALP`

Similar to Common Lisp's EQUAL and EQUALP, `IP-EQUAL` is more specific than
`IP-EQUALP`. The former considers different classes to always be unequal, while
the latter allows comparisons across all leaf classes described in the [Class
Hierarchy](#Class-Hierarchy). For example:

```
NETADDR> (ip-equal #i1.1.1.1 #i1.1.1.1/32)
NIL
NETADDR> (ip-equalp #i1.1.1.1 #i1.1.1.1/32)
T
NETADDR> (ip-equalp #i1.1.1.1 #i1.1.1.1/31)
NIL
NETADDR> (ip-equal #i1.0.0.0/8 #i1.0.0.0-1.255.255.255)
NIL
NETADDR> (ip-equalp #i1.0.0.0/8 #i1.0.0.0-1.255.255.255)
T
NETADDR> (ip-equal (make-ip-set #i(1.1.1.1)) (make-ip-set #i(1.1.1.1/32)))
NIL
NETADDR> (ip-equalp (make-ip-set #i(1.1.1.1)) (make-ip-set #i(1.1.1.1/32)))
T
```

`IP-EQUAL` always returns NIL if classes are different. However, `IP-EQUALP`
returns T if the underlying object refers to the same set of IP addresses,
regardless of the concrete object type. In general, if you are comparing
individual `IP-LIKE`s, you'll want to use `IP-EQUAL`. If you are comparing
`IP-SET`s, which may contain a mixture of classes internally, or `IP-NETWORK`s
and `IP-RANGE`s, you'll want to use `IP-EQUALP`.

## IP Syntax

NETADDR provides a shorthand syntax for writing `IP-LIKE`s with the reader
macro `#i`, enabled by calling `ENABLE-IP-SYNTAX`. An address, network, or
range written directly after `#i` reads as a single object; a parenthesized,
whitespace-separated list of them reads as a list of objects. An element may
also be a string, or `,FORM` to use the string that `FORM` evaluates to at run
time. Example usage is shown below:

```
NETADDR> #i1.2.3.4
#<IP-ADDRESS 1.2.3.4>
NETADDR> #i192.168.1.0/24
#<IP-NETWORK 192.168.1.0/24>
NETADDR> #i::-ffff::
#<IP-RANGE ::-ffff::>
NETADDR> #i(0.0.0.0 1.1.1.1)
(#<IP-ADDRESS 0.0.0.0> #<IP-ADDRESS 1.1.1.1>)
NETADDR> (multiple-value-bind (x y z) (values "1.1.1.1" "::/96" "10.20.30.40-11.20.30.40")
           #i(,x ,y ,z))
(#<IP-ADDRESS 1.1.1.1> #<IP-NETWORK ::/96> #<IP-RANGE 10.20.30.40-11.20.30.40>)
NETADDR> (let ((prefix "10.0.0.0")) #i,(format nil "~a/8" prefix))
#<IP-NETWORK 10.0.0.0/8>
```

`ENABLE-IP-SYNTAX` copies the current readtable and adds `#i` to it, so other
reader extensions you have enabled are kept; within a file being compiled or
loaded the change is local to that file, and `DISABLE-IP-SYNTAX` restores the
previous readtable. To use the syntax without changing `*READTABLE*`, bind it
to `*IP-SYNTAX-READTABLE*`, or install `IP-READER` as the `#i` dispatch macro
in a readtable of your own (for example with `named-readtables`'
`:dispatch-macro-char`).
