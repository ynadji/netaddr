# netaddr

NETADDR is a Common Lisp library for manipulating IP addresses, subnets, ranges,
and sets. It is inspired by its namesake library in Python,
[netaddr](https://github.com/netaddr/netaddr). NETADDR supports/provides:

* IPv4 and IPv6 addresses, subnets, and ranges.
* Shorthand syntax for the above with a reader macro `#I`. See the [IP
  Syntax](#IP-syntax) section for details.
* Helper lookup functions for reserved space, e.g., `PRIVATE?`, `RESERVED?`, and
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
                                    │
                       ┌────────────┼────────────┐
                       │            │            │
                       │            │            │
                       ▼            ▼            ▼
                 ┌──────────┐ ┌──────────┐ ┌──────────┐
                 │IP-ADDRESS│ │IP-NETWORK│ │ IP-RANGE │
                 └──────────┘ └──────────┘ └──────────┘
```

Users of this library will only instantiate the leaf classes in the tree above,
using their respective `MAKE-*` functions, or in the case of the three that
inherit from `IP-LIKE`, the short-hand `#I` notation. `IP-SET`s are comprised of
a set of `IP-LIKE`s. Most operations will expect either `IP-LIKE`s as arguments
and/or `IP+`s. For example, `CONTAINS?` takes an `IP+` as its first argument and
an `IP-LIKE` as its second argument because:

* An `IP-ADDRESS` `CONTAINS?` itself.
* An `IP-NETWORK` and an `IP-RANGE` `CONTAINS?` themselves, any subset of those
  networks or ranges, and any `IP-ADDRESS` that is a member of the network or
  range.
* An `IP-SET` `CONTAINS?` any of its member `IP-LIKE`s, and so on.

## Equality

## IP Syntax

NETADDR provides a shorthand syntax for defining `IP-LIKE`s from strings with
the reader macro `#I` that can be enabled by first calling `ENABLE-IP-SYNTAX`.
If a single argument is provided, a single object is returned. If multiple
arguments are provided, a list of objects is returned. Example usage is shown
below:

```
NETADDR> #I("1.2.3.4")
#<IP-ADDRESS 1.2.3.4>
NETADDR> #I("192.168.1.0/24")
#<IP-NETWORK 192.168.1.0/24>
NETADDR> #I("::-ffff::")
#<IP-RANGE ::-ffff::>
NETADDR> #I("0.0.0.0" "1.1.1.1")
(#<IP-ADDRESS 0.0.0.0> #<IP-ADDRESS 1.1.1.1>)
NETADDR> (multiple-value-bind (x y z) (values "1.1.1.1" "::/96" "10.20.30.40-11.20.30.40")
           #I(x y z))
(#<IP-ADDRESS 1.1.1.1> #<IP-NETWORK ::/96> #<IP-RANGE 10.20.30.40-11.20.30.40>)
```
