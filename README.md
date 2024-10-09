# netaddr

NETADDR is a Common Lisp library for manipulating IP addresses, subnets, ranges,
and sets. It is inspired by its namesake library in Python,
[netaddr](https://github.com/netaddr/netaddr). NETADDR supports/provides:

* IPv4 and IPv6 addresses, subnets, and ranges.
* Shorthand syntax for the above, e.g., `#I("192.168.1.0/24")`, `#I("::-ffff::")`,
  and `#I("0.0.0.0" "1.1.1.1")` do what you would expect. This can be enabled
  by calling `ENABLE-IP-SYNTAX` before usage.
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
and/or `IP+`s. For example, any of the leaf classes can be used with `CONTAINS?`
because:

* An `IP-ADDRESS` `CONTAINS?` itself.
* An `IP-NETWORK` and an `IP-RANGE` `CONTAINS?` themselves, any subset of those
  networks or ranges, and any `IP-ADDRESS` that is a member of the network or
  range.
* An `IP-SET` `CONTAINS?` any of its member `IP-LIKE`s.

## Equality

