# netaddr

NETADDR is a Common Lisp library for manipulating IP addresses, subnets, ranges,
and sets. It is inspired by its namesake library in Python,
[netaddr](https://github.com/netaddr/netaddr). NETADDR supports/provides:

* IPv4 and IPv6 addresses, subnets, and ranges.
* Shorthand syntax for the above, e.g., `#I("192.168.1.0/24)`, `#I(::-ffff::)`,
  and `#I("0.0.0.0", "1.1.1.1")` do what you would expect. This can be enabled
  by calling `(NETADDR:ENABLE-IP-SYNTAX)` before usage.
* Helper lookup functions for reserved space, e.g., `PRIVATE?`, `RESERVED?`, and
  `PUBLIC?`.
* An `IP-SET` data structure for working with sets of addresses, subnets, and
  ranges.
* Set operations on the above like union, intersection, difference, and
  symmetric difference.
* Membership checking of IPs against subnets, ranges, and sets.
