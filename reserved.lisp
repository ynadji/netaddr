(in-package :netaddr)

;; TODO: These should probably be IP-SETs when the time comes.

;; IPv4 special subnets
(defvar *ipv4-loopback* (list #I("127.0.0.0/8"))) ; LIST for consistency.
(defvar *ipv4-rfc-1918* #I("10.0.0.0/8" "172.16.0.0/12" "192.168.0.0/16"))
(defvar *ipv4-link-local* (list #I("169.254.0.0/16")))
(defvar *ipv4-multicast* (list #I("224.0.0.0/4")))
(defvar *ipv4-6to4* (list #I("192.88.99.0/24")))
(defvar *ipv4-reserved*
  (append #I("0.0.0.0/8" "192.0.2.0/24" "240.0.0.0/4" "198.51.100.0/24" "203.0.113.0/24" "203.0.113.0/24" "233.252.0.0/24")
          *ipv4-loopback*
          *ipv4-rfc-1918*
          *ipv4-link-local*))
(defvar *ipv4-not-globally-reachable*
  (append *ipv4-reserved*
          #I("100.64.0.0/10" "192.0.0.0/24" "192.0.0.170/31" "198.18.0.0/15" "255.255.255.255/32")))
(defvar *ipv4-not-globally-reachable-exceptions*
  #I("192.0.0.9/32" "192.0.0.10/32"))

;; IPv6 special subnets
(defvar *ipv6-lookback* (list #I("::1/128")))
(defvar *ipv6-unique-local* (list #I("fc00::/7")))
(defvar *ipv6-link-local* (list #I("fe80::/10")))
(defvar *ipv6-multicast* (list #I("ff00::/8")))
(defvar *ipv6-reserved*
  #I("ff00::/12" "::/8" "0100::/8" "0200::/7" "0400::/6" "0800::/5" "1000::/4" "4000::/3"
     "6000::/3" "8000::/3" "a000::/3" "c000::/3" "e000::/4" "f000::/5" "f800::/6" "fe00::/9"))
(defvar *ipv6-not-globally-reachable*
  (append #I("::/128" "::ffff:0:0/96" "64:ff9b:1::/48" "100::/64" "2001::/23" "2001:db8::/32" "2002::/16")
          *ipv6-lookback*
          *ipv6-unique-local*
          *ipv6-link-local*))
(defvar *ipv6-not-globally-reachable-exceptions*
  #I("2001:1::1/128" "2001:1::2/128" "2001:3::/32" "2001:4:112::/48" "2001:20::/28" "2001:30::/28"))

;; Merged special subnets
(defvar *private* (append *ipv4-rfc-1918*))
(defvar *non-routable* (append *ipv4-not-globally-reachable* *ipv6-not-globally-reachable*))
(defvar *non-routable-exceptions* (append *ipv4-not-globally-reachable-exceptions*
                                          *ipv6-not-globally-reachable-exceptions*))
(defvar *reserved* (append *ipv4-reserved* *ipv6-reserved*))

;; Lookup functions
(defun private? (ip)
  (member ip *private* :test #'in-set?))

(defun public? (ip)
  (ax:if-let ((exception (member ip *non-routable-exceptions* :test #'in-set?)))
    exception
    (not (member ip *non-routable* :test #'in-set?))))

(defun reserved? (ip)
  (member ip *reserved* :test #'in-set?))
