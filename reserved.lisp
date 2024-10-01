(in-package :netaddr)

(enable-ip-syntax)

;; IPv4 special subnets
(defvar *ipv4-loopback* (make-ip-set (list #I("127.0.0.0/8"))))
(defvar *ipv4-rfc-1918* (make-ip-set #I("10.0.0.0/8" "172.16.0.0/12" "192.168.0.0/16")))
(defvar *ipv4-link-local* (make-ip-set (list #I("169.254.0.0/16"))))
(defvar *ipv4-multicast* (make-ip-set (list #I("224.0.0.0/4"))))
(defvar *ipv4-6to4* (make-ip-set (list #I("192.88.99.0/24"))))
(defvar *ipv4-reserved*
  (ip-set-union (make-ip-set #I("0.0.0.0/8" "192.0.2.0/24" "240.0.0.0/4" "198.51.100.0/24" "203.0.113.0/24" "203.0.113.0/24" "233.252.0.0/24"))
                *ipv4-loopback*
                *ipv4-rfc-1918*
                *ipv4-link-local*))
(defvar *ipv4-not-globally-reachable*
  (ip-set-union *ipv4-reserved*
                (make-ip-set #I("100.64.0.0/10" "192.0.0.0/24" "192.0.0.170/31" "198.18.0.0/15" "255.255.255.255/32"))))
(defvar *ipv4-not-globally-reachable-exceptions*
  (make-ip-set #I("192.0.0.9/32" "192.0.0.10/32")))

;; IPv6 special subnets
(defvar *ipv6-lookback* (make-ip-set (list #I("::1/128"))))
(defvar *ipv6-unique-local* (make-ip-set (list #I("fc00::/7"))))
(defvar *ipv6-link-local* (make-ip-set (list #I("fe80::/10"))))
(defvar *ipv6-multicast* (make-ip-set (list #I("ff00::/8"))))
(defvar *ipv6-reserved*
  (make-ip-set #I("ff00::/12" "::/8" "0100::/8" "0200::/7" "0400::/6" "0800::/5" "1000::/4" "4000::/3"
                  "6000::/3" "8000::/3" "a000::/3" "c000::/3" "e000::/4" "f000::/5" "f800::/6" "fe00::/9")))
(defvar *ipv6-not-globally-reachable*
  (ip-set-union (make-ip-set #I("::/128" "::ffff:0:0/96" "64:ff9b:1::/48" "100::/64" "2001::/23" "2001:db8::/32" "2002::/16"))
                *ipv6-lookback*
                *ipv6-unique-local*
                *ipv6-link-local*))
(defvar *ipv6-not-globally-reachable-exceptions*
  (make-ip-set #I("2001:1::1/128" "2001:1::2/128" "2001:3::/32" "2001:4:112::/48" "2001:20::/28" "2001:30::/28")))

;; Merged special subnets
(defvar *private* *ipv4-rfc-1918*)
(defvar *non-routable* (ip-set-union *ipv4-not-globally-reachable* *ipv6-not-globally-reachable*))
(defvar *non-routable-exceptions* (ip-set-union *ipv4-not-globally-reachable-exceptions*
                                                *ipv6-not-globally-reachable-exceptions*))
(defvar *reserved* (ip-set-union *ipv4-reserved* *ipv6-reserved*))

;; Lookup functions
(defun private? (ip)
  "Returns T if IP is a private IP address, otherwise NIL."
  (check-type ip ip-like)
  (contains? *private* ip))

(defun public? (ip)
  "Returns T if IP is a public IP address, otherwise NIL."
  (check-type ip ip-like)
  (ax:if-let ((exception (contains? *non-routable-exceptions* ip)))
    exception
    (not (contains? *non-routable* ip))))

(defun reserved? (ip)
  "Returns T if IP is a reserved IP address, otherwise NIL."
  (check-type ip ip-like)
  (contains? *reserved* ip))
