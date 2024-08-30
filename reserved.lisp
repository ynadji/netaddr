(in-package :netaddr)

;; TODO: These should probably be IP-SETs when the time comes.

(defvar *ipv4-loopback* (list #I("127.0.0.0/8"))) ; LIST for consistency.

(defvar *ipv4-rfc-1918* #I("10.0.0.0/8" "172.16.0.0/12" "192.168.0.0/16"))



