(in-package :netaddr)

;; TODO: These should probably be IP-SETs when the time comes.

;; BUG: So you can't use your reader macros here for some reason. RUTILS does
;; this so maybe take a look there? https://github.com/vseloved/rutils/blob/master/core/readtable.lisp
;; and here's example usage in another library:
;; https://github.com/vseloved/cl-nlp/blob/f180b6c3c0b9a3614ae43f53a11bc500767307d0/src/core/stats.lisp#L3
;; and he does use it internally:
;; https://github.com/vseloved/rutils/blob/79cb02922f025e818ef4100957abdc9f8d671e2c/core/list.lisp#L104

;;(defvar *ipv4-loopback* (list #N("127.0.0.0/8"))) ; LIST for consistency.

;;(defvar *ipv4-rfc-1918* #N("10.0.0.0/8" "172.16.0.0/12" "192.168.0.0/16"))


