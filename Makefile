coverage:
	sbcl --load coverage.lisp

test:
	time sbcl --eval "(progn (asdf:test-system :netaddr) (quit))"

.PHONY: docs
docs:
	sbcl --eval "(ql:quickload :staple-markdown)" --eval "(staple:generate :netaddr :if-exists :supersede)" --eval "(quit)"
