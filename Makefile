coverage:
	sbcl --load coverage.lisp

test:
	time sbcl --eval "(progn (asdf:test-system :netaddr) (quit))"

.PHONY: test test-all
test-all: test
	time ros -L ecl run --eval "(asdf:test-system :netaddr)" --eval "(uiop:quit)"
	time ros -L abcl-bin run --eval "(asdf:test-system :netaddr)" --eval "(uiop:quit)"
	time ros -L lispworks run --eval "(asdf:test-system :netaddr)" --eval "(lw:quit)"

.PHONY: docs
docs:
	sbcl --eval "(ql:quickload :staple-markdown)" --eval "(staple:generate :netaddr :if-exists :supersede)" --eval "(quit)"
