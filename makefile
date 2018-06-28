LISP ?= sbcl

build:
			$(LISP) --load timer.asd \
			--eval '(ql:quickload :timer)' \
			--eval '(asdf:make :timer)' \
			--eval '(quit)'
