LISP ?= sbcl

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load advt2024.asd \
		--eval '(ql:quickload :advt2024)' \
		--eval '(asdf:make :advt2024)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
