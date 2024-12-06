LISP ?= sbcl

all: build

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load advt2024.asd \
		--eval '(ql:quickload :advt2024)' \
		--eval '(asdf:make :advt2024)'

