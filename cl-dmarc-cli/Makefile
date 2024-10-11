#LISP ?= sbcl
LISP ?= ros run --

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load cl-dmarc-cli.asd \
		--eval '(ql:quickload :cl-dmarc-cli)' \
		--eval '(asdf:make :cl-dmarc-cli)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
