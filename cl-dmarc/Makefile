#LISP ?= sbcl
LISP ?= ros run --

all: test-dmarc-importer

run-dmarc-importer:
	rlwrap $(LISP) --load run-dmarc-importer.lisp

build-dmarc-importer:
	$(LISP)	--non-interactive \
		--load dmarc-importer.asd \
		--eval '(ql:quickload :dmarc-importer)' \
		--eval '(asdf:make :dmarc-importer)'

test-dmarc-importer:
	$(LISP) --non-interactive \
		--load run-dmarc-importer-tests.lisp
