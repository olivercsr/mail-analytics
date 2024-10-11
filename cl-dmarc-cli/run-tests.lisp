
(load "cl-dmarc-cli.asd")
(load "cl-dmarc-cli-tests.asd")

(ql:quickload "cl-dmarc-cli-tests")

(in-package :cl-dmarc-cli-tests)

(uiop:quit (if (run-all-tests) 0 1))
