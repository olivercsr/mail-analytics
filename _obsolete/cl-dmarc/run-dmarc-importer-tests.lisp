
(load "dmarc-importer.asd")
(load "dmarc-importer-tests.asd")

(ql:quickload "dmarc-importer-tests")

(in-package :dmarc-importer-tests)

(uiop:quit (if (run-all-tests) 0 1))
