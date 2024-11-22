(in-package :asdf-user)

(defsystem "dmarc-importer-tests"
  :description "Test suite for the dmarc-importer system"
  :author "Oliver Wegner <oliver.wegner@csr-informatik.de>"
  :version "0.0.1"
  :depends-on (:dmarc-importer
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:module "packages"
                                              :serial t
                                              :components ((:file "dmarc-importer")))
                                     (:file "test-dmarc-importer"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
