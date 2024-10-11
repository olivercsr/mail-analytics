(in-package :asdf-user)
(defsystem "cl-dmarc-cli-tests"
  :description "Test suite for the cl-dmarc-cli system"
  :author "Oliver Wegner <oliver.wegner@csr-informatik.de>"
  :version "0.0.1"
  :depends-on (:cl-dmarc-cli
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-cl-dmarc-cli"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
