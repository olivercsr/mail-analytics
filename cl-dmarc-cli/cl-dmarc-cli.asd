(in-package :asdf-user)

(defsystem "cl-dmarc-cli"
  :author "Oliver Wegner <oliver.wegner@csr-informatik.de>"
  :version "0.0.1"
  :license "unlicensed"
  :description ""
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on ("alexandria"
               "xmls")

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "cl-dmarc-cli"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "cl-dmarc-cli"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "cl-dmarc-cli:main")
