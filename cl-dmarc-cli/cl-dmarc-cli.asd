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
               "serapeum"
               "xmls"
               ;;"cl-transit"
               "postmodern"
               )

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "types")
                                     (:file "storage")
                                     (:module "storages"
                                              :serial t
                                              :components ((:file "memory")
                                                           (:file "postgres")))
                                     (:file "read-xml")
                                     (:file "cl-dmarc-cli"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "cl-dmarc-cli"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "cl-dmarc-cli:main")
