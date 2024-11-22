(in-package :asdf-user)

(defsystem "dmarc-importer"
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
               ;;"xmls"
               "cxml"
               "cxml-dom"
               "xpath"
               ;;"cl-transit"
               "postmodern"
               )

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:module "packages"
                                              :serial t
                                              :components ((:file "shared")
                                                           (:file "dmarc-importer")))
                                     (:file "types")
                                     (:file "storage")
                                     (:module "storages"
                                              :serial t
                                              :components ((:file "memory")
                                                           (:file "postgres")))
                                     (:file "read-xml")
                                     (:module "systems"
                                              :serial t
                                              :components ((:file "dmarc-importer"))))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "bin/dmarc-importer"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "dmarc-importer:main")
