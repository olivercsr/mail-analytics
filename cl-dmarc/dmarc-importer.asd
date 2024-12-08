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
               "arrow-macros"
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
                                                           (:file "file-processor")
                                                           (:file "dmarc-reading")
                                                           (:file "storage")
                                                           (:file "dmarc-importer")))
                                     (:file "types")
                                     (:file "trees")
                                     (:file "file-processor")
                                     (:module "file-processors"
                                              :serial t
                                              :components ((:file "noop")
                                                           (:file "filesystem")))
                                     (:file "read-xml")
                                     (:file "storage")
                                     (:module "storages"
                                              :serial t
                                              :components ((:file "memory")
                                                           (:file "postgres")))
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
