(in-package :asdf-user)

(defsystem "mail-reader"
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
               ;;"cxml"
               ;;"cxml-dom"
               ;;"xpath"
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
                                                           (:file "dmarc-finder")
                                                           (:file "mail-reader")))
                                     ;;(:file "types")
                                     ;;(:file "storage")
                                     ;;(:module "storages"
                                     ;;         :serial t
                                     ;;         :components ((:file "memory")
                                     ;;                      (:file "postgres")))
                                     ;;(:file "read-xml")
                                     (:file "dmarc-finder")
                                     (:module "dmarc-finders"
                                              :serial t
                                              :components ((:file "noop")))
                                     (:module "systems"
                                              :serial t
                                              :components ((:file "mail-reader"))))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "bin/mail-reader"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "mail-reader:main")
