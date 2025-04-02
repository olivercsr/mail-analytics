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
  :depends-on (;; language features
               "alexandria"
               "serapeum"
               "arrow-macros"

               ;; concurrency
               "bordeaux-threads"  ;; threading library

               ;; streams
               "flexi-streams"

               ;; uuid
               "frugal-uuid"

               ;; uuid
               ;;"uuid"

               ;; data conversion
               "babel"             ;; data conversion
               "cl-base64"

               ;; http
               "drakma"

               ;; xml processing
               ;;"xmls"
               "cxml"              ;; xml processing
               "cxml-dom"          ;; xml processing
               "xpath"             ;; xml-xpath processing
               ;;"cl-transit"

               ;; zip archives
               "zippy"

               ;; database connectivity
               "postmodern"        ;; postgres client

               ;; message broker connectivity
               ;;"cl-rdkafka"        ;; kafka client
               ;;"cl-amqp"
               "cl-rabbit"

               ;; mime parsing
               "cl-mime"
               )

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:module "packages"
                                              :serial t
                                              :components ((:file "shared")
                                                           (:file "apputils")
                                                           (:file "pubsub")
                                                           (:file "mail-processor")
                                                           (:file "archiver")
                                                           (:file "attachment-processor")
                                                           (:file "dmarc-reading")
                                                           (:file "storage")
                                                           (:file "storage.existdb")
                                                           (:file "dmarc-importer")))
                                     (:file "apputils")
                                     (:file "pubsub")
                                     (:module "pubsubs"
                                              :serial t
                                              :components (;;(:file "kafka")
                                                           ;;(:file "amqp")
                                                           (:file "rabbit")))
                                     (:file "types")
                                     (:file "trees")
                                     (:file "mail-processor")
                                     (:module "archivers"
                                              :serial t
                                              :components ((:file "zip")))
                                     (:file "archiver")
                                     (:file "attachment-processor")
                                     ;;(:module "file-processors"
                                     ;;         :serial t
                                     ;;         :components ((:file "noop")
                                     ;;                      (:file "filesystem")))
                                     (:file "read-xml")
                                     (:file "storage")
                                     (:module "storages"
                                              :serial t
                                              :components ((:file "memory")
                                                           (:file "postgres")
                                                           (:file "existdb")))
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
