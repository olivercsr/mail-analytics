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
               ;;"uuid"

               ;; data conversion
               "babel"             ;; data conversion

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
                                                           (:file "event-listener")
                                                           (:file "mail-processor")
                                                           (:file "archiver")
                                                           ;;(:file "file-processor")
                                                           (:file "dmarc-reading")
                                                           (:file "storage")
                                                           (:file "storage.existdb")
                                                           (:file "dmarc-importer")))
                                     (:file "types")
                                     (:file "trees")
                                     (:file "event-listener")
                                     (:module "event-listeners"
                                              :serial t
                                              :components (;;(:file "kafka")
                                                           ;;(:file "amqp")
                                                           (:file "rabbit")))
                                     (:file "mail-processor")
                                     (:file "archiver")
                                     (:module "archivers"
                                              :serial t
                                              :components ((:file "zip")))
                                     ;;(:file "file-processor")
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
