(defpackage :dmarc-importer
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:au :apputils)
   ;;(:x :xmls)
   (:x :cxml)
   (:xd :cxml-dom)
   (:xp :xpath)
   (:pg :postmodern)
   (:ps :pubsub)
   (:psr :pubsub.rabbit)
   ;;(:psk :pubsub.kafka)
   (:mp :mail-processor)
   (:ap :attachment-processor)
   (:ar :archiver)
   (:arz :archiver.zip)
   (:st :storage)
   (:ste :storage.existdb)
   (:stp :storage-processor))
  (:export :main))
