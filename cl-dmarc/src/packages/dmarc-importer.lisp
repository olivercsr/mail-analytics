(defpackage :dmarc-importer
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   ;;(:x :xmls)
   (:x :cxml)
   (:xd :cxml-dom)
   (:xp :xpath)
   (:pg :postmodern)
   (:el :event-listener)
   (:elr :event-listener.rabbit)
   ;;(:elk :event-listener.kafka)
   (:mp :mail-processor)
   (:ar :archiver)
   (:arz :archiver.zip)
   (:st :storage)
   (:ste :storage.existdb))
  (:export :main))
