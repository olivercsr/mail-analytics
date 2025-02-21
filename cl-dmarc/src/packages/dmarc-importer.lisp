(defpackage :dmarc-importer
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   ;;(:x :xmls)
   (:x :cxml)
   (:xd :cxml-dom)
   (:xp :xpath)
   (:pg :postmodern)
   (:el :event-listener)
   (:ar :archiver)
   (:arz :archiver.zip)
   (:st :storage)
   (:ste :storage.existdb))
  (:export :main))
