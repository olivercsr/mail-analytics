(defpackage :dmarc-importer
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   ;;(:x :xmls)
   (:x :cxml)
   (:xd :cxml-dom)
   (:xp :xpath)
   (:pg :postmodern)
   (:el :event-listener)
   (:st :storage)
   (:ste :storage.existdb))
  (:export :main))
