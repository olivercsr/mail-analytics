(defpackage :dmarc-importer
  (:use :cl :shared)
  (:local-nicknames
   ;;(:x :xmls)
   (:x :cxml)
   (:xd :cxml-dom)
   (:xp :xpath)
   (:pg :postmodern))
  (:export :main))
