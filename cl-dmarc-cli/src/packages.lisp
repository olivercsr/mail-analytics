(defpackage :cl-dmarc-cli
  (:use :cl)
  (:local-nicknames
   ;;(:x :xmls)
   (:x :cxml)
   (:xd :cxml-dom)
   (:xp :xpath)
   (:pg :postmodern))
  (:export :main))
