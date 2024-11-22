(defpackage :shared
  (:use :cl))

(defpackage :storage
  (:use :cl)
  (:local-nicknames
   (:pg :postmodern)))

(defpackage :dmarc-reading
  (:use :cl)
  (:local-nicknames
   ;;(:x :xmls)
   (:x :cxml)
   (:xd :cxml-dom)
   (:xp :xpath)))
