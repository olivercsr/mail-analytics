(defpackage :attachment-processor
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)
   (:au :apputils)
   (:ps :pubsub)
   (:psr :pubsub.rabbit)
   (:ar :archiver)
   ;;(:mi :cl-mime)
   ;;(:b64 :cl-base64)
   ))
