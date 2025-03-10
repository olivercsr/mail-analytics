(defpackage :mail-processor
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)
   (:mi :cl-mime)
   (:b64 :cl-base64)))
