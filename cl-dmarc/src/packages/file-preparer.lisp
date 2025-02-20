(defpackage #:file-preparer
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)))

(defpackage #:file-preparer.zip
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)
   (:fp :file-preparer)
   (:zip :org.shirakumo.zippy)))
