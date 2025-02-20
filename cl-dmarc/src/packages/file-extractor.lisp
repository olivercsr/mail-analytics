(defpackage #:file-extractor
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)))

(defpackage #:file-extractor.zip
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)
   (:fp :file-extractor)
   (:zip :org.shirakumo.zippy)))
