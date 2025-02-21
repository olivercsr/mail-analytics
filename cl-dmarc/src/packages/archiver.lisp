(defpackage #:archiver
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)))

(defpackage #:archiver.zip
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)
   (:flex :flexi-streams)
   (:arch :archiver)
   (:zip :org.shirakumo.zippy)))
