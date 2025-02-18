(defpackage :storage
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:pg :postmodern))
  (:export :postgres-storage
           :upsert-reporter))
