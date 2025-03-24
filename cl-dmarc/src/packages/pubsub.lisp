(defpackage :pubsub
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting))
  ;;(:export
  ;; :connect
  ;; :disconnect)
  )

;;(defpackage :pubsub.kafka
;;  (:use :cl :arrow-macros :shared)
;;  (:local-nicknames
;;   (:a :alexandria)
;;   (:s :serapeum)
;;   (:se :serapeum.exporting)
;;   (:el :pubsub))
;;  (:export
;;   :kafka-pubsub
;;   :kafka-pubsub-address
;;   :kafka-pubsub-handler))

;;(defpackage :pubsub.amqp
;;  (:use :cl :arrow-macros :shared)
;;  (:local-nicknames
;;   (:a :alexandria)
;;   (:s :serapeum)
;;   (:se :serapeum.exporting)
;;   (:el :pubsub))
;;  ;;(:export
;;  ;; :amqp-pubsub)
;;  )

(defpackage :pubsub.rabbit
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)
   (:au :apputils)
   (:ps :pubsub)
   (:rb :cl-rabbit)))
