(defpackage :event-listener
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting))
  ;;(:export
  ;; :connect
  ;; :disconnect)
  )

;;(defpackage :event-listener.kafka
;;  (:use :cl :arrow-macros :shared)
;;  (:local-nicknames
;;   (:a :alexandria)
;;   (:s :serapeum)
;;   (:se :serapeum.exporting)
;;   (:el :event-listener))
;;  (:export
;;   :kafka-event-listener
;;   :kafka-event-listener-address
;;   :kafka-event-listener-handler))

;;(defpackage :event-listener.amqp
;;  (:use :cl :arrow-macros :shared)
;;  (:local-nicknames
;;   (:a :alexandria)
;;   (:s :serapeum)
;;   (:se :serapeum.exporting)
;;   (:el :event-listener))
;;  ;;(:export
;;  ;; :amqp-event-listener)
;;  )

(defpackage :event-listener.rabbit
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum)
   (:se :serapeum.exporting)
   (:st :startable)
   (:el :event-listener)
   (:rb :cl-rabbit)))
