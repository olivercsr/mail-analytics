(defpackage :event-listener
  (:use :cl :arrow-macros :shared)
  (:local-nicknames
   (:a :alexandria)
   (:s :serapeum))
  (:export :connect
   :disconnect

   :kafka-event-listener
           :kafka-event-listener-address
           :kafka-event-listener-handler))
