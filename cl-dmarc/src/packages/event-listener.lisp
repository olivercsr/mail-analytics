(defpackage :event-listener
  (:use :cl :arrow-macros :shared)
  (:export :connect
   :disconnect

           :make-kafka-event-listener
           :kafka-event-listener-address
           :kafka-event-listener-handler))
