(in-package :event-listener)

(defclass kafka-event-listener ()
  ((address :initform "localhost:9092"
            :initarg  :address)
   (handler :initform #'(lambda (arg)
                          (format t "HANDLER: ~a~%" arg))
            :initarg  :handler)))

(defmethod connect ((event-listener kafka-event-listener))
  (format t "CONNECT~%"))

(defmethod disconnect ((event-listener kafka-event-listener))
  (format t "DISCONNECT~%"))
