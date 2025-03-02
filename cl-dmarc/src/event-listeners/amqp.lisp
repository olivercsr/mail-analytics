(in-package :event-listener.amqp)

(se:defclass amqp-event-listener ()
  ((host :initform "localhost"
         :initarg  :host)
   (port :initform 5672
         :initarg  :port)))

(defmethod el:connect ((event-listener amqp-event-listener))
  (format t "AMQP CONNECT"))

(defmethod el:disconnect ((event-listener amqp-event-listener))
  (format t "AMQP DISCONNECT"))
