(in-package :pubsub.amqp)

(se:defclass amqp-pubsub ()
  ((host :initform "localhost"
         :initarg  :host)
   (port :initform 5672
         :initarg  :port)))

(defmethod ps:connect ((pubsub amqp-pubsub))
  (format t "AMQP CONNECT"))

(defmethod ps:disconnect ((pubsub amqp-pubsub))
  (format t "AMQP DISCONNECT"))


;;(defun foo ()
;;  (let ((conn (make-instance 'amqp:amqp-method-connection-start)))
;;    (format t "foo: ~a~%" conn)))
;;
;;(foo)
