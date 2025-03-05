(in-package :event-listener.rabbit)

(se:defclass rabbit-event-listener ()
  ((host :initform "localhost"
         :initarg  :host)
   (port :initform 5672
         :initarg  :port)))

(defmethod el:connect ((event-listener rabbit-event-listener))
  (format t "RABBIT CONNECT")
  )

(defmethod el:disconnect ((event-listener rabbit-event-listener))
  (format t "RABBIT DISCONNECT"))


;;(defun foo ()
;;  (rb:with-connection (conn)
;;    (let ((socket (rb:tcp-socket-new conn)))
;;      (rb:socket-open socket "localhost" 5672)
;;      (rb:login-sasl-plain conn "/" "guest" "guest")
;;      (rb:with-channel (conn 1)
;;        (rb:exchange-declare conn 1 "test-ex" "topic")
;;        (let ((queue-name "foo"))
;;          (rb:queue-declare conn 1 :queue queue-name)
;;          (rb:queue-bind conn 1 :queue queue-name :exchange "test-ex" :routing-key "xx")
;;          (rb:basic-consume conn 1 queue-name)
;;          (let* ((result (rb:consume-message conn))
;;                 (message (rb:envelope/message result)))
;;            (format t "Got message: ~s~%content: ~s~%props: ~s~%"
;;                    result (babel:octets-to-string (rb:message/body message) :encoding :utf-8)
;;                    (rb:message/properties message))
;;            (rb:basic-ack conn 1 (rb:envelope/delivery-tag result))))))))
;;
;;(foo)
