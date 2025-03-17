(in-package :event-listener.rabbit)

(se:defclass rabbit-event-listener ()
  ((host :initform "localhost"
         :initarg  :host)
   (port :initform 5672
         :initarg  :port)
   (vhost :initform "/"
          :initarg :vhost)
   (user :initform "guest"
         :initarg :user)
   (password :initform "guest"
             :initarg :password)
   (exchange :initform ""
             :initarg :exchange)
   (exchange-type :initform "direct"
                  :initarg :exchange-type)
   (routing-key :initform "xx"
                :initarg :routing-key)
   (queue :initform ""
          :initarg :queue)
   (handler :initform #'(lambda (arg &rest args)
                          (format t "HANDLER: ~a ~a~%" arg args))
            :initarg  :handler)
   (connection :initarg :connection)
   (channel :initarg :channel)
   (socket)
   (listener-thread)))

(defmethod el:connect ((event-listener rabbit-event-listener))
  (format t "RABBIT CONNECT~%")
  (with-slots (host port vhost user password exchange exchange-type routing-key queue
               connection channel socket listener-thread)
      event-listener
    (let* (;;(conn (cl-rabbit:new-connection))
           ;;(sock (rb:tcp-socket-new conn))
           )
      ;;(rb:socket-open sock host port)
      ;;(rb:login-sasl-plain conn vhost user password)
      ;;(setf connection conn)
      ;;(setf socket sock)
      (setf listener-thread
            (bt2:make-thread #'(lambda ()
                                 (format t "LISTENER-THREAD START~%")
                                 ;;(rb:with-channel (connection 1)
                                   (rb:exchange-declare connection 1 exchange exchange-type
                                                        :durable t
                                                        :auto-delete t)
                                   (rb:queue-declare connection 1 :queue queue
                                                                  :durable t
                                                                  :auto-delete nil)
                                   (rb:queue-bind connection 1 :queue queue
                                                               :exchange exchange
                                                               :routing-key routing-key)
                                   (rb:basic-consume connection 1 queue)
                                   (loop for result = (rb:consume-message connection)
                                         do (when result
                                              (let* ((message (rb:envelope/message result))
                                                     (body (babel:octets-to-string (rb:message/body message)
                                                                                   :encoding :utf-8))
                                                     (props (rb:message/properties message)))
                                                (funcall (slot-value event-listener 'handler) event-listener body props)
                                                (format t "Got message: ~a~%content: ~a~%props: ~a~%"
                                                        result body props)
                                                (rb:basic-ack connection 1 (rb:envelope/delivery-tag result))
                                                body)))
                                   ;;)
                                 ;;(rb:with-connection (conn)
                                 ;;  (let ((socket (rb:tcp-socket-new conn)))
                                 ;;    (rb:socket-open socket "localhost" 5672)
                                 ;;    (rb:login-sasl-plain conn "/" "guest" "guest")
                                 ;;    ))
                                 (format t "LISTENER-THREAD END~%"))))
      )))

(defmethod el:disconnect ((event-listener rabbit-event-listener))
  (format t "RABBIT DISCONNECT~%")
  (with-slots (connection listener-thread) event-listener
    (when (bt2:thread-alive-p listener-thread)
      (bt2:destroy-thread listener-thread))
    (handler-case
        (bt2:join-thread listener-thread)
      (bt2:abnormal-exit (c)
        (format t "ABNORMAL-EXIT ~a~%" c)))
    (setf listener-thread nil)
    (cl-rabbit:destroy-connection connection)
    (setf connection nil)))

(defmethod el:send-message ((event-listener rabbit-event-listener) message &key (encoding :utf-8))
  (format t "RABBIT SEND-MESSAGE ~a~%" message)
  (with-slots (exchange routing-key connection) event-listener
    (cl-rabbit:with-channel (connection 1)
      (cl-rabbit:basic-publish connection 1
                               :exchange exchange
                               :routing-key routing-key
                               :body message
                               :encoding encoding
                               :properties '((:app-id . "Application id"))))))


;;(defun foo ()
;;  (rb:with-connection (conn)
;;    (let ((socket (rb:tcp-socket-new conn)))
;;      (rb:socket-open socket "localhost" 5672)
;;      (rb:login-sasl-plain conn "/" "guest" "guest")
;;      (rb:with-channel (conn 1)
;;        (rb:exchange-declare conn 1 "dmarcEmailMessages" "direct"
;;                             :durable t
;;                             :auto-delete t)
;;        (let ((queue-name "dmarcEmails"))
;;          (rb:queue-declare conn 1 :queue queue-name
;;                            :durable t
;;                            :auto-delete nil)
;;          (rb:queue-bind conn 1 :queue queue-name :exchange "dmarcEmailMessages" :routing-key "xx")
;;          ;;(format t "aaaaaa~%")
;;          (rb:basic-consume conn 1 queue-name)
;;          ;;(format t "bbbbbb~%")
;;          (let* ((result (rb:consume-message conn))
;;                 (message (rb:envelope/message result))
;;                 (body (babel:octets-to-string (rb:message/body message)
;;                                               :encoding :utf-8))
;;                 (props (rb:message/properties message)))
;;            (format t "Got message: ~s~%content: ~s~%props: ~s~%"
;;                    result body props)
;;            (rb:basic-ack conn 1 (rb:envelope/delivery-tag result))
;;            body))))))
;;
;;(foo)
