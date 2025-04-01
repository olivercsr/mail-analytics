(in-package :pubsub.rabbit)

(se:defclass rabbit-pubsub ()
  ((host :initform "localhost"
         :initarg  :host
         :reader host)
   (port :initform 5672
         :initarg  :port
         :reader port)
   (vhost :initform "/"
          :initarg :vhost
          :reader vhost)
   (user :initform "guest"
         :initarg :user
         :reader user)
   (password :initform "guest"
             :initarg :password
             :reader password)
   (channel :initform 1
            :initarg :channel
            :reader channel)
   (exchange :initform ""
             :initarg :exchange
             :reader exchange)
   (exchange-type :initform "direct"
                  :initarg :exchange-type
                  :reader exchange-type)
   (routing-key :initform "xx"
                :initarg :routing-key
                :reader routing-key)
   (queue :initform ""
          :initarg :queue
          :reader queue)
   (handler :initform #'(lambda (arg &rest args)
                          (format t "HANDLER: ~a ~a~%" arg args))
            :initarg :handler
            :reader handler)
   (connection)
   (socket)
   ;;(listener-thread :accessor listener-thread)
   ))

(defmethod au:start ((startable rabbit-pubsub) &rest args)
  (declare (ignorable args))
  (with-slots (host port vhost user password channel exchange exchange-type queue routing-key
               connection socket)
      startable
    (format t "start ~a~%" channel)
    (let* ((conn (cl-rabbit:new-connection))
           (sock (cl-rabbit:tcp-socket-new conn)))
      (cl-rabbit:socket-open sock host port)
      (cl-rabbit:login-sasl-plain conn vhost user password)
      (cl-rabbit:channel-open conn channel)
      (cl-rabbit:exchange-declare conn channel exchange exchange-type
                                  :durable t
                                  :auto-delete t)
      (cl-rabbit:queue-declare conn channel
                               :queue queue
                               :durable t
                               :auto-delete nil)
      (cl-rabbit:queue-bind conn channel
                            :queue queue
                            :exchange exchange
                            :routing-key routing-key)
      (setf connection conn)
      (setf socket sock)
      startable)))

(defmethod au:stop ((startable rabbit-pubsub))
  (with-slots (connection socket channel)
      startable
    (format t "stop~%")
    (cl-rabbit:channel-close connection channel)
    (cl-rabbit:destroy-connection connection)
    (setf socket nil)
    (setf connection nil)))

(defmethod ps:consume ((pubsub rabbit-pubsub) &rest args)
  (with-slots (host port vhost user password exchange exchange-type routing-key queue handler
               connection channel socket listener-thread)
      pubsub
    (format t "consume ~a~%" channel)
    (cl-rabbit:basic-consume connection channel queue)
    (let* ((packet (cl-rabbit:consume-message connection))
           (message (cl-rabbit:envelope/message packet))
           (body (-> message
                   (cl-rabbit:message/body)
                   ;;(babel:octets-to-string :encoding :utf-8)
                   ))
           (props (-> message
                    (cl-rabbit:message/properties))))
      (format t "CONSUME. calling handler...~a ~a ~a ~%" (type-of (cl-rabbit:message/body message)) (type-of body) body)
      (funcall handler pubsub body props)
      (->> packet
        (cl-rabbit:envelope/delivery-tag)
        (cl-rabbit:basic-ack connection channel)))

    ;;(let* (;;(conn (cl-rabbit:new-connection))
    ;;       ;;(sock (rb:tcp-socket-new conn))
    ;;       )
    ;;  ;;(rb:socket-open sock host port)
    ;;  ;;(rb:login-sasl-plain conn vhost user password)
    ;;  ;;(setf connection conn)
    ;;  ;;(setf socket sock)
    ;;  (setf listener-thread
    ;;        (bt2:make-thread #'(lambda ()
    ;;                             (format t "LISTENER-THREAD START ~a ~a~%" queue channel-number)
    ;;                             (cl-rabbit:with-connection (conn)
    ;;                               (let ((sock (cl-rabbit:tcp-socket-new conn)))
    ;;                                 (cl-rabbit:socket-open sock host port)
    ;;                                 (cl-rabbit:login-sasl-plain conn vhost user password)
    ;;                                 (setf connection conn)
    ;;                                 (cl-rabbit:with-channel (conn channel-number)
    ;;                                   (cl-rabbit:exchange-declare conn channel-number exchange exchange-type
    ;;                                                               :durable t
    ;;                                                               :auto-delete t)
    ;;                                   (cl-rabbit:queue-declare conn channel-number
    ;;                                                            :queue queue
    ;;                                                            :durable t
    ;;                                                            :auto-delete nil)
    ;;                                   (cl-rabbit:queue-bind conn channel-number
    ;;                                                         :queue queue
    ;;                                                         :exchange exchange
    ;;                                                         :routing-key queue)
    ;;                                   (cl-rabbit:basic-consume conn channel-number queue)
    ;;                                   (loop for result = (cl-rabbit:consume-message conn)
    ;;                                         do (when result
    ;;                                              (let* ((message (cl-rabbit:envelope/message result))
    ;;                                                     (body (-> message
    ;;                                                             (cl-rabbit:message/body)
    ;;                                                             (babel:octets-to-string :encoding :utf-8)))
    ;;                                                     (props (cl-rabbit:message/properties message)))
    ;;                                                (funcall handler pubsub body props)
    ;;                                                (format t "got message: ~a~%content: ~a~%props: ~a~%"
    ;;                                                        result body props)
    ;;                                                (->> result
    ;;                                                  (cl-rabbit:envelope/delivery-tag)
    ;;                                                  (cl-rabbit:basic-ack conn channel-number))
    ;;                                                body))))))
    ;;                             ;;(rb:with-channel (connection 1)
    ;;                               ;;(rb:exchange-declare connection 1 exchange exchange-type
    ;;                               ;;                     :durable t
    ;;                               ;;                     :auto-delete t)
    ;;                               ;;(rb:queue-declare connection 1 :queue queue
    ;;                               ;;                               :durable t
    ;;                               ;;                               :auto-delete nil)
    ;;              ;;                 (format t "before queue-bind ~a~%" channel-number)
    ;;              ;;                 (rb:queue-bind connection channel-number
    ;;              ;;                                :queue queue
    ;;              ;;                                :exchange exchange
    ;;              ;;                                :routing-key routing-key)
    ;;              ;;                 (format t "before basic-consume ~a~%" channel-number)
    ;;              ;;                 (rb:basic-consume connection channel-number queue)
    ;;              ;;                 (format t "before loop ~a~%" channel-number)
    ;;              ;;                 (loop for result = (rb:consume-message connection)
    ;;              ;;                       do (when result
    ;;              ;;                            (let* ((message (rb:envelope/message result))
    ;;              ;;                                   (body (babel:octets-to-string (rb:message/body message)
    ;;              ;;                                                                 :encoding :utf-8))
    ;;              ;;                                   (props (rb:message/properties message)))
    ;;              ;;                              (funcall (slot-value pubsub 'handler) pubsub body props)
    ;;              ;;                              (format t "Got message: ~a~%content: ~a~%props: ~a~%"
    ;;              ;;                                      result body props)
    ;;              ;;                              (rb:basic-ack connection channel-number (rb:envelope/delivery-tag result))
    ;;              ;;                              body)))
    ;;                               ;;)
    ;;                             ;;(rb:with-connection (conn)
    ;;                             ;;  (let ((socket (rb:tcp-socket-new conn)))
    ;;                             ;;    (rb:socket-open socket "localhost" 5672)
    ;;                             ;;    (rb:login-sasl-plain conn "/" "guest" "guest")
    ;;                             ;;    ))
    ;;                             (format t "LISTENER-THREAD END~%")))))
    ))

;;(defmethod el:disconnect ((pubsub rabbit-pubsub))
;;  (with-slots (connection channel-number channel listener-thread) pubsub
;;    (format t "RABBIT DISCONNECT ~a~%" channel-number)
;;    (when (bt2:thread-alive-p listener-thread)
;;      (bt2:destroy-thread listener-thread))
;;    (handler-case
;;        (bt2:join-thread listener-thread)
;;      (bt2:abnormal-exit (c)
;;        (format t "ABNORMAL-EXIT ~a ~a~%" channel-number c)))
;;    (setf listener-thread nil)
;;    ;;(cl-rabbit:destroy-connection connection)
;;    (setf channel nil)
;;    (setf connection nil)))

(defmethod ps:produce ((pubsub rabbit-pubsub) topic message &key (encoding :utf-8))
  (with-slots (exchange routing-key connection channel) pubsub
    (format t "RABBIT PRODUCE ~a ~a ~a ~a~%" channel topic (type-of message) message)
    (cl-rabbit:basic-publish connection channel
                             :exchange topic
                             :routing-key topic
                             :body message ;;(etypecase message
                                   ;;  (stream (with-output-to-string (out)
                                   ;;            (uiop:copy-stream-to-stream message out)))
                                   ;;  (t message))
                             :encoding encoding
                             :properties '((:app-id . "Application id")))))


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
