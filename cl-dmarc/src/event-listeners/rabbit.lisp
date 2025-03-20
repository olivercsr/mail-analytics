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
   (channel-number :initform 1
                   :initarg :channel-number)
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
   (connection)
   (channel)
   (socket)
   (listener-thread)))

(defmethod au:start ((event-listener rabbit-event-listener) &rest args)
  (with-slots (host port vhost user password channel-number exchange exchange-type queue routing-key
               connection socket channel)
      event-listener
    (format t "start ~a~%" channel-number)
    (let* ((conn (cl-rabbit:new-connection))
           (sock (cl-rabbit:tcp-socket-new conn)))
      (cl-rabbit:socket-open sock host port)
      (cl-rabbit:login-sasl-plain conn vhost user password)
      (let ((chan (cl-rabbit:channel-open conn channel-number)))
        (cl-rabbit:exchange-declare conn channel-number exchange exchange-type
                                    :durable t
                                    :auto-delete t)
        (cl-rabbit:queue-declare conn channel-number
                                 :queue queue
                                 :durable t
                                 :auto-delete nil)
        (cl-rabbit:queue-bind conn channel-number
                              :queue queue
                              :exchange exchange
                              :routing-key routing-key)
        (setf connection conn)
        (setf socket sock)
        (setf channel chan)
        event-listener))))

(defmethod au:stop ((event-listener rabbit-event-listener))
  (with-slots (connection socket channel)
      event-listener
    (format t "stop~%")
    (cl-rabbit:channel-close connection channel)
    (cl-rabbit:destroy-connection connection)
    (setf channel nil)
    (setf socket nil)
    (setf connection nil)))

(defmethod el:consume ((event-listener rabbit-event-listener))
  (with-slots (host port vhost user password exchange exchange-type routing-key queue handler
               connection channel-number channel socket listener-thread)
      event-listener
    (format t "consume ~a~%" channel-number)
    (cl-rabbit:basic-consume connection channel-number queue)
    (let* ((packet (cl-rabbit:consume-message connection))
           (message (cl-rabbit:envelope/message packet))
           (body (-> message
                   (cl-rabbit:message/body)
                   (babel:octets-to-string :encoding :utf-8))))
      (format t "got packet. body: ~a~%" body)
      (->> packet
        (cl-rabbit:envelope/delivery-tag)
        (cl-rabbit:basic-ack connection channel-number)))

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
    ;;                                                (funcall handler event-listener body props)
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
    ;;              ;;                              (funcall (slot-value event-listener 'handler) event-listener body props)
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

;;(defmethod el:disconnect ((event-listener rabbit-event-listener))
;;  (with-slots (connection channel-number channel listener-thread) event-listener
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

(defmethod el:produce ((event-listener rabbit-event-listener) message &key (encoding :utf-8))
  (with-slots (exchange routing-key connection channel-number channel) event-listener
    (format t "RABBIT PRODUCE ~a ~a~%" channel-number message)
    (cl-rabbit:basic-publish connection channel-number
                             :exchange "mail-attachments"
                             :routing-key "mail-attachments"
                             :body message
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
