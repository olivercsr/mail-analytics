(in-package :event-listener.rabbit)

(se:defclass rabbit-event-listener ()
  ((host :initform "localhost"
         :initarg  :host)
   (port :initform 5672
         :initarg  :port)
   (handler :initform #'(lambda (arg &rest args)
                          (format t "HANDLER: ~a ~a~%" arg args))
            :initarg  :handler)
   (handler-thread)))

(defmethod el:connect ((event-listener rabbit-event-listener))
  (format t "RABBIT CONNECT")
  (setf (slot-value event-listener 'handler-thread)
        (bt2:make-thread #'(lambda ()
                             (format t "HANDLER-THREAD START~%")
                             (rb:with-connection (conn)
                               (let ((socket (rb:tcp-socket-new conn)))
                                 (rb:socket-open socket "localhost" 5672)
                                 (rb:login-sasl-plain conn "/" "guest" "guest")
                                 (rb:with-channel (conn 1)
                                   (rb:exchange-declare conn 1 "dmarcEmailMessages" "direct"
                                                        :durable t
                                                        :auto-delete t)
                                   (let ((queue-name "dmarcEmails"))
                                     (rb:queue-declare conn 1 :queue queue-name
                                                              :durable t
                                                              :auto-delete nil)
                                     (rb:queue-bind conn 1 :queue queue-name :exchange "dmarcEmailMessages" :routing-key "xx")
                                     ;;(format t "aaaaaa~%")
                                     (rb:basic-consume conn 1 queue-name)
                                     ;;(format t "bbbbbb~%")
                                     (loop for result = (rb:consume-message conn)
                                           do (when result
                                                (let* ((message (rb:envelope/message result))
                                                       (body (babel:octets-to-string (rb:message/body message)
                                                                                     :encoding :utf-8))
                                                       (props (rb:message/properties message)))
                                                  (funcall (slot-value event-listener 'handler) body props)
                                                  (format t "Got message: ~a~%content: ~a~%props: ~a~%"
                                                          result body props)
                                                  (rb:basic-ack conn 1 (rb:envelope/delivery-tag result))
                                                  body)))))))
                             (format t "HANDLER-THREAD END~%")))))

(defmethod el:disconnect ((event-listener rabbit-event-listener))
  (format t "RABBIT DISCONNECT")
  (let ((thread (slot-value event-listener 'handler-thread)))
    (when (bt2:thread-alive-p thread)
      (bt2:destroy-thread thread))
    (handler-case
        (bt2:join-thread thread)
      (bt2:abnormal-exit (c)
        (format t "ABNORMAL-EXIT ~a~%" c))))
  (setf (slot-value event-listener 'handler-thread)
        nil))


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
