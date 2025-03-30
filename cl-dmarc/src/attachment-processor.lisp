(in-package :attachment-processor)

;;(defgeneric lock-next-file (attachment-processor))
;;(defgeneric file-done (attachment-processor file))
;;(defgeneric reset-stuck-files (attachment-processor))

(se:defclass attachment-processor ()
  (pubsub-thread))

(defmethod au:start ((startable attachment-processor) &rest args)
  (declare (ignorable args))
  (format t "start attachment-processor~%")
  (with-slots (pubsub-thread)
      startable
    (let ((thread (make-instance 'au:bordeaux-threadable
                                 :handler #'(lambda ()
                                              (let ((pubsub (make-instance 'psr:rabbit-pubsub
                                                                           :host "localhost"
                                                                           :port 5672
                                                                           :vhost "/"
                                                                           :user "guest"
                                                                           :password "guest"
                                                                           ;;:connection rabbit-connection-mails
                                                                           :channel 1
                                                                           ;;:channel rabbit-channel-mails
                                                                           :exchange "mail-attachments"
                                                                           :exchange-type "direct"
                                                                           :routing-key "mail-attachments"
                                                                           :queue "mail-attachments"
                                                                           :handler #'(lambda (pubsub body props &rest args)
                                                                                        (format t "ATTACHMENT PROCESSOR ~a ~a~%~%" pubsub args)
                                                                                        ))))
                                                (au:start pubsub)
                                                (ps:consume pubsub)
                                                (au:stop pubsub))))))
      (au:start thread)
      (setf pubsub-thread thread)
      startable)))

(defmethod au:stop ((startable attachment-processor))
  (format t "stop attachment-processor~%")
  (with-slots (pubsub-thread)
      startable
    (au:stop pubsub-thread)
    (setf pubsub-thread nil)
    startable))
