(in-package :mail-processor)

(se:defclass mail-processor ()
  (pubsub-thread))

(defun decode-part (mime file-handler)
  (format t "decode (~a/~a) ~a~%"
          (mi:content-type mime) (mi:content-subtype mime)
          (mi:content-transfer-encoding mime))
  (case (cl-mime:content-transfer-encoding mime)
    (:base64 (with-input-from-string (content-stream (b64:base64-string-to-string (mi:content mime)))
               (funcall file-handler "file01" content-stream)))
    (t (with-input-from-string (content-stream (mi:content mime))
         (funcall file-handler "file01" content-stream)))))

(defun process-part (mime file-handler)
  (format t "process-part enter ~a ~a (~a/~a) ~a ~a~%"
          (mi:content-disposition mime) (mi:content-disposition-parameters mime)
          (mi:content-type mime) (mi:content-subtype mime)
          (mi:content-transfer-encoding mime) (type-of (mi:content mime)))
  (case (type-of mime)
    (mi:multipart-mime (dolist (part (mi:content mime))
                         (process-part part file-handler)))
    (t (a:switch ((mi:content-disposition mime) :test #'equal)
         ("attachment" (decode-part mime file-handler))))))

(defun process-mail (mail file-handler)
  ;;(format t "processing mail ~a~%" mail)
  (let ((mime (mi:parse-mime mail)))
    (process-part mime file-handler)))

(defmethod au:start ((startable mail-processor) &rest args)
  (declare (ignorable args))
  (format t "start mail-processor~%")
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
                                                                           :exchange "dmarcEmailMessages"
                                                                           :exchange-type "direct"
                                                                           :routing-key "dmarcEmailMessages"
                                                                           :queue "dmarcEmails"
                                                                           :handler #'(lambda (pubsub body props &rest args)
                                                                                        (format t "MAIL PROCESSOR ~a ~a~%~%" pubsub args)
                                                                                        (process-mail body
                                                                                                      #'(lambda (filename content-stream)
                                                                                                          (format t "processing mail-attachment ~a~%" filename)
                                                                                                          (ps:produce pubsub "foobar")))))))
                                                (au:start pubsub)
                                                (ps:consume pubsub)
                                                (au:stop pubsub))))))
      (au:start thread)
      (setf pubsub-thread thread)
      startable)))

(defmethod au:stop ((startable mail-processor))
  (format t "stop mail-processor~%")
  (with-slots (pubsub-thread)
      startable
    (au:stop pubsub-thread)
    (setf pubsub-thread nil)
    startable))

;;(defun foo (path)
;;  (with-open-file (in path :direction :input)
;;    (process-mail in #'(lambda (filename content-stream)
;;                         (format t "HANDLER ~a ~a~%" filename content-stream)
;;                         (cl-rabbit:with-connection (conn)
;;                           (let ((socket (cl-rabbit:tcp-socket-new conn)))
;;                             (cl-rabbit:socket-open socket "localhost" 5672)
;;                             (cl-rabbit:login-sasl-plain conn "/" "guest" "guest")
;;                             (cl-rabbit:with-channel (conn 1)
;;                               (cl-rabbit:basic-publish conn 1
;;                                                        ;;:exchange "dmarcFilesExchange"
;;                                                        :exchange ""
;;                                                        :routing-key "xx"
;;                                                        :body (with-output-to-string (out)
;;                                                                (let ((buf (make-array 128)))
;;                                                                  (loop for len = (read-sequence buf content-stream)
;;                                                                        while (> len 0)
;;                                                                        do (write-sequence buf out :end len))))
;;                                                        :encoding :utf-8
;;                                                        :properties '((:app-id . "Application id")))))))))
;;  nil)

;;(foo #p"../../dmarc-data/mails/mail01.eml")

;;(with-input-from-string (in "foobar")
;;  (with-output-to-string (out)
;;    (loop for c = (read-char in nil)
;;          while c
;;          do (write-char c out))))
