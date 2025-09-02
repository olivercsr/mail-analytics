(in-package :mail-processor)

(se:defclass mail-processor ()
  ((pubsub :initarg :pubsub)
   (pubsub-thread)))

(defun ensure-filename (mime)
  (or (some->> mime
        (cl-mime:content-disposition-parameters)
        (assoc :filename)
        (cadr))
      (format nil "mime-attachment-~a-~a"
              (get-universal-time)
              (fuuid:to-string (fuuid:make-v4)))))

(defun decode-part (mime file-handler)
  (format t "decode (~a/~a) ~a ~a ~a ~a ~a~%"
          (mi:content-type mime)
          (mi:content-subtype mime)
          (mi:content-id mime)
          (mi:content-disposition mime)
          (mi:content-disposition-parameters mime)
          (->> mime
            (mi:content-disposition-parameters)
            (assoc :filename)
            (cadr))
          (mi:content-transfer-encoding mime))
  (let ((filename (ensure-filename mime)))
    (case (cl-mime:content-transfer-encoding mime)
      (:base64 (funcall file-handler
                        filename
                        (-> mime
                          (cl-mime:content)
                          (cl-base64:base64-string-to-usb8-array))))
      (t (funcall file-handler filename (mi:content mime))))))

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

(se:defun mail-handler (pubsub body props &rest args)
  (declare (ignorable props))
  (format t "MAIL-HANDLER ~a ~a~%~%" pubsub args)
  (process-mail (babel:octets-to-string body)
                #'(lambda (filename content)
                    (format t "processing mail-attachment ~a ~a ~a~%" filename (type-of content) content)
                    (let (
                          ;;(ar (-> content
                          ;;      (uiop:slurp-stream-string)
                          ;;      (babel:string-to-octets :encoding :utf-8)))
                          )
                      (format t "slurped ~a ~a~%" (type-of content) content)
                      (ps:produce pubsub "mail-attachments" content)
                      ))))

(defmethod au:start ((startable mail-processor) &rest args)
  (declare (ignorable args))
  (format t "start mail-processor~%")
  (with-slots (pubsub pubsub-thread)
      startable
    (let ((thread (make-instance 'au:bordeaux-threadable
                                 :handler #'(lambda ()
                                              (au:start pubsub)
                                              (ps:consume pubsub)
                                              (au:stop pubsub)))))
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

(let ((b "UEsDBAoAAAAAAIydgVqBu8L6FAAAABQAAAADABwAZm9vVVQJAAP4Jexn+CXsZ3V4CwABBOgDAAAE6AMAAGZvb2JhcmJhejEyMzQ1Njc4OTAKUEsBAh4DCgAAAAAAjJ2BWoG7wvoUAAAAFAAAAAMAGAAAAAAAAQAAAKSBAAAAAGZvb1VUBQAD+CXsZ3V4CwABBOgDAAAE6AMAAFBLBQYAAAAAAQABAEkAAABRAAAAAAA="))
  (with-input-from-string (in (cl-base64:base64-string-to-string b))
    (-> in
      (uiop:slurp-stream-string)
      (babel:string-to-octets :encoding :utf-8) ;; this messes things up!
      ;;(type-of)
      ))
  ;;(cl-base64:base64-string-to-usb8-array b)
  ;;(-> b
  ;;  (cl-base64:base64-string-to-string)
  ;;  (type-of))
  )
