(in-package :mail-processor)

(defun decode-part (mime file-handler)
  (format t "decode (~a/~a) ~a~%"
          (mi:content-type mime) (mi:content-subtype mime)
          (mi:content-transfer-encoding mime))
  (let* ((out ;;(make-string-output-stream)
              (flex:make-in-memory-output-stream
               :element-type '(unsigned-byte 8))
               ;;:element-type 'char
              )
         )
    (case (cl-mime:content-transfer-encoding mime)
      (:base64 (progn
                 (funcall file-handler "file01" ;;(-> out
                          ;;  (flex:get-output-stream-sequence)
                          ;;  (flex:make-in-memory-input-stream))
                          (with-output-to-string (s) ;; TODO: how do we handle large files properly (i.e. stream-/chunk-wise instead of keeping the entire file in memory?)
                            (b64:base64-string-to-stream (mi:content mime)
                                                         :stream s)))))
      (t (with-input-from-string (content (mi:content mime))
           (funcall file-handler "file01" content))))))

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


(defun foo ()
  (with-open-file (in #p"../../../dmarc-data/mails/mail01.eml"
                      :direction :input)
    (process-mail in #'(lambda (filename content-stream)
                         (format t "HANDLER ~a ~a~%" filename content-stream)
                         (cl-rabbit:with-connection (conn)
                           (let ((socket (cl-rabbit:tcp-socket-new conn)))
                             (cl-rabbit:socket-open socket "localhost" 5672)
                             (cl-rabbit:login-sasl-plain conn "/" "guest" "guest")
                             (cl-rabbit:with-channel (conn 1)
                               (cl-rabbit:basic-publish conn 1
                                                        ;;:exchange "dmarcFilesExchange"
                                                        :exchange ""
                                                        :routing-key "xx"
                                                        :body content-stream
                                                        :properties '((:app-id . "Application id")))))))))
  nil)

(foo)
