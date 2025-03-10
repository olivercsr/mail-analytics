(in-package :mail-processor)

(defun decode-part (mime out-stream)
  (format t "decode (~a/~a) ~a~%"
          (mi:content-type mime) (mi:content-subtype mime)
          (mi:content-transfer-encoding mime))
  (case (cl-mime:content-transfer-encoding mime)
    (:base64 (b64:base64-string-to-stream (mi:content mime)
                                          :stream out-stream))
    (t (with-input-from-string (content (mi:content mime))
         (uiop:copy-stream-to-stream content out-stream)))))

(defun process-part (mime make-output-stream)
  (format t "process-part enter ~a ~a (~a/~a) ~a ~a~%"
          (mi:content-disposition mime) (mi:content-disposition-parameters mime)
          (mi:content-type mime) (mi:content-subtype mime)
          (mi:content-transfer-encoding mime) (type-of (mi:content mime)))
  (case (type-of mime)
    (mi:multipart-mime (dolist (part (mi:content mime))
                         (process-part part make-output-stream)))
    (t (a:switch ((mi:content-disposition mime) :test #'equal)
         ("attachment" (decode-part mime (funcall make-output-stream)))))))

(defun process-mail (mail make-output-stream)
  ;;(format t "processing mail ~a~%" mail)
  (let ((mime (mi:parse-mime mail)))
    (process-part mime make-output-stream)))


(with-open-file (in #p"../../../dmarc-data/mails/mail01.eml"
                    :direction :input)
  (process-mail in #'(lambda ()
                       *standard-output*)))
