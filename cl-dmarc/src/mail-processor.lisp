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

(defun process-part (mime)
  (format t "process-part enter ~a ~a (~a/~a) ~a ~a~%"
          (mi:content-disposition mime) (mi:content-disposition-parameters mime)
          (mi:content-type mime) (mi:content-subtype mime)
          (mi:content-transfer-encoding mime) (type-of (mi:content mime)))
  (case (type-of mime)
    (mi:multipart-mime (dolist (part (mi:content mime))
                         (process-part part)))
    (t (a:switch ((mi:content-disposition mime) :test #'equal)
         ("attachment" (format t "OUT: ~a~%"
                               (length (with-output-to-string (out)
                                         (decode-part mime out))))
                       ;;(a:switch ((mi:content-subtype mime) :test #'equal)
                       ;;  ("gzip" (format t "found gzip mime-part~%"))
                       ;;  ("zip"  (format t "found zip mime-part~%")))
                       ))))
  ;;(case (type-of mime)
  ;;  ;;(mi:text-mime (print (mi:content mime)))
  ;;  (mi:text-mime (format t "text-mime~%"))
  ;;  (mi:multipart-mime
  ;;   (progn
  ;;     ;;(format t "multipart~%")
  ;;     (dolist (part (mi:content mime))
  ;;       ;;(format t "part ~a/~a~%" (mi:content-type part) (mi:content-subtype part))
  ;;       (process-part part)
  ;;       ;;(if (and (string-equal (mi:content-type part) "text")
  ;;       ;;         (string-equal (mi:content-subtype part) "plain"))
  ;;       ;;    (mi:print-mime *standard-output* part nil nil))
  ;;       )))
  ;;  (mi:mime (format t "mime~%"))
  ;;  (t (format t "other~%")))
  )

(defun process-mail (mail)
  ;;(format t "processing mail ~a~%" mail)
  (let ((mime (mi:parse-mime mail)))
    (process-part mime)))


(with-open-file (in #p"../../../dmarc-data/mails/mail01.eml"
                    :direction :input)
  (process-mail in))
