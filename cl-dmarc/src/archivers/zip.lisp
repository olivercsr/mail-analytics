(in-package #:archiver.zip)

(se:defun unarchive (in-stream out-handler)
  (format t "unarchive ~a ~a~%" (type-of in-stream) in-stream)
  (zip:with-zip-file (zip-file in-stream)
    (loop for entry across (zip:entries zip-file)
          do (let ((entry-out (flex:make-in-memory-output-stream
                               :element-type '(unsigned-byte 8))))
               (format t "processing zip file entry ~a~%"
                       (zip:file-name entry))
               (zip:entry-to-stream entry-out entry)
               (funcall out-handler (-> entry-out
                                      (flex:get-output-stream-sequence)
                                      (flex:make-in-memory-input-stream))))))
  ;;(with-open-file (in #p"../../../dmarc-data/compressed/google.com!csr-informatik.de!1728864000!1728950399.zip"
  ;;                    :element-type '(unsigned-byte 8))
  ;;  (with-open-file (out #p"foobar"
  ;;                       :direction :output
  ;;                       :if-exists :supersede
  ;;                       :if-does-not-exist :create
  ;;                       :element-type '(unsigned-byte 8))
  ;;    (zip:with-zip-file (zip in)
  ;;      (format t "~a~%" (zip:entry-to-stream out (elt (zip:entries zip) 0))))))
  )

;;(with-open-file (in #p"../../../dmarc-data/compressed/foo.zip"
;;                    :element-type '(unsigned-byte 8))
;;  (let ((ar (make-array 202
;;                        :adjustable nil
;;                        :fill-pointer nil
;;                        :displaced-to nil
;;                        :element-type (stream-element-type in))))
;;    (read-sequence ar in)
;;    (format t "xxx ~a ~a~%" (type-of in) (type-of ar))
;;    (unarchive ar
;;               #'(lambda (entry)
;;                   (format t "ENTRY ~a ~a~%" (type-of entry) (loop for c = (read-byte entry nil :eof)
;;                                                                   while (not (equal c :eof))
;;                                                                   collect c))))))
