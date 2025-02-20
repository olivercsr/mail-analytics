(in-package #:file-extractor.zip)

(se:defclass zip-file-extractor () ())

(defmethod fp:extract ((extractor zip-file-extractor) stream)
  (format t "extract ~a~%" stream)
  (with-open-file (in #p"../../../dmarc-data/compressed/google.com!csr-informatik.de!1728864000!1728950399.zip"
                      :element-type '(unsigned-byte 8))
    (with-open-file (out #p"foobar"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (org.shirakumo.zippy:with-zip-file (zip in)
        (format t "~a~%" (org.shirakumo.zippy:entry-to-stream out (elt (org.shirakumo.zippy:entries zip) 0)))))))
