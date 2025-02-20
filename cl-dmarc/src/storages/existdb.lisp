(in-package :storage.existdb)

(se:defclass existdb-storage ()
  ((base-url :initform (error 'st:parameter-missing
                              :parameter-name "base-url")
             :initarg  :base-url
             :reader   base-url)))


(defmethod store-report ((db existdb-storage) id report)
  (format t "[EXISTDB] store-report ~a ~a~%" id report)
  (let ((url (format nil "http://localhost:8008")))
    (http:http-request "http://localhost:8080/exist/rest/db"
                       :method :put)))
