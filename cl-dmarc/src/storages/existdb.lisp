(in-package :storage.existdb)

(defclass existdb-storage () ())


(defmethod store-report ((db existdb-storage) id report)
  (format t "EXISTDB!")
  (http:http-request "http://localhost:8080/exist/rest/db"
                     :method :get))
