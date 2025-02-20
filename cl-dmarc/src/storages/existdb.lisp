(in-package :storage.existdb)

(se:defclass existdb-storage ()
  ((base-url :initform (error 'st:parameter-missing
                              :parameter-name "base-url")
             :initarg  :base-url
             :reader   base-url)
   (username :initform ""
             :initarg  :username
             :reader   username)
   (password :initform ""
             :initarg  :password
             :reader   password)
   (collection :initform (error 'st:parameter-missing
                                :parameter-name "collection")
               :initarg  :collection
               :reader   collection)))


(defmethod st:store-report ((db existdb-storage) id report)
  (format t "[EXISTDB] store-report ~a ~a~%" id report)
  (let ((url (format nil "~a/exist/rest/db/~a/~a"
                     (base-url db)
                     (collection db)
                     id)))
    (http:http-request url
                       :method :put
                       :basic-authorization (list (username db)
                                                  (password db))
                       :content-type "application/xml"
                       :content report
                       )))
