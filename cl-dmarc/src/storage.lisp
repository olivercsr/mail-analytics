(in-package :storage)

(define-condition parameter-missing (error)
  ((parameter-name :initarg :parameter-name
                   :initform nil
                   :reader parameter-name))
  (:documentation "Signals that a mandatory parameter is missing"))

(se:defgeneric upsert-reporter (storage reporter))
(se:defgeneric store-report (storage id report))
(se:defgeneric get-report (storage id))
(se:defgeneric find-reports-by-daterange (storage start-date end-date))
