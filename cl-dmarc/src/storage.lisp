(in-package :storage)

(define-condition parameter-missing (error)
  ((parameter-name :initarg :parameter-name
                   :initform nil
                   :reader parameter-name))
  (:documentation "Signals that a mandatory parameter is missing"))

(defgeneric upsert-reporter (storage reporter))
(defgeneric store-report (storage id report))
(defgeneric get-report (storage id))
(defgeneric find-reports-by-daterange (storage start-date end-date))
