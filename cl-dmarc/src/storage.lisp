(in-package :storage)

(defgeneric store-report (storage id report))
(defgeneric get-report (storage id))
(defgeneric find-reports-by-daterange (storage start-date end-date))
