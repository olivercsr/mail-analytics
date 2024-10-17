(in-package :cl-dmarc-cli)

(defclass postgres-storage () (pg-client))

(defmethod store-report ((db postgres-storage) id report)
  (format t "postgres-storage ~a ~a~%" id report))
