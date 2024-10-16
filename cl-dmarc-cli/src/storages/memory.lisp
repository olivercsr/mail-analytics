(in-package :cl-dmarc-cli)

(defclass memory-storage () ())

(defmethod store-report ((storage memory-database) report)
  (format t "memory-storage ~a~%" report))
