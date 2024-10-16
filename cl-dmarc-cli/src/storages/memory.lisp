(in-package :cl-dmarc-cli)

(defclass memory-storage () ((htable :initform (make-hash-table))))

(defmethod store-report ((storage memory-database) id report)
  (setf (gethash id report)
        report))

(defmethod get-report ((storage memory-database) id)
  (gethash id report))

(defmethod find-reports-by-daterange ((storage memory-storage) start-date end-date)
  nil)


;;(let ((ht (make-hash-table)))
;;  (setf (gethash :foo ht) 123.0)
;;  (gethash :foo ht))

;;(let ((s (make-instance 'memory-storage)))
;;  (format t "memory-storage: ~a~%" (slot-value s 'htable)))
