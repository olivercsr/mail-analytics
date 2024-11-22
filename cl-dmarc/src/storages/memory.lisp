(in-package :storage)

(defclass memory-storage () ((htable :initform (make-hash-table))))

(defmethod store-report ((storage memory-storage) id report)
  (setf (gethash id (slot-value storage 'htable))
        report))

(defmethod get-report ((storage memory-storage) id)
  (gethash id (slot-value storage 'htable)))

(defmethod find-reports-by-daterange ((storage memory-storage) start-date end-date)
  nil)


;;(let ((ht (make-hash-table)))
;;  (setf (gethash :foo ht) 123.0)
;;  (gethash :foo ht))

;;(let ((s (make-instance 'memory-storage)))
;;  (store-report s :aaa (list 11 22 33))
;;  (store-report s :bbb (list 22 33 44))
;;  (format t "memory-storage: ~a~%~a~%"
;;          (slot-value s 'htable)
;;          (get-report s :bbb)))
