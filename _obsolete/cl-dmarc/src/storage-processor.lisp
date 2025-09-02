(in-package :storage-processor)

(se:defclass storage-processor ()
  ((pubsub :initarg :pubsub)
   (storage :initarg :storage)
   (pubsub-thread)))

(se:defun make-storage-handler (this)
  #'(lambda (pubsub data props &rest args)
      (declare (ignorable props))
      (let* ((decoded  (with-input-from-string (in (babel:octets-to-string data))
                         (cl-json:decode-json in)))
             (filename (cdr (assoc :filename decoded)))
             (body     (let ((l (cdr (assoc :body decoded))))
                         (make-array (length l)
                                     :initial-contents l
                                     :element-type '(unsigned-byte 8)))))
        (format t "STORAGE-HANDLER ~a ~a ~a ~a ~a ~a ~a ~a~%~%"
                pubsub
                (type-of data) data
                (type-of filename) filename
                (type-of body) body
                args)
        (st:store-report this filename body))))

(defmethod au:start ((startable storage-processor) &rest args)
  (declare (ignorable args))
  (format t "start storage-processor~%")
  (with-slots (pubsub pubsub-thread stoage)
      startable
    (let ((thread (make-instance 'au:bordeaux-threadable
                                 :handler #'(lambda ()
                                              (au:start pubsub)
                                              (ps:consume pubsub)
                                              (au:stop pubsub)))))
      (au:start thread)
      (setf pubsub-thread thread)
      startable)))

(defmethod au:stop ((startable storage-processor))
  (format t "stop storage-processor~%")
  (with-slots (pubsub-thread)
      startable
    (au:stop pubsub-thread)
    (setf pubsub-thread nil)
    startable))

;;(with-input-from-string (in (babel:octets-to-string (babel:string-to-octets "{\"foo\": 123}")))
;;  (cl-json:decode-json in))
