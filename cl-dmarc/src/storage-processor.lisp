(in-package :storage-processor)

(se:defclass storage-processor ()
  ((pubsub :initarg :pubsub)
   (storage :initarg :storage)
   (pubsub-thread)))

(se:defun storage-handler (pubsub body props &rest args)
  (declare (ignorable body props))
  (format t "STORAGE-HANDLER ~a ~a~%~%" pubsub args)
  )

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
