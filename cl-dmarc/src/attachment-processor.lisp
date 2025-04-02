(in-package :attachment-processor)

;;(defgeneric lock-next-file (attachment-processor))
;;(defgeneric file-done (attachment-processor file))
;;(defgeneric reset-stuck-files (attachment-processor))

(se:defclass attachment-processor ()
  ((pubsub :initarg :pubsub)
   (pubsub-thread)))

(se:defun attachment-handler (pubsub body props &rest args)
  (declare (ignorable props))
  (format t "ATTACHMENT-HANDLER ~a ~a~%~%" pubsub args)
  (ar:ensure-unarchived body #'(lambda (entry)
                                 (format t "ensure-unarchived ~a~%" entry))))

(defmethod au:start ((startable attachment-processor) &rest args)
  (declare (ignorable args))
  (format t "start attachment-processor~%")
  (with-slots (pubsub pubsub-thread)
      startable
    (let ((thread (make-instance 'au:bordeaux-threadable
                                 :handler #'(lambda ()
                                              (au:start pubsub)
                                              (ps:consume pubsub)
                                              (au:stop pubsub)))))
      (au:start thread)
      (setf pubsub-thread thread)
      startable)))

(defmethod au:stop ((startable attachment-processor))
  (format t "stop attachment-processor~%")
  (with-slots (pubsub-thread)
      startable
    (au:stop pubsub-thread)
    (setf pubsub-thread nil)
    startable))
