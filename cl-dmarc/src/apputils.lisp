(in-package :apputils)

(se:defgeneric start (startable &rest args))
(se:defgeneric stop (startable))

(se:defclass bordeaux-threadable ()
  ((handler :initform #'(lambda ()
                          (format t "DEFAULT HANDLER~%"))
            :initarg :handler)
   (thread)))

(defmethod start ((threadable bordeaux-threadable) &rest args)
  (declare (ignorable args))
  (with-slots (handler thread)
      threadable
    (setf thread (bt2:make-thread handler))))

(defmethod stop ((threadable bordeaux-threadable))
  (with-slots (thread)
      threadable
    (when (bt2:thread-alive-p thread)
      (bt2:destroy-thread thread))
    (handler-case
        (bt2:join-thread thread)
      (bt2:abnormal-exit (c)
        (format t "abnormal-exit ~a~%" c)))
    (setf thread nil)))
