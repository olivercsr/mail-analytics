(in-package :file-processor)

(defgeneric lock-next-file (file-processor))
(defgeneric file-done (file-processor file))
(defgeneric reset-stuck-files (file-processor))

(se:defclass file-processor ()
  (pubsub-thread))

(defmethod au:start ((startable file-processor) &rest args)
  (declare (ignorable args))
  (format t "start file-processor~%")
  ;; TODO
  )

(defmethod au:stop ((startable file-processor))
  (format t "stop file-processor~%")
  ;; TODO
  )
