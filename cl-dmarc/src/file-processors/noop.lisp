(in-package :file-processor)

(defclass noop-file-processor () ())

(defmethod lock-next-file ((file-processor noop-file-processor))
  (format t "noop~%"))
