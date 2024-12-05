(in-package :file-processor)

(defclass noop-file-processor () ())

(defmethod lock-next-file ((file-processor noop-file-processor))
  (format t "noop~%"))

(defmethod file-done ((file-processor noop-file-processor) file)
  (format t "noop~%"))

(defmethod archive-done-files ((file-processor noop-file-processor))
  (format t "noop~%"))

(defmethod reset-stuck-files ((file-processor noop-file-processor))
  (format t "noop~%"))

