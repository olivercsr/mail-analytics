(in-package :file-processor)

(defclass filesystem-file-processor ()
  ((base-path :initform #p"."
              :initarg :base-path)))

(defmethod lock-next-file ((file-processor filesystem-file-processor))
  (let ((files (shared:traverse-nodes (list (slot-value file-processor 'base-path))
                                      :action #'(lambda (path &rest args)
                                                  (declare (ignore args))
                                                  (uiop:directory-files (truename path)
                                                                        "*.xml"))
                                      :children #'(lambda (path)
                                                    (uiop:subdirectories (truename path)))
                                      :merge-results #'(lambda (new-results results)
                                                         (concatenate 'list
                                                                      new-results
                                                                      results)))))
    ))

(defmethod file-done ((file-processor filesystem-file-processor) file)
  (format t "file-done ~a~%" file))

(defmethod archive-done-files ((file-processor filesystem-file-processor))
  (format t "archive-done-files~%"))

(defmethod reset-stuck-files ((file-processor filesystem-file-processor))
  (format t "reset-stuck-files~%"))

;;(let ((ffp (make-instance 'filesystem-file-processor
;;                          :base-path #p".")))
;;  (lock-next-file ffp))
