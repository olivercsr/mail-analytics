(in-package :file-processor)

(defclass filesystem-file-processor ()
  ((source-path :initform #p"source"
                :initarg :source-path)
   (processing-path :initform #p"processing"
                    :initarg :processing-path)
   (done-path :initform #p"done"
              :initarg :done-path)
   (failed-path :initform #p"failed"
                :initarg :failed-path)))

(defmethod lock-next-file ((file-processor filesystem-file-processor))
  (let* ((file (-> (list (slot-value file-processor 'source-path))
                   (shared:traverse-nodes
                    :action #'(lambda (path &rest args)
                                (declare (ignore args))
                                (uiop:directory-files (truename path)
                                                      "*.xml"))
                    :children #'(lambda (path)
                                  (uiop:subdirectories (truename path)))
                    :merge-results #'(lambda (new-results results)
                                       (concatenate 'list
                                                    new-results
                                                    results)))
                   car))
         (processing-path (-> (slot-value file-processor 'processing-path)
                              truename
                              namestring)))
    (format t "lock-next-file ~a~%" file)
    (when file
      (rename-file (pathname file)
                   (->> file
                     file-namestring
                     (concatenate 'string processing-path)
                     pathname)))))

(defmethod file-done ((file-processor filesystem-file-processor) file)
  (format t "file-done ~a~%" file))

(defmethod archive-done-files ((file-processor filesystem-file-processor))
  (format t "archive-done-files~%"))

(defmethod reset-stuck-files ((file-processor filesystem-file-processor))
  (format t "reset-stuck-files~%"))


;;(let ((ffp (make-instance 'filesystem-file-processor
;;                          :source-path #p"../../../dmarc-data/source/"
;;                          :processing-path #p"../../../dmarc-data/processing/")))
;;  (lock-next-file ffp))

;;(->> "/foo/bar.md"
;;  file-namestring
;;  (concatenate 'string "/doo/")
;;  pathname)

(rename-file)
