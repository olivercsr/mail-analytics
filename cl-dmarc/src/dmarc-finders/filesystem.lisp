(in-package :dmarc-finder)

(defclass filesystem-dmarc-finder ()
  ((base-path :initform #p"."
              :initarg :base-path)))

(defmethod find-dmarc-reports ((dmarc-finder filesystem-dmarc-finder))
  (shared:traverse-nodes (list (slot-value dmarc-finder 'base-path))
                         :action #'(lambda (path &rest args)
                                     (declare (ignore args))
                                     (uiop:directory-files (truename path)
                                                           "*.xml"))
                         :children #'(lambda (path)
                                       (uiop:subdirectories (truename path)))
                         :merge-results #'(lambda (new-results results)
                                            (concatenate 'list
                                                         new-results
                                                         results))))


(let ((f (make-instance 'filesystem-dmarc-finder
                        :base-path #p".")))
  (find-dmarc-reports f))
