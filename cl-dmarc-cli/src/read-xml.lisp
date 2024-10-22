(in-package :cl-dmarc-cli)

;;(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun traverse-nodes (nodes
                       &key (node-p #'identity)
                         (action #'(lambda (&rest args)
                                     (declare (ignore args))))
                         (children #'cdr)
                         (parents '())
                         (results '()))
  (let ((node       (car nodes))
        (rest-nodes (cdr nodes)))
    (if (funcall node-p node)
        (let ((result (funcall action node parents)))
          (traverse-nodes (concatenate 'list
                                       rest-nodes
                                       (funcall children node))
                          :action   action
                          :node-p   node-p
                          :children children
                          :parents  (cons node parents)
                          :results  (cons result results)))
        results)))

;;(defun report-metadata-node (xml)
;;  (car
;;   (remove-if
;;    #'null
;;    (traverse-nodes (list xml)
;;                    :node-p #'x:node-p
;;                    :children #'x:node-children
;;                    :action #'(lambda (node parents)
;;                                (declare (ignore parents))
;;                                (when (equal (x:node-name node)
;;                                             "report_metadata")
;;                                  node))))))

;;(defun process-report (xml)
;;  (let ((report-metadata nil)
;;        (policy-published nil))
;;    (traverse-nodes (list xml)
;;                    :node-p #'x:node-p
;;                    :children #'x:node-children
;;                    :action #'(lambda (node parents)
;;                                (declare (ignore parents))
;;                                (cond ((equal (x:node-name node) "report_metadata")
;;                                       (setf report-metadata node))
;;                                      ((equal (x:node-name node) "policy_published")
;;                                       (setf policy-published node)))))
;;    (traverse-nodes (list xml)
;;                    :node-p #'x:node-p
;;                    :children #'x:node-children
;;                    :action #'(lambda (node parents)
;;                                (declare (ignore parents))
;;                                (when (equal (x:node-name node) "record")
;;                                  (let ((row nil))
;;                                    (format t "NNNNNNNNNNN ~a ~a ~a~%"
;;                                            (x:node-name report-metadata)
;;                                            (x:node-name policy-published)
;;                                            (x:node-name node))))))))

;;(defun parse-xml (stream)
;;  (x:parse stream))



;;(let* ((policies-evaluated '())
;;       (results (traverse-nodes '((11 ((22) (33) (44 ((55) (99) (33 ((44) (33))))) (33 ((11))))))
;;                                :action #'(lambda (node parents)
;;                                            ;;(format t "node: ~a parents: ~a~%" node parents)
;;                                            (when (equal (car node) 33)
;;                                              (setf policies-evaluated
;;                                                    (cons (car node)
;;                                                          policies-evaluated))
;;                                              (car node))))))
;;       `((:pe ,policies-evaluated)
;;         (:rs ,results)))

;;(let ((al nil))
;;  (setf al (acons :x 11 al))
;;  (setf al (acons :y 22 al))
;;  (list al
;;        (assoc :y al)
;;        (rassoc 11 al)))
