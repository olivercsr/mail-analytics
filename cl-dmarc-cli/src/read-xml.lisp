(in-package :cl-dmarc-cli)

;;(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun traverse-nodes (nodes
                       &key (node-p #'identity)
                         (action #'(lambda (&rest args)
                                     (declare (ignore args))))
                         (children #'cdr)
                         (parents '()))
  (let ((node       (car nodes))
        (rest-nodes (cdr nodes)))
    (when (funcall node-p node)
      (funcall action node parents)
      (traverse-nodes (concatenate 'list
                                   rest-nodes
                                   (funcall children node))
                      :action   action
                      :node-p   node-p
                      :children children
                      :parents  (cons node parents)))))

(defun report-metadata-node (xml)
  (car (traverse-nodes (list xml)
                       :node-p   #'x:node-p
                       :wanted-p #'(lambda (node)
                                     (equal (x:node-name node)
                                            "report_metadata"))
                       :children #'x:node-children)))

(defun parse-xml (stream)
  (x:parse stream))



;;(let ((policies-evaluated '()))
;;  (traverse-nodes '((11 ((22) (33) (44 ((55) (99) (33 ((44) (33))))) (33 ((11))))))
;;                  :action #'(lambda (node parents)
;;                              ;;(format t "node: ~a parents: ~a~%" node parents)
;;                              (when (equal (car node) 33)
;;                                (setf policies-evaluated
;;                                      (cons (car node)
;;                                            policies-evaluated)))))
;;  policies-evaluated)

;;(let ((al nil))
;;  (setf al (acons :x 11 al))
;;  (setf al (acons :y 22 al))
;;  (list al
;;        (assoc :y al)
;;        (rassoc 11 al)))
