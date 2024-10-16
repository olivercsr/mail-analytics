(in-package :cl-dmarc-cli)

;;(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun find-nodes (nodes &key (node-p #'identity) (wanted-p #'identity) (children #'cdr) (results nil))
  (let ((node       (car nodes))
        (rest-nodes (cdr nodes)))
    (if (funcall node-p node)
        (find-nodes (concatenate 'list
                                 rest-nodes
                                 (funcall children node))
                    :results  (if (funcall wanted-p node)
                                  (cons node results)
                                  results)
                    :node-p   node-p
                    :wanted-p wanted-p
                    :children children)
        results)))

;;(defun find-nodes (node value &key (node-p #'identity) (node-value #'car) (node-children #'cdr))
;;  (when (funcall node-p node)
;;    (mapcan
;;     #'identity
;;     (cons
;;      (when (equal (funcall node-value node)
;;                   value)
;;        (list node))
;;      (mapcar #'(lambda (node)
;;                  (find-nodes node value :node-p node-p :node-value node-value :node-children node-children))
;;              (funcall node-children node))))))

(defun report-metadata-node (xml)
  (car (find-nodes (list xml)
                   :node-p   #'x:node-p
                   :wanted-p #'(lambda (node)
                                 (equal (x:node-name node)
                                        "report_metadata"))
                   :children #'x:node-children)))

(defun parse-xml (stream)
  (x:parse stream))



(find-nodes '((11 ((22) (33) (44 ((55) (99) (33 ((44) (33))))) (33 ((11))))))
            :wanted-p #'(lambda (node)
                          (eql (car node)
                               33)))
