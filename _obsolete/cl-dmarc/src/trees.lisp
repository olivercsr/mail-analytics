(in-package :shared)

(defun traverse-nodes (nodes
                       &key (node-p #'identity)
                         (action #'(lambda (&rest args)
                                     (declare (ignore args))))
                         (children #'cdr)
                         (parents '())
                         (results '())
                         (merge-results #'cons))
  (let ((node       (car nodes))
        (rest-nodes (cdr nodes)))
    (if (funcall node-p node)
        (let ((result (funcall action node parents)))
          (traverse-nodes (concatenate 'list
                                       rest-nodes
                                       (funcall children node))
                          :action        action
                          :node-p        node-p
                          :children      children
                          :parents       (cons node parents)
                          :results       (funcall merge-results result results)
                          :merge-results merge-results))
        results)))
