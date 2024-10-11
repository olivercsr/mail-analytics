(in-package :cl-dmarc-cli)

(defun find-node (node-p node-value node-children node value)
  (format t "find-node ~a ~a~%" node value)
  (when (funcall node-p node)
    (if (equal (funcall node-value node) value)
        (progn
          (format t "hit ~a~%" node)
          node)
        (let ((res (mapcan #'(lambda (node)
                               (find-node node-p node-value node-children node value))
                           (funcall node-children node))))
          (format t "res ~a~%" res)
          res))))

(defun access-node (node name)
  (find-node #'x:node-p #'x:node-name #'x:node-children node name))

(defun parse-xml (stream)
  (x:parse stream))



(find-node #'identity #'car #'cdr '(11 ((22) (33) (44 ((55) (33))))) 33)
