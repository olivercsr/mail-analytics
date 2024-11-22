(in-package :dmarc-reading)

;;(declaim (optimize (speed 0) (space 0) (debug 3)))

;;(defun traverse-nodes (nodes
;;                       &key (node-p #'identity)
;;                         (action #'(lambda (&rest args)
;;                                     (declare (ignore args))))
;;                         (children #'cdr)
;;                         (parents '())
;;                         (results '()))
;;  (let ((node       (car nodes))
;;        (rest-nodes (cdr nodes)))
;;    (if (funcall node-p node)
;;        (let ((result (funcall action node parents)))
;;          (traverse-nodes (concatenate 'list
;;                                       rest-nodes
;;                                       (funcall children node))
;;                          :action   action
;;                          :node-p   node-p
;;                          :children children
;;                          :parents  (cons node parents)
;;                          :results  (cons result results)))
;;        results)))

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

(defun read-records (dom reporter-fn report-fn evaluation-fn dkim-fn spf-fn)
  (let* ((metadata-set (xp:evaluate "/feedback/report_metadata" dom))
         (metadata     (car (xp:all-nodes metadata-set)))
         (policy-set   (xp:evaluate "/feedback/policy_published" dom))
         (policy       (car (xp:all-nodes policy-set)))
         (records-set  (xp:evaluate "/feedback/record" dom)))
    (xp:do-node-set (record records-set)
      (let* ((reporter (make-reporter :id nil
                                      :org-name (xp:string-value (xp:evaluate "org_name" metadata))
                                      :email (xp:string-value (xp:evaluate "email" metadata))
                                      :extra-contact-info (xp:string-value (xp:evaluate "extra_contact_info" metadata))))
             (reporter-result (funcall reporter-fn reporter))
             (report (make-report :report-id (xp:string-value (xp:evaluate "report-id" metadata))
                                  :begin (xp:string-value (xp:evaluate "date_range/begin" metadata))
                                  :end (xp:string-value (xp:evaluate "date_range/end" metadata))
                                  :error (xp:string-value (xp:evaluate "error" metadata))
                                  :policy-domain (xp:string-value (xp:evaluate "domain" policy))
                                  :policy-adkim (xp:string-value (xp:evaluate "adkim" policy))
                                  :policy-aspf (xp:string-value (xp:evaluate "aspf" policy))
                                  :policy-p (xp:string-value (xp:evaluate "p" policy))
                                  :policy-sp (xp:string-value (xp:evaluate "sp" policy))
                                  :policy-pct (xp:string-value (xp:evaluate "pct" policy))
                                  :policy-fo (xp:string-value (xp:evaluate "fo" policy))))
             (report-result (funcall report-fn reporter-result report))
             (evaluation (make-evaluation :report-id (xp:string-value (xp:evaluate "report_id" metadata))
                                          :begin (xp:string-value (xp:evaluate "date_range/begin" metadata))
                                          :source-ip (xp:string-value (xp:evaluate "row/source_ip" record))
                                          :count (xp:number-value (xp:evaluate "row/count" record))
                                          :disposition (xp:string-value (xp:evaluate "row/policy_evaluated/disposition" record))
                                          :dkim (xp:string-value (xp:evaluate "row/policy_evaluated/dkim" record))
                                          :spf (xp:string-value (xp:evaluate "row/policy_evaluated/spf" record))
                                          :envelope-from (xp:string-value (xp:evaluate "identifiers/envelope_from" record))
                                          :envelope-to (xp:string-value (xp:evaluate "identifiers/envelope_to" record))
                                          :header-from (xp:string-value (xp:evaluate "identifiers/header_from" record))))
             (evaluation-result (funcall evaluation-fn reporter-result report-result evaluation))
             (dkim-evaluations (mapcar #'(lambda (dkim)
                                           (make-dkim-evaluation :domain (xp:string-value (xp:evaluate "domain" dkim))
                                                                 :selector (xp:string-value (xp:evaluate "selector" dkim))
                                                                 :result (xp:string-value (xp:evaluate "result" dkim))
                                                                 :human-result (xp:string-value (xp:evaluate "human_result" dkim))))
                                       (xp:all-nodes (xp:evaluate "auth_results/dkim" record))))
             (dkim-evaluation-results (mapcar #'(lambda (dkim-evaluation)
                                                  (funcall dkim-fn reporter-result report-result evaluation-result dkim-evaluation))
                                              dkim-evaluations))
             (spf-evaluations (mapcar #'(lambda (spf)
                                          (make-spf-evaluation :domain (xp:string-value (xp:evaluate "domain" spf))
                                                               :scope (xp:string-value (xp:evaluate "scope" spf))
                                                               :result (xp:string-value (xp:evaluate "result" spf))))
                                      (xp:all-nodes (xp:evaluate "auth_results/spf" record))))
             (spf-evaluation-results (mapcar #'(lambda (spf-evaluation)
                                                 (funcall spf-fn reporter-result report-result evaluation-result spf-evaluation))
                                             spf-evaluations)))
        (list :reporter reporter-result
              :report report-result
              :evaluation evaluation-result
              :dkim-evaluations dkim-evaluation-results
              :spf-evaluations spf-evaluation-results)))))



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
