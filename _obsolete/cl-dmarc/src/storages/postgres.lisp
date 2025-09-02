(in-package :storage)

(defclass postgres-storage () ())

(defmethod upsert-reporter ((db postgres-storage) reporter)
  (pg:execute (:insert-into 'reporter
               :set
               ;;'id 123
               'org_name (reporter-org-name reporter)
               'email (reporter-email reporter)
               'extra_contact_info (reporter-extra-contact-info reporter)
               :on-conflict-update 'id
               :update-set
               'org-name (reporter-org-name reporter)
               'email (reporter-email reporter)
               'extra_contact_info (reporter-extra-contact-info reporter))))

(defmethod store-report ((db postgres-storage) id report)
  (format t "postgres-storage ~a ~a~%" id report)
  (pg:execute (:insert-into 'reporter
               :set
               'report_id id
               'begin (report-begin report)
               'end (report-end report)
               'reporter_id (report-reporter-id report)
               'error (report-error report)
               'policy_domain (report-policy-domain report)
               'policy_adkim (report-policy-adkim report)
               'policy_aspf (report-policy-aspf report)
               'policy_p (report-policy-p report)
               'policy_sp (report-policy-sp report)
               'policy_pct (report-policy-pct report)
               'policy_fo (report-policy-fo report)
               :on-conflict-update '(report_id begin)
               :update-set
               'end (report-end report)
               'reporter_id (report-reporter-id report)
               'error (report-error report)
               'policy_domain (report-policy-domain report)
               'policy_adkim (report-policy-adkim report)
               'policy_aspf (report-policy-aspf report)
               'policy_p (report-policy-p report)
               'policy_sp (report-policy-sp report)
               'policy_pct (report-policy-pct report)
               'policy_fo (report-policy-fo report))))

(defmethod get-report ((db postgres-storage) id)
  (format t "aaaaaaaaaaaaa"))

(defmethod find-reports-by-daterange ((db postgres-storage) start-date end-date)
  )



;; NOTE: start postgres with e.g.
;;   podman run -it --rm -p 5432:5432 -e POSTGRES_USER=dmarc \
;;     -e POSTGRES_PASSWORD=dmarc -e POSTGRES_DB=dmarc-tool \
;;     docker.io/postgres:16-alpine
;;
;;(pg:with-connection '("dmarc-tool" "dmarc" "dmarc" "localhost"
;;                      :port 5432
;;                      :pooled-p nil
;;                      :use-binary t
;;                      ;;:use-ssl :try
;;                      :application-name "dmarc-tool")
;;  (format t "db ~a~%" pg:*database*)
;;  (let ((s (make-instance 'postgres-storage
;;                          :connection nil)))
;;    (pg:execute (:insert-into 'foo :set '_id 123 'name "heyjo"))
;;    (pg:query (:select '_id 'name :from 'foo))
;;    ))
