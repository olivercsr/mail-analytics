(defpackage :shared
  (:use :cl :arrow-macros)
  (:export :traverse-nodes

   :reporter
   :report
   :evaluation
   :dkim-evaluation
   :spf-evaluation

           :reporter-id
           :reporter-org-name
           :reporter-email
           :reporter-extra-contact-info

           :report-report_id
           :report-begin
           :report-end
           :report-reporter_id
           :report-error
           :report-policy_domain
           :report-policy_adkim
           :report-policy_aspf
           :report-policy_p
           :report-policy_sp
           :report-policy_pct
           :report-policy_fo))
