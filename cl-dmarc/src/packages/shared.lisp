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
           :reporter-extra-contact-info))
