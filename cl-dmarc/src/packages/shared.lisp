(defpackage :shared
  (:use :cl :arrow-macros)
  (:export :traverse-nodes

   :make-reporter
           :reporter-id
           :reporter-org-name
           :reporter-email
           :reporter-extra-contact-info

           :make-report
           :make-evaluation
           :make-dkim-evaluation
           :make-spf-evaluation
           ))
