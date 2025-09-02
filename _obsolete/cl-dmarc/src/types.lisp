(in-package :shared)

(sp:defconstructor reporter
  (id string)
  (org-name string)
  (email string)
  (extra-contact-info string))

(sp:defconstructor report
  (report-id string)
  (begin integer)
  (end integer)
  (reporter-id string)
  (error string)
  (policy-domain string)
  (policy-adkim string)
  (policy-aspf string)
  (policy-p integer)
  (policy-sp string)
  (policy-pct string)
  (policy-fo string))

(sp:defconstructor evaluation
  (id string)
  (report-id string)
  (begin integer)
  (source-ip string)
  (count integer)
  (disposition string)
  (dkim string)
  (spf string)
  (envelope-from string)
  (envelope-to string)
  (header-from string))

(sp:defconstructor dkim-evaluation
  (id string)
  (evaluation-id string)
  (domain string)
  (selector string)
  (result string)
  (human-result string))

(sp:defconstructor spf-evaluation
  (id string)
  (evaluation-id string)
  (domain string)
  (scope string)
  (result string))
