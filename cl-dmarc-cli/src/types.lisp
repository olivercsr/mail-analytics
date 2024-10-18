(in-package :cl-dmarc-cli)

(defstruct report
  id
  org-name
  email
  start-date
  end-date)

(defstruct policy
  domain
  adkim
  aspf
  p
  sp
  pct
  np)

(defstruct policy-evaluation
  disposition
  dkim
  spf)

(defstruct auth-result-dkim
  domain
  selector
  result)

(defstruct auth-result-spf
  domain
  result)

(defstruct record-row
  source-ip
  policy-evaluation
  header-from
  auth-result-dkim
  auth-result-spf
  count)
