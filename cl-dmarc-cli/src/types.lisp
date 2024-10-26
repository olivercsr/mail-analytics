(in-package :cl-dmarc-cli)

(defstruct report
  id
  org-name
  email
  start-date
  end-date)

(defstruct policy
  id
  domain
  adkim
  aspf
  p?
  sp?
  pct
  np)

(defstruct identifier
  envelope-from
  envelope-to
  header-from)

(defstruct auth-result-dkim
  domain
  selector
  result)

(defstruct auth-result-spf
  domain
  result)

(defstruct policy-evaluation
  disposition
  dkim
  spf)

(defstruct record-row
  source-ip
  policy-evaluation
  header-from
  auth-results-dkim
  auth-results-spf
  count)
