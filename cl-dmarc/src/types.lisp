(in-package :shared)

(defstruct reporter
  id
  org-name
  email
  extra-contact-info)

(defstruct report
  report-id
  begin
  end
  reporter-id
  error
  policy-domain
  policy-adkim
  policy-aspf
  policy-p
  policy-sp
  policy-pct
  policy-fo)

(defstruct evaluation
  id
  report-id
  begin
  source-ip
  count
  disposition
  dkim
  spf
  envelope-from
  envelope-to
  header-from)

(defstruct dkim-evaluation
  id
  evaluation-id
  domain
  selector
  result
  human-result)

(defstruct spf-evaluation
  id
  evaluation-id
  domain
  scope
  result)
