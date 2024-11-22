(in-package :dmarc-finder)

(defclass noop-dmarc-finder () ())

(defmethod find-dmarc-reports ((dmarc-finder noop-dmarc-finder))
  (format t "noop~%"))
