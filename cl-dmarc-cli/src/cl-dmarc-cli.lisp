(in-package :cl-dmarc-cli)

;; Define your project functionality here...

;;(defvar test-xml
;;  "<?xml version=\"1.0\"?>
;;<!-- test document -->
;;<book title='The Cyberiad'>
;;  <!-- comment in here -->
;;  <author xmlns='http://authors'>Stanislaw Lem</author>
;;  <info:subject xmlns:info='http://bookinfo' rank='1'>&quot;Cybernetic Fables&quot;</info:subject>
;;</book>")

;;(defun greet (&optional (name "Oliver Wegner"))
;;  (format t "Hello ~a from ~a!~&" name "cl-dmarc-cli"))

(defun help ()
  (format t "~&Usage:

  cl-dmarc-cli [name]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  ;;(greet (or (first argv)
  ;;           "dear lisp user"))
  ;;(let ((dmarc-data (parse-xml *standard-input*)))
  ;;  (format t "~a - ~a~%" (report-metadata-node dmarc-data) dmarc-data))
  )

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))



;;(greet)

;;(with-open-file (stream "../../dmarc-data/amazonses.com\!csr-informatik.de\!1711065600\!1711152000.xml")
;;  (let* (;;(xml-data (parse-xml stream))
;;         ;;(metadata (report-metadata-node xml-data))
;;         (cxml-data (cxml:parse-stream stream (cxml-dom:make-dom-builder)))
;;         ;;context (xpath:make-context cxml-data)
;;         )
;;    (format t "data: ~a~%" cxml-data)
;;    ;;(process-report xml-data)
;;    ;;(xpath:evaluate "//child" context)
;;    ))

;;(let* ((dom (x:parse-file "../dmarc-data/enterprise.protection.outlook.com!csr-informatik.de!1715299200!1715385600.xml"
;;                          (xd:make-dom-builder)))
;;       ;;(result (xp:evaluate "//report_metadata" dom))
;;       )
;;  ;;(format t "hhhhhhhhhhhhhhhhhhhh ~a ~a~%" dom (dom:child-nodes (car (xp:all-nodes result))))
;;  (read-records dom))
