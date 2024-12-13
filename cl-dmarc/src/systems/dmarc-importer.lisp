;;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))
(in-package :dmarc-importer)
;;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

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
;;  (format t "Hello ~a from ~a!~&" name "dmarc-importer"))

(defun help ()
  (format t "~&Usage:

  dmarc-importer [name]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (let ((event-listener (-> (make-instance 'el:kafka-event-listener)
                            (el:connect))))
    (format t "EVENT-LISTENER: ~a~%" event-listener))
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

;;(pg:with-connection '("dmarc" "dmarc" "dmarc" "localhost"
;;                      :port 5432
;;                      :pooled-p nil
;;                      :use-binary t
;;                      ;;:use-ssl :try
;;                      :application-name "dmarc-tool")
;;  (let* ((db (make-instance 'postgres-storage))
;;         (dom (x:parse-file "../dmarc-data/enterprise.protection.outlook.com!csr-informatik.de!1715299200!1715385600.xml"
;;                            (xd:make-dom-builder)))
;;         ;;(result (xp:evaluate "//report_metadata" dom))
;;         (reporter-fn #'(lambda (reporter)
;;                          (upsert-reporter db reporter)))
;;         (report-fn #'(lambda (&rest args)
;;                        (format t "====== ~a~%" args)
;;                        123)))
;;    ;;(format t "hhhhhhhhhhhhhhhhhhhh ~a ~a~%" dom (dom:child-nodes (car (xp:all-nodes result))))
;;    (read-records dom reporter-fn report-fn report-fn report-fn report-fn)))

;;(main)

;;(defun myffi ()
;;  (let* ((conf (cl-rdkafka/ll:rd-kafka-conf-new))
;;         (errstrlen 200)
;;         (consumer (cffi:with-foreign-object (errstr :char errstrlen)
;;                     (cl-rdkafka/ll:rd-kafka-new cl-rdkafka/ll:rd-kafka-consumer
;;                                                 conf
;;                                                 ;;(cffi:null-pointer)
;;                                                 errstr
;;                                                 errstrlen
;;                                                 ))))
;;    (format t "===================== ~a ~a ~a~%" conf errstr consumer)
;;    consumer))
