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

  ;; NOTE: rabbitmq-c (which we're using via ffi beneath the surface) does not support connection-sharing
  ;;   across threads and instead recommends creating a separate connection for each thread
  (let* ((storage (make-instance 'ste:existdb-storage
                                 :base-url   "http://localhost:8080"
                                 :username   "admin"
                                 :password   ""
                                 :collection "dmarc"))
         (mail-pubsub (make-instance 'psr:rabbit-pubsub
                                     :host "localhost"
                                     :port 5672
                                     :vhost "/"
                                     :user "guest"
                                     :password "guest"
                                     ;;:connection rabbit-connection-mails
                                     :channel 1
                                     ;;:channel rabbit-channel-mails
                                     :exchange "dmarcEmailMessages"
                                     :exchange-type "direct"
                                     :routing-key "dmarcEmailMessages"
                                     :queue "dmarcEmails"
                                     :handler #'mp:mail-handler))
         (attachment-pubsub (make-instance 'psr:rabbit-pubsub
                                           :host "localhost"
                                           :port 5672
                                           :vhost "/"
                                           :user "guest"
                                           :password "guest"
                                           ;;:connection rabbit-connection-mails
                                           :channel 1
                                           ;;:channel rabbit-channel-mails
                                           :exchange "mail-attachments"
                                           :exchange-type "direct"
                                           :routing-key "mail-attachments"
                                           :queue "mail-attachments"
                                           :handler #'ap:attachment-handler))
         (storage-pubsub (make-instance 'psr:rabbit-pubsub
                                        :host "localhost"
                                        :port 5672
                                        :vhost "/"
                                        :user "guest"
                                        :password "guest"
                                        :channel 1
                                        :exchange "dmarc-reports"
                                        :exchange-type "direct"
                                        :routing-key "dmarc-reports"
                                        :queue "dmarc-reports"
                                        :handler #'stp:storage-handler))
         (mail-processor (make-instance 'mp:mail-processor
                                        :pubsub mail-pubsub))
         (attachment-processor (make-instance 'ap:attachment-processor
                                              :pubsub attachment-pubsub))
         (storage-processor (make-instance 'stp:storage-processor
                                           :pubsub storage-pubsub
                                           :storage storage)))
    (au:start mail-processor)
    (au:start attachment-processor)
    (au:start storage-processor)
    (sleep 40)
    (au:stop storage-processor)
    (au:stop attachment-processor)
    (au:stop mail-processor))

  ;;(let ((pubsub (-> (make-instance 'elk:kafka-pubsub
  ;;                                         :address "localhost:9092"
  ;;                                         :group   "database-importer"
  ;;                                         :topics  '("dmarc-file-received"))
  ;;                          (ps:connect))))
  ;;  (format t "PUBSUB: ~a~%" pubsub)
  ;;  (sleep 60)
  ;;  (ps:disconnect pubsub))

  ;;(pg:with-connection '("dmarc" "dmarc" "dmarc" "localhost"
  ;;                      :port 5432
  ;;                      :pooled-p nil
  ;;                      :use-binary t
  ;;                      ;;:use-ssl :try
  ;;                      :application-name "dmarc-tool")
  ;;  (let ((db (make-instance 'st:postgres-storage)))
  ;;    (st:upsert-reporter db nil))) ;; TODO: implement

  ;;(let ((archiver (make-instance 'arz:zip-archiver)))
  ;;  (with-open-file (in #p"../../../dmarc-data/compressed/google.com!csr-informatik.de!1728864000!1728950399.zip"
  ;;                      :element-type '(unsigned-byte 8))
  ;;    (ar:unarchive archiver in #'(lambda (entry)
  ;;                                  ;;(format t "out-handler: ~a~%" entry)
  ;;                                  (with-open-file (out #p"barbaz"
  ;;                                                       :direction :output
  ;;                                                       :if-exists :supersede
  ;;                                                       :if-does-not-exist :create
  ;;                                                       :element-type '(unsigned-byte 8))
  ;;                                    ;;(write-sequence entry :stream out)
  ;;                                    (uiop:copy-stream-to-stream entry out
  ;;                                                                :element-type '(unsigned-byte 8)))))))

  ;;(let* ((existdb (make-instance 'ste:existdb-storage
  ;;                               :base-url   "http://localhost:8080"
  ;;                               :username   "admin"
  ;;                               :password   ""
  ;;                               :collection "dmarc-importer"))
  ;;       (path #p"../../../dmarc-data/source/google.com!csr-informatik.de!1707782400!1707868799.xml")
  ;;       (id (file-namestring path)))
  ;;  (format t "EXISTDB INSTANCE: ~a~%" existdb)
  ;;  (with-open-file (content path
  ;;                           :element-type '(unsigned-byte 8))
  ;;    (format t "================== ~a ~a ~a ~a"
  ;;            (streamp content)
  ;;            (input-stream-p content)
  ;;            (open-stream-p content)
  ;;            (stream-element-type content))
  ;;    (st:store-report existdb id content)))

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
  #+sbcl
  (sb-int:with-float-traps-masked
      ;; NOTE: otherwise, librdkafka will cause division-by-zero error:
      ;;   librdkafka will call libm. libm will use a double to represent infinity (by doing a
      ;;   division by zero), which is valid for floating point numbers in libc. however,
      ;;   sbcl seems to treat that as a fatal event and signal a division-by-zero error instead
      ;;   of continuing running the code with the inf double value.
      ;;
      ;;   also refer to:
      ;;   - https://bugs.launchpad.net/sbcl/+bug/1519630
      ;;   - https://github.com/sbcl/sbcl/blob/master/src/code/float-trap.lisp
      ;;   - https://www.gnu.org/software/libc/manual/html_node/FP-Exceptions.html
      (:divide-by-zero :invalid)
    (%main (uiop:command-line-arguments))))



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

;;(defun myffi ()
;;  (let* ((conf (cl-rdkafka/ll:rd-kafka-conf-new))
;;         (errstrlen 200)
;;         (consumer (cffi:with-foreign-object (errstr :char errstrlen)
;;                     (cl-rdkafka/ll:rd-kafka-conf-set conf "bootstrap.servers" "localhost" errstr errstrlen)
;;                     (cl-rdkafka/ll:rd-kafka-conf-set conf "group.id" "mygroup" errstr errstrlen)
;;                     (cl-rdkafka/ll:rd-kafka-conf-set conf "auto.offset.reset" "earliest" errstr errstrlen)
;;                     (cl-rdkafka/ll:rd-kafka-conf-set conf "allow.auto.create.topics" "true" errstr errstrlen)
;;                     (cl-rdkafka/ll:rd-kafka-new cl-rdkafka/ll:rd-kafka-consumer
;;                                                 conf
;;                                                 ;;(cffi:null-pointer)
;;                                                 errstr
;;                                                 errstrlen
;;                                                 ))))
;;    (format t "===================== ~a~%" consumer)
;;    consumer))

;;#+sbcl
;;(sb-int:with-float-traps-masked
;;    (:divide-by-zero :invalid)
;;  (myffi))

;;(main)
