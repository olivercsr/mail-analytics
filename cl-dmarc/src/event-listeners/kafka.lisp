;;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

(in-package :event-listener.kafka)

(defclass kafka-event-listener ()
  ((address :initform "localhost:9092"
            :initarg  :address)
   (handler :initform #'(lambda (arg)
                          (format t "HANDLER: ~a~%" arg))
            :initarg  :handler)
   (topics :initform  '()
           :initarg   :topics)
   (group :initform   "mygroup"
          :initarg    :group)
   (consumer)
   (handler-thread)))

(defmethod connect ((event-listener kafka-event-listener))
  (format t "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ~a~%" event-listener)
  (let* ((consumer (make-instance 'kf:consumer
                                  :conf (list "bootstrap.servers"        (slot-value event-listener 'address)
                                              "group.id"                 (slot-value event-listener 'group)
                                              "enable.auto.commit"       "false"
                                              ;;"auto.offset.reset"        "earliest"
                                              "allow.auto.create.topics" "true")
                                  :serde #'babel:octets-to-string  ;; TODO: implement JSON deserialization? maybe even transit+JSON?
                                  ;;:key-serde 123
                                  ;;:value-serde 234
                                  ))
         ;;(conf (cl-rdkafka/ll:rd-kafka-conf-new))
         ;;(errstrlen 8192)
         ;;(consumer (cffi:with-foreign-object (errstr :char errstrlen)
         ;;            (cl-rdkafka/ll:rd-kafka-new cl-rdkafka/ll:rd-kafka-consumer
         ;;                                        ;;(cffi:null-pointer)
         ;;                                        conf
         ;;                                        errstr
         ;;                                        errstrlen)))
         (top-level *standard-output*))
    ;;(break)
    ;;(format t "=========================== ~a ~a~%" conf consumer)
    (format t "CONNECT ~a~%" consumer)
    (kf:subscribe consumer (slot-value event-listener 'topics))
    (setf (slot-value event-listener 'consumer)
          consumer)
    (setf (slot-value event-listener 'handler-thread)
          (bt2:make-thread #'(lambda ()
                               (format top-level "HANDLER-THREAD START~%")
                               (loop for msg = (progn
                                                 (format top-level "POLLING...~%")
                                                 (kf:poll consumer 5000))
                                     ;;while msg
                                     ;;for key = (kf:key msg)
                                     ;;for value = (kf:value msg)
                                     do (when msg
                                          (let ((key (kf:key msg))
                                                (value (kf:value msg)))
                                            (format top-level "MESSAGE RECEIVED ~a: ~a~%" key value)
                                            (kf:commit consumer))))
                               (format top-level "HANDLER-THREAD END~%"))))
    ;;(a:when-let ((msg (kf:poll consumer 30000)))
    ;;  (format t "MESSAGE RECEIVED: ~a => ~a~%" (kf:key msg) (kf:value msg))
    ;;  (kf:commit consumer))
    event-listener))

(defmethod disconnect ((event-listener kafka-event-listener))
  (format t "DISCONNECTING...~%")
  (let ((thread (slot-value event-listener 'handler-thread)))
    (when (bt2:thread-alive-p thread)
      (bt2:destroy-thread thread))
    (handler-case
        (bt2:join-thread thread)
      (bt2:abnormal-exit (c)
        (format t "ABNORMAL-EXIT ~a~%" c))))
  (setf (slot-value event-listener 'handler-thread)
        nil)
  (kf:unsubscribe (slot-value event-listener 'consumer))
  (kf:close (slot-value event-listener 'consumer))
  (setf (slot-value event-listener 'consumer)
        nil)
  (format t "DISCONNECTED~%")
  event-listener)
