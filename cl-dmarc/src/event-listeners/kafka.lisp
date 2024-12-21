;;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

(in-package :event-listener)

(defclass kafka-event-listener ()
  ((address :initform "localhost:9092"
            :initarg  :address)
   (handler :initform #'(lambda (arg)
                          (format t "HANDLER: ~a~%" arg))
            :initarg  :handler)
   (consumer)))

(defmethod connect ((event-listener kafka-event-listener))
  (let* ((consumer (make-instance 'kf:consumer
                                  :conf '("bootstrap.servers" "127.0.0.1:9092"
                                          "group.id" "dmarc-importer"
                                          "enable.auto.commit" "false"
                                          "auto.offset.reset" "earliest"
                                          "allow.auto.create.topics" "true")
                                  :serde #'babel:octets-to-string
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
         )
    ;;(break)
    ;;(format t "=========================== ~a ~a~%" conf consumer)
    (format t "CONNECT ~a~%" consumer)
    (kf:subscribe consumer "dmarc-file-received")
    (setf (slot-value event-listener 'consumer)
          consumer)
    event-listener))

(defmethod disconnect ((event-listener kafka-event-listener))
  (format t "DISCONNECT~%")
  event-listener)
