(in-package :attachment-processor)

;;(defgeneric lock-next-file (attachment-processor))
;;(defgeneric file-done (attachment-processor file))
;;(defgeneric reset-stuck-files (attachment-processor))

(se:defclass attachment-processor ()
  ((pubsub :initarg :pubsub)
   (pubsub-thread)))

(se:defun attachment-handler (pubsub body props &rest args)
  (declare (ignorable props))
  (format t "ATTACHMENT-HANDLER ~a ~a~%~%" pubsub args)
  (ar:ensure-unarchived body #'(lambda (filename entry-stream)
                                 (let ((entry-vector (flex:with-output-to-sequence (out)
                                                       (loop for b = (read-byte entry-stream nil nil)
                                                             while b
                                                             do (write-byte b out)))))
                                   (format t "ensure-unarchived ~a ~a ~a~%" filename entry-stream entry-vector)
                                   (ps:produce pubsub "dmarc-reports" entry-vector)))))

(defmethod au:start ((startable attachment-processor) &rest args)
  (declare (ignorable args))
  (format t "start attachment-processor~%")
  (with-slots (pubsub pubsub-thread)
      startable
    (let ((thread (make-instance 'au:bordeaux-threadable
                                 :handler #'(lambda ()
                                              (au:start pubsub)
                                              (ps:consume pubsub)
                                              (au:stop pubsub)))))
      (au:start thread)
      (setf pubsub-thread thread)
      startable)))

(defmethod au:stop ((startable attachment-processor))
  (format t "stop attachment-processor~%")
  (with-slots (pubsub-thread)
      startable
    (au:stop pubsub-thread)
    (setf pubsub-thread nil)
    startable))

;;(let ((in-vector (babel:string-to-octets "foobar")))
;;  (format t "in-vector: ~a ~a~%" (type-of in-vector) in-vector)
;;  (flex:with-input-from-sequence (in (babel:string-to-octets "foobar"))
;;    (format t "in: ~a ~a~%" (type-of in) (stream-element-type in))
;;    (let ((out-str (flex:with-output-to-sequence (out)
;;                     (format t "out: ~a ~a~%" (type-of out) (stream-element-type out))
;;                     (loop for b = (read-byte in nil nil)
;;                           while b
;;                           do (progn
;;                                (format t "~a: ~a~%" b (type-of b))
;;                                (write-byte b out)
;;                                )))))
;;      (format t "out-str: ~a ~a~%" (type-of out-str) out-str))))

;;(remove-if #'null
;;           (list (when nil
;;                   '("foo" . 123))
;;                 (when t
;;                   '("bar" . 234))))
