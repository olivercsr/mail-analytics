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
                                 (let* ((char-count 0)
                                        (entry-vector (flex:with-output-to-sequence (out)
                                                        (loop for b = (read-byte entry-stream nil :eof)
                                                              until (eq b :eof)
                                                              do (progn
                                                                   (incf char-count)
                                                                   (write-byte b out))))))
                                   (adjust-array entry-vector (length entry-vector))
                                   (format t "ensure-unarchived ~a ~a ~a ~a ~a ~a~%"
                                           filename char-count (length entry-vector) (type-of entry-vector) entry-stream entry-vector)
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
;;                     (loop for b = (read-byte in nil :eof)
;;                           until (equal b :eof)
;;                           do (progn
;;                                (format t "~a: ~a~%" b (type-of b))
;;                                (write-byte b out)
;;                                )))))
;;      (format t "out-str: ~a ~a~%" (type-of out-str) out-str))))

;;(with-open-file (in #p"../../dmarc-data/source/amazonses.com!csr-informatik.de!1711065600!1711152000.xml"
;;                    :element-type '(unsigned-byte 8))
;;  (let ((cnt 0))
;;    (loop for b = (read-byte in nil :eof)
;;          until (or (equal b :eof)
;;                    (>= cnt 1600))
;;          do (incf cnt))
;;    (format t "cnt: ~a~%" cnt)))

;;(remove-if #'null
;;           (list (when nil
;;                   '("foo" . 123))
;;                 (when t
;;                   '("bar" . 234))))

;;(let ((v (make-array 1 :adjustable t :fill-pointer 0))
;;      (cnt 0))
;;  (loop for i = 11
;;        until (> cnt 2731)
;;        do (progn
;;             (vector-push-extend i v)
;;             (incf cnt)))
;;  (adjust-array v (length v))
;;  (values (type-of v) (length v) (array-rank v) (array-dimensions v) (array-total-size v)))
