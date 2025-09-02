(in-package #:cl-user)
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 0) (compilation-speed 0)))

(defun streams01 ()
  (let ((s (with-output-to-string (x)
             (format x "output (~a)" (type-of x)))))
    (format t "stuff was: ~s~%" s)))


(deftype octet () '(unsigned-byte 8))

(defclass octet-input-stream (fundamental-binary-input-stream)
  ((data :initarg :data
         :type (vector octet))
   (position :initform 0)))

(defmethod stream-element-type ((stream octet-input-stream))
  'octet)

(defmethod stream-read-byte ((stream octet-input-stream))
  (with-slots (data position) stream
    (if (< position (length data))
        (prog1 (aref data position)
          (incf position))
        :eof)))

(defun streams02 ()
  (let* ((arr (make-array 3 :element-type 'octet
                            :initial-contents '(50 100 150)))
         (stream (make-instance 'octet-input-stream :data arr)))
    ;; NOTE: this means that sbcl internally knows how to call gray streams' implementation
    ;;   methods when operating on a stream class instance that is derived from the gray
    ;;   streams class(es):
    (loop for octet = (read-byte stream nil nil)
          while octet
          do (format t "~8,'0b~%" octet))))


(defun streams03 ()
  (let ((s (with-output-to-string (x)
             ;(setq x (flex:make-flexi-stream x :external-format :utf-8))
             (write-char #\Ä x)
             (write-string "äöü" x)
             (write-sequence "abc" x)
             ;;(write-byte 123 x)
             )))
    (format t "output: ~s~%" s)))


;;(streams01)
;;(streams02)
;;(streams03)
