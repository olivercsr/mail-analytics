(in-package :mail-processor)

(defun process-mail (mail)
  (format t "processing mail ~a~%" mail)
  (let ((mime (mi:parse-mime mail)))
    (case (type-of mime)
      (mi:text-mime (print *standard-output* (mi:content mime)))
      (mi:multipart-mime
       (dolist (part (mi:content mime))
         (if (and (string-equal (mi:content-type part) "text")
                  (string-equal (mi:content-subtype part) "plain"))
             (mi:print-mime *standard-output* part nil nil))))
      (mi:mime (print "mime"))
      (t (print "other")))))
