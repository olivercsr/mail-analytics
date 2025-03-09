(in-package :mail-processor)

(defun process-mail (mail)
  ;;(format t "processing mail ~a~%" mail)
  (let ((mime (mi:parse-mime mail)))
    (case (type-of mime)
      (mi:text-mime (print (mi:content mime)))
      (mi:multipart-mime
       (progn
         (format t "multipart~%")
         (dolist (part (mi:content mime))
           (if (and (string-equal (mi:content-type part) "text")
                    (string-equal (mi:content-subtype part) "plain"))
               (mi:print-mime *standard-output* part nil nil)))))
      (mi:mime (print "mime"))
      (t (print "other")))
    ))


(process-mail "From: Some One <someone@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed;
        boundary=\"XXXXboundary text\"

This is a multipart message in MIME format.

--XXXXboundary text
Content-Type: text/plain

this is the body text

--XXXXboundary text
Content-Type: text/plain;
Content-Disposition: attachment;
        filename=\"test.txt\"

this is the attachment text

--XXXXboundary text--")
