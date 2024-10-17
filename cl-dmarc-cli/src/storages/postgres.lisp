(in-package :cl-dmarc-cli)

(defclass postgres-storage () ((pg-connection :initarg :connection)))

(defmethod store-report ((db postgres-storage) id report)
  (format t "postgres-storage ~a ~a~%" id report))

(defmethod get-report ((db postgres-storage) id)
  (format t "aaaaaaaaaaaaa"))

(defmethod find-reports-by-daterange ((db postgres-storage) start-date end-date)
  )



;; NOTE: start postgres with e.g.
;;   podman run -it --rm -p 5432:5432 -e POSTGRES_USER=dmarc \
;;     -e POSTGRES_PASSWORD=dmarc -e POSTGRES_DB=dmarc-tool \
;;     docker.io/postgres:16-alpine
;;
;;(pg:with-connection '("dmarc-tool" "dmarc" "dmarc" "localhost"
;;                      :port 5432
;;                      :pooled-p nil
;;                      :use-binary t
;;                      ;;:use-ssl :try
;;                      :application-name "dmarc-tool")
;;  (format t "db ~a~%" pg:*database*)
;;  (let ((s (make-instance 'postgres-storage
;;                          :connection nil)))
;;    (pg:execute (:insert-into 'foo :set '_id 123 'name "heyjo"))
;;    (pg:query (:select '_id 'name :from 'foo))
;;    ))
