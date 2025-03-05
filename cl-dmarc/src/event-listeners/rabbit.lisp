(in-package :event-listener.rabbit)

(se:defclass rabbit-event-listener ()
  ((host :initform "localhost"
         :initarg  :host)
   (port :initform 5672
         :initarg  :port)))

(defmethod el:connect ((event-listener rabbit-event-listener))
  (format t "RABBIT CONNECT")
  )

(defmethod el:disconnect ((event-listener rabbit-event-listener))
  (format t "RABBIT DISCONNECT"))
