(in-package #:conet-dhcp-server)

(defclass dhcp-server ()
  ((subnet :type na:network :initarg :subnet :reader subnet)))

(defmethod start ((server dhcp-server))
  (log:info (format nil "starting dhcp server with subnet ~A" (subnet server))))
