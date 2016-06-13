(in-package #:conet-dhcp-server)

(defvar *dhcp-server-port* 67)

(defclass dhcp-server ()
  ((subnet :type na:network :initarg :subnet :reader subnet)))

(defmethod start ((server dhcp-server))
  (log:info (format nil "starting dhcp server with subnet ~A" (subnet server)))
  (usocket:socket-server "0.0.0.0"
                         1111
                         (lambda (buffer)
                           (handle-buffer server buffer))
                         nil
                         :protocol :datagram
                         :in-new-thread nil))

(defmethod handle-buffer ((server dhcp-server) buffer)
  buffer)
