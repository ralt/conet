(in-package #:conet-dhcp-server)

(defclass dhcp-discover-packet (dhcp-packet) ())

(defun make-dhcp-discover-packet (buffer)
  (make-instance 'dhcp-discover-packet))

(defmethod reply ((packet dhcp-discover-packet))
  "Returns a dhcp-offer-packet."
  packet)
