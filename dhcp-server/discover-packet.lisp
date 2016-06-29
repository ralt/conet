(in-package #:conet-dhcp-server)

(defclass dhcp-discover-packet (dhcp-packet) ())

(defmethod reply ((packet dhcp-discover-packet))
  "Returns a dhcp-offer-packet."
  packet)
