(in-package #:conet-dhcp-server)

(defclass dhcp-offer-packet (dhcp-packet) ())

(defun make-dhcp-offer-packet (xid yiaddr siaddr mac subnet-mask router lease-time dns-servers)
  (let ((options (list
                  (make-option +dhcp-message-type+ +dhcpoffer+)
                  (make-option +subnet-mask+ subnet-mask)
                  (make-option +router+ router)
                  (make-option +ip-address-lease-time+ lease-time)
                  (make-option +server-identifier+ router)
                  (make-option +dns-name-server+ dns-servers))))
    (make-dhcp-packet 'dhcp-offer-packet
     :operation-code +reply-message+
     :xid xid
     :ciaddr 0
     :yiaddr yiaddr
     :siaddr siaddr
     :mac mac
     :options (make-options options))))
