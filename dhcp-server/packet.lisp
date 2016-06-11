(in-package #:conet-dhcp-server)

(deftype byte-array (size)
  `(simple-array (unsigned-byte 8) ,size))

(defun make-byte-array (contents)
  (make-array (length contents)
              :element-type '(unsigned-byte 8)
              :initial-contents contents))

(defun make-single-byte-array (element)
  (make-array 1
              :element-type '(unsigned-byte 8)
              :initial-element element))

(defclass dhcp-packet-option ()
  ((code :type (byte-array 1) :initarg :code)
   (length :type (byte-array 1))
   (data :type (byte-array *) :initarg :data)))

(defmethod initialize-instance :after ((option dhcp-packet-option) &key)
  (setf (slot-value option 'length)
        (make-single-byte-array (length (slot-value option 'data)))))

(defun make-option (code data)
  (make-instance 'dhcp-packet-option
                 :code (make-single-byte-array code)
                 :data (make-byte-array data)))

(defclass dhcp-packet-options ()
  ((magic-cookie :type (byte-array 4) :initform #(#x63 #x82 #x53 #x63))
   (options :type (simple-array dhcp-packet-option *) :initarg :options)))

(defun make-options (options)
  (make-instance 'dhcp-packet-options
                 :options (make-array (length options)
                                      :element-type 'dhcp-packet-option
                                      :initial-contents options)))

(defclass dhcp-packet ()
  ((operation-code :type (byte-array 1) :initarg :operation-code)
   (hardware-type :type (byte-array 1) :initarg :hardware-type)
   (hardware-address-length :type (byte-array 1) :initarg :hardware-address-length)
   (hops :type (byte-array 1) :initarg :hops)
   (transaction-identifier :type (byte-array 4) :initarg :transaction-identifier)
   (seconds :type (byte-array 2) :initarg :seconds)
   (flags :type (byte-array 2) :initarg :flags)
   (client-ip-address :type (byte-array 4) :initarg :client-ip-address)
   (your-ip-address :type (byte-array 4) :initarg :your-ip-address)
   (server-ip-address :type (byte-array 4) :initarg :server-ip-address)
   (gateway-ip-address :type (byte-array 4) :initarg :gateway-ip-address)
   (client-hardware-address :type (byte-array 16) :initarg :client-hardware-address)
   (server-name :type (byte-array 64) :initarg :server-name)
   (file :type (byte-array 128) :initarg :file)
   (options :type dhcp-packet-options :initarg :options)))

(defun default-ip-address (iaddr)
  (or iaddr (make-byte-array #(0 0 0 0))))

(defun make-dhcp-packet (&key
                           operation-code xid (ciaddr nil) (yiaddr nil)
                           (siaddr nil) (giaddr nil) mac options)
  (make-instance
   'dhcp-packet
   :operation-code (make-single-byte-array operation-code)
   :hardware-type (make-single-byte-array +ethernet+)
   :hops (make-single-byte-array 0)
   :transaction-identifier xid
   :seconds (make-single-byte-array 0)
   :flags (make-single-byte-array 0)
   :client-ip-address (default-ip-address ciaddr)
   :your-ip-address (default-ip-address yiaddr)
   :server-ip-address (default-ip-address siaddr)
   :gateway-ip-address (default-ip-address giaddr)
   :client-hardware-address mac
   :server-name nil
   :file nil
   :options options))

(defun make-dhcp-offer-packet (xid yiaddr siaddr mac subnet-mask router lease-time dns-servers)
  (let ((options (list
                  (make-option +dhcp-message-type+ +dhcpoffer+)
                  (make-option +subnet-mask+ subnet-mask)
                  (make-option +router+ router)
                  (make-option +ip-address-lease-time+ lease-time)
                  (make-option +server-identifier+ router)
                  (make-option +dns-name-server+ dns-servers))))
    (make-dhcp-packet
     :operation-code +reply-message+
     :xid xid
     :ciaddr 0
     :yiaddr yiaddr
     :siaddr siaddr
     :mac mac
     :options (make-options options))))
