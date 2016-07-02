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

(defgeneric reply (packet)
  (:documentation "Returns the reply for a DHCP request packet."))

(defun render (packet)
  "Returns a buffer of the rendered DHCP packet.")

(defun default-ip-address (iaddr)
  (or iaddr (make-byte-array #(0 0 0 0))))

(defun make-dhcp-packet (class &key
                           operation-code xid (ciaddr nil) (yiaddr nil)
                           (siaddr nil) (giaddr nil) mac options)
  (make-instance
   class
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

(define-condition dhcp-parse-error (error) ())

(defvar *parsers* (make-hash-table))

(defmacro define-packet-parser (option-type parser)
  `(setf (gethash ,option-type *parsers*) #',parser))

(define-packet-parser +dhcpdiscover+ make-dhcp-discover-packet)

(defun parse-dhcp-packet (buffer)
  "Returns a DHCP packet object from the raw buffer.

This function can do 2 things:
- Raise a dhcp-parse-error condition, if the buffer
  is not a DHCP packet
- Return a specalized dhcp-packet object.

'Specialized' here means that it will return either
a dhcp-discover-packet, a dhcp-request-packet, a
dhcp-information-packet or a dhcp-release-packet
object. This lets the caller immediately call the
'reply' method on these objects to get the dhcp-packet
object to render. (For example, a dhcp-discover-packet
will return a dhcp-offer-packet.)

The way to do this is the following:

- First, check some basic heuristics (like the first bytes,
  minimum length, etc)
- Then, go straight to the 'options' field and parse it
- Based on the option 53 (message type), we can know which
  dhcp-packet subclass should be used."
  (unless (looks-like-dhcp-packet buffer)
    (error 'dhcp-parse-error))
  ;; We just assume that the first option is the message type.
  ;; TODO: review this.
  (let ((message-type (elt buffer 242)))
    (funcall (gethash message-type *parsers*))))

(defun looks-like-dhcp-packet (buffer)
  "Decide if a buffer looks like a DHCP packet."
  (and
   ;; Operation code is valid
   (or (= (first buffer) #x01)
       (= (first buffer) #x02))
   ;; Minimum length
   (> (length buffer) 240)))
