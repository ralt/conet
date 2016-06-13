(in-package #:cl-user)

;; A small tool to send DHCP packets for testing.

(ql:quickload :usocket)

;; Taken from https://en.wikipedia.org/wiki/Dynamic_Host_Configuration_Protocol#DHCP_discovery
(defvar *dhcp-discover-request-list*
  '(;; OP
    #x01
    ;; HType
    #x01
    ;; HLen
    #x06
    ;; Hops
    #x00
    ;; Xid
    #x39 #x03 #xF3 #x26
    ;; Secs
    #x00
    ;; Flags
    #x80 #x00
    ;; Client address
    #x00 #x00 #x00 #x00
    ;; Your address
    #x00 #x00 #x00 #x00
    ;; Server addres
    #x00 #x00 #x00 #x00
    ;; Gateway address
    #x00 #x00 #x00 #x00
    ;; MAC
    #x00 #x05 #x3C #x04
    #x8D #x59 #x00 #x00
    #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    ;; Magic cookie
    #x63 #x82 #x53 #x63
    ;; Discover
    53 1 1
    ;; Request list
    55 4 1 3 15 6))
(defvar *dhcp-discover-request* (make-array (length *dhcp-discover-request-list*)
                                            :element-type '(unsigned-byte 8)
                                            :initial-contents *dhcp-discover-request-list*))

(let ((s (usocket:socket-connect (second *posix-argv*)
                                 (parse-integer (third *posix-argv*))
                                 :protocol :datagram)))
  (usocket:socket-send s *dhcp-discover-request* (length *dhcp-discover-request*))
  (usocket:wait-for-input s :ready-only t)
  (format t "~A~%" (usocket:socket-receive s nil 1024)))

(uiop:quit 0)
