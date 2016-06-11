(in-package #:conet-dhcp-server)

;;; Reference: http://www.tcpipguide.com/free/t_DHCPMessagingMessageTypesandFormats.htm

;; Operation codes
(defconstant +request-message+ 1)
(defconstant +reply-message+ 2)

;; Hardware types
(defconstant +ethernet+ 1)

;; Options
(defconstant +subnet-mask+ 1)
(defconstant +router+ 3)
(defconstant +dns-name-server+ 6)
(defconstant +dhcp-message-type+ 53)
(defconstant +server-identifier+ 54)

;; Message types for option +dhcp-message-type+
(defconstant +dhcpdiscover+ 1)
(defconstant +dhcpoffer+ 2)
(defconstant +dhcprequest+ 3)
(defconstant +dhcpdecline+ 4)
(defconstant +dhcpack+ 5)
(defconstant +dhcpnak+ 6)
(defconstant +dhcprelease+ 7)
(defconstant +dhcpinform+ 8)
