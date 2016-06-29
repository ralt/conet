(asdf:defsystem #:conet-dhcp-server
  :description "A DHCP server."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:unix-opts :log4cl :network-addresses :usocket)
  :components ((:file "package")
               (:file "constants")
               (:file "packet")
               (:file "offer-packet")
               (:file "discover-packet")
               (:file "server")
               (:file "main")))
