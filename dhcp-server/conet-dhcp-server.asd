(asdf:defsystem #:conet-dhcp-server
  :description "A DHCP server."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:network-addresses)
  :components ((:file "package")
               (:file "main")))
