;;;;ray-tracer.asd
;;;;
;;;;Copyright (c) 2022 Izaak Walton

(asdf:defsystem #:ray-tracer
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Public License"
  :description "A ray-tracer in Common Lisp"
  :depends-on (#:zpng)
  :serial t
  :components ((:file "package")
	       (:file "vectors")
	       (:file "draw")
	       (:file "gradient")))
