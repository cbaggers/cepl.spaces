;;; cepl.spaces.asd

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:cepl.spaces
  :description "Adds abstractions over vector spaces to CEPL"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:fn
               #:rtg-math
               #:varjo
               #:cepl
               #:documentation-utils)
  :components ((:file "constants")
               (:file "nht-routes")
               (:file "space")
               (:file "predefined-spaces")
               (:file "pos")
               (:file "space-errors")
               (:file "space-walking")
               (:file "space-transforms")
               (:file "pos-funcs")
               (:file "gpu")
               (:file "docs")))
