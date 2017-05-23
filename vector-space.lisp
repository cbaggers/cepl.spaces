(in-package :cepl.spaces)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------
;; Vector Space

(eval-when (:compile-toplevel :load-toplevel :execute)
  (varjo:v-deftype vec-space-g () ())
  (add-alternate-type-name 'vec-space 'vec-space-g))

(defmethod cepl.pipelines::infer-implicit-uniform-type
    ((thing vec-space))
  'vec-space-g)

;;-------------------------------------------------------------------------
