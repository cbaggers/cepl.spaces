;;;; package.lisp

(uiop:define-package :cepl.spaces.routes
    (:use :cl :fn :named-readtables
          :cepl-utils :cepl.errors :cepl.defn)
  (:export :id! :free-id :reset :get-route :map-route :reduce-route :add-id))

(uiop:define-package :cepl.spaces
    (:use :cl
          :cepl.defn :cepl-utils  :cepl.types :cepl.errors
          :cepl.internals :cepl.pipelines :cepl.memory
          :rtg-math.types :rtg-math
          :named-readtables
          :varjo :vari)
  (:shadowing-import-from :rtg-math :m! :v!)
  (:export :space :vec-space :make-space :make-space*
           :parent-space :model-space-p :relational-space-p
           :get-transform :get-transform-via
           :with-space-routing-via :in
           :*screen-space* :*ndc-space* :*clip-space* :*world-space*
           :sv! :svec4))
