(in-package :cepl.space)

;;----------------------------------------------------------------------
;; spatial relationship
;;
;; {TODO} add function based relationships
;; - could also do with way of saying "just #'affine-inverse it"
;; - basically this needs more options

(defstruct (spatial-relationship (:constructor %make-sr) (:conc-name sr-))
  (source-id 0 :type fixnum :read-only t)
  (target-id 0 :type fixnum :read-only t)
  (to nil :type (or null mat4) :read-only nil)
  (from nil :type (or null mat4) :read-only nil))

(defmethod print-object ((r spatial-relationship) stream)
  (let ((source (if (> (sr-source-id r) -1)
                    (%space-ref (sr-source-id r))
                    "M")))
    (cond
      ((and (sr-to r) (sr-from r))
       (format stream "#<~a ↔ ~s>" source (%space-ref (sr-target-id r))))
      ((sr-to r)
       (format stream "#<~a → ~s>" source (%space-ref (sr-target-id r))))
      (t (format stream "#<~a ← ~s>" source (%space-ref (sr-target-id r)))))))

(defun make-spatial-relationship (source-id target-id to-m4 from-m4)
  (%make-sr :source-id source-id
            :target-id target-id
            :to to-m4
            :from from-m4))

;;----------------------------------------------------------------------
;; Space

(defconstant +model-space+ 0)
(defconstant +relational-space+ 1)
(defconstant +hierachical-space+ 2)

(defvar *last-space-id* -1)

(defstruct (vec-space (:constructor %make-space) (:conc-name %space-))
  (uid (incf *last-space-id*) :type fixnum :read-only t)
  (nht-id (error "id must be provided") :type fixnum :read-only t)
  (kind (error "space kind must be provided") :type (mod 3) :read-only t)
  (parent nil :type (or null vec-space) :read-only t)
  (children nil :type (or null (array vec-space (*))) :read-only t)
  (root nil :type (or null vec-space) :read-only t)
  (neighbours (make-array 0 :element-type 'spatial-relationship
                          :initial-element (%make-sr))
              :type (array spatial-relationship (*))
              :read-only t)
  ;;
  ;; # optimization data
  ;;
  ;; distance to the root:
  ;; when transforming between hierarchical spaces with same root
  ;; it lets you start walking from the deeper one first, which means
  ;; that if they are on the same path from root the walk will find the
  ;; less deep node and thus early out.
  (depth (error "space depth must be provided") :type fixnum))

(defmethod print-object ((space vec-space) stream)
  (case= (%space-kind space)
    (+model-space+ (format stream "#<R1-SPACE ~s>" (%space-uid space)))
    (+relational-space+ (format stream "#<R-SPACE ~s>" (%space-uid space)))
    (+hierachical-space+ (format stream "#<H-SPACE ~s>" (%space-uid space)))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Constructors

;; usage idas:
;; (make-space* (space-a m4to) (space-b m4to m4from))

(defun make-space* (&rest relationships)
  (unless relationships
    (error "CEPL.SPACE: All spaces must be created with at least 1 relationship"))
  (if (eq :parent (first relationships))
      (dbind (&key parent (transform (m4:identity)))
          relationships
        (%make-heirarchical-space parent transform))
      (%make-relational-space (parse-relationships relationships))))

;; (make-space a m4to)
;; (make-space a m4to m4from)

(defun make-space (target-space transform-to &optional transform-from)
  (%make-model-space target-space transform-to transform-from))

(defun disconnect-space (space)
  (declare (ignore space))
  (error "Please implement disconnect-space"))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Space cache

(defparameter *spaces-array-growth-rate* 64)

(defun make-space-array ()
  (make-array *spaces-array-growth-rate*
              :element-type '(or null vec-space)
              :initial-element nil
              :adjustable t :fill-pointer 0))

(defparameter spaces (make-space-array))

(defun %add-space-to-array (space)
  (when (>= (%space-nht-id space) (length spaces))
    (adjust-array spaces (+ *spaces-array-growth-rate* (length spaces))
                  :element-type '(or null vec-space)
                  :initial-element nil))
  (setf (aref spaces (%space-nht-id space)) space)
  space)

(defun %space-ref (id)
  (aref spaces id))

(defmethod free ((space vec-space))
  (free-space space))

(defun free-space (space)
  (let ((id (%space-nht-id space)))
    (when (> id 0)
      (setf (aref spaces id) nil)
      (cepl.space.routes:free-id id))
    (disconnect-space space)
    nil))

;;----------------------------------------------------------------------
;; Hierarchical Space

(defun %make-heirarchical-space (parent &optional (transform (m4:identity)))
  (assert parent)
  (let ((space
         (%make-space
          :nht-id -1 :kind +hierachical-space+ :parent parent
          :root (%space-root parent) :depth (1+ (%space-depth parent))
          :children (make-array 0 :element-type 'spatial-relationship
                                :adjustable t :fill-pointer 0)
          :neighbours (make-array
                       1 :element-type 'spatial-relationship
                       :initial-element (make-spatial-relationship
                                         -1 -1 transform nil)))))
    (vector-push-extend space (%space-children parent))
    (%add-space-to-array space)
    space))

(defun parent-space (space)
  (or (%space-parent space)
      (case= (%space-kind space)
        (+hierachical-space+
         (error "You have found bug in CEPL:~%Hierarchical space without parent found: ~s" space))
        (+model-space+
         (error "Model spaces do not have a parent space.~%~s" space))
        (+relational-space+
         (error "Relational spaces do not have a parent space.~%~s" space)))))

;;----------------------------------------------------------------------
;; Relational Space

(defun relational-space-p (space)
  (= (%space-kind space) +relational-space+))

(defun make-relational-space (relationships)
  (%make-relational-space
   (parse-relationships relationships)))

(defun %make-relational-space (relationships)
  (let* ((id (cepl.space.routes:id!))
         (spatial-relationships
          (mapcar (lambda (x)
                    (dbind (target to-m4 from-m4) x
                      (make-spatial-relationship
                       id (%space-nht-id target) to-m4 from-m4)))
                  relationships))
         (space
          (%make-space :nht-id id
                       :kind +relational-space+
                       :neighbours (make-array
                                    (length spatial-relationships)
                                    :element-type 'spatial-relationship
                                    :initial-contents spatial-relationships)
                       :depth 0)))
    (cepl.space.routes:add-id id (mapcar #'sr-target-id spatial-relationships))
    (%add-space-to-array space)
    space))

(defun parse-relationships (relationships)
  (mapcar #'parse-relationship relationships))

(defun parse-relationship (r)
  (dbind (target-space &optional (to-m4 (m4:identity)) (from-m4 (m4:identity)))
      (listify r)
    (list target-space to-m4 from-m4)))

(defun %rspace-to-neighbour-relationship (from-space to-space)
  (let ((from-id (%space-nht-id from-space))
        (to-id (%space-nht-id to-space)))
    (if (> (%space-uid from-space) (%space-uid to-space))
        (let* ((owning-space from-space))
          (or (find to-id (%space-neighbours owning-space)
                    :test #'= :key #'sr-target-id)
              (error "couldnt not find space ~s in neighbours for space ~s"
                     (%space-ref to-id) owning-space)))
        (let* ((owning-space to-space))
          (or (find from-id (%space-neighbours owning-space)
                    :test #'= :key #'sr-target-id)
              (error "couldnt not find space ~s in neighbours for space ~s"
                     (%space-ref to-id) owning-space))))))

(defun %rspace-to-neighbour-transform (from-id to-id)
  (let* ((from-space (%space-ref from-id))
         (to-space (%space-ref to-id))
         (relationship (%rspace-to-neighbour-relationship from-space to-space)))
    (if (> from-id to-id)
        (or (sr-to relationship)
            (error "relationship exists between ~s and ~s but it is one way"
                   (%space-ref from-id) (%space-ref to-id)))
        (or (sr-from relationship)
            (error "relationship exists between ~s and ~s but it is one way"
                   (%space-ref to-id) (%space-ref from-id))))))

(defun %set-rspace-to-neighbour-transform (from-space to-space transform)
  (let* ((from-id (%space-nht-id from-space))
         (to-id (%space-nht-id to-space))
         (relationship (%rspace-to-neighbour-relationship from-space to-space)))
    (if (> from-id to-id)
        (if (sr-to relationship)
            (setf (sr-to relationship) transform)
            (error "relationship exists between ~s and ~s but it is one way"
                   (%space-ref from-id) (%space-ref to-id)))
        (if (sr-from relationship)
            (setf (sr-from relationship) transform)
            (error "relationship exists between ~s and ~s but it is one way"
                   (%space-ref to-id) (%space-ref from-id))))))

;;----------------------------------------------------------------------
;; Model Space

(defun model-space-p (space)
  (= (%space-kind space) +model-space+))

(defun %make-model-space (target-space transform-to &optional transform-from)
  (%make-space
   :nht-id -1 :kind +model-space+ :depth 0
   :neighbours (make-array
                1 :element-type 'spatial-relationship
                :initial-element (make-spatial-relationship
                                  -1 (%space-nht-id target-space)
                                  transform-to
                                  (or transform-from
                                      (m4:affine-inverse transform-to))))))

(defun upgrade-from-model-space (model-space)
  (declare (ignore model-space))
  (error "implement upgrade-from-model-space"))

(declaim (inline %mspace-only-sr))
(defun %mspace-only-sr (mspace)
  (declare ;;(optimize (speed 3) (debug 0))
   (inline %space-neighbours))
  (aref (%space-neighbours mspace) 0))

(defun %update-mspace-transform (mspace transform)
  (setf (sr-to (aref (%space-neighbours mspace) 0))
        transform)
  (error "implement %update-mspace-transform"))

;;----------------------------------------------------------------------
;; Helpers

(defmacro kind-case ((space &key error) &key m r h)
  (let ((vec-space-g (gensym "space")))
    `(let ((,vec-space-g ,space))
       (case= (%space-kind ,vec-space-g)
         (+model-space+
          ,(or m (when error
                   '(error "m->m transform is not valid here"))))
         (+relational-space+
          ,(or r (when error
                   '(error "m->r transform is not valid here"))))
         (+hierachical-space+
          ,(or h (when error
                   '(error "m->h transform is not valid here"))))))))

(defmacro kind-case* ((from to &key error) &key m->m m->r m->h
                                             r->m r->r r->h
                                             h->m h->r h->h)
  (let ((from-g (gensym "from"))
        (to-g (gensym "to")))
    `(let ((,from-g ,from)
           (,to-g ,to))
       (case= (%space-kind ,from-g)
         (+model-space+
          ,(if (or m->m m->r m->h)
               `(case= (%space-kind ,to-g)
                  (+model-space+
                   ,(or m->m (when error
                               '(error "m->m transform is not valid here"))))
                  (+relational-space+
                   ,(or m->r (when error
                               '(error "m->r transform is not valid here"))))
                  (+hierachical-space+
                   ,(or m->h (when error
                               '(error "m->h transform is not valid here")))))
               (when error '(error "m->* transforms are not valid here"))))
         (+relational-space+
          ,(if (or r->m r->r r->h)
               `(case= (%space-kind ,to-g)
                  (+model-space+
                   ,(or r->m (when error
                               '(error "r->m transform is not valid here"))))
                  (+relational-space+
                   ,(or r->r (when error
                               '(error "r->r transform is not valid here"))))
                  (+hierachical-space+
                   ,(or r->h (when error
                               '(error "r->h transform is not valid here")))))
               (when error '(error "r->* transforms are not valid here"))))
         (+hierachical-space+
          ,(if (or h->m h->r h->h)
               `(case= (%space-kind ,to-g)
                  (+model-space+
                   ,(or h->m (when error
                               '(error "h->m transform is not valid here"))))
                  (+relational-space+
                   ,(or h->r (when error
                               '(error "h->r transform is not valid here"))))
                  (+hierachical-space+
                   ,(or h->h (when error
                               '(error "h->h transform is not valid here")))))
               (when '(error "h->* transforms are not valid here"))))))))
