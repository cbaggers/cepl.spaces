(in-package :cepl.spaces)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------
;; Type

(eval-when (:compile-toplevel :load-toplevel :execute)

  (varjo:v-deftype svec4-g () :vec4
                   :valid-metadata-kinds spatial-meta)

  (varjo:add-alternate-type-name 'svec4 'svec4-g)

  (varjo:v-deftype svec3-g () :vec3
                   :valid-metadata-kinds spatial-meta)

  (varjo:add-alternate-type-name 'svec3 'svec3-g))

;;-------------------------------------------------------------------------
;; Metadata type

(varjo:def-metadata-kind spatial-meta ()
  in-space)

(varjo:def-metadata-infer svec4 spatial-meta env
  (values :in-space (get-current-space env)))

(varjo:def-metadata-infer svec3 spatial-meta env
  (values :in-space (get-current-space env)))

(defmethod varjo:combine-metadata ((meta-a spatial-meta)
                                   (meta-b spatial-meta))
  (let ((space-a (in-space meta-a))
        (space-b (in-space meta-b)))
    (if (eq space-a space-b)
        meta-a
        (error "Space Analysis Failed: Could not establish at compile time which
space the resulting svec was in between:
~a
and
~a" space-a space-b))))

;;-------------------------------------------------------------------------
;; Spatial Vec4

(varjo:def-shadow-type-constructor svec4 #'(v! :vec4))
(varjo:def-shadow-type-constructor svec4 #'(v! :vec3 :float))
(varjo:def-shadow-type-constructor svec4 #'(v! :vec2 :float :float))
(varjo:def-shadow-type-constructor svec4 #'(v! :float :float :float :float))

(varjo:v-define-compiler-macro svec4 (&whole whole &environment env (vec :vec4))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,vec)))

(varjo:v-define-compiler-macro svec4 (&whole whole &environment env
                                             (v3 :vec3) (f :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,v3 ,f)))

(varjo:v-define-compiler-macro svec4 (&whole whole &environment env
                                             (v2 :vec2) (f2 :float)
                                             (f3 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,v2 ,f2 ,f3)))

(varjo:v-define-compiler-macro svec4 (&whole whole &environment env
                                             (f0 :float) (f1 :float)
                                             (f2 :float) (f3 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f0 ,f1 ,f2 ,f3)))


(v-def-glsl-template-fun v! (p) "~a" (svec4-g) :vec4)
(v-def-glsl-template-fun svec-* (a b) "(~a * ~a)" (v-mat4 svec4-g) 1)
(v-def-glsl-template-fun svec-* (a b) "(~a * ~a)" (v-mat4 :vec4) 1)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-mat4 svec4-g) 1)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-mat4 :vec4) 1)

;;-------------------------------------------------------------------------
;; Spatial Vec3

(varjo:def-shadow-type-constructor svec3 #'(v! :vec3))
(varjo:def-shadow-type-constructor svec3 #'(v! :vec2 :float))
(varjo:def-shadow-type-constructor svec3 #'(v! :float :float :float))

(varjo:v-define-compiler-macro svec3 (&whole whole &environment env (vec :vec3))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      vec))

(varjo:v-define-compiler-macro svec3 (&whole whole &environment env
                                             (v2 :vec2) (f2 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,v2 ,f2)))

(varjo:v-define-compiler-macro svec3 (&whole whole &environment env
                                             (f0 :float) (f1 :float)
                                             (f2 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f0 ,f1 ,f2)))


(v-def-glsl-template-fun v! (p) "~a" (svec3-g) :vec3)

(v-def-glsl-template-fun svec-* (a b) "(~a * ~a)" (v-mat3 svec3-g) 1)
(v-def-glsl-template-fun svec-* (a b) "(~a * ~a)" (v-mat3 :vec3) 1)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-mat3 svec3-g) 1)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-mat3 :vec3) 1)

;;-------------------------------------------------------------------------
;; Unified Constructor

(v-def-glsl-template-fun sv! (v) "~a" (:vec3) svec3)
(v-def-glsl-template-fun sv! (v f) "vec3(~a,~a)" (:vec2 :float) svec3)
(v-def-glsl-template-fun sv! (f v) "vec3(~a,~a)" (:float :vec2) svec3)
(v-def-glsl-template-fun sv! (f0 f1 f2) "vec3(~a,~a,~a)" (:float :float :float)
                         svec3)

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (v :vec3))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      v))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (v :vec2) (f :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,v ,f)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (f :float) (v :vec2))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f ,v)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (f0 :float) (f1 :float) (f2 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f0 ,f1 ,f2)))

(v-def-glsl-template-fun sv! (v) "~a" (:vec4) svec4)
(v-def-glsl-template-fun sv! (v f) "vec4(~a,~a)" (:vec3 :float) svec4)
(v-def-glsl-template-fun sv! (f v) "vec4(~a,~a)" (:float :vec3) svec4)
(v-def-glsl-template-fun sv! (v0 v1) "vec4(~a,~a)" (:vec2 :vec2) svec4)
(v-def-glsl-template-fun sv! (v f f) "vec4(~a,~a,~a)" (:vec2 :float :float)
                         svec4)
(v-def-glsl-template-fun sv! (f v f) "vec4(~a,~a,~a)" (:float :vec2 :float)
                         svec4)
(v-def-glsl-template-fun sv! (f f v) "vec4(~a,~a,~a)" (:float :float :vec2)
                         svec4)
(v-def-glsl-template-fun sv! (f0 f1 f2 f3) "~a" (:float :float :float :float)
                         svec4)

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (v :vec4))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      v))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (v :vec3) (f :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,v ,f)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (f :float) (v :vec3))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f ,v)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (v0 :vec2) (v1 :vec2))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,v0 ,v1)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (v :vec2) (f2 :float) (f3 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,v ,f2 ,f3)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (f2 :float) (v :vec2) (f3 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f2 ,v ,f3)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (f2 :float) (f3 :float) (v :vec2))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f2 ,f3 ,v)))

(varjo:v-define-compiler-macro sv! (&whole whole &environment env
                                           (f0 :float) (f1 :float)
                                           (f2 :float) (f3 :float))
  (if (varjo:variable-in-scope-p '*current-space* env)
      whole
      `(v! ,f0 ,f1 ,f2 ,f3)))
