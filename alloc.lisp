(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *constant-index* -1)

;; varod == variable-od
;; constod == constant od
;; pointerod == pointer-od
;; voidod = void od
;; funcod == function od
;; bifuncod = built-in function od

(defun $a-var (typename space offset)
  (make-instance 'od-indirect :dtype typename :base space :key offset))

(defun $a-pointer (space offset)
  (make-instance 'od-indirect :dtype "pointer" :base space :key offset))

(defun $a-void ()
  (make-instance 'od-direct :dtype "void"))

  ;; a leaf constant contains an atomic value, e.g. number, string, char, boolean, etc.
  ;; a composite initialized constant is made up of a pointer to something probably in the *constants* base
  ;; only leaf constants can be od-direct
(defun $a-manifestconst (typename value)
  (make-instance 'od-direct :dtype typename :value value))

(defun $a-initialized (typename base key value)
  (make-instance 'od-ininitialized :dtype typename :value value :base base :key key))

