(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; var == variable dd
;; pointer == pointer dd
;; void = void dd

(defun $a-var (typename space offset)
  (make-instance 'dd-indirect :dtype typename :base space :key offset))

(defun $a-pointer (typename space offset)
  (declare (ignore typename))
  (make-instance 'dd-indirect :dtype "pointer" :base space :key offset))

(defun $a-void ()
  (make-instance 'dd-direct :dtype "void"))

  ;; a leaf constant contains an atomic value, e.g. number, string, char, boolean, etc.
  ;; a composite initialized constant is made up of a pointer to something probably in the *constants* base
  ;; only leaf constants can be od-direct
(defun $a-manifestconstant (typename value)
  (make-instance 'dd-direct :dtype typename :value value))

(defun $a-initialized (typename base key value)
  (make-instance 'dd-initialized-indirect :dtype typename :value value :base base :key key))

(defun $a-defsynonym (name d)
  ($g-defsynonym name d))

