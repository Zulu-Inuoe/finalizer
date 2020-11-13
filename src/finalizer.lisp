(defpackage #:finalizer
  (:use
   #:cl)
  (:import-from
   #:trivial-garbage)
  (:export
   #:make-finalizers
   #:define-finalizer
   #:finalizer
   #:register-finalizer
   #:cancel-finalizer

   #:finalizable))

(in-package #:finalizer)

(defgeneric make-finalizers (object)
  (:method-combination list)
  (:documentation
   "Get the ordered list of finalizers for `object'"))

(defmacro define-finalizer (class (&rest slots) &body body)
  "Defines a `finalizer' method for `class' in an which the slot-values values of `slots' are bound to their respective names.
Ex:

  (define-finalizer net-logger (sock log-stream)
    (usocket:socket-close sock)
    (close log-stream))"
  (let ((object-sym (gensym "OBJECT")))
    `(defmethod make-finalizers list ((,object-sym ,class))
       (let (,@(mapcar
                 (lambda (slot-name)
                   `(,slot-name (slot-value ,object-sym ',slot-name)))
                 slots))
         (lambda () ,@body)))))

(defun finalizer (object)
  "Get the finalizer for `object'.
A finalizer is a function of no arguments that cleans up any resources used by `object'.
The finalizer should not hold a reference to `object', and may run as part of the garbage collector.
See `trivial-garbage:finalize'."
  (let ((finalizers (make-finalizers object)))
    (cond
      ;; Return a function which does nothing.
      ((null finalizers) (lambda ()))
      ;; Return the single finalizer
      ((null (cdr finalizers)) (car finalizers))
      ;; Return a function which calls each finalizer in turn
      (t (lambda () (mapc #'funcall finalizers))))))

(defun register-finalizer (object)
  "Register `object' for finalization, using the function obtained from `finalizer'."
  (let ((finalizer (finalizer object)))
    (trivial-garbage:finalize object finalizer)
    finalizer))

(defun cancel-finalizer (object)
  "Cancel finalization for `object'."
  (trivial-garbage:cancel-finalization object)
  (values))

(defclass finalizable ()
  ()
  (:documentation
   "Mixin to automatically register for finalization on `initialize-instance'."))

(defmethod initialize-instance :around ((object finalizable) &key &allow-other-keys)
  "Registers the object for finalization."
  (unwind-protect (call-next-method)
    (register-finalizer object)))
