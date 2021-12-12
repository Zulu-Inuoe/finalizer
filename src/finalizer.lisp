(defpackage #:com.inuoe.finalizer
  (:use #:cl)
  (:export
   #:finalizer
   #:define-finalizer
   #:finalizable)
  (:import-from
   #:trivial-garbage))
(in-package #:com.inuoe.finalizer)

(defgeneric finalizer (object)
  (:documentation
   "Get the finalizer for `object'.
A finalizer is a function of no arguments that cleans up any resources used by `object'.
The finalizer should not hold a reference to `object', and may run as part of the garbage collector.
See `trivial-garbage:finalize'.")
  (:method-combination list)
  (:method :around (object)
    "Collect the ordered list of finalizers into a single function."
    (let ((finalizers (call-next-method)))
      (cond
        ;; Return a function which does nothing.
        ((null finalizers) (lambda ()))
        ;; Return the single finalizer
        ((null (cdr finalizers)) (car finalizers))
        ;; Return a function which calls each finalizer in turn
        (t (lambda () (mapc #'funcall finalizers)))))))

(defmacro define-finalizer (class (&rest slots) &body body)
  "Defines a `finalizer' method for `class' in an which the slot-values values of `slots' are bound to their respective names.
ex:

  (define-finalizer net-logger (sock log-stream)
    (usocket:socket-close sock)
    (close log-stream))"
  (let* ((object-sym (gensym "OBJECT"))
         (bindings (mapcar (lambda (slot-name)
                             `(,slot-name (slot-value ,object-sym ',slot-name)))
                           slots)))
    `(defmethod finalizer list ((,object-sym ,class))
       (let (,@bindings)
         (lambda () ,@body)))))

(defclass finalizable ()
  ()
  (:documentation
   "Mixin to automatically register for finalization on `initialize-instance'."))

(defmethod initialize-instance :around ((object finalizable) &key &allow-other-keys)
  "Registers the object for finalization."
  (unwind-protect (call-next-method)
    (trivial-garbage:finalize object (finalizer object))))
