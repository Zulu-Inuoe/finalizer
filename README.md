# finalizer

Composable finalizer library.

trivial-garbage is a portable layer over a simple `finalize` concept. This builds upon that by creating a `make-finalizers` generic function which builds a list of finalizers to run for an object.

This allows building extensible finalization for objects.

Additionally, the `define-finalizer` macro helps in creating finalizers in the common pattern of closing-over slot values.

# Usage

We can define a finalizer for a class with `define-finalizer`
``` common-lisp
;; Define a finalizer specialized on the class `net-logger',
;; which closes over the slots `sock' and `log-stream'
(define-finalizer net-logger (sock log-stream)
  (usocket:socket-close sock)
  (close log-stream))
```

The library also defines a class `finalizable`, which automatically registers objects for finalization in its `initialize-instance` method:

``` common-lisp
(defclass my-file (finalizable)
  (stream))

(define-finalizer my-file (stream)
  (close stream))

;; The instance of `my-file' shall be registered for finalization
(make-instance 'my-file (open "foo.txt"))
```

To register for finalization manually, there is the `register-finalizer` function:

``` common-lisp
(defclass my-file ()
  (stream))

(define-finalizer my-file (stream)
  (close stream))

(let ((my-file (make-instance 'my-file (open "foo.txt"))))
  ;; This will construct & register finalizer for `my-file`
  (register-finalizer my-file))
```

# License

See [LICENSE](LICENSE.txt)
