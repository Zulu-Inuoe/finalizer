(defsystem #:com.inuoe.finalizer
  :version "0.0.1"
  :description "Common 'finalizer' mechanism."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :components
  ((:file "finalizer"))
  :depends-on
  (#:trivial-garbage))
