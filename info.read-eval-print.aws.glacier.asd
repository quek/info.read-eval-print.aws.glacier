;;;; info.read-eval-print.aws.glacier.asd

(asdf:defsystem #:info.read-eval-print.aws.glacier
  :description "access AWS Glaicer"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "package")
               (:file "parameter")
               (:file "signing")
               (:file "request")
               (:file "multipart-upload")
               (:file "glacier"))
  :depends-on (:drakma
               :percent-encoding
               :ironclad
               :cl-ppcre
               :simple-date-time
               :cl-json))

