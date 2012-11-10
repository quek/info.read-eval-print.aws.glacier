(defpackage :info.read-eval-print.aws.glacier.test
  (:use :cl :info.read-eval-print.aws.glacier))

(in-package :info.read-eval-print.aws.glacier.test)

(setf *account-id* "99999999999")
(setf *access-key* "AKIDEXAMPLE")
(setf *secret-key* "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY")
(setf *region* "us-east-1")

;; http://docs.amazonwebservices.com/general/latest/gr/signature-v4-test-suite.html
;; https://awsiammedia.s3.amazonaws.com/public/sample/aws4_testsuite/aws4_testsuite.zip
(defparameter *test-data-dir* #p"~/letter/lisp/craft/info.read-eval-print.aws.glacier/aws4_testsuite/")

(defun test-data (name)
  (delete #\cr
          (alexandria:read-file-into-string (merge-pathnames name *test-data-dir*)
                                            :external-format :utf-8)))

(defun parse-test-req (content)
  (ppcre:register-groups-bind (method path query-string headers body)
      ((ppcre:create-scanner "(\\S+) ([^? ]+)(?:\\?(\\S+))? .+?\\n(.+)?\\n\\n(.*)"
                             :single-line-mode t)
       content)
    (values method
            path
            (loop for kv in (ppcre:split "&" query-string)
                  collect (let ((kv (ppcre:split "=" kv)))
                            (cons (car kv)
                                  (cadr kv))))
            (loop for kv in (ppcre:split "\\n" headers)
                  collect (ppcre:register-groups-bind (k v)
                              ("([^:]+)?:\s*\(.*\)" kv)
                            (cons k v)))
            (babel:string-to-octets body :encoding :utf-8))))

(assert (string= (test-data "get-vanilla-ut8-query.creq")
                 (multiple-value-call #'info.read-eval-print.aws.glacier::create-canonical-request
                   (parse-test-req (test-data "get-vanilla-ut8-query.req")))))

(assert (string= (test-data "get-vanilla-ut8-query.sts")
                 (info.read-eval-print.aws.glacier::create-string-to-sign
                  (dt:from-string "Mon, 09 Sep 2011 23:36:00 GMT") "us-east-1" "host"
                  (multiple-value-call #'info.read-eval-print.aws.glacier::create-canonical-request
                    (parse-test-req (test-data "get-vanilla-ut8-query.req"))))))

(assert (string= (test-data "get-vanilla-ut8-query.authz")
                 (info.read-eval-print.aws.glacier::authorization
                  :path "/"
                  :query-parameters `(("ሴ" . "bar"))
                  :date (dt:from-string "Mon, 09 Sep 2011 23:36:00 GMT")
                  :region "us-east-1"
                  :service "host"
                  :headers `(("Date" . "Mon, 09 Sep 2011 23:36:00 GMT")
                             ("Host" . "host.foo.com")))))


(assert (string= (test-data "post-x-www-form-urlencoded.authz")
                 (info.read-eval-print.aws.glacier::authorization
                  :method :post
                  :path "/"
                  :query-parameters nil
                  :date (dt:from-string "Mon, 09 Sep 2011 23:36:00 GMT")
                  :region "us-east-1"
                  :service "host"
                  :headers `(("Date" . "Mon, 09 Sep 2011 23:36:00 GMT")
                             ("Host" . "host.foo.com")
                             ("Content-Type" . "application/x-www-form-urlencoded"))
                  :content (babel:string-to-octets "foo=bar"))))
