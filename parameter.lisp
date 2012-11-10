(in-package #:info.read-eval-print.aws.glacier)

(defparameter *account-id* "99999999999" "account ID")
(defparameter *access-key* "AKIDEXAMPLE" "access key")
(defparameter *secret-key* "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" "secret key")
(defparameter *region* "us-east-1" "region (endpoint)")

(defparameter *part-size* (* 1024 1024 1) "Part size of multipart upload")
