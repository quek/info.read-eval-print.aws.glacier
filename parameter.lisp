(in-package #:info.read-eval-print.aws.glacier)

(defparameter *account-id* "-"
  "The AccountId is the AWS Account ID. This value must match the AWS
  Account ID associated with the credentials used to sign the
  request. You can either specify AWS Account ID or optionally a '-'
  in which case Amazon Glacier uses the AWS Account ID associated with
  the credentials used to sign the request. If you specify your
  Account ID, do not include dashes in it.
  http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-put.html")

(defparameter *access-key* "AKIDEXAMPLE" "access key")

(defparameter *secret-key* "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" "secret key")

(defparameter *region* "us-east-1" "region (endpoint)")


(defparameter *part-size* (* 1024 1024 32) "Part size of multipart upload")
