;;;; package.lisp

(defpackage #:info.read-eval-print.aws.glacier
  (:use #:cl)
  (:export #:*account-id*
           #:*access-key*
           #:*secret-key*
           #:*region*
           #:create-vault
           #:delete-vault
           #:describe-vault
           #:list-vaults
           #:upload-archive-multipart
           #:upload-archive
           #:delete-archive
           #:list-jobs
           #:initiate-job
           #:describe-job
           #:get-job-output
           #:get-job-output-stream))
