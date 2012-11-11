(in-package #:info.read-eval-print.aws.glacier)


(defun create-vault (vault-name)
  (request-to-glacier :put (format nil "vaults/~a" vault-name)))

(defun delete-vault (vault-name)
  (request-to-glacier :delete (format nil "vaults/~a" vault-name)))

(defun describe-vault (vault-name)
  (request-to-glacier :get (format nil "vaults/~a" vault-name)))

(defun list-vaults ()
  (request-to-glacier :get "vaults"))

;; TODO describe のデフォルト値にファイル名とか日時とかサイズとか入れといた方がいい気がする。
;; :description :default でそんなのが入る感じで。
(defun upload-archive-multipart (vault-name upload-file &key description (part-size *part-size*))
  (multiple-value-bind (multipart-upload-id part-size)
      (initiate-multipart-upload vault-name description part-size )
    (print (list multipart-upload-id part-size))
    (multiple-value-bind  (hashes archive-size)
        (upload-part vault-name multipart-upload-id upload-file part-size)
      (complete-multipart-upload vault-name multipart-upload-id hashes archive-size))))

;; TODO describe のデフォルト値にファイル名とか日時とかサイズとか入れといた方がいい気がする。
(defun upload-archive (vault-name upload-file &key description)
  (with-open-file (in upload-file :element-type ' (unsigned-byte 8))
    (let* ((file-length (file-length in))
           (buffer (make-octet-vector file-length))
           (tree-hash (progn (read-sequence buffer in) (base-16 (tree-hash buffer))))
           (content-hash (base-16 (ironclad:digest-sequence :sha256 buffer))))
      (request-to-glacier :post (format nil "vaults/~a/archives" vault-name)
                          :headers `(,@(when description
                                         `(("x-amz-archive-description" . ,description)))
                                     ("x-amz-sha256-tree-hash" . ,tree-hash)
                                     ("x-amz-content-sha256" . ,content-hash))
                          :content buffer))))

(defun delete-archive (vault-name archive-id)
  (request-to-glacier :delete (format nil "vaults/~a/archives/~a"
                                      vault-name archive-id)))


(defun list-jobs (vault-name &key (completed nil completed-supplyed-p)
                               limit
                               marker
                               status-code)
  "status-code: One of the values InProgress, Succeeded, or Failed."
  (request-to-glacier :get (format nil "vaults/~a/jobs" vault-name)
                      :parameters `(,@(when completed-supplyed-p
                                        `(("completed" . ,(if completed
                                                              "true"
                                                              "false"))))
                                    ,@(when limit
                                        `(("limit" . ,(princ-to-string limit))))
                                    ,@(when marker
                                        `(("marker" . ,marker)))
                                    ,@(when status-code
                                        `(("statuscode" . ,status-code))))))

(defun initiate-job (vault-name &key type archive-id description format sns-topic)
  (check-type type (member nil :archive-retrieval :inventory-retrieval))
  (check-type format (member nil :csv :json))
  (let ((content (make-hash-table :test #'equal)))
    (setf (gethash "Type" content) (string-downcase type))
    (when archive-id
      (setf (gethash "ArchiveId" content) archive-id))
    (when description
      (setf (gethash "Description" content) description))
    (when format
      (setf (gethash "Format" content) (symbol-name format)))
    (when sns-topic
      (setf (gethash "SNSTopic" content) sns-topic))
    (multiple-value-bind (body status header)
        (request-to-glacier :post (format nil "vaults/~a/jobs" vault-name)
                            :content (babel:string-to-octets (json:encode-json-to-string content)
                                                             :encoding :utf-8))
      (if (= status 202)                ;Accepted
          (cdr (assoc :x-amz-job-id header))
          (error (list body status header))))))

(defun describe-job (vault-name job-id)
  (request-to-glacier :get (format nil "vaults/~a/jobs/~a" vault-name job-id)))

(defun get-job-output (vault-name job-id)
  (request-to-glacier :get (format nil "vaults/~a/jobs/~a/output" vault-name job-id)))

(defun get-job-output-stream (vault-name job-id)
  (request-to-glacier :get (format nil "vaults/~a/jobs/~a/output" vault-name job-id)
                      :want-stream t))

(defmacro with-job-output-stream ((stream vault-name job-id) &body body)
  `(with-open-stream (,stream (get-job-output-stream ,vault-name ,job-id))
     ,@body))
