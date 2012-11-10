(in-package #:info.read-eval-print.aws.glacier)


(defun create-vault (vault-name)
  (request-to-glacier :put (format nil "vaults/~a" vault-name)))

(defun delete-vault (vault-name)
  (request-to-glacier :delete (format nil "vaults/~a" vault-name)))

(defun describe-vault (vault-name)
  (request-to-glacier :get (format nil "vaults/~a" vault-name)))

(defun list-vaults ()
  (request-to-glacier :get "vaults"))

(defun upload-archive-multipart (vault-name upload-file &key description (part-size *part-size*))
  (multiple-value-bind (multipart-upload-id part-size)
      (initiate-multipart-upload vault-name description part-size )
    (print (list multipart-upload-id part-size))
    (multiple-value-bind  (hashes archive-size)
        (upload-part vault-name multipart-upload-id upload-file part-size)
      (complete-multipart-upload vault-name multipart-upload-id hashes archive-size))))

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
