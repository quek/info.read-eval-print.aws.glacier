(in-package #:info.read-eval-print.aws.glacier)

(defconstant +hash-block-size+ (* 1024 1024))

(defun initiate-multipart-upload (vault-name description part-size)
  (multiple-value-bind (body status headers)
      (request-to-glacier :post (format nil "vaults/~a/multipart-uploads" vault-name)
                          :headers `(,@(when description
                                         `(("x-amz-archive-description" . ,description)))
                                     ("x-amz-part-size" . ,part-size)))
    (if (= status 201)
        (values (cdr (assoc :x-amz-multipart-upload-id headers))
                part-size
                (cdr (assoc :location headers)))
        (error (list body status headers)))))

(defun upload-part (vault-name multipart-upload-id file part-size)
  (let ((buffer (make-octet-vector part-size)))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (let* ((file-length (file-length in))
             (part-count (ceiling file-length part-size)))
        (values (loop repeat part-count
                      for i = 0 then (+ i read-size)
                      for read-size = (read-sequence buffer in)
                      for content = (subseq buffer 0 read-size)
                      for hash = (tree-hash content)
                      do (upload-one-part vault-name
                                          multipart-upload-id
                                          content
                                          hash
                                          i)
                      collect hash)
                file-length)))))

(defun complete-multipart-upload (vault-name multipart-upload-id hashes archive-size)
  (request-to-glacier :post (format nil "vaults/~a/multipart-uploads/~a" vault-name multipart-upload-id)
                      :headers `(("x-amz-sha256-tree-hash" . ,(base-16 (shrink-hashes hashes)))
                                 ("x-amz-archive-size" . ,archive-size))))

(defun abort-multipart-upload (vault-name multipart-upload-id)
  (request-to-glacier :delete (format nil "vaults/~a/multipart-uploads/~a" vault-name multipart-upload-id)))

(defun list-parts (vault-name multipart-upload-id)
  (request-to-glacier :get (format nil "vaults/~a/multipart-uploads/~a" vault-name multipart-upload-id)))

(defun upload-one-part (vault-name multipart-upload-id content hash range-from)
  (print
   (multiple-value-list
    (request-to-glacier
     :put (format nil "vaults/~a/multipart-uploads/~a" vault-name multipart-upload-id)
     :headers `(("Content-Range" . ,(format nil "bytes ~d-~d/*"
                                            range-from (1- (+ range-from (length content)))))
                ("Content-Type" . "application/octet-stream")
                ("x-amz-sha256-tree-hash" . ,(base-16 hash))
                ("x-amz-content-sha256" . ,(base-16 (ironclad:digest-sequence :sha256 content))))
     :content content
     :content-type nil))))

(defun tree-hash (content)
  (shrink-hashes (loop for start from 0 by +hash-block-size+
                       while (< start (length content))
                       collect (let ((digester (ironclad:make-digest :sha256)))
                                 (ironclad:update-digest digester content :start start
                                           :end (min (length content) (+ start +hash-block-size+)))
                                 (ironclad:produce-digest digester)))))

(defun shrink-hashes (hashes)
  (if (= 1 (length hashes))
      (car hashes)
      (shrink-hashes (loop for (a b) on hashes by #'cddr
                           if (and a b)
                             collect (let ((digester (ironclad:make-digest :sha256)))
                                       (ironclad:update-digest digester a)
                                       (ironclad:update-digest digester b)
                                       (ironclad:produce-digest digester))
                           else
                             collect a))))
