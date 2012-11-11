(in-package #:info.read-eval-print.aws.glacier)

(deftype octet ()
  '(unsigned-byte 8))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet))

(defun octet-vector (&rest octets)
  (make-array (length octets) :element-type 'octet :initial-contents octets))

(defun format-line (stream format &rest args)
  (apply #'format stream format args)
  (format stream "~c" #\lf))

(defun base-16 (sequence)
  (with-output-to-string (out)
    (loop for i across sequence
          do (format out "~(~02,'0x~)" i))))

(defun hash-sha256 (octet-vector)
  (base-16 (ironclad:digest-sequence :sha256 octet-vector)))

(defun create-canonical-request (method path parameters headers content)
  (with-output-to-string (out)
    (format-line out "~a" method)
    (format-line out "~a" path)
    (format-line out "~{~a=~a~^&~}"
                 (loop for (k . v) in (sort (copy-seq parameters) #'string<= :key #'car)
                       collect (percent-encoding:encode k :encoding :utf-8)
                       collect (percent-encoding:encode v :encoding :utf-8)))
    (format-line out "~{~(~a~)~^;~}"
                 (loop for (k . v) in (sort (copy-seq headers) #'string<= :key #'car)
                       do (format-line out "~(~a~):~a" k v)
                       collect k
                       finally (format-line out "")))
    (format out (hash-sha256 (or content (octet-vector))))))


(defun create-string-to-sign (date region service canonical-request)
  (with-output-to-string (out)
    (format-line out "AWS4-HMAC-SHA256")
    (format-line out (dt:|yyyymmddThhmmssZ| date))
    (format-line out "~a/~a/~a/aws4_request" (dt:yyyymmdd date) region service)
    (format out (hash-sha256 (babel:string-to-octets canonical-request :encoding :utf-8)))))


(defgeneric hmac-sha256 (key data)
  (:method ((key string) data)
    (hmac-sha256 (babel:string-to-octets key :encoding :utf-8) data))
  (:method (key (data string))
    (hmac-sha256 key (babel:string-to-octets data :encoding :utf-8)))
  (:method (key data)
    (let ((hmac (ironclad:make-hmac key :sha256)))
      (ironclad:update-hmac hmac data)
      (ironclad:hmac-digest hmac))))

(defun calculate-aws-signature (date region service string-to-sign)
  (with-output-to-string (out)
    (loop for i across (reduce #'hmac-sha256
                               (list (dt:yyyymmdd date)
                                     region
                                     service
                                     "aws4_request"
                                     string-to-sign)
                               :initial-value (format nil "AWS4~a" *secret-key*))
          do (format out "~(~02,'0x~)" i))))

(defun authorization (&key (method :get)
                        path
                        query-parameters
                        date
                        region
                        service
                        headers
                        content)
  (let* ((canonical-request (create-canonical-request method path query-parameters headers content))
         (string-to-sign (create-string-to-sign date region service canonical-request))
         (signature (calculate-aws-signature date region service string-to-sign)))
   (format nil "AWS4-HMAC-SHA256 Credential=~a/~a/~a/~a/aws4_request, SignedHeaders=~a, Signature=~a"
           *access-key*
           (dt:yyyymmdd date)
           region
           service
           (format nil "~(~{~a~^;~}~)" (sort (loop for (k . v) in headers
                                                 collect k)
                                           #'string<=))
           signature)))
