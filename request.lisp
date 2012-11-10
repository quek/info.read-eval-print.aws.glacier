(in-package #:info.read-eval-print.aws.glacier)

(defparameter *x-amz-glacier-version* "2012-06-01"
  "The Amazon Glacier API version to use.")

(defun request-to-glacier (method path &key headers content content-type)
  (let ((url (format nil "https://glacier.~a.amazonaws.com/~a/~a"
                     *region* *account-id* path)))
    (request url :method method :headers headers :content content :content-type content-type)))

(defun request (url &key (method :get) headers content content-type)
  (let* ((uri (puri:parse-uri url))
         (tz (car (last (multiple-value-list (decode-universal-time (get-universal-time))))))
         (date (dt:hour+ (dt:now) tz))
         (host (puri:uri-host uri))
         (service (car (ppcre:split "\\." host)))
         (drakma-headers `(("x-amz-glacier-version" . ,*x-amz-glacier-version*)
                           ("Date" . ,(dt:|yyyymmddThhmmssZ| date))
                           ,@headers))
         (headers `(("Host" . ,host)
                    ,@drakma-headers))
         (authorization (authorization :method method
                                       :path (puri:uri-path uri)
                                       :date date
                                       :region *region*
                                       :service service
                                       :headers headers
                                       :content content)))
    (let ((drakma:*text-content-types* `(("application" . "json"))))
      (drakma:http-request url
                           :method method
                           :additional-headers `(,@drakma-headers
                                                 ("Authorization" . ,authorization))
                           :content content
                           :content-type content-type))))
