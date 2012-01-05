(require 'hex-util)

(defvar monotone-cert-cache (make-hash-table :test 'equal)
  "Contains a cache of all certificates for all revisions.")

(defun monotone-cert-value (revision cert-name)
  "Return the value for certificate named cert-name in revision"
  (car (monotone-cert-values revision cert-name)))


(defun monotone-cert-values (revision cert-name)
  "Returns a list with all values for cert-name attached to revision."
  (monotone-get-cert-values-for-key
   (monotone-get-certs-with-key-value (monotone-certs-for-revision revision) 'name cert-name)
   'value))


(defun monotone-get-certs-with-key-value (certs key value)
  "Returns a list of certificates for with key has value.
Filter the certs list on the criteria that the certificate
contains the key value pair."
  (interactive)
  (let ((result))
    (mapc (lambda (cert) 
	    (if (equal value (cdr (assoc key cert)))
		(setq result (cons cert result))))
	  certs)
    result))

(defun monotone-get-cert-values-for-key (certs key)
  "Returns list of the values for key."
  (interactive)
  (mapcar (lambda (cert)
	    (cdr (assoc key cert)))
	  certs))

(defun monotone-certs-for-revision (revision)
  "Get certificates for revision.
The result is an list containing the certificates for the revision.
It will first lookup the revision in the monotone-cert-cache, if it is not
in the cache, run the monotone command to find out the certificates for this
revision."
  (or  (gethash revision monotone-cert-cache)
       (progn (monotone-cache-all-certs)
	      (gethash revision monotone-cert-cache))))


(defun monotone-certs-clear-cache ()
  (interactive)
  (clrhash monotone-cert-cache))

(defun monotone-cache-all-certs ()
  "Fill the certificate cache with all certificates 
from the database"
  (interactive)
  (message "Start fetching all certificates")
  (clrhash monotone-cert-cache)
  (dolist (raw-certificate (split-string (monotone-raw-certs) (if monotone-pre-v26 "\n\n+" "\n")))
    (if (> (length raw-certificate) 0)
	(let ((certificate-list (split-string raw-certificate " | ")))
	  (if (= (length certificate-list) 3)
	      (let ((revision (nth 0 certificate-list))
		    (name (nth 1 certificate-list))
		    (value 
		     (if monotone-pre-v26
			 (base64-decode-string (nth 2 certificate-list))
		       (decode-hex-string (substring (nth 2 certificate-list) 2 -1)))))
		(puthash revision 
			 (cons (list (cons 'name name)
				     (cons 'value value))
			       (gethash revision monotone-cert-cache))
			 monotone-cert-cache))))))
  (message "Done fetching all certificates"))

(defun monotone-raw-certs ()
  "Get the raw representation of all certs"
  (shell-command-to-string 
   (format 
    (if monotone-pre-v26 
	"%s db execute \"SELECT id,name,value FROM revision_certs\""
      "%s db execute \"SELECT id,name,quote(value) FROM revision_certs\"")
    monotone-cmd)))

(provide 'e-monotone-certs)
