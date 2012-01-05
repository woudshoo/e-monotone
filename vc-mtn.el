(require 'e-monotone)


;; determining if a file is used by monotone
;; logic:
;;
;;      monotone log --brief --last=1 'filename'
;;
;; Under version control:
;;
;;    * exit code is 0   
;; AND
;;    * has at least one line of output
;;
;; Now I assume that newly added files do not work
;; but that is just too bad


;; vc-mtn-registered (file)
;; vc-mtn-state (file)
;;  'up-to-date        The working file is unmodified with respect to the
;;                     latest version on the current branch, and not locked.
;;
;;  'edited            The working file has been edited by the user.  If
;;                     locking is used for the file, this state means that
;;                     the current version is locked by the calling user.
;;
;;  USER               The current version of the working file is locked by
;;                     some other USER (a string).
;;
;;  'needs-patch       The file has not been edited by the user, but there is
;;                     a more recent version on the current branch stored
;;                     in the master file.
;;
;;  'needs-merge       The file has been edited by the user, and there is also
;;                     a more recent version on the current branch stored in
;;                     the master file.  This state can only occur if locking
;;                     is not used for the file.
;;
;;  'unlocked-changes  The current version of the working file is not locked,
;;                     but the working file has been changed with respect
;;                     to that version.  This state can only occur for files
;;                     with locking; it represents an erroneous condition that
;;                     should be resolved by the user (vc-next-action will
;;                     prompt the user to do it).


(defun vc-mtn-registered (file)
  "Check if FILE is known by monotone"
  (message (format "Checking file: %s" file))
  (vc-mtn-state file))


(defun vc-mtn-state (file)
  "Return the state of the file, return nil if file not under version control"
;  (message (format "vc-mtn-state for: %s (status list: %s)" file (monotone-status-list)))
  (if (monotone-status-list)
	(let ((top-level-dir (monotone-parent-directory (monotone-status-data 'mt-directory)))
	      (file-list (monotone-status-data 'file))
	      (match))
	  (while (and (not match) file-list)
	    (if (string= 
		 (expand-file-name (concat top-level-dir (caar file-list)))
		 file)
		(setq match (car file-list)))
	    (setq file-list (cdr file-list)))
	  (cdr (assq (cdr match) 
		     '((unchanged up-to-date)
		       (missing needs-patch)
		       (added edited)
		       (added-and-missing edited)
		       (renamed-and-edited edited)
		       (edited edited)
		       (renamed edited)))))
      nil))


(defun vc-mtn-workfile-version (file)
  "Returns the version of the file"
  (monotone-status-data 'revision))

(defun vc-mtn-test-registered (file)
  (interactive "Ftest file name: ")
  (message (format "Result: %S" (vc-mtn-registered file))))

(defun vc-mtn-latest-on-branch-p (file)
  (monotone-revision-based-on-head-p))

(defun vc-mtn-checkout-model (file)
  'implicit)

; (defun vc-mtn-workfile-unchanged-p (file)
; this one is tricky due to monotones restrictive path
; handling

;(defun vc-mtn-register (file &optional rev comment)
;  "Not implemented yet."
;  nil)

(defun vc-mtn-checkin (file rev comment)
  "Commit changes in FILE.
We ignore the REV value and comment is the changelog."
  (error "Not supported yet!"))

(defun vc-mtn-find-version (file rev buffer)
  "Find version and put in buffer"
  (error "Not supported yet!"))

(defun vc-mtn-checkout (file &optional editable rev)
  "Checkout file"
  (error "Not supported yet!"))

(defun vc-mtn-revert (file &optional contents-done)
  (error "Not supported yet!"))

(defun vc-mtn-print-log (file &optional buffe)
  "Insert the revision log of FILE into BUFFER,
or the *vc* buffer if BUFFER is nil"
  (error "Not supported yet!"))


(provide 'vc-mtn)