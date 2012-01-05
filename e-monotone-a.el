;; Faces for annotate
;;
(require 'e-monotone-certs)
(require 'e-monotone-graphs)

(defface monotone-annotate-random-1
  '((t :foreground "red"))
  "First random face")
(defface monotone-annotate-random-2
  '((t :foreground "blue"))
  "Second random face")
(defface monotone-annotate-random-3
  '((t :foreground "green"))
  "Third random face")
(defface monotone-annotate-random-4
  '((t :foreground "purple"))
  "Fourth random face")
(defface monotone-annotate-random-5
  '((t :foreground "brown"))
  "Fifth random face")
(defface monotone-annotate-random-6
  '((t :foreground "OliveDrab"))
  "Sixth random face")
(defface monotone-annotate-random-7
  '((t :foreground "DeepPink"))
  "Seventh for oldest changes")


(defface monotone-annotate-fade-1
  '((t :foreground "black"))
  "Face for most recent changes")
(defface monotone-annotate-fade-2
  '((t :foreground "DarkOrchid4"))
  "Face for recent changes")
(defface monotone-annotate-fade-3
  '((t :foreground "DarkOrchid3"))
  "Face for changes in the past")
(defface monotone-annotate-fade-4
  '((t :foreground "DarkOrchid2"))
  "Face for old changes")
(defface monotone-annotate-fade-5
  '((t :foreground "DarkOrchid1"))
  "Face for oldest changes")

;; This should be a const
(setq monotone-annotate-random-face-list
      '(monotone-annotate-random-1
       monotone-annotate-random-2
       monotone-annotate-random-3
       monotone-annotate-random-4
       monotone-annotate-random-5
       monotone-annotate-random-6
       monotone-annotate-random-7
))

(setq monotone-annotate-fade-face-list
      '(monotone-annotate-fade-1
	monotone-annotate-fade-2
	monotone-annotate-fade-3
	monotone-annotate-fade-4
	monotone-annotate-fade-5))


(setq monotone-annotate-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "ss" 'monotone-annotate-shortened-revs)
	(define-key map "sd" 'monotone-annotate-date-cert)
	(define-key map "sa" 'monotone-annotate-author-cert)
	(define-key map "sf" 'monotone-annotate-full-revs)
	(define-key map "cr" 'monotone-annotate-colour-revs)
	(define-key map "cd" 'monotone-annotate-colour-date)
	(define-key map "W"  'monotone-annotate-wami)
	(define-key map " " 'scroll-up)
	(define-key map (kbd "DEL") 'scroll-down)
	(define-key map (kbd "RET") 'monotone-annotate-visit-file)
	(define-key map "q" 'quit-window)
	map))

(easy-menu-define monotone-annotate-menu monotone-annotate-mode-map "Menu used in `monotone-annotate-mode'."
  '("Monotone"
    ["Short revs" monotone-annotate-shortened-revs t]
    ["Full revs"  monotone-annotate-full-revs t]
    ["Dates"      monotone-annotate-date-cert t]
    ["Author"     monotone-annotate-author-cert t]
    "----"
    ["Colour by rev" monotone-annotate-colour-revs t]
    ["Colour by date" monotone-annotate-colour-date t]
    "----"
    ["Where am I" monotone-annotate-wami t]))
;;;
;;; Annotate
;;;  

;; eventually this function should be able to colour the annotated
;; version based on different criteria, e.g.: author, date
;; or a combination of them.  
(defun monotone-annotate-file ()
  "Display an annotated version of the file."
  (interactive)
  (let ((file-name (monotone-find-file-on-current-line))
	(current-directory default-directory))
    (if file-name 
	(let ((annotate-buffer (get-buffer-create (format "*annotate %s*" file-name))))
	  (monotone-switch-to-buffer-if-not-current annotate-buffer)
	  (kill-all-local-variables)
	  (setq buffer-read-only nil)
	  (setq default-directory current-directory)
	  (erase-buffer)
	  (monotone-annotate-mode)
	  (setq monotone-annotated-file file-name)
	  (monotone-run-next annotate-buffer
			     (list `(monotone-process-annotate . ("annotate" "--brief" ,file-name)))))
      (message "Can not determine file name"))))



(defun monotone-annotate-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map monotone-annotate-mode-map)
  (make-local-variable 'monotone-revisions)
  (make-local-variable 'monotone-annotated-file)
  (setq mode-name "Monotone annotate"))

  

(defun monotone-process-annotate ()
  ""
  (interactive)
  ;; remove revision and set properties
  (goto-char (point-min))
  (setq monotone-revisions (list))
  (while (not (eobp))
    (let ((revision (buffer-substring-no-properties (point) (+ (point) 40)))
	  (start-point (point)))
      (forward-line 1)
      (if (not (member revision monotone-revisions))
	  (setq monotone-revisions (cons revision monotone-revisions)))
      (put-text-property start-point (+ start-point 40 1) 'indicator t)
      (add-text-properties start-point (point)
			   (list 'revision (buffer-substring start-point (+ start-point 40))
				 'front-sticky t
				 'rear-nonsticky t))))
  (goto-char (point-min))
  (setq buffer-read-only t)
  (monotone-annotate-shortened-revs)
  (monotone-annotate-colour-revs))

(defun monotone-annotate-update-indicator (translator)
  "Update all the text on the left hand side.
The translator function gets one argument, a revision,
and should return what is displayed."
  (save-excursion 
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (let ((cache (list))
	  (max-length 0)
	  format-string)
      ;; file cache and determine maximum length
      (while (not (eobp))
	(let ((start-point (point))
	      (revision (monotone-annotate-revision-at-point)))
	  (if (not (assoc revision cache))
	      (progn
		(setq cache (cons (cons revision (funcall translator revision)) cache))
		(setq max-length (max (length (cdar cache)) max-length)))))
	(forward-line))
      ;; delete old indicator and replace it with new one
      (goto-char (point-min))
      (setq max-length (min max-length 40))
      (setq format-string (format "%%-%ds" max-length))
      (while (not (eobp))
	(let ((start-point (point))
	      (end-point (next-single-property-change (point) 'indicator))
	      (revision (monotone-annotate-revision-at-point)))
	  (if end-point 
	      (progn 
		(delete-region start-point end-point)
		(insert-and-inherit (format format-string (cdr (assoc revision cache))))
		(add-text-properties start-point (+ start-point max-length)
				     (list 'indicator t)))))
	(forward-line 1))))
  (setq buffer-read-only t))

(defun monotone-annotate-colour (face-provider)
  "Set face provided by face-provider per line."
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (let ((cache (list)))
      (while (not (eobp))
	(let* ((revision (monotone-annotate-revision-at-point))
	       (face (cdr (assoc revision cache))))
	  (if (not face)
	      (progn 
		(setq face (funcall face-provider revision))
		(setq cache (cons (cons revision face) cache))))
	  
	  (put-text-property (point)
			     (progn 
			       (forward-line 1)
			       (1- (point)))
			     'face face))))
    (setq buffer-read-only t)))
			     

(defun monotone-annotate-full-revs ()
  "Show full 40 character revision hash before source."
  (interactive)
  (monotone-annotate-update-indicator (lambda (revision) revision)))

(defun monotone-annotate-shortened-revs ()
  "Show shortened hash revision before source."
  (interactive)
  (monotone-annotate-update-indicator (lambda (revision) 
					(cdr (assoc revision (assoc 'abbreviations (monotone-status-list)))))))


(defun monotone-annotate-date-cert ()
  "Show commit date before source."
  (interactive)
  (monotone-annotate-update-indicator (lambda (revision)
					(format-time-string "%c"
							    (date-to-time
							     (monotone-cert-value revision "date"))))))

(defun monotone-annotate-author-cert ()
  "Show author before source."
  (interactive)
  (monotone-annotate-update-indicator (lambda (revision)
					(monotone-cert-value revision "author"))))

(defun monotone-annotate-colour-revs ()
  (interactive)
  (let ((available-colours monotone-annotate-random-face-list)
	(colour-mapping))
    (monotone-annotate-colour
     (lambda (rev)
       (if (assoc rev colour-mapping)
	   (cdr (assoc rev colour-mapping))
	 (setq colour-mapping (cons (cons revision (car available-colours)) colour-mapping))
	 (setq available-colours (cdr available-colours))
	 (cdar colour-mapping))))))

(defun monotone-annotate-colour-date ()
  (interactive)
  ;; Hm colouring scheme needs to be smart
  ;; but for testing use first 
  ;; 
  ;; 1 - day ago
  ;; 2 - days ago
  ;; 4 - days ago
  ;; 8 - days ago
  ;; older
  (let ((cutoff-list)
	(cutoff-value 0)
	(faces monotone-annotate-fade-face-list))
    (while faces
      (setq cutoff-list (cons (cons cutoff-value (car faces)) cutoff-list))
      (setq cutoff-value (* 2 (1+ cutoff-value)))
      (setq faces (cdr faces)))
    
    (monotone-annotate-colour
     (lambda (rev)
       (let* ((date (date-to-time (monotone-cert-value (monotone-annotate-revision-at-point) "date")))
	      (days (time-to-number-of-days (time-subtract (current-time) date)))
	      (todo-list cutoff-list))
	 (while (and todo-list (< days (caar todo-list)))
	   (setq todo-list (cdr todo-list)))
	 (cdar todo-list))))))

			   
(defun monotone-annotate-wami ()
  (interactive)
  (let ((current-directory default-directory)
	(revision-list monotone-revisions)
	(current-revision (monotone-status-data 'revision))
	(wami-buffer (get-buffer-create "*wami monotone*")))
    (monotone-switch-to-buffer-if-not-current wami-buffer)
    (monotone-wami-mode)
    (setq monotone-wami-current-revision current-revision)
    (setq default-directory current-directory)
    (setq monotone-wami-revision-list revision-list)
    (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-listed-revisions-p)
    (monotone-wami-create-full-graph)))


(defun monotone-annotate-revision-at-point ()
  "Returns the revision of the line at point.
However it assumes the revision information
is readable at point."
;;  (buffer-substring (point) (+ (point) 40)))
  (get-text-property (point) 'revision))

(defun monotone-annotate-visit-file ()
  "Visit the underlying file at current line."
  (interactive)
  (let ((current-line (line-number-at-pos (point)))
	(current-column (- (point) (previous-single-property-change (point) 'indicator))))
    (find-file monotone-annotated-file)
    (goto-line (1+ current-line))
    (move-beginning-of-line 0)
    (forward-char (1- current-column))))

(provide 'e-monotone-a)