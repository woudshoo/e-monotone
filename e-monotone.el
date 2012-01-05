;;; e-monotone.el --- pcl-cvs like emacs mode
 
;; Copyright (C) 2005 Willem Rein Oudshoorn
;; Copyright (C) 2005 Tom Koelman

;; Author: Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Author: Tom Koelman <tkoelman@xs4all.nl>
;; Created: May 2005
;; Version: 0.3
;; Keywords: tools

;; This file is NOT part of GNU Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; version 2
;; of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

;; Note that you are ONLY allowed to redistributed under version 2 of the License,
;; and not as customary any later version.  
;; 
;; If this is a problem, contact the authors, either by the e-mail given above or 
;; at the following postal address:
;;
;; Willem Rein Oudshoorn
;; Westerweg 41a
;; 1851 AB  Heiloo
;; the Netherlands
;;
;;; Commentary:

;; e-monotone is a front-end to the monotone version control system.
;; It is largely inspired on PCL-CVS. 

;; This will be my attempt to an emacs interface for monotone.
;; 
;; User interface should contain a few different parts
;;
;; 1) Working on checked out trees.  This should be more
;;    or less comparable with how pcl-cvs manages it.
;;
;;    Of course we need to figure out how we are going
;;    to handle merging etcetera.
;;
;; 2) Managing the "databases" that is, navigating through
;;    a database and check which branches are available etc.
;;
;; 3) Not sure if it warants its own place, but syncing with
;;    different databases should be part of point 2.  However
;;    It could also be seen part of 1 if you consider a more
;;    centralized development style.
;;
;;    I think the more important at the moment is point 1.


;;; TODO:

;; - Simplify code (it really is a messs)
;; - Simplify all the repetitive code for key bindings
;; - Improve layout of "displayed" section in examine buffer
;; - Fix highlighting of diff in commit buffer
;; - Implement merging
;; - Make status-list easier to access by writing access method or redesigning structure
;; - make TAB key behave (jump from field to field)
;; - add keybinding O for updating from database
;; - Use indicator in overview of what is displayed
;; - Allow toggling in the overview of what is displayed
;; - Indicate when multiple heads, with which head we are connected.
;; - Allow diffing when selecting, head, or revision.
;; v Do not run full monotone command when state of single file is changed, like Added or Dropped...
;;    - Fix this code, it is now very simple
;; - Add hook to now when file is changed, like saving modified file should change state.
;; - add doc strings for all the key-bindings

;; - and much much more

;;; BUGS:

;; - After commit the content of the diff ends up in the kill-ring
;; - Empty project, after setup, does not work

;;; Code:


;; require 'vc for utility function which asks if the file needs to be saved
(require 'vc)
(require 'e-monotone-a)
(require 'e-monotone-wami)
(require 'e-monotone-graphs)


;; User configuration

(defvar monotone-configuration 
  '(("MT" "monotone" t)
    ("_MTN" "mtn" nil))
  "Configuration for monotone.
This contains a list of possible configurations.
Each configuration is a list containing three elements,
the name of the bookkeeping directory, the name or complete
path to the monotone executable and a flag.
The flag is t if iit is a pre0.26 version of monotone and nil
if it is 0.26 or later.")

(defvar monotone-status-faces
  '((unchanged . font-lock-constant-face)
    (ignored . font-lock-comment-face)
    (missing . font-lock-warning-face)
    (added . font-lock-warning-face)
    (added-and-missing . font-lock-warning-face)
    (renamed-and-edited . font-lock-warning-face)
    (edited . font-lock-warning-face)
    (renamed . font-lock-warning-face)
    (dropped . font-lock-warning-face))
  "Faces used for the different file states.
This is a list containging pairs (STATE . FACE) where STATE
is one of: unchanged, ignored, missing, added, added-and-missing,
edited, renamed or dropped.  A FACE should be a real face, 
not a simplified form like \"bold\".")

;; Need to fix the entries below, they are not real faces
;; and do not work for example when used in monotone-status-faces
(defconst monotone-ok-state-face  "italic"
  "Face used in the header of the *monotone* buffer to indicate
that everything is ok." )
(defconst monotone-warning-state-face "bold"
  "Face used in the header of the *monotone* buffer to notify the user
that there are changes.")

(defconst monotone-label-face "bold"
  "Face used for the labels in the header of the *monotone* buffer.")



(defconst monotone-possible-state-list
  '(added added-and-missing dropped edited renamed renamed-and-edited
	  ignored missing unchanged unknown)
  "List of states that is displayed by the keybinding 'a'")

(defconst monotone-known-state-list
  '(added added-and-missing dropped edited renamed renamed-and-edited
	  unchanged)
  "List of states of all files tracked by monotone, activated by the binding 'k'")

(defvar monotone-interesting-state-list
  '(added added-and-missing dropped edited renamed missing renamed-and-edited)
  "List of states that are displayed when pressing 'i'. 
This is the 'interesting' list, so it normally contains 
modified, added, missing files etcetera.")

(defvar monotone-display-states
  (copy-sequence monotone-interesting-state-list)
  "Default list of status to display.  
When the *monotone* buffer is created by monotone-examine this
is the file list that is displayed initially.")

(defvar monotone-commit-buffer "*commit monotone*"
  "The name of the buffer for the commit messages")

(defvar monotone-main-buffer "*monotone*"
  "The name of the main buffer, containing monotone status information")

(defconst monotone-modified-state-list
  '(added dropped edited renamed renamed-and-edited)
  "List of states to determine if there is something to commit.
The list of files we have is checked to see if there is any file
with a state named in this list.  If there is, we assume that
there is something to commit.  If there is not such a file, we 
assume that we have a clean checkout.")

(defvar monotone-mode-line-process nil
  "Mode-line control for displaying info on monotone process status.")

;;; Helper functions
;;
;;
(defun monotone-make-buffer ()
  "Create the main monotone buffer.  
Current code assumes that it will return the existing monotone buffer
if it exists.  This assumption is wrong, but the current implementation works."
  (get-buffer-create monotone-main-buffer))


;; status data structure
;; 
;; (('file (("filename" . status) ...))
;;  ('revision . "HEX STRING))
;;  ('heads ("head one" "head two" "head three" ...))
;;  ('abbreviations (("HEX STRING" . "HEX ABBREV") ... )
;;  ('branch . "Branch name")  
;;  ('target-branch . "Branch name")
;;               target-branch is/should be nil if it equal to branch
;;  ('database . "Dabase name") ;; Not implemented
;;  ('counters ((status . nr) ...)) 
;;  ('mt-directory . "...")
;; Marked structure
;; ((filename-1 . mark) (filename-2 . mark)...)

;; should this be a macro??
;(defmacro monotone-status-data (type)
;  `(cdr (assoc ,type (monotone-status-list))))
(defun monotone-status-data (type)
  (cdr (assoc type (monotone-status-list))))
;; should this be a macro???
;(defmacro monotone-status-data-set (type data)
;  `(setcdr (assoc ,type (monotone-status-list)) ,data))
(defun monotone-status-data-set (type data)
  (setcdr (assoc type (monotone-status-list)) data))
;; Need to get rid of this!!
(defun monotone-status-list ()
  "Return the status list datastructure for the current monotone session.
This function is deprecated because there should not be a notion of 
the current monotone session."
  (with-current-buffer (monotone-make-buffer)
    monotone-status))

(defun monotone-make-tmp-buffer ()
  "Creata a temporary buffer for running processes.
Note that it will reuse a single temporary buffer.  This 
is not good because it will prevent multiple monotone sessions.
Also note that the buffer is not cleared.  So the caller should
clear the buffer it self."
  ;; Create buffer
  (get-buffer-create " *monotone-tmp*"))


(defun monotone-parent-directory (directory)
  "Returns the parent directory of the argument.
This is not a very robust function.  It asssums
the argument ends in a trailing /.  Also it will work purely on
the string, so directories like ~/ do not work and
finally if the parent does not exist it either returns nil
or leaves the argument unchanged."
  (file-name-directory
   (directory-file-name
    (file-name-directory directory))))

;; This seems very complicated for what it is supposed to do.
;; I copied this more or less from add-log.el where similar
;; code is used to find the changelog file
(defun monotone-find-MT-top (&optional directory)
  "Traverse directory upwards until MT directory found.
The result is the first directory that contains an MT directory.
If no such directory exists, return nil"
  (let ((current-directory (file-name-as-directory (or directory default-directory)))
	(mt-directory)
	(parent-dir)
	(result))
    (while (and
	    (let ((todo monotone-configuration))
	      (while (and (not result) todo)
		(if (file-directory-p (concat current-directory (nth 0 (car todo))))
		    (progn
		      (setq result current-directory)
		      (setq monotone-cmd (nth 1 (car todo)))
		      (setq monotone-bookkeeping-dir (nth 0 (car todo)))
		      (setq monotone-pre-v26 (nth 2 (car todo)))))
		(setq todo (cdr todo)))
	      (not result))
	    (progn
	      (setq parent-dir 
		    (file-name-directory
		     (directory-file-name
		      (file-name-directory current-directory))))
	      (and parent-dir			 
		   (not (string= (file-name-directory current-directory)   parent-dir)))))

      (setq current-directory parent-dir))
    result))

(defun monotone-longest-common-prefix (str1 str2)
  "Returns the common prefix of str1 and str2.  
So for example the result of 'abce' and 'abec' is 'ab'."
  (substring str1 0 (- (abs (compare-strings str1 0 nil str2 0 nil)) 1)))


(defun monotone-abbreviations-from-revisions (revision-list)
  "Creates an abbreviation list.
Input is a list of strings and the result is an assoc list of which
the keys are the original strings and the values the shortes prefix
uniquely identifying the string in the list of strings."
  (let ((previous-lcp "")
	(next-lcp "")
	(previous-revision "")
	(next-revision "")
	(revision-list (append revision-list '("")))
	(abbreviation-list '()))
  (while (and
	  (setq previous-revision (pop revision-list))
	  (> (length revision-list) 0))
    (setq next-revision (car revision-list))
    (setq next-lcp (monotone-longest-common-prefix previous-revision next-revision))
    (if (> (length previous-lcp) (length next-lcp))
	(setq lcp previous-lcp)
      (setq lcp next-lcp))
    (setq abbreviation (substring previous-revision 0 (+ (length lcp) 1)))
    (setq abbreviation-list (cons (cons previous-revision abbreviation) abbreviation-list))
    (setq previous-lcp next-lcp))
  abbreviation-list))

(defun monotone-abbreviate (revision)
  "Abbreviate revision to shortest unique prefis.
This assumes that the abbreviation mappint is already constructed.
For this see 'monotone-process-revisions' and 'monotone-abbreviations-from-revisions'"
  (cdr (assoc revision (assoc 'abbreviations (monotone-status-list)))))

(defun monotone-default-target-branch ()
  "Returns the branch we are going to commit to."
  (or (monotone-status-data 'target-branch)
      (monotone-status-data 'branch)))

(defun monotone-target-branch (&optional ask-branch)
  "Returns the target branch for commit.
If the ask-branch argument is non nil, ask the user to supply a branch name,
otherwise return the current branch."
  (let ((default-target (monotone-default-target-branch)))
    (if (or ask-branch (not default-target))
	(monotone-ask-new-branch)
      default-target)))


(defun monotone-ask-new-branch ()
  "Ask the user for a new branch."
  (let* ((default-target (monotone-default-target-branch))
	 (new-target     (read-string "New target branch: " default-target
				      'monotone-branch-history default-target)))
    (if (string= new-target (monotone-status-data 'branch))
	(monotone-status-data-set 'target-branch nil)
      (monotone-status-data-set 'target-branch new-target))
    new-target))


(defun monotone-change-target-branch ()
  (interactive)
  (monotone-ask-new-branch)
  (with-current-buffer (monotone-make-buffer)
    (let ((read-only-status buffer-read-only))
      (setq buffer-read-only nil)
      (monotone-update-options)
      (setq buffer-read-only read-only-status)))
  (monotone-default-target-branch))

;; needed???
(defun monotone-list-all-branches ()
  "Returns a list containing all branches that are currently in the database"
  (monotone-status-data 'branches-list))


(defun monotone-process-nothing ()
  ""
)

(defun monotone-process-revisions ()
  "Read from the current buffer a list of revisions. These revisions are put in 
the datastructe given by (monotone-status-list).  
The expected format is a revision per line, where a revision is a string of 40 characters.
Note that point is not preserved."
  (goto-char (point-min))
  (let ((revision-list (list)))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (point) (+ (point) 40))))
	(setq revision-list (cons line revision-list)))
      (forward-line 1))
    (monotone-status-data-set 'abbreviations (monotone-abbreviations-from-revisions revision-list))))

(defun monotone-process-base-revision ()
  "Read from the current buffer the base revision.
This revision is put in the datastructure given by (monotone-status-list).
The expected format is one line containing 40 characters indicating the base revision id."
  (goto-char (point-min))
  (let ((revision (buffer-substring-no-properties (point) (+ (point) 40))))
    (monotone-status-data-set 'revision revision)))

(defun monotone-change-files-status (files keyword)
  "Change the status of the given files in (monotone-status-list)"
  (if files
      (let (file-status)
	(dolist (elt (monotone-status-data 'file))
	  (if (member (car elt) files)
	      (add-to-list 'file-status (cons (car elt) (monotone-state-keyword-state (cdr elt) keyword)))
	    (add-to-list 'file-status (cons (car elt) (cdr elt)))))
	(monotone-status-data-set 'file (reverse file-status)))))

(defun monotone-examine (directory)
  "Creates a buffer called *monotone* that is in monotone-mode that contains a file 
   list of what's in DIRECTORY, complete with a monotone status for every file."
  (interactive "Ddirectory: ") 
  (setq mt-directory (monotone-find-MT-top directory))
  (unless mt-directory
    (error "Can not find monotone bookkeeping directory for %s" directory))
  (switch-to-buffer (monotone-make-buffer))
  (set-buffer (monotone-make-buffer))
  (setq buffer-read-only nil)
  (erase-buffer)
  (monotone-mode)
  (setq default-directory mt-directory)
  (setq monotone-mt-directory (file-name-as-directory (concat default-directory monotone-bookkeeping-dir)))
  (monotone-status-data-set 'mt-directory monotone-mt-directory)
  (setq list-buffers-directory default-directory)
  (monotone-regenerate-buffer)
  ;; Now we should provide the information about the revision and the branch
  (monotone-regenerate-content-and-buffer))

(defun mt-examine (directory)
  "Shortcut for monotone-examine"
  (interactive "Ddirectory: ") 
  (monotone-examine directory)
)

(defun monotone-regenerate-content-and-buffer ()
  "Refresh from scratch all monotone data structures and redisplay the results.
This function should be called from  within a monotone buffer.
In short, it will run monotone automate inventory and monotone automate heads.  
Populate all datastructures from this and call monotone-regenerate-buffer to force the redisplay."
  (interactive)
  (let ((process-buffer (monotone-make-tmp-buffer))  ; Can we get rid of this.
	(directory default-directory)
	(status monotone-status))
    (monotone-certs-clear-cache)
    (monotone-read-options-file)
    (with-current-buffer process-buffer
      (setq default-directory directory)
      (set (make-local-variable 'monotone-status) status)
      (monotone-run-next process-buffer (list '(monotone-process-inventory . ("automate" "inventory"))
 					      '(monotone-process-revisions . ("complete" "revision" ""))
					      '(monotone-process-heads . ("automate" "heads"))
					      '(monotone-process-base-revision . ("automate" "get_base_revision_id"))
					      '(monotone-regenerate-buffer . nil))))))
      

(defun monotone-run-next (buffer process-list)
  "Run one by one the commands specified in process-list.
process-list is a list of pairs (process-function . ARG-LIST)
or (FUNCTION-LIST . ARG-LIST).

process-function is a function with no arguments which is run
with the current buffer equal to 'buffer'. 
'buffer' is the buffer used as output buffer 
for the program 'monotone-cmd' with arguments specified in ARG-LIST

FUNCTION-LIST is a list of functions as described in the previous paragraph.
In this case, all the functions are sequentially evalulated in the
context described above.

If ARG-LIST is nil, do not run monotone but just execute process-function
This is only allowed as the last element of the process-list.  In this
case the current-buffer will be equal to the monotone buffer.

Also before running the command we will update the monotone buffer
to indicate the running state of monotone."
   (let*
       ((process-function (caar process-list))
        (process-args (cdar process-list)))
     (with-current-buffer (monotone-make-buffer)
       (if (and process-args monotone-running)
	   (error "Monotone is already running")))
     (if process-args
	 (let ((process 
		(apply 'start-process "monotone" buffer monotone-cmd process-args)))
	   (set-process-sentinel process 'monotone-cmd-finished-2)
	   (set-process-filter process 'monotone-cmd-filter)
	   (with-current-buffer buffer
	     (erase-buffer)
	     (set (make-local-variable 'monotone-process-function) process-function)
	     (set (make-local-variable 'monotone-process-list) (cdr process-list)))
	   (monotone-start-running))
       (with-current-buffer (monotone-make-buffer)
	 (if (listp process-function)
	     (mapc (lambda (f) (apply f nil)) process-function)
	   (apply process-function nil))))))

(defun monotone-passwd-prompt (keypairid)
  "Function that fetches the password for keypairid.
It will prompt the user to provide the passphrase for the keypairid.  
This function could be extended to provide caching the passphrase 
or have other mechanisms for identification."
  (read-passwd (format "Passphrase for %s: " keypairid)))

(defun monotone-cmd-filter (process output)
  "Look in output stream for monotone authentication question.
This function should be used as a process-filter.  Its function
is to pretend it is not there, so just behave like there was
no process filter in place, until it notices that monotone is waiting
for a passphrase from the user.  If that is the case, it will
use the monotone-passwd-prompt function to get the passphrase
and send this back to monotone."
  (save-excursion
    ;; Save output in the correct buffer
    (set-buffer (process-buffer process))
    (goto-char (process-mark process))
    (insert output)
    (set-marker (process-mark process) (point))
    ;; Check for passphrase
    (goto-char (point-max))
    (beginning-of-line)
    (when (looking-at "^enter passphrase for key ID \\[\\(.*\\)\\]")
      (let ((passphrase (monotone-passwd-prompt (match-string 1))))
	(process-send-string process passphrase)
	(process-send-string process "\n")))
    ))

(defun monotone-stopped-running ()
  "This function will update the screen to notify the user that monotone
is not running anymore."
  (with-current-buffer (monotone-make-buffer)
    (let ((read-only-status buffer-read-only))
      (setq buffer-read-only nil)
      (setq monotone-running nil)
      (setq monotone-mode-line-process "exit")
      (force-mode-line-update)
      (monotone-update-status-overview)
      (setq buffer-read-only read-only-status))))

(defun monotone-start-running ()
  "This function will update the screen to notify the user that monotone
is running"
  (with-current-buffer (monotone-make-buffer)
    (let ((read-only-status buffer-read-only))
      (setq buffer-read-only nil)
      (setq monotone-running t)
      (setq monotone-mode-line-process "run")
      (force-mode-line-update)
      (setq buffer-read-only nil)
      (monotone-update-status-overview)
      (setq buffer-read-only read-only-status))))


(defun monotone-cmd-finished-2 (process event)
  "Process the final output of monotone commands.
When the process finished normally run all the commands that 
are mentioned in the variable monotone-process-function.  
This variable either contains a function taking no arguments
or is a list of functions taking no arguments.  
Before running these functions the current buffer is set
to the output buffer of the process.

After all the functions are run, the process-list variable is
checked to see if a new process should be started.  If so, 
monotone-run-next is called with as arguments the current 
process buffer and the process-list.

If the process did not terminate normally, the output buffer
is shown to the user and no processing is done.  Also no new
processes are started.
"
  (cond 
   ((equal event "finished\n")
    (with-current-buffer (process-buffer process)
      (let ((process-list monotone-process-list))
	(monotone-stopped-running)
	(if (listp monotone-process-function)
	    (mapc (lambda (f) (apply f nil)) monotone-process-function)
	  (apply monotone-process-function nil))
	(if process-list
	    (monotone-run-next (current-buffer) process-list)))))
   (t (progn 
	(monotone-stopped-running)
	(princ (format "Monotone unexpectedly finished with status: '%s'" event))
	(switch-to-buffer (process-buffer process))))))

(defun monotone-process-heads ()
  "Read from the current buffer a list of revisions. These revisions are put in 
the datastructe given by (monotone-status-list).  
The expected format is a revision per line, where a revision is a string of 40 characters.
Note that point is not preserved."
  (goto-char (point-min))
  (let ((head-list (list)))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (point) (+ (point) 40))))
	(put-text-property 0 (length (monotone-abbreviate line))
			   'font-lock-face 'underline line)
	(add-text-properties 0 40 `(keymap ,monotone-revision-keymap) line)
	(setq head-list (cons line head-list)))
      (forward-line 1))
    (monotone-status-data-set 'heads head-list)))

;; Fix the rename stuff below
(defconst monotone-inventory-state-mapping
  '(
    ("   " . unchanged)                      ; 'u'
    ("  U" . unknown)
    ("  I" . ignored)                        ; 'i'
    ("  P" . edited)                         ; 'e'
    (" AP" . added)                          ; 'a'
    (" AM" . added-and-missing)              ;
    ("D  " . dropped)                        ; 'd'
    ("D U" . dropped)                        ; dropped and unknown
    (" R " . renamed)                        ; 'r'
    (" RP" . renamed-and-edited)             ;
    ("  M" . missing)                        ; 'm'
    )
  "Mapping from monotone automate inventory output to montone-examine states.
If monotone automate inventory is changed this will need to change as well.")

(defun monotone-state-keyword-state (state keyword)
  "Returns the expected state after applying keyword to state.
Note that this can be wrong."
  (if (or (string= keyword "revert")
	  (string= keyword "commit"))
      'unchanged
    (if (string= keyword "drop")
	(if (eq state 'added)
	    'unchanged
	    'dropped
	    )
      (if (string= keyword "add")
	  (if (eq state 'dropped)
		  'unchanged
		  'added
		  )
	)
      )
    )
)

(defun monotone-process-inventory ()
  "Read from the current buffer the state of all the files under monotone control.
This information is put in the datastructure given by (monotone-status-list).
The data in the buffer should be in the format generated by monotone automate inventory.
Every line starts with three characters indicating the state, 
as given in monotone-inventory-state-mapping.  After the state follows a space followed
by a number, followed by a space, followed by a number. After the second number is a space,
followed by a filename. 
"
  (goto-char (point-min))
  (let ((file-status nil))
    (monotone-status-data-set 'file nil)
    (while (re-search-forward "^\\(...\\) \[0-9\]+ \[0-9\]+ \\(.*\\)$" nil t)
      (let ((status (cdr (assoc (match-string 1) monotone-inventory-state-mapping)))
	    (file-name (match-string 2)))
	(add-to-list 'file-status  (cons file-name status))))
    (monotone-status-data-set 'file (nreverse file-status))))

  

(defun monotone-insert-state-overview ()
"Inserts in the current buffer at point the state overview.
The buffer should be a monotone buffer because it will
need the variables monotone-running and monotone-status to 
generate the correct text.

The generated text is one line long and ends with two new lines."
  (if monotone-running
      (insert "Monotone is running")
    (if (monotone-revision-based-on-head-p)
	(insert "Working copy based on head, ")
      (insert "Working copy NOT based on head, "))
    (if (monotone-something-to-commit-p)
	(insert "and there are pending changes")
      (insert "with no local modifications")))
  (insert "\n\n"))

(defun monotone-add-file-state-line (counts states)
  (let (calculated-arguments)
    (dolist (state states)
      (setq calculated-arguments (cons 
				  (format "%s:" state) calculated-arguments))
      (setq calculated-arguments (cons 
				  (if (assoc state counts)
				      (cdr (assoc state counts))
				    0) calculated-arguments)))
    (setq calculated-arguments (nreverse calculated-arguments))
    (if (= 2 (length states))
	(insert (apply 'format "%-19s%5s    %-19s%5s\n" calculated-arguments))
      (insert (apply 'format "%-19s%5s    %-19s%5s    %-8s%5s\n" calculated-arguments)))))

(defun monotone-insert-file-overview ()
  (let ((counts (monotone-status-data 'counts)))
    (monotone-add-file-state-line counts '(edited added unknown))
    (monotone-add-file-state-line counts '(renamed-and-edited added-and-missing ignored))
    (monotone-add-file-state-line counts '(renamed dropped))
    (monotone-add-file-state-line counts '(unchanged missing TOTAL))))


(defun monotone-generate-file-counts ()
  (let ((counts (list)))
    ;; counts will be a list of the form:
    ;; ((type . number) ...)
    (dolist (entry (monotone-status-data 'file))
      (let ((type (cdr entry)))
	(unless (assoc type counts)
	  (add-to-list 'counts (cons type 0)))
	(setcdr (assoc type counts) (1+ (cdr (assoc type counts))))))
    (unless (assoc 'TOTAL counts)
      (add-to-list 'counts (cons 'TOTAL 0)))
    (setcdr (assoc 'TOTAL counts) (monotone-total-for-states counts monotone-possible-state-list))
    (monotone-status-data-set 'counts counts)))


(defun monotone-total-for-states (count-list states)
  (let ((total 0))
    (dolist (state states total)
      (if (assoc state count-list)
	  (setq total (+ total (cdr (assoc state count-list))))))))
    
(defun monotone-something-to-commit-p ()
  (< 0 (monotone-total-for-states (monotone-status-data 'counts) monotone-modified-state-list)))

(defun monotone-revision-based-on-head-p ()
  (member (monotone-status-data 'revision) (monotone-status-data 'heads)))

(defun monotone-store-point (begin-marker end-marker)
  "Store current point marker in buffer local variable.
User monotone-restore-point to move point to this marker."
  (setq monotone-stored-point (point-marker))
  (if (and (>= monotone-stored-point begin-marker) 
	   (<= monotone-stored-point end-marker))
      (setq monotone-stored-point (point))))

(defun monotone-restore-point ()
  "Move point to value set with monotone-store-point"
  (if monotone-stored-point
      (goto-char monotone-stored-point)))

(defun monotone-update-area (name)
  "Will delete from the buffer the text between the start marker 'name' 
end the end marker 'name'.  If the named marker not exist do not delete
but create the start marker.  Point is moved to just after the start marker.
Also store the current point to restore in monotone-end-area."
  (let ((begin-mark (cadr (assoc name monotone-markers)))
	(end-mark (cddr (assoc name monotone-markers))))
    (if (and begin-mark end-mark)
	(progn 
	  (monotone-store-point begin-mark end-mark)
	  (delete-region begin-mark end-mark)
	  (goto-char begin-mark)
	  (set-marker-insertion-type begin-mark nil))
      (add-to-list 'monotone-markers (cons name (cons (point-marker) (make-marker)))))))

(defun monotone-end-area (name)
  "Make the end marker 'name' point to the current position"
  (let ((begin-mark (cadr (assoc name monotone-markers)))
	(end-mark (cddr (assoc name monotone-markers))))
    (if (and begin-mark end-mark)
	(progn
	  (set-marker end-mark (point))
	  (set-marker-insertion-type begin-mark t)
	  (monotone-restore-point))
      (message "End mark is nil for name: %s, list of markers: %s" name monotone-markers))))

;; Regenerate buffer should not destroy the current buffer,
;; but just refresh the relevant parts.  This is a little
;; bit tricky because we need to find the relevant parts.
;; Probably best for this to use markers.
;;
;; So the layout is roughly as follows:
;; 
;; <begining-of-head-marker>
;; revision, head lines
;; <end-of-head-marker>
;; <begining-of-options-marker>
;; content of options file
;; <end-of-options-marker>
;; <beginning-of-status-overview-marker>
;; status overview
;; <end-of-status-overview-marker>
;; <beginning-of-file-list-marker>
;; file list
;; <end-of-file-list-marker>
;; 
;; This should maybe be integrated with the dired buffer?

(defun monotone-update-head ()
  "Insert head information in the currentbuffer.
Reads for this the MT/revision file.  This could be refactored
so the reading of the file is done somewhere else."
    (monotone-update-area 'head)
    (insert (format "directory %s\n" default-directory))
    (insert "revision  ")
    (let ((revision (or (monotone-status-data 'revision) "0000000000000000000000000000000000000000"))
	  (old-point (point)))
      ;; Put the revision in the data structure
      (insert revision)
      (setq abbreviation-length
	    (length (monotone-abbreviate revision)))
      (add-text-properties old-point (+ old-point abbreviation-length)
			   '(font-lock-face underline))
      (add-text-properties old-point (+ old-point 40)
			   `(keymap ,monotone-revision-keymap)))
    (insert "\nheads    ")
    (dolist (elt (monotone-status-data 'heads))
      (insert " ")
      (insert elt))
    (insert "\n\n")
    (monotone-end-area 'head))


(defun monotone-read-options-file ()
  "Read the options file and parse the content.  
the resulting alist is merged with the monotone status list."
  (let ((mt-directory (monotone-status-data 'mt-directory)))
    (with-temp-buffer
      (insert-file-contents (concat mt-directory "options"))
      (condition-case nil
	  (while t
	    (let ((key (read (current-buffer)))
		  (value (read (current-buffer))))
	      (if (eq key 'branch)
		  (progn
		    (monotone-status-data-set key value)
		    (if (string= value (monotone-status-data 'target-branch))
			(monotone-status-data-set 'target-branch nil))))))
	;;  add to monotone status list
	('end-of-file nil)))))

(defun monotone-update-options ()
  "Show branch information in buffer"
  (unless (monotone-status-data 'branch)
    (monotone-read-options-file))
  (monotone-update-area 'options)
  (if (monotone-status-data 'target-branch)
      (progn
	(insert "new branch ")
	(insert (format "%S" (monotone-target-branch)))
	(insert "\n\n")))
  (let ((length (cadr (insert-file-contents (concat monotone-mt-directory "options")))))
    (goto-char (+ (point) length)))
  (insert "\n")
  (monotone-end-area 'options))

(defun monotone-update-status-overview ()
  "Update the status overview in the current buffer"
    (monotone-update-area 'status-overview)
    (monotone-generate-file-counts)
    (monotone-insert-state-overview)
    (monotone-insert-file-overview)
    (insert (format "\nCurrently displaying %s\n\n" monotone-display-states))
    (monotone-end-area 'status-overview))

(defun monotone-update-file-list ()
    (monotone-update-area 'file-list)
    (insert "\n") ;; note we can't handle emtpy regions very well.
    (let* ((file-list  (assoc-default 'file monotone-status)))
      (dolist (elt file-list)
	(let* ((key-word (cdr elt))
	       (file-name (car elt))
	       (string (format "%12s  %s\n" key-word file-name))
	       (sp (point)))
	  (when (memq key-word monotone-display-states)
	    (if (assoc file-name monotone-marked-files)
		(insert (cdr (assoc file-name monotone-marked-files)))
	      (insert " "))
	    (insert string)
	    (add-text-properties sp (point) (list 'file-name file-name))))))
    (monotone-end-area 'file-list))

(defun monotone-regenerate-buffer ()
  "This will generate the buffer contents from the status list"
  (setq buffer-read-only nil)
  ;; Add the information found in the MT directory
  (let ((current-point (point))) ;; This needs to be improved!!
    (monotone-update-head)
    (monotone-update-options)
    (monotone-update-status-overview)
    (monotone-update-file-list)
    (goto-char current-point))
  (setq buffer-read-only t))

(defun monotone-find-file-on-current-line ()
  (get-text-property (point) 'file-name))
  

(defun monotone-get-file-list-and-ask-user-for-confirmation (action &optional default-function)
  (let ((selected-files (mapcar 'car monotone-marked-files)))
    (if (= (length selected-files) 0)
	(if default-function
	    (list (funcall default-function))
	  nil)
      (with-output-to-temp-buffer "*monotone selection*"
	(princ (mapconcat 'identity selected-files "\n")))
      ;; ask for confirmation
      (if (y-or-n-p (format "Going to %s %d file%s " 
			    action 
			    (length selected-files)
			    (if (not (= (length selected-files) 1)) "s" "")
			    ))
	  selected-files
	nil))))


(defun monotone-filter-list (function in-list)
  (let ((result (list)))
    (mapc (lambda (elt) (if (funcall function elt) (push elt result))) in-list)
    (reverse result)))
	


;;; ---------------------------------------------------------------------------
;;; Create key board actions that toggle the
;;; display of different files


(defun monotone-toggle-display (status)
  (if (memq status monotone-display-states)
      (setq monotone-display-states (delq status monotone-display-states))
    (setq monotone-display-states (cons status monotone-display-states))))


;; This looks a little bit ugly. 

(defmacro monotone-create-toggle-action (var)
  `(defun ,(intern (concat "monotone-toggle-display-" var)) ()
     ,(format "Toggle the display of files with status %S" var)
     (interactive)
     (monotone-toggle-display (intern ,var))
     (monotone-regenerate-buffer)))

(monotone-create-toggle-action "unknown")
(monotone-create-toggle-action "added")
(monotone-create-toggle-action "ignored")
(monotone-create-toggle-action "dropped")
(monotone-create-toggle-action "missing")
(monotone-create-toggle-action "edited")
(monotone-create-toggle-action "unchanged")
(monotone-create-toggle-action "renamed")

(defun monotone-show-interesting-files () 
  "Show interesting files."
  (interactive)
  (setq monotone-display-states
	(copy-sequence monotone-interesting-state-list))
  (monotone-regenerate-buffer))

(defun monotone-show-all-files () 
  (interactive) 
  (setq monotone-display-states 
	(copy-sequence monotone-possible-state-list)) 
  (monotone-regenerate-buffer))

(defun monotone-show-known-files ()
  (interactive)
  (setq monotone-display-states
	(copy-sequence monotone-known-state-list))
  (monotone-regenerate-buffer))

(defun monotone-visit-file ()
  (interactive)
  (let ((file-name (monotone-find-file-on-current-line)))
    (if file-name
	(find-file file-name)
      (message "Can not determine file name"))))

  

(defmacro monotone-create-simple-action (name)
  `(defun ,(intern (concat "monotone-" name "-file")) () 
     (interactive)
     (let ((file-names (monotone-get-file-list-and-ask-user-for-confirmation ,name 'monotone-find-file-on-current-line))
	   (current-directory default-directory))
       (if file-names
	   (progn
	     (monotone-change-files-status file-names ,name)
	     (monotone-run-next (monotone-make-tmp-buffer)
				(list
				 (cons 'monotone-process-nothing (cons ,name file-names))
				 '(monotone-regenerate-buffer . nil))))))))

(monotone-create-simple-action "add")
(monotone-create-simple-action "drop")
(monotone-create-simple-action "revert")


;;;----------------------------------------------------
;;; functions for handling the commit buffer

(defun monotone-remove-MT-lines ()
  "Remove lines starting with 'MT:' from the buffer."
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "^MT:.*$" (point-max) t)
    (beginning-of-line)
    (delete-region (point) (progn (forward-line) (point)))))

(defun monotone-prepend-MT ()
  "Prepend in current buffer MT: string on all lines"
  (goto-char (point-min))
  (while (not (eobp))
    (beginning-of-line)
    (insert "MT: ")
    (forward-line))
  (goto-char (point-min)))

(defun monotone-extract-commit-message ()
  "Returns the content of buffer after removing all lines starting with the string MT:"
  ;; First remove the MT: lines
  (monotone-remove-MT-lines)
  (buffer-substring-no-properties (point-min) (point-max)))

(defvar branch-history nil
  "Branch minibuffer history list.")

;;;---------------------------------------------------------
;;; functions for commiting
;;;
(defun monotone-commit-do-commit (&optional ask-branch)
  (interactive "P")
  (let ((commit-message-arg (list 
			     (format "--message=%s" (monotone-extract-commit-message))
			     (format "--branch=%s" (monotone-target-branch ask-branch)))))
    (if commit-file-list
	(progn
	  (monotone-change-files-status commit-file-list "commit")

	  (monotone-run-next (monotone-make-tmp-buffer)
			     (list `(monotone-process-nothing . ,(cons "commit" (append commit-message-arg
											commit-file-list)))
				   '(monotone-process-revisions . ("complete" "revision" ""))
				   '(monotone-process-heads . ("automate" "heads"))
				   `((list (lambda () (kill-buffer ,(current-buffer)))
					   monotone-regenerate-buffer) . nil))))

    (monotone-run-next (monotone-make-tmp-buffer)
		       (list `(monotone-regenerate-content-and-buffer . ,(cons "commit" 
									       (append commit-message-arg 
										     commit-file-list)))
			     `((list (lambda () (kill-buffer ,(current-buffer)))) . nil))))))


(defun monotone-simple-commit (&optional ask-branch)
  "Commit marked files or, if no files are marked, all changes."
  (interactive "P")
  (monotone-target-branch ask-branch)
  (save-some-buffers)
  (let ((buf (get-buffer-create monotone-commit-buffer))
	(file-list (monotone-get-file-list-and-ask-user-for-confirmation "commit")))
    (monotone-switch-to-buffer-if-not-current buf)
    (set-buffer buf)
    (toggle-read-only -1)
    (monotone-commit-mode)
    (setq commit-file-list file-list)))
			     
(defun monotone-commit (&optional ask-branch)
  "Commit marked files or, if no files are marked, all changes. Commit buffer will contain relevant diffs."
  (interactive "P")
  (monotone-target-branch ask-branch)
  ;; 1. ask user to save buffers
  (save-some-buffers)
  (let ((buf (get-buffer-create monotone-commit-buffer))
	(file-list (monotone-get-file-list-and-ask-user-for-confirmation "commit")))
    (monotone-switch-to-buffer-if-not-current buf)
    (set-buffer buf)
    (toggle-read-only -1)
    (monotone-commit-mode)
    (setq commit-file-list file-list)
    (monotone-run-next (current-buffer)
		       (list (cons (list (lambda () 
					   (insert "You are going to commit, edit this buffer for the commit message\n"
						   "All lines starting with MT: will be removed before the actual commit\n"
						   "takes place.  The key combination C-c C-c will finalize the commit.\n"
						   "In case you want to see the final commit message as is, without the MT: lines\n"
						   "use the key combination C-c C-r\n"))
					 'monotone-prepend-MT)
				   (cons "diff" commit-file-list))))))




;;;-----------------------------------------------------------------------
;;; handling marks
(defun monotone-unmark-all ()
  (interactive)
  (setq monotone-marked-files (list))
  (setq buffer-read-only nil)
  (monotone-update-file-list)
  (setq buffer-read-only t))

(defun monotone-mark-file ()
  "Mark the file on current line with '*' mark.
Also update the display to ensure the user get feedback of the marking.
If the file is already marked, or the cursor is not on a line indicating
a file to be marked, do nothing."
  (interactive)
  (let ((file-name (monotone-find-file-on-current-line)))
    (if (and file-name (not (assoc file-name monotone-marked-files)))
	(progn
	  (setq monotone-marked-files (cons (cons file-name ?*) monotone-marked-files))
	  (setq buffer-read-only nil)
	  (monotone-update-file-list)
	  (setq buffer-read-only t)))
    (forward-line)))

(defun monotone-unmark-file ()
  "Unmark the fil eon the current line.
Remove the mark of the file and update the buffer to give feedback to the user.
If no mark is associated with the file, or no file name is present on the line containing
point do nothing."
  (interactive)
  (let ((file-name (monotone-find-file-on-current-line)))
    (if (and file-name (assoc file-name monotone-marked-files))
	(progn 
	  (setq monotone-marked-files (delq (assoc file-name monotone-marked-files) monotone-marked-files))
	  (setq buffer-read-only nil)
	  (monotone-update-file-list)
	  (setq buffer-read-only t)))
    (forward-line)))
	
(defun monotone-toggle-mark-file ()
  (interactive)
  (let ((file-name (monotone-find-file-on-current-line)))
    (if file-name
	(progn
	  (if (assoc file-name monotone-marked-files) 
	      ;; it exists
	      (setq monotone-marked-files (assq-delete-all file-name monotone-marked-files))
	    ;; it did not exists
	    (setq monotone-marked-files (cons (cons file-name ?*) monotone-marked-files)))
	  (setq buffer-read-only nil)
	  (monotone-update-file-list)
	  (setq buffer-read-only t)))))
  

(setq monotone-revision-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map "=" 'monotone-diff-revision)
	map))

(setq monotone-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'forward-line)
    (define-key map "p" 'previous-line)
    (define-key map "a" 'monotone-show-all-files)
    (define-key map "i" 'monotone-show-interesting-files)
    (define-key map "k" 'monotone-show-known-files)
    (define-key map "sa" 'monotone-toggle-display-added)
    (define-key map "si" 'monotone-toggle-display-ignored)
    (define-key map "sd" 'monotone-toggle-display-dropped)
    (define-key map "sm" 'monotone-toggle-display-missing)
    (define-key map "se" 'monotone-toggle-display-edited)
    (define-key map "su" 'monotone-toggle-display-unchanged)
    (define-key map "sr" 'monotone-toggle-display-renamed)
    (define-key map "s?" 'monotone-toggle-display-unknown)
    (define-key map "g" 'monotone-regenerate-content-and-buffer)
    (define-key map "D" 'monotone-drop-file)
    (define-key map "U" 'monotone-revert-file)
    (define-key map "=" 'monotone-diff-file)
    (define-key map "A" 'monotone-add-file)
    (define-key map "C" 'monotone-commit)
    (define-key map "c" 'monotone-simple-commit)
    (define-key map "m" 'monotone-mark-file)
    (define-key map "u" 'monotone-unmark-file)
    (define-key map "B" 'monotone-change-target-branch)
    (define-key map (kbd "M-u") 'monotone-unmark-all)
    (define-key map "f" 'monotone-visit-file)
    (define-key map "q" 'quit-window)
    (define-key map "t" 'monotone-annotate-file)
    (define-key map "W" 'monotone-where-am-i)
    (define-key map (kbd "RET") 'monotone-visit-file)
    ;; menu bar
    map))

(easy-menu-define monotone-menu monotone-mode-map "Menu used in `monotone-mode'."
  '("Monotone"
    ["Open file.."  monotone-visit-file t]  ;; should check if cursor is on line
    ["Add"          monotone-add-file t]
    ["Drop"         monotone-drop-file t]
    ["Revert"       monotone-revert-file t]
    "----"
    ("Show/Hide"
      ["Added"       monotone-toggle-display-added t]
      ["Ignored"     monotone-toggle-display-ignored t]
      ["Dropped"     monotone-toggle-display-dropped t]
      ["Missing"     monotone-toggle-display-missing t]
      ["Edited"      monotone-toggle-display-edited t]
      ["Renamed"     monotone-toggle-display-renamed t]
      ["Unknown"     monotone-toggle-display-unknown t])
    ["Show interesting" monotone-show-interesting-files  t]
    ["Show all" monotone-show-all-files t]
    ["Show known files" monotone-show-known-files t]
    "----"
    ["Mark"         monotone-mark-file t]
    ["Unmark"       monotone-unmark-file t]
    ["Unmark all"   monotone-unmark-all t]
    "----"
    ["Commit"       monotone-commit t]
    ["Change branch.."  monotone-change-target-branch t]
    "----" 
    ["Current diff" monotone-diff-file t]  ;; chouls check if cursor is on line??
    ["Where am I"   monotone-where-am-i t]))



(setq monotone-commit-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c C-c") 'monotone-commit-do-commit)
	(define-key map (kbd "C-c C-r") 'monotone-remove-MT-lines)
	map))

(easy-menu-define monotone-commit-menu monotone-commit-mode-map "Menu used in `monotone-commit-mode'."
  '("Monotone"
    ["Execute commit" monotone-commit-do-commit t]
    ["Remove MT: lines" monotone-remove-MT-lines t]))
    
(defconst monotone-examine-head-font-lock-keywords
  '(("^\\s-*branch\\s-" . monotone-label-face)
    ("^\\s-*new branch\\s-" . monotone-label-face)
    ("^\\s-*revision\\s-" . monotone-label-face)
    ("^\\s-*heads\\s-" . monotone-label-face)
    ("^\\s-*database\\s-" . monotone-label-face)
    ("^\\s-*key\\s-" . monotone-label-face)
    ("^\\s-*keydir\\s-" . monotone-label-face)
    ("^\\s-*directory\\s-" . monotone-label-face)
    ("Working copy \\(NOT based on head\\)" (1 monotone-warning-state-face))
    ("and there are \\(pending changes\\)" (1 monotone-warning-state-face))
    ("Monotone is running" . font-lock-warning-face)))

(defun monotone-examine-font-lock-keywords ()
  (let ((font-lock-list monotone-examine-head-font-lock-keywords))
    (dolist (elt monotone-status-faces font-lock-list)
      (add-to-list 'font-lock-list 
		   (cons (format "^.\\s-*%s\\s-" (car elt)) (cdr elt))))))

      
(defun monotone-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "")
  (setq mode-line-process
	'("Monotone: " monotone-mode-line-process))
  (make-local-variable 'monotone-mode-line-process)
  (use-local-map monotone-mode-map)
  (setq major-mode 'monotone-mode)
  (set (make-local-variable 'monotone-status) (list (cons 'file nil) 
						    (cons 'revision nil) 
						    (cons 'heads nil)
						    (cons 'counts nil)
						    (cons 'branch nil)
						    (cons 'target-branch nil)
						    (cons 'branches-list nil)
						    (cons 'abbreviations nil)
						    (cons 'mt-directory nil)))
  (set (make-local-variable 'monotone-examine-keywords) (monotone-examine-font-lock-keywords))
  (set (make-local-variable 'font-lock-defaults) '(monotone-examine-keywords))
  (set (make-local-variable 'monotone-markers) (list))
  (set (make-local-variable 'monotone-mt-directory) nil)
  (set (make-local-variable 'monotone-running) nil)
  (set (make-local-variable 'monotone-stored-point) nil)
  (set (make-local-variable 'monotone-marked-files) (list))
  (make-local-variable 'monotone-display-states))


(defvar monotone-commit-font-lock-keywords
  '(("^\\(MT:\\)" (1 font-lock-comment-face)))
  "Highlight MT: lines")

(defun monotone-commit-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map monotone-commit-mode-map)
  (setq major-mode 'monotone-commit-mode)
  (setq mode-name "Monotone Commit")
  (set (make-local-variable 'comment-start) "MT:")
  (set (make-local-variable 'commit-file-list) nil)
  (set (make-local-variable 'font-lock-defaults) '(monotone-commit-font-lock-keywords)))
  
;;;
;;; Utilities
;;; 

(defun monotone-switch-to-buffer-if-not-current (buf)
  (when (not (equal (current-buffer) buf))
    (switch-to-buffer-other-window buf)))



;;;
;;; Diffing
;;; 
(defun monotone-process-diff ()
  (setq buffer-read-only t)
  ;; for older emacs versions. Works fine in CVS version of May 2005
  (define-key (current-local-map) "q" 'quit-window)
  (goto-char (point-min))
  (diff-hunk-next))

(defun monotone-diff-file ()
  "Show diff between file at point on disk and file at current revision in database."
  (interactive)
  (let ((file-name (monotone-find-file-on-current-line))
	(current-directory default-directory))
    (if file-name
	(let ((diff-buffer (get-buffer-create "*monotone-diff*"))
	      (file-buffer (find-buffer-visiting file-name))
	      (main-buffer (monotone-make-buffer)))
	  ;; Ask if we need to save the buffer we are going to diff
	  (when file-buffer
	    (with-current-buffer file-buffer
	      (vc-buffer-sync t)))
	  ;; Bring diff buffer to foreground, clear current content
	  ;; initialize it and direct the output of monotone diff
	  ;; to this buffer
	  (monotone-switch-to-buffer-if-not-current diff-buffer)
	  (kill-all-local-variables)
	  (setq buffer-read-only nil)
	  (setq default-directory current-directory)
	  (erase-buffer)
	  (diff-mode)
	  (monotone-run-next diff-buffer 
			     (list `(monotone-process-diff . ("diff" ,file-name))))
	  ;; Now make sure we leave the main-buffer the active buffer
	  (switch-to-buffer-other-window main-buffer)
	  (set-buffer main-buffer)
	  )
      (message "Can not determine file name"))))

(defun monotone-diff-revision ()
  "Show diff between files on disk and revision at point."
  (interactive)
  (let ((revision (thing-at-point 'word))
	(current-directory default-directory)
	(main-buffer (current-buffer))
	(diff-buffer (get-buffer-create "*monotone-diff*")))
    (monotone-switch-to-buffer-if-not-current diff-buffer)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (setq default-directory current-directory)
    (erase-buffer)
    (diff-mode)
    (monotone-run-next diff-buffer
		       (list `(monotone-process-diff . ("diff" "-r" ,revision))))
    (switch-to-buffer-other-window main-buffer)
    (set-buffer main-buffer)))

(defun monotone-where-am-i ()
  (interactive)
  (let ((current-directory default-directory)
	(current-revision (monotone-status-data 'revision))
	(wami-buffer (get-buffer-create "*wami monotone*")))
    (monotone-switch-to-buffer-if-not-current wami-buffer)
    (monotone-wami-mode)
    (setq monotone-wami-current-revision current-revision)
    (setq default-directory current-directory)
    (monotone-wami-create-full-graph)))

	       
;;;
;;; The end
;;; 
(provide 'e-monotone)
;;; e-monotone.el ends here

