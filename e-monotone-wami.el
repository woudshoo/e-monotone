
(defvar monotone-preferred-branches
  nil)

(defvar monotone-wami-node-displayed-certs
  '("branch"
    "version"
    ("date" . (lambda (x) (format-time-string "%c" 
					      (date-to-time 
					       (mapconcat (lambda (y) y)
							  (split-string x "T") " ")))))))

;; replace this with a defconst
(setq monotone-wami-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "q" 'quit-window)
	(define-key map "g" 'monotone-wami-regenerate-content)
	(define-key map "+" 'monotone-wami-more-nodes)
	(define-key map "=" 'monotone-wami-more-nodes)
	(define-key map "sd" 'monotone-wami-generate-default)
	(define-key map "sf" 'monotone-wami-generate-full)
	(define-key map "sc" 'monotone-wami-generate-compact)
	(define-key map "sb" 'monotone-wami-generate-branch-crossing)
	(define-key map "si" 'monotone-wami-generate-branch-incoming)
	(define-key map "sm" 'monotone-wami-generate-major-branch-crossing)
	(define-key map "sa" 'monotone-wami-generate-listed-revisions)
	(define-key map "sh" 'monotone-wami-generate-head-and-tails)
	(define-key map "-" 'monotone-wami-less-nodes)
	(define-key map "_" 'monotone-wami-less-nodes)
	(define-key map "cr" 'monotone-wami-set-current-revision)
	(define-key map "cC" 'monotone-certs-clear-cache)
	(define-key map [mouse-1] 'monotone-wami-mouse-recenter-graph)
	map))

(easy-menu-define monotone-wami-menu monotone-wami-mode-map "Menu used in `montone-wami-mode'."
  '("Monotone"
    ["Show all nodes" monotone-wami-generate-full t]
    ["Show compact version" monotone-wami-generate-compact t]
    ["Show heads" monotone-wami-generate-head-and-tails t]
    ["Show incoming branches" monotone-wami-generate-branch-incoming t]
    ["Show branch changes" monotone-wami-generate-branch-crossing t]
    ["Revert to default graph" monotone-wami-generate-default t]
    "----"
    ["Display more nodes" monotone-wami-more-nodes t]
    ["Display less nodes" monotone-wami-less-nodes t]
    "----"
    ["Change to revision.." monotone-wami-set-current-revision t]
    ["Clear certifate cache" monotone-certs-clear-cache t]
    "----"
    ["Quit" quit-window  t]))

(defconst monotone-wami-font-lock-keywords
  '(("^[^:]+:" . monotone-label-face)))

(defun monotone-wami-mode ()
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'monotone-wami-full-graph-hook)
  (make-local-variable 'monotone-wami-generate-graph)
  (make-local-variable 'monotone-wami-format-nodes)
  (make-local-variable 'monotone-wami-format-edges)
  (make-local-variable 'monotone-wami-target-number-of-nodes)
  (make-local-variable 'monotone-wami-node-selection-p)

  (setq monotone-wami-full-graph-hook 'monotone-wami-default-hook)
  (setq monotone-wami-generate-graph 'monotone-wami-generate-graph-default)
  (setq monotone-wami-format-nodes 'monotone-wami-format-nodes-default)
  (setq monotone-wami-format-edges 'monotone-wami-format-edges-test)
  (setq monotone-wami-target-number-of-nodes 10)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-default-p)
  (setq monotone-wami-color-list nil)
  (setq monotone-wami-hue-list nil)
  (setq monotone-wami-revision-list nil)
  
  (use-local-map monotone-wami-mode-map)
  (setq header-line-format '("Nodes: " 
			     (:eval (number-to-string monotone-wami-target-number-of-nodes))
			     "/"
			     (:eval (number-to-string (hash-table-count (nth 0 monotone-wami-current-graph))))
			     "/"
			     (:eval (number-to-string (hash-table-count (nth 0 monotone-wami-pre-limit-graph))))
			     "/"
			     (:eval (number-to-string (hash-table-count (nth 0 monotone-wami-full-graph))))))
  (set (make-local-variable 'font-lock-defaults) '(monotone-wami-font-lock-keywords))
  (setq mode-name "Monotone WAMI"))

(defun monotone-wami-create-full-graph ()
  "Generate graph.
Run monotone automate graph and process the output according 
to the current settings for the graph processing variables.
see ..."
  (setq buffer-read-only nil)
  (erase-buffer)
  (monotone-run-next (current-buffer)
		     (list '(monotone-process-wami-graph . ("automate" "graph")))))

(defun monotone-process-wami-graph ()
  (interactive)
  (setq monotone-wami-full-graph (monotone-process-graph))
  (monotone-wami-regenerate-content))


(defun monotone-wami-regenerate-content ()
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (funcall monotone-wami-full-graph-hook)
  (setq buffer-read-only t))
  
(defun monotone-wami-more-nodes ()
  (interactive)
  (setq monotone-wami-target-number-of-nodes (+ monotone-wami-target-number-of-nodes 4))
  (monotone-wami-redisplay-with-limits))

(defun monotone-wami-less-nodes ()
  (interactive)
  (setq monotone-wami-target-number-of-nodes 
	(max 1 (- monotone-wami-target-number-of-nodes 4)))
  (monotone-wami-redisplay-with-limits))

(defun monotone-wami-generate-default ()
  (interactive)
  (setq monotone-wami-generate-graph 'monotone-wami-generate-graph-default)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-default-p)
  (setq monotone-wami-format-edges 'monotone-wami-format-edges-test)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-two-branches ()
  (interactive)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-two-branches-p)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-compact ()
  (interactive)
  (setq monotone-wami-generate-graph 'monotone-wami-generate-graph-default)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-full ()
  (interactive)
  (setq monotone-wami-generate-graph 'monotone-wami-generate-graph-non-compactified)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-listed-revisions ()
  (interactive)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-listed-revisions-p)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-branch-crossing ()
  (interactive)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-branch-crossing-p)
  (setq monotone-wami-format-edges 'monotone-wami-format-edges-test)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-major-branch-crossing ()
  (interactive)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-major-branch-crossing-p)
  (setq monotone-wami-format-edges 'monotone-wami-format-edges-test)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-branch-incoming ()
  (interactive)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-branch-incoming-p)
  (setq monotone-wami-format-edges 'monotone-wami-format-edges-incoming)
  (monotone-wami-regenerate-content))

(defun monotone-wami-generate-head-and-tails ()
  (interactive)
  (setq monotone-wami-node-selection-p 'monotone-wami-node-selection-head-and-tails-p)
  (setq monotone-wami-format-edges 'monotone-wami-format-edges-test)
  (monotone-wami-regenerate-content))

(defun monotone-wami-default-hook ()
  (setq monotone-wami-pre-limit-graph (funcall monotone-wami-generate-graph (wro-copy-graph monotone-wami-full-graph)))
  (monotone-wami-redisplay-with-limits))

(defun monotone-wami-redisplay-with-limits ()
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq monotone-wami-current-graph (wro-neighbourhood-of-node monotone-wami-current-revision
							       monotone-wami-target-number-of-nodes
							       monotone-wami-pre-limit-graph))
  (monotone-wami-append-certificates monotone-wami-current-revision)
  (let ((current-position (point)))
    (insert-image
     (wro-graph-image-representation
      monotone-wami-current-graph
      monotone-wami-format-nodes
      monotone-wami-format-edges)
     "graphical representation of version graph")
    ;; try out tooltip code
    (add-text-properties current-position (point) `(help-echo monotone-wami-mouse-tooltip))
    ;; add list of fixed SPR's
;    (monotone-wami-append-certificate-values-for-ancestors monotone-wami-current-revision "fixed")
    (goto-char current-position))
  (setq buffer-read-only t))
	     

(defun monotone-wami-generate-graph-default (graph)
  (wro-compactify-edges (lambda (node)
			  (or (equal monotone-wami-current-revision node)
			      (funcall monotone-wami-node-selection-p node)))
			graph))


(defun monotone-wami-generate-graph-non-compactified (graph)
  graph)


(defun monotone-wami-node-selection-default-p (node)
  (wro-not-simple-node-p node monotone-wami-full-graph))

(defun monotone-wami-node-selection-head-and-tails-p (node)
  (or (= (length (wro-outgoing-edges node monotone-wami-full-graph)) 0)
      (= (length (wro-incoming-edges node monotone-wami-full-graph)) 0)))

(defun monotone-wami-node-selection-branch-crossing-p (node)
  "Returns true if node is at border of branches.
More precise, returns true if one of the following holds:
There is no incoming edge, 
There is no outgoing edge,
there exists an edge of which the other side is in another branche"
  (let ((incoming (wro-incoming-edges node monotone-wami-full-graph))
	(outgoing (wro-outgoing-edges node monotone-wami-full-graph))
	(branches (monotone-cert-values node "branch")))
    (or (= (length incoming) 0)
	(= (length outgoing) 0)
	(progn 
	  (while (and outgoing
		      (equal (monotone-cert-values (cdar outgoing) "branch")
			     branches))
	    (setq outgoing (cdr outgoing)))
	  outgoing)
	(progn 
	  (while (and incoming
		      (equal (monotone-cert-values (caar incoming) "branch")
			     branches))
	    (setq incoming (cdr incoming)))
	  incoming))))
	
(defun monotone-wami-map-branches (branches)
  "Map the branches to a sorted list of unique version numbers"
  (let (versions)
    (mapc (lambda (x) 
	    (let ((components (split-string x "\\.")))
	      (unless (or (not (nth 2 components)) (member (nth 2 components) versions))
		(setq versions (cons (nth 2 components) versions)))))
	  branches)
    (sort versions 'string<)))

(defun monotone-wami-node-selection-major-branch-crossing-p (node)
  "Returns true if node is at border of branches.
More precise, returns true if one of the following holds:
There is no incoming edge, 
There is no outgoing edge,
there exists an edge of which the other side has another version"
  (let ((incoming (wro-incoming-edges node monotone-wami-full-graph))
	(outgoing (wro-outgoing-edges node monotone-wami-full-graph))
	(branches (monotone-wami-map-branches (monotone-cert-values node "branch"))))
    (or (= (length incoming) 0)
	(= (length outgoing) 0)
	(progn 
	  (while (and outgoing
		      (equal (monotone-wami-map-branches (monotone-cert-values (cdar outgoing) "branch"))
			     branches))
	    (setq outgoing (cdr outgoing)))
	  outgoing)
	(progn 
	  (while (and incoming
		      (equal (monotone-wami-map-branches (monotone-cert-values (caar incoming) "branch"))
			     branches))
	    (setq incoming (cdr incoming)))
	  incoming))))
	
(defun monotone-wami-node-selection-branch-incoming-p (node)
  "Returns true if
There is no incoming edge if in current branch
There is no outgoing edge if in current branch
There is an edge with the other side is in another branch, and the outgoing side
is in the current branch"
  (let ((incoming (wro-incoming-edges node monotone-wami-full-graph))
	(outgoing (wro-outgoing-edges node monotone-wami-full-graph))
	(branch (monotone-wami-branch node))
	(current-branch (monotone-wami-branch monotone-wami-current-revision)))
    (or (and (= (length incoming) 0) (equal branch current-branch))
	(and (= (length outgoing) 0) (equal branch current-branch))
	(if (equal branch current-branch)
	    (progn
	      (while (and incoming 
			  (equal (monotone-wami-branch (caar incoming))
				 branch))
		(setq incoming (cdr incoming)))
	      incoming)
	  (while (and outgoing
		      (not (equal (monotone-wami-branch (cdar outgoing))
			   current-branch)))
	    (setq outgoing (cdr outgoing)))
	  outgoing))))

(defun monotone-wami-branch (revision)
  "Returns a single branch for revision.
If a revision has multiple revision certificates use
the global variable monotone-preferred-branches to find
the branch to return. "
  (let ((branches (monotone-cert-values revision "branch"))
	(try-list monotone-preferred-branches)
	(result))
    (if (= (length branches) 1)
	(car branches)
      (while (and (not result)  try-list)
	(let ((todo branches))
	  (while (and (not result) todo)
	    (if  (string-match (car try-list) (car todo))
		(setq result (car todo)))
	    (setq todo (cdr todo))))
	(setq try-list (cdr try-list))))
    (if result
	result
      (car branches))))
  
(defun monotone-wami-node-selection-listed-revisions-p (node)
  (member node monotone-wami-revision-list))


(defun monotone-wami-print-list (list)
  (let ((result (if list (format "%s" (car list)))))
    (setq list (cdr list))
    (mapc (lambda (x) (setq result (concat result (format "\\n%s" x)))) list)
    result))
		       
(defun monotone-wami-node-certs-list (node)
  (apply 'append
	 (mapcar (lambda (e) 
		   (cond ((stringp e)
			  (monotone-cert-values node e))
			 ((listp e)
			  (mapcar (cdr e) (monotone-cert-values node (car e))))
			 (t (list "<<<< ILLEGAL >>>"))))
		 monotone-wami-node-displayed-certs)))


(defun monotone-wami-format-nodes-default (node type)
  (let ((abbreviation (monotone-abbreviate node)))
    (format "%S [label=\"%s\",href=\"\\N\",fontcolor=%S%s%s];\n"
	    node
	    (monotone-wami-print-list (cons abbreviation (monotone-wami-node-certs-list node)))
	    (if (eq type 'partial) "black" "black")
	    (if (equal node monotone-wami-current-revision)  ",shape=box,peripheries=5" 
	      (concat (if (eq type 'partial) ",shape=hexagon" "") 
		      (if (= (length (wro-outgoing-edges node monotone-wami-full-graph)) 0)
			  ",peripheries=1" ",peripheries=0")))
	    (format ",fillcolor=\"%s\",style=filled"
		    (monotone-wami-test-get-color node type)))))


(defun monotone-wami-format-edges-default (edge count)
  
  (let ((output-string (if (wro-graph-contains-edge edge monotone-wami-full-graph)
			   (format "%S -> %S;\n" (car edge) (cdr edge)) 
			 (format "%S -> %S [color=red];\n" (car edge) (cdr edge))))
	(result ""))
    (setq count (min count 2))
    (while (> count 0)
      (setq result (concat result output-string))
      (setq count (1- count)))
    result))

(defun monotone-wami-format-edges-test (edge count)
  (let ((edge-string (format "%S -> %S" (car edge) (cdr edge)))
	(options))
    (if (not (wro-graph-contains-edge edge monotone-wami-full-graph))
	(setq options (cons "color=blue" options)))
    (if (> count 1)
	(setq options (cons "style=bold" options)))
    (if (equal (monotone-cert-values (car edge) "branch")
	       (monotone-cert-values (cdr edge) "branch"))
	(setq options (cons "weight=3" options)))
    
    (if (> (length options) 0)
	(progn
	  (setq edge-string (concat edge-string "[" (car options)))
	  (setq options (cdr options))
	  (mapc (lambda (e) (setq edge-string (concat edge-string "," e))) options)
	  (setq edge-string (concat edge-string "]"))))
    (concat edge-string "\n;")))

;; Here comes the headache. 
;; Somehow monotone refuse to store certain edges.  This

;;  show edge if:
;;  * end point of edge ends in 'current' branch
;; // * if begin point is current branch 
;;    AND NOT
;;    there is an edge of the end point to an end point
;;    of an edge of the begin point.  

;; * if the end point of the edge is not in current branch
;;   and the all the end points of the outgoing edges
;;   who are in current brancch have a predecessor in current branch
(defun monotone-wami-format-edges-incoming (edge count)
  (let ((current-branch (monotone-wami-branch monotone-wami-current-revision)))
    (if (equal (monotone-wami-branch (cdr edge)) current-branch)
	(monotone-wami-format-edges-test edge count)
      (let ((outgoing (wro-outgoing-edges (cdr edge) monotone-wami-current-graph))
	    (result t))
	(while (and result outgoing)
	  (if (equal (monotone-wami-branch (cdar outgoing)) current-branch)
	      (let ((incoming (wro-incoming-edges (cdar outgoing) monotone-wami-current-graph)))
		;; loop over all incoming, if none of them
		;; have an beginpoint in current branch,
		;; quit and display this edge
		(while (and incoming 
			    (not (equal (monotone-wami-branch (caar incoming)) current-branch)))
		  (setq incoming (cdr incoming)))
		(if (not incoming)
		    (setq result nil))))
	  (setq outgoing (cdr outgoing)))
	(if result
	    ""
	  (monotone-wami-format-edges-test edge count))))))


(defun monotone-wami-test-get-color (node type)
  (let* ((branch (monotone-wami-branch node)) 
	 (color (assoc branch monotone-wami-color-list)))
    (if color
	(cdr color)
      (let ((new-hue (monotone-wami-get-new-color monotone-wami-hue-list)))
	(setq color (cons branch (format "%g,0.5,1.0" new-hue)))
	(setq monotone-wami-color-list (cons color monotone-wami-color-list))
	(setq monotone-wami-hue-list (cons new-hue monotone-wami-hue-list))
	(cdr color)))))

(defun monotone-wami-get-new-color (colors-already-in-use)
  "Returns a new hue value which is as far away as possible
from the hues already in use.

We look at all the gaps in the colors-already-in-use list
and  returns the hue halfway in the greates gap."
  (if (= (length colors-already-in-use) 0)
      0.0
    (if (= (length colors-already-in-use) 1)
	(mod (+ 0.5 (car colors-already-in-use)) 1.0)
      ;; the difficult case
      (let* ((sorted-list (sort (monotone-wami-copy-list colors-already-in-use) '<))
	     (smallest-value (car sorted-list))
	     (working-list (append 
			    (mapcar (lambda (e) (- e smallest-value)) sorted-list)
			    '(1.0)))
	     (last-seen-value 0.0)
	     (base-value 0.0)
	     (largest-difference 0.0))
	(mapc (lambda (e) 
		(if (> (- e last-seen-value) largest-difference)
		    (progn
		      (setq largest-difference (- e last-seen-value))
		      (setq base-value last-seen-value)))
		(setq last-seen-value e))
	      working-list)
	(+ base-value smallest-value (/ largest-difference 2.0))))))
		
	
  

(defun monotone-wami-set-current-revision (current-revision)
  (interactive (list 
		(completing-read "New current revision: " (monotone-status-data 'abbreviations)
				 nil t nil
				 'monotone-wami-revision-history
				 monotone-wami-current-revision)))
  (setq monotone-wami-current-revision current-revision)
  (monotone-wami-regenerate-content))


(defun monotone-wami-copy-list (list)
  (mapcar (lambda (x) x) list))


(defun monotone-wami-append-certificates (revision)
  "Insert certificates of revision at point"
  (interactive)
  (let ((certificates (monotone-certs-for-revision revision)))
    (mapc (lambda (cert) (insert (format "%s: %s\n" (cdr (assoc 'name cert)) (cdr (assoc 'value cert)))))
	  certificates)))


(defun monotone-wami-flatten (l)
  (labels ((monotone-wami-flatten-into-result (lst result)
					      (if (listp lst)
						  (progn 
						    (dolist (x lst)
						      (setq result (monotone-wami-flatten-into-result x result)))
						    result)
						(cons x result))))
    (reverse (monotone-wami-flatten-into-result l nil))))

(defun monotone-wami-remove-list (list-to-remove list)
  (let ((result))
    (mapc (lambda (x) 
	    (if (not (member x list-to-remove)) 
		(progn
		  (setq result (cons x result)))) )
	  list)
    result))

(defun monotone-wami-append-certificate-values-for-ancestors (revision cert-name)
  (let ((fixed-list (monotone-wami-flatten (wro-graph-walk-ancestors
					    (lambda (x) (monotone-cert-values x cert-name)) 
					   revision monotone-wami-full-graph)))
	(complete-list (monotone-wami-flatten (wro-graph-walk-all 
					       (lambda (x) (monotone-cert-values x cert-name))
					       monotone-wami-full-graph))))
;;     (insert (format "%s" fixed-list))
;;     (setq complete-list (monotone-wami-remove-list fixed-list complete-list))
;;     (insert "\n\nNOT FIXED:\n")
;;     (insert (format "%s" complete-list))
))
    

(defun monotone-wami-mouse-recenter-graph  (event)
  "Set new current revision"
  (interactive "e")
  (let* ((image (posn-image (event-end event)))
	(imap  (plist-get (cdr image) :imap))
	(pos   (posn-object-x-y (event-end event))))
    (setq wim-ti image)
    (if (or (not image) (not imap) (not pos))
	(message "Not on image? %s %s %s" image imap pos)
      (while (and imap pos)
	(let ((rule (car imap)))
	  (if (and (> (car pos) (nth 0 rule))
		   (> (cdr pos) (nth 1 rule))
		   (< (car pos) (nth 2 rule))
		   (< (cdr pos) (nth 3 rule)))
	      (progn
		(message "Found it!: %s"
			 (nth 4 rule))
		(setq pos nil)
		(setq monotone-wami-current-revision (nth 4 rule))
		(monotone-wami-regenerate-content)
		))
	  (setq imap (cdr imap))))
      (if pos
	  (message "Didnt find anything at: %s" pos)))))



(defun monotone-process-graph ()
  "Make graph structure from monotone output.
Assume the content of the current buffer is in the format of 
monotone automate graph.  This format consists of one line per node and
each consists of one or more strings separated by spaces.  The first
word on the line is the node and all other words are parent nodes
of this node.

The result of the function is the equivalent graph in the wro-graph datastructure,
and this graph is returned."
  (goto-char (point-min))
  (let ((new-edges (list))
	(new-nodes (list)))
    (while (not (eobp))
      (let ((node (buffer-substring-no-properties (point) (+ (point) 40))))
	(forward-char 40)
	(setq new-nodes (cons node new-nodes))
	(while (looking-at " ")
	  (forward-char 1)
	  (setq new-edges (cons (cons (buffer-substring-no-properties (point) (+ (point) 40)) node) new-edges))
	  (forward-char 40)))
      (forward-char 1))
    ;; The following setq is handy for debugging
    (setq wro-mt-graph (wro-make-graph new-nodes new-edges))
    wro-mt-graph))


(provide 'e-monotone-wami)

