;;; e-monotone-graphs.el -- experimenting with graphs

;; Copyright (C) 2005 Willem Rein Oudshoorn

;; Author: Willem Rein Oudshoorn <woudshoo@xs4all.nl>

;;; Commentary:

;; This is a quick and dirty implementation
;; of some basic graph algorithms that are
;; useful for monotone graphs.
;; that is, the graphs are DAG and 
;; can consist of a reasonably large number
;; of vertices, but the average number of edges
;; coming in or leaving a node is small.  Typically
;; in the range from 1 to 4.
;;
;; Furthermore, we allow multiple edges between two nodes 
;; and keep track of some simple meta data per node.
;;

;;; Definitions:
;;
;; A graph is a list containing four hash tables:
;;
;; (node-table outgoing-edge-table incoming-edge-table edge-table)
;;
;; nodes  - are symbols and or arbitrary lisp objects.
;; edges  - are pairs, containing (begin-node end-node)
;;
;; The keys in the 'node-table' are the nodes
;; the outgoing-edge-table and incoming-edge-table 
;; map nodes to a list of edges
;;
;; the 'edge-table' keeps track of the number of edges
;;
;; so an example is:

;; (#[a -> t, b -> "help", c-> "dag"]        ;; the nodes are a,b and c
;;  #[a -> ((a . b) (a . c))
;;    b -> ((a . c))]
;;  #[b -> ((a . b))
;;    c -> ((a . c))]
;;  #[(a . b) -> 2
;;    (a . c) -> 1])
;;
;;
;; Where edge table contains all edges with multiplicity.


(defvar monotone-dot-cmd "dot" 
  "Name of the dot executable. 
Either just use the name of the executable or provide
a complete path")



;; Utilities
;; sequences and types
;;
;;  We keep a mapping M: id --> CD (complicated datastructure)
;;  plus composition operator  C_id: id x id --> id 
;;  which should cache the behaviour of:  C : CD x CD --> CD
;; 
;;  Assume now that there number of different CD's is limited and
;;  we want to do the composition often.  For this we will cache
;;  the mapping C_id.
;;
;;
;;  Cache of C_id is done by nested hash table or hash table plus property list.
;;  
;;  All of this is cached in the following structure
;;
;;  (      ; list
;;    #[id-1  ->  #[id-2  -> result]]
;;    #[id    ->  CD]
;;    function:  CD x CD -> CD        ; 
;;    function:  CD x CD -> nil,t     ; the equal function
;;    current-max-id                  ; id is now an integer.
;;    (list (cd . id)*)               ; to find the mapping back
;;  )
;;  id's are automatically generated and can be tested by eq.
;;  for now they are integers.

;; (defun wro-id-composition-cache-value (id-1 id-2 cache)
;;   (let ((secondary-cache (gethash id-1 (nth 0 cache))))
;;     (when secondary-cache
;;       (gethash id-2 secondary-cache))))

;; (defun wro-id-compisition-add-to-cache (id-1 id-2 result cache)
;;   (let ((secondary-cache (gethash id-1 (nth 0 cache))))
;;     (unless secondary-cache 
;;       (setf secondary-cache (make-hash-table))
;;       (puthash id-1 secondary-cache (nth 0 cache)))
;;     (puthash id-2 result secondary-cache)))

;; (defun wro-id-cd-cache-value (id cache)
;;   (gethash id (nth 1 cache)))

;; (defun wro-id-composition-value (id-1 id-2 cache)
;;   (let ((result-id (wro-id-composition-cache-value id-1 id-2 cache)))
;;     (unless result-id
;;       (let* ((real-value (funcall (nth 3 cache) (wro-id-cd-cache-value id-1 cache) (wro-id-cd-cache-value id-2 cache))))
;; 	(setf result-id (wro-find-or-create-id-for-cd real-value cache))))
;;     result-id))

;; (defun wro-find-or-create-id-for-cd (real-value cache)
  

	     



	



(defun wro-make-graph (node-list edge-list)
  "Create graph structure from node-list and edge-list.
The resulting graph will have t as a value for all the nodes and
if some edges contain nodes not present in the node list they will
be added to the nodes list."
  (let* ((node-table (make-hash-table :test 'equal))
	(incoming-table (make-hash-table :test 'equal))
	(outgoing-table (make-hash-table :test 'equal))
	(edge-table (make-hash-table :test 'equal))
	(graph (list node-table outgoing-table incoming-table edge-table)))
    ;; file node-list
    (mapc (lambda (n) (puthash n t node-table)) node-list)
    ;; file edge-structure
    (mapc (lambda (e) (wro-add-edge e graph)) edge-list)
    graph))


(defun wro-node-count (graph)
  "Returns the number of nodes in the graph."
  (hash-table-count (nth 0 graph)))

(defun wro-add-edge (edge graph)
  "Add edge to existing graph.
This will add the source and destiniation node to the graph with value t if
they are not present in the node list.   They will not change the value 
of the node if the nodes are already present in the graph."
  (unless (gethash (car edge) (nth 0 graph)) (puthash (car edge) t (nth 0 graph)))
  (unless (gethash (cdr edge) (nth 0 graph)) (puthash (cdr edge) t (nth 0 graph)))
  (wro-add-edge-dont-update-nodes edge graph))

(defun wro-add-edge-dont-update-nodes (edge graph)
  "Add edge but do not update node list.
This is when you know that the nodes are already
in the node list, or update the node list later
when iterating over the nodes."
  (if (gethash edge (nth  3 graph))
      (puthash edge (1+ (gethash edge (nth 3 graph))) (nth 3 graph))
    (puthash edge 1 (nth 3 graph))
    (puthash (car edge) (cons edge (gethash (car edge) (nth 1 graph))) (nth 1 graph))
    (puthash (cdr edge) (cons edge (gethash (cdr edge) (nth 2 graph))) (nth 2 graph))))
  
(defun wro-remove-edge-dont-update-nodes (edge graph &optional all-edges)
  "Remove edge from graph.
This will remove the edge from the graph.  This will not remove any nodes,
but will make sure the incoming and outgoing node lists are kept up to date.
If the flag all-edges is given it will remove all edges between the source
and destination node of edge.  Otherwise decrease the edge count by one."
  (if (and (not all-edges) (gethash edge (nth 3 graph)) (> (gethash edge (nth 3 graph)) 1))
      (puthash edge (1- (gethash edge (nth 3 graph))) (nth 3 graph))
    (let ((incoming (wro-incoming-edges (cdr edge) graph))
	  (outgoing (wro-outgoing-edges (car edge) graph)))
      (puthash (car edge) (remove edge outgoing) (nth 1 graph))
      (puthash (cdr edge) (remove edge incoming) (nth 2 graph))
      (remhash edge (nth 3 graph)))))

(defun wro-remove-edges-for-node-dont-update-nodes (node graph)
  "Remove all edges that are incident to node."
  (let ((incoming (wro-incoming-edges node graph))
	(outgoing (wro-outgoing-edges node graph)))
    (mapc (lambda (edge) (wro-remove-edge-dont-update-nodes edge graph t)) incoming)
    (mapc (lambda (edge) (wro-remove-edge-dont-update-nodes edge graph t)) outgoing)))

      

(defun wro-copy-graph (graph)
  "Make a copy of the graph."
  (list (copy-hash-table (nth 0 graph))
	(copy-hash-table (nth 1 graph))
	(copy-hash-table (nth 2 graph))
	(copy-hash-table (nth 3 graph))))
  
(defun wro-outgoing-edges (node graph)
  "Return edges which start at node"
  (gethash node (nth 1 graph)))

(defun wro-incoming-edges (node graph)
  "Return edges which end at node"
  (gethash node (nth 2 graph)))

(defun wro-graph-contains-edge (edge graph)
  "Return true if the graph contains the edge"
  (gethash edge (nth 3 graph)))


(defun wro-graph-toposorted-nodes (graph)
  "Returns an array containing the nodes toposorted."
  (let ((result (make-vector (wro-node-count graph) nil))
	(nodes-done (make-hash-table :test 'equal))
	(max-index 0)
	(current-index 0))
    (flet ((wro-add-node-to-done-list (node) 
				      (aset result max-index node) 
				      (incf max-index) 
				      (puthash node t nodes-done))
	   (wro-only-ancestors-in-done (node)
				       (let ((result t))
					 (mapc (lambda (edge)
						 (unless (gethash (car edge) nodes-done)
						   (setf result nil)))
					     (wro-incoming-edges node graph))
					 result)))
      ;; First find all the nodes with no ancestors
      ;; this is O(#Nodes)
      (maphash (lambda (node ignored)
		 (when (= 0 (length (wro-incoming-edges node graph)))
		   (wro-add-node-to-done-list node)))
	       (nth 0 graph))
      (when (= 0 max-index)
	(error "Graph given to toposort does not have any roots"))
      
      ;; Now loop over the nodes we have done to add
      ;; new nodes.  Stop if there are no nodes to be processed
      ;; O(#Edges)
      (while (< current-index max-index)
	(mapc (lambda (edge)
		(let ((candidate (cdr edge)))
		  (when (and (not (gethash candidate nodes-done))
			     (wro-only-ancestors-in-done candidate))
		    (wro-add-node-to-done-list candidate))))
	      (wro-outgoing-edges (aref result current-index) graph))
	(incf current-index))
      
      ;; check if we have done all nodes
		
      (unless (= max-index (wro-node-count graph))
	(error "Graph given to toposort contains loops"))
      
      result)))



(defun wro-compactify-edges (predicate graph)
  "Remove all nodes for which predicate returns false.
The edges in the new graph correspond to paths
from between the points in the old graph.
See notes in documentation on why and how this works."
  (let ((nodes-to-be-removed))
    (maphash (lambda (node ignored)
	       (unless (funcall predicate node)
		   (let ((incoming (wro-incoming-edges node graph))
			 (outgoing (wro-outgoing-edges node graph))
			 (todo))
		     ;; remove node from edges hashes
		     (setq nodes-to-be-removed (cons node nodes-to-be-removed))
		     (while incoming
		       (setq todo outgoing)
		       (while todo
			 (wro-add-edge-dont-update-nodes (cons (caar incoming) (cdar todo)) graph)
			 (setq todo (cdr todo)))
		       (setq incoming (cdr incoming)))
		     (wro-remove-edges-for-node-dont-update-nodes node graph)
		     (remhash node (nth 1 graph))
		     (remhash node (nth 2 graph)))))
	     (nth 0 graph))
    ;; Remove nodes from the node hash
    (mapc (lambda (e) (remhash e (nth 0 graph))) nodes-to-be-removed))
  graph)


(defun wro-merge-node-lists (lst-1 lst-2)
  "Assume the list contains strings in ascending order.
return the merged list containing elements of list 1 andd list 2 with no duplicates."
  (let ((result (list)))
    (while (and lst-1 lst-2)
      (let ((compare-result (compare-strings (car lst-1) nil nil
					     (car lst-2) nil nil)))
	(cond
	 ((eq t compare-result)  ;; first elements are equal
	   (setf result (cons (car lst-1) result))
	   (setf lst-1 (cdr lst-1))
	   (setf lst-2 (cdr lst-2)))
	 ((> 0 compare-result)  ;; first element lst-1 < first element lst-2
	  (setf result (cons (car lst-1) result))
	  (setf lst-1 (cdr lst-1)))
	 ((< 0 compare-result)  ;; first element lst-1 > first element lst-2
	    (setf result (cons (car lst-2) result))
	    (setf lst-2 (cdr lst-2))))))
    (setf lst-1 (or lst-1 lst-2))  ;; make lst-1 the remainder list
    (while lst-1
      (setf result (cons (car lst-1) result))
      (setf lst-1 (cdr lst-1)))
    (reverse result)))
    

	

(defun wro-compactify-by-classification (predicate graph)
  "Remove all nodes for which pridicate returns false.
Add virtual nodes representing groups of deleted nodes.
A group of deleted nodes is identified by the collection
of ancestor and descendant nodes in the original graph
for which predicate returns t."

  ;; algorithme
  ;; 1 - toposort node
  ;; 2 - walk from roots to leaves to create the ancestor sets
  ;; 3 - walk from leaves to root to create descendant sets
  ;; 4 - collect nodes together that have the same ancestor and descendent set
  ;; 5 - create nodes for the sets
  (let ((sorted-nodes (wro-graph-toposorted-nodes graph))
	(nr-of-nodes (wro-node-count graph))
	(index 0)
	(predecessor-mapping (make-hash-table :test 'equal)))
    (while (< index nr-of-nodes)
      (let ((node (aref sorted-nodes index))
	    (predecessor-set nil))
	(if (funcall predicate node)
	  ;; node needs to stay:  the set we add contains just of this node
	    (setf predecessor-set (list node))
	  ;; else merge the previous sets together
	  (mapc (lambda (edge)
		  (setf predecessor-set 
			(wro-merge-node-lists predecessor-set 
					      (gethash (car edge) predecessor-mapping))))
		
		(wro-incoming-edges node graph))
	  (puthash node predecessor-set predecessor-mapping))
	(puthash node predecessor-set predecessor-mapping))
      (incf index))

	;;
	;;
))

(defun wro-not-simple-node-p (node graph)
  "Return false if incoming and outgoing degree are 1."
  (or (not (= (length (wro-incoming-edges node graph)) 1))
      (not (= (length (wro-outgoing-edges node graph)) 1))))



(defun wro-neighbourhood-of-node (node count graph)
  "Limits the graph to a neighbourhood of node.
Starting at node and containing at least count nodes.
The nodes table contains t if it is a full node,
or 'partial if it contains edges to other parts
of the graph, not part of the subgraph"
  (let ((nodes (make-hash-table :test 'equal))
	(todo-nodes (list node)))
    (while (and todo-nodes (< (hash-table-count nodes) count))
      (let ((node (car todo-nodes)))
	(unless (gethash node nodes)
	  (puthash node t nodes)
	  (setq todo-nodes (append todo-nodes 
				   (mapcar 'car (wro-incoming-edges node graph))
				   (mapcar 'cdr (wro-outgoing-edges node graph)))))
	(setq todo-nodes (cdr todo-nodes))))
    ;; Improve check on partial
    (mapc (lambda (node) (unless (gethash node nodes) 
			   (puthash node 'partial nodes))) todo-nodes)

    ;; Reduce the graph to the desired size
    (let* ((new-graph (wro-copy-graph graph))
	   (new-nodes (nth 0 new-graph)))
      ;; replace nodes
      (maphash (lambda (node value)
		 (puthash node value new-nodes)) nodes)
      
      (wro-compactify-edges (lambda (n) (gethash n nodes)) new-graph))))


(defun wro-subgraph (nodes graph)
  "Limits the graph"
  (let ((new-graph (list nodes 
			 (make-hash-table :test 'equal)
			 (make-hash-table :test 'equal)
			 (make-hash-table :test 'equal))))
    (maphash (lambda (node ignored)
	       (mapc (lambda (edge)
		       (if (gethash (cdr edge) nodes)
			   (progn
			     (wro-add-edge-dont-update-nodes edge new-graph)
			     (puthash edge (gethash edge (nth 3 graph)) (nth 3 new-graph)))))
		     (wro-outgoing-edges node graph)))
	     nodes)
    new-graph))
 
(defun wro-remove-duplicate-edges (graph)
  "Remove all duplicate edges"
  ;; Hope replacing values in hash table is ok
  (maphash (lambda (key value) (puthash key 1 (nth 3 graph))) (nth 3 graph))
  graph)


;; Exporting

(defun wro-graph-monotone-representation (graph buffer)
  (with-current-buffer (get-buffer-create buffer)
    (maphash (lambda (node ignored)
	       (insert (format "%s" node))
	       (mapc (lambda (edge) (insert (format " %s" (car edge)))) 
		     (wro-incoming-edges node graph))
	       (insert "\n"))
	     (nth 0 graph))))

;; Hm hm, how to use extra information

(defun wro-graph-dot-representation (graph buffer &optional node-function edge-function)
  (with-current-buffer (get-buffer-create buffer)
    (insert "digraph wro {\n")
    (insert "\tnode [shape=plaintext,height=0,width=0,fontname=Helvetica];\n")
    (insert "\tsize = \"10,10\";\n")
    (insert "\trankdir = BT;\n")
    (wro-insert-nodes node-function graph)
    (wro-insert-edges edge-function graph)
    (insert "\n}")))

(defun wro-insert-edges (edge-function graph)
  (maphash (lambda (edge count)
	     (insert (if edge-function (funcall edge-function edge count)
		       (format "%S -> %S;\n" (car edge) (cdr edge))))) 
	   (nth 3 graph)))


(defun wro-insert-nodes (node-function graph)
  (maphash (lambda (node type)
	     (insert (if node-function (funcall node-function node type)
		       (format "%S;\n" node))))
	   (nth 0 graph)))


(defun wro-graph-image-representation (graph &optional node-function edge-function)
  (let ((out-file-name (make-temp-file "wro-graph-image"))
	(in-file-name (make-temp-file "wro-graph-source"))
	(image-map-file-name (make-temp-file "wro-graph-imap")))
    (save-excursion
      (with-temp-file in-file-name
	(wro-graph-dot-representation graph (current-buffer) node-function edge-function))
      (call-process monotone-dot-cmd in-file-name nil nil "-Timap" "-o" image-map-file-name in-file-name)
      (call-process monotone-dot-cmd in-file-name nil nil "-Tpng" "-o" out-file-name in-file-name)
      (create-image out-file-name 'png nil :imap (wro-read-imap image-map-file-name)))))

(defun wro-read-imap (file-name)
  (interactive)
  (let ((result))
    (with-temp-buffer
      (insert-file file-name)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "rect")
	    (progn
	      (if (re-search-forward 
		   "rect \\(\[0-9a-f\]*\\) \\(\[0-9\]*\\),\\(\[0-9\]*\\) \\(\[0-9\]*\\),\\(\[0-9\]*\\)$" nil t)
		  (setq result (cons (list (string-to-number (match-string 2))
					   (string-to-number (match-string 3))
					   (string-to-number (match-string 4))
					   (string-to-number (match-string 5))
					   (match-string 1))
				     result)))))
	(forward-line 1)))
    result))

(defun wro-graph-dot-representation-for-modot (graph)
  (interactive)
  (switch-to-buffer "modot-graph.dot")
  (maphash (lambda (node ignored)
	     (insert (format "%s" node))
	     (mapc (lambda (edge)
		     (insert (format " %s" (car edge))))
		   (wro-incoming-edges node graph))
	     (insert "\n"))
	   (nth 0 graph)))

(defun wro-check-graph (str graph)
  "Check the graph structure for inconsistencies.
Inconsistencies are:
- Nodes that occur in edges but not in de node list.
- Edges occuring in the incoming/outgoing list and not in the edge list
- Edges occuring in the edge-list and not in the incomming/outgoing list"
  (interactive)
  (message "Checking: %s" str)
  (maphash (lambda (edge count)
	     (if (= count 0)
		 (message "Zero count edge: %s" edge))
	     (if (not (gethash (car edge) (nth 0 graph)))
		 (message "node %s of edge %s not known"
			  (car edge) edge))
	     (if (not (gethash (cdr edge) (nth 0 graph)))
		 (message "node %s of edge %s not known"
			  (cdr edge) edge))
	     (if (not (member edge (wro-incoming-edges (cdr edge) graph)))
		 (message "edge %s not know as incoming for node %s"
			  edge (cdr edge)))
	     (if (not (member edge (wro-outgoing-edges (car edge) graph)))
		 (message "edge %s not know as outgoing for node %s"
			  edge (car edge))))
	   (nth 3 graph))
  (maphash (lambda (node ignored)
	     (mapc (lambda (e) 
		     (if (not (equal (car e) node))
			 (message "nodes %s outgoing list contains illegal edge %s"
				  node e))
		     (if (not (gethash e (nth 3 graph)))
			 (message "nodes %s outgoing list edge %s not known"
				  node e))) 
		   (wro-outgoing-edges node graph)))
	   (nth 0 graph))
	     
  (maphash (lambda (node ignored)
	     (mapc (lambda (e) 
		     (if (not (equal (cdr e) node))
			 (message "nodes %s incoming list contains illegal edge %s"
				  node e))
		     (if (not (gethash e (nth 3 graph)))
			 (message "nodes %s incoming list edge %s not known"
				  node e))) 
		   (wro-incoming-edges node graph)))
	   (nth 0 graph))
  (message "Done checking: %s" str))
	     
		 

(defun wro-graph-connected-p (graph)
  "Check if the graph is connected"
  (interactive))
  

;;; iterating functions

(defun wro-graph-walk-ancestors (f node graph)
  "Iterate over all ancestors and apply f on the nodes walked.
If f returns non nil the result is put in a list which is returned."
  (interactive)
  (let ((todo (list node))
	(nodes-seen (make-hash-table :test 'equal))
	(result))
    (puthash node t nodes-seen)
    (while todo
      (let ((f-result (funcall f (car todo)))
	    (more-nodes (wro-incoming-edges (car todo) graph)))
	(if f-result
	    (setq result (cons f-result result)))
	;; pop node just evalulated from todo list
	(setq todo (cdr todo))
	;; add new nodes to todo list
	(while more-nodes
	  (if (not (gethash (caar more-nodes) nodes-seen))
	      (progn
		(puthash (caar more-nodes) t nodes-seen)
		(setq todo (cons (caar more-nodes) todo))))
	  (setq more-nodes (cdr more-nodes)))))
    result))

(defun wro-graph-walk-all (f graph)
  (let ((result))
    (maphash (lambda (k v) (let ((f-result (funcall f k)))
			     (if f-result 
				 (setq result (cons f-result result)))))
	     (car graph))
    result))

(provide 'e-monotone-graphs)
