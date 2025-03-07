;;; graphael-core.el --- Core graph data structure -*- lexical-binding: t; -*-

;; Author: Billy Lamberta
;; URL: https://github.com/lamberta/graphael
;; Package-Requires: ((emacs "26.2"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Core classes and functions for graph data structures.

;;; Code:

(require 'eieio)
(require 'org-id)
(require 'seq)

;;; Core Classes
;;;; Node Class

(defclass graph-node ()
  ((id
     :initarg :id
     :reader node-id
     :initform (org-id-uuid)
     :type string
     :documentation "UUID string uniquely identifying the node")
    (label
      :initarg :label
      :accessor node-label
      :initform nil
      :type (or null string)
      :documentation "Optional descriptive label for the node")
    (attrs
      :initarg :attrs
      :accessor node-attrs
      :initform '()
      :type list
      :documentation "Optional user-defined plist for node metadata"))
  :documentation "A node in the graph, identified by UUID.")

(cl-defmethod initialize-instance :after ((node graph-node) _slots)
  "Validate NODE's UUID after initialization."
  (unless (graph-uuid-p (node-id node))
    (error "Invalid UUID: %s" (node-id node))))

(cl-defmethod cl-print-object ((node graph-node) stream)
  "Print a string representation of NODE to STREAM."
  (let ((label (when (slot-boundp node 'label)
                 (node-label node))))
    (if label
      (princ (format "#<graph-node %s \"%s\">" (node-id node) label) stream)
      (princ (format "#<graph-node %s>" (node-id node)) stream))))

;;;; Edge Class

(defclass graph-edge ()
  ((id
     :initarg :id
     :reader edge-id
     :initform (org-id-uuid)
     :type string
     :documentation "UUID string uniquely identifying the edge")
    (from
      :initarg :from
      :reader edge-from
      :type string
      :documentation "Source node UUID for this directed edge")
    (to
      :initarg :to
      :reader edge-to
      :type string
      :documentation "Target node UUID for this directed edge")
    (weight
      :initarg :weight
      :accessor edge-weight
      :initform 1.0
      :type number
      :documentation "Optional edge weight, defaulting to 1.0")
    (label
      :initarg :label
      :accessor edge-label
      :initform nil
      :type (or null string)
      :documentation "Optional descriptive label for the edge")
    (attrs
      :initarg :attrs
      :accessor edge-attrs
      :initform '()
      :type list
      :documentation "Optional user-defined plist for edge metadata"))
  :documentation "A directed edge connecting two nodes in the graph.")

(cl-defmethod initialize-instance :after ((edge graph-edge) _slots)
  "Validate EDGE's parameters after initialization."
  (unless (graph-uuid-p (edge-id edge))
    (error "Invalid UUID provided: %s" (edge-id edge)))
  (let ((from (edge-from edge))
        (to (edge-to edge))
        (weight (edge-weight edge)))
    (when (and from to (string= from to))
      (error "Self-loops are not allowed"))
    (unless (and (numberp weight) (>= weight 0))
      (error "Edge weight must be a non-negative number"))))

(cl-defmethod cl-print-object ((edge graph-edge) stream)
  "Print a string representation of EDGE to STREAM."
  (let ((label (when (slot-boundp edge 'label)
                 (edge-label edge))))
    (if label
      (princ (format "#<graph-edge %s %s→%s \"%s\">"
               (edge-id edge)
               (edge-from edge)
               (edge-to edge)
               label) stream)
      (princ (format "#<graph-edge %s %s→%s>"
               (edge-id edge)
               (edge-from edge)
               (edge-to edge)) stream))))

(cl-defmethod (setf edge-weight) (new-weight (edge graph-edge))
  "Set the weight of EDGE to NEW-WEIGHT, which must be non-negative."
  (unless (and (numberp new-weight) (>= new-weight 0))
    (error "Edge weight must be a non-negative number"))
  (setf (slot-value edge 'weight) new-weight))

;;;; Graph Class

(defclass graph ()
  ((nodes
     :initform (make-hash-table :test 'equal)
     :accessor graph-nodes
     :type hash-table
     :documentation "Maps node UUIDs to node objects")
    (edges
      :initform (make-hash-table :test 'equal)
      :accessor graph-edges
      :type hash-table
      :documentation "Maps edge UUIDs to edge objects")
    (outgoing-edges
      :initform (make-hash-table :test 'equal)
      :accessor graph-outgoing-edges
      :type hash-table
      :documentation "Maps node UUIDs to vectors of outgoing edge IDs")
    (incoming-edges
      :initform (make-hash-table :test 'equal)
      :accessor graph-incoming-edges
      :type hash-table
      :documentation "Maps node UUIDs to vectors of incoming edge IDs")
    (mutex
      :initform (make-mutex "graph-mutex")
      :accessor graph-mutex
      :documentation "Mutex for thread-safe operations"))
  :documentation "Thread-safe directed graph with UUID-identified nodes and edges.")

(cl-defmethod cl-print-object ((g graph) stream)
  "Print a string representation of graph G to STREAM."
  (let ((nodes (hash-table-count (graph-nodes g)))
        (edges (hash-table-count (graph-edges g))))
    (princ (format "#<graph %d nodes %d edges>" nodes edges) stream)))

;;; Utilities
;;;; Transaction and Validation

(cl-defmacro with-graph-transaction ((graph) &rest body)
  "Execute BODY atomically within GRAPH context.
The body is executed with a mutex lock to ensure thread-safety
of all graph operations. This prevents concurrent modifications
from causing data inconsistencies."
  (declare (indent 1))
  `(with-mutex (graph-mutex ,graph)
     ,@body))

(defun graph-uuid-p (id)
  "Return `t' if ID is a properly formatted UUID string."
  (and (stringp id)
    (not (null (string-match-p
                 "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
                 id)))))

;;;; Property Functions

(defun graph-node-attr-put (node property value)
  "Set PROPERTY to VALUE on NODE.
Modifies the node's `attrs' plist."
  (let ((attrs (or (node-attrs node) ())))
    (setf (node-attrs node) (plist-put attrs property value))))

(defun graph-node-attr-get (node property &optional default)
  "Get value of PROPERTY from NODE `attrs', or DEFAULT if not found."
  (or (plist-get (node-attrs node) property) default))

(defun graph-edge-attr-put (edge property value)
  "Set PROPERTY to VALUE on EDGE.
Modifies the edge's `attrs' plist."
  (let ((attrs (or (edge-attrs edge) ())))
    (setf (edge-attrs edge) (plist-put attrs property value))))

(defun graph-edge-attr-get (edge property &optional default)
  "Get value of PROPERTY from EDGE `attrs', or DEFAULT if not found."
  (or (plist-get (edge-attrs edge) property) default))

;;; Basic Graph API
;;;; Common Queries

(cl-defmethod graph-node-count ((graph graph))
  "Return the number of nodes in GRAPH."
  (hash-table-count (graph-nodes graph)))

(cl-defmethod graph-edge-count ((graph graph))
  "Return the number of edges in GRAPH."
  (hash-table-count (graph-edges graph)))

;;;; Predicates

(cl-defmethod graph-empty-p ((graph graph))
  "Return t if GRAPH has no nodes, nil otherwise."
  (zerop (hash-table-count (graph-nodes graph))))

(cl-defgeneric graph-node-p (graph node-or-id)
  "Return t if GRAPH contains the node identified by NODE-OR-ID.")

(cl-defmethod graph-node-p ((graph graph) (id string))
  "Return t if GRAPH contains node with ID string."
  (and (graph-uuid-p id)
    (gethash id (graph-nodes graph))))

(cl-defmethod graph-node-p ((graph graph) (node graph-node))
  "Return t if GRAPH contains NODE."
  (graph-node-p graph (node-id node)))

(cl-defgeneric graph-edge-p (graph edge-or-id)
  "Return t if GRAPH contains the edge identified by EDGE-OR-ID.")

(cl-defmethod graph-edge-p ((graph graph) (edge-id string))
  "Return t if GRAPH contains edge with EDGE-ID."
  (and (graph-uuid-p edge-id)
    (gethash edge-id (graph-edges graph))))

(cl-defmethod graph-edge-p ((graph graph) (edge graph-edge))
  "Return t if GRAPH contains EDGE."
  (graph-edge-p graph (edge-id edge)))

;;;; Retrieval

(cl-defmethod graph-node-get ((graph graph) (id string))
  "Return node by ID from GRAPH, or nil if not found."
  (when (graph-node-p graph id)
    (gethash id (graph-nodes graph))))

(cl-defmethod graph-edge-get ((graph graph) (id string))
  "Return edge by ID from GRAPH, or nil if not found."
  (when (graph-edge-p graph id)
    (gethash id (graph-edges graph))))

;;; Node Operations

(cl-defgeneric graph-node-add (graph &key id node label attrs)
  "Add a node to GRAPH specified by keywords.
If both ID and NODE are provided, an error is signaled.
If neither is provided, a new node is created.
Optional LABEL provides descriptive string for the node.
Optional ATTRS can be any metadata to associate with the node.
Returns the added node.")

(cl-defmethod graph-node-add ((graph graph) &key id node label attrs)
  "Add a node to GRAPH specified by keywords.
If both ID and NODE are provided, an error is signaled.
If neither is provided, a new node is created.
Optional LABEL provides descriptive string for the node.
Optional ATTRS can be any metadata to associate with the node.
Returns the added node."
  (let* ((n (cond
              ((and id node)
                (error "Cannot provide both :ID and :NODE keyword arguments"))
              (node
                (unless (cl-typep node 'graph-node)
                  (error "Invalid graph-node instance provided: %s" node))
                (when (graph-node-p graph (node-id node))
                  (error "Node already exists in graph: %s" (node-id node)))
                node)
              (id
                (unless (graph-uuid-p id)
                  (error "Invalid UUID provided: %s" id))
                (when (graph-node-p graph id)
                  (error "Node already exists in graph: %s" id))
                (make-instance 'graph-node :id id))
              (t
                (make-instance 'graph-node))))
         (node-id (node-id n)))

    ;; Set optional properties
    (when label (setf (node-label n) label))
    (when attrs (setf (node-attrs n) attrs))

    (with-graph-transaction (graph)
      (puthash node-id n (graph-nodes graph))
      (puthash node-id (make-vector 0 nil) (graph-outgoing-edges graph))
      (puthash node-id (make-vector 0 nil) (graph-incoming-edges graph)))
    n))

(cl-defgeneric graph-node-remove (graph node-or-id)
  "Remove node from GRAPH. Return the removed node.")

(cl-defmethod graph-node-remove ((graph graph) (node-id string))
  "Remove node with NODE-ID and its edges from GRAPH.
Return the removed node."
  (unless (graph-uuid-p node-id)
    (error "Invalid UUID: %s" node-id))
  (unless (graph-node-p graph node-id)
    (error "Node does not exist in graph: %s" node-id))

  (let* ((node (graph-node-get graph node-id))
         (outgoing (seq-into (gethash node-id (graph-outgoing-edges graph)) 'list))
         (incoming (seq-into (gethash node-id (graph-incoming-edges graph)) 'list))
         (all-edges (append outgoing incoming))
         (edge-updates (mapcar (lambda (edge-id)
                                 (cons edge-id (graph-edge-get graph edge-id)))
                         all-edges)))

    (with-graph-transaction (graph)
      ;; Remove all edges first
      (dolist (edge-id all-edges)
        (remhash edge-id (graph-edges graph)))

      ;; Update edge references in connected nodes
      (dolist (edge-pair edge-updates)
        (let* ((edge-id (car edge-pair))
               (edge (cdr edge-pair))
               (to-id (edge-to edge))
               (from-id (edge-from edge)))

          ;; Update incoming edges for target nodes of outgoing edges
          (when (member edge-id outgoing)
            (puthash to-id
              (vconcat (seq-remove (lambda (e) (string= e edge-id))
                         (gethash to-id (graph-incoming-edges graph))))
              (graph-incoming-edges graph)))

          ;; Update outgoing edges for source nodes of incoming edges
          (when (member edge-id incoming)
            (puthash from-id
              (vconcat (seq-remove (lambda (e) (string= e edge-id))
                         (gethash from-id (graph-outgoing-edges graph))))
              (graph-outgoing-edges graph)))))

      ;; Finally remove the node itself
      (remhash node-id (graph-nodes graph))
      (remhash node-id (graph-outgoing-edges graph))
      (remhash node-id (graph-incoming-edges graph)))
    node))

(cl-defmethod graph-node-remove ((graph graph) (node graph-node))
  "Remove NODE and its edges from GRAPH.
Return the removed node."
  (graph-node-remove graph (node-id node)))

(cl-defgeneric graph-neighbors (graph node-or-id &optional incoming)
  "Return neighboring nodes connected to NODE in GRAPH.
If INCOMING is non-nil, get predecessor nodes instead of successor nodes.")

(cl-defmethod graph-neighbors ((graph graph) (node-id string) &optional incoming)
  "Return neighboring nodes connected to NODE-ID in GRAPH.
If INCOMING is non-nil, get predecessor nodes instead of successor nodes."
  (unless (graph-uuid-p node-id)
    (error "Invalid UUID: %s" node-id))
  (unless (graph-node-p graph node-id)
    (error "Node %s does not exist" node-id))

  (let ((edges (graph-node-edges graph node-id incoming)))
    (mapcar (lambda (edge)
              (graph-node-get graph
                (if incoming
                  (edge-from edge)
                  (edge-to edge))))
      edges)))

(cl-defmethod graph-neighbors ((graph graph) (node graph-node) &optional incoming)
  "Return neighboring nodes connected to NODE in GRAPH.
If INCOMING is non-nil, get predecessor nodes instead of successor nodes."
  (graph-neighbors graph (node-id node) incoming))

;;; Edge Operations
;;;; Edge Reference Management

(defun graph--edge-ref-add (graph edge)
  "Add EDGE's reference to GRAPH's edge reference vectors."
  (let ((edge-id (edge-id edge))
        (from-id (edge-from edge))
        (to-id (edge-to edge)))
    (puthash from-id
      (vconcat (gethash from-id (graph-outgoing-edges graph))
        (vector edge-id))
      (graph-outgoing-edges graph))
    (puthash to-id
      (vconcat (gethash to-id (graph-incoming-edges graph))
        (vector edge-id))
      (graph-incoming-edges graph))))

(defun graph--edge-ref-remove (graph edge)
  "Remove EDGE's reference from GRAPH's edge reference vectors."
  (let ((edge-id (edge-id edge))
        (from-id (edge-from edge))
        (to-id (edge-to edge)))
    (puthash from-id
      (vconcat (seq-remove (lambda (e) (string= e edge-id))
                 (gethash from-id (graph-outgoing-edges graph))))
      (graph-outgoing-edges graph))
    (puthash to-id
      (vconcat (seq-remove (lambda (e) (string= e edge-id))
                 (gethash to-id (graph-incoming-edges graph))))
      (graph-incoming-edges graph))))

;;;; Edge Creation and Removal

(cl-defmethod graph-edge-add ((graph graph) &key id edge from to weight label attrs)
  "Add an edge to GRAPH specified by keywords.
Either :EDGE or both :FROM and :TO must be provided.
If :EDGE is provided, add that edge object directly.
If :ID is provided, use it as the edge UUID, otherwise generate one.
:FROM and :TO can be node objects or node UUID strings.
:WEIGHT sets edge weight (defaults to 1.0).
:LABEL provides descriptive string for the edge.
:ATTRS can be any metadata to associate with the edge.
Return the added edge."
  (let* ((e (cond
              ((and edge (or from to))
                (error "Cannot provide both :EDGE and :FROM/:TO keywords"))

              (edge
                (unless (cl-typep edge 'graph-edge)
                  (error "Invalid graph-edge instance provided: %s" edge))
                (when (graph-edge-p graph (edge-id edge))
                  (error "Edge already exists in graph: %s" (edge-id edge)))
                edge)

              ((and from to)
                (let ((from-id (if (cl-typep from 'graph-node)
                                 (node-id from)
                                 from))
                       (to-id (if (cl-typep to 'graph-node)
                                (node-id to)
                                to)))
                  (unless (graph-uuid-p from-id)
                    (error "Invalid FROM UUID: %s" from-id))
                  (unless (graph-uuid-p to-id)
                    (error "Invalid TO UUID: %s" to-id))
                  (unless (graph-node-p graph from-id)
                    (error "Source node does not exist in graph: %s" from-id))
                  (unless (graph-node-p graph to-id)
                    (error "Target node does not exist in graph: %s" to-id))
                  (when (string= from-id to-id)
                    (error "Self-loops are not allowed"))

                  (make-instance 'graph-edge
                    :id (or id (org-id-uuid))
                    :from from-id
                    :to to-id)))

              (t
                (error "Must provide either :EDGE or both :FROM and :TO"))))
         (edge-id (edge-id e)))

    ;; Set optional properties
    (when weight (setf (edge-weight e) weight))
    (when label (setf (edge-label e) label))
    (when attrs (setf (edge-attrs e) attrs))

    ;; Add to graph
    (with-graph-transaction (graph)
      (puthash edge-id e (graph-edges graph))
      (graph--edge-ref-add graph e))
    e))

(cl-defgeneric graph-edge-remove (graph edge-or-id)
  "Remove edge from GRAPH. Return the removed edge.")

(cl-defmethod graph-edge-remove ((graph graph) (edge-id string))
  "Remove edge with EDGE-ID from GRAPH. Return the removed edge."
  (unless (graph-uuid-p edge-id)
    (error "Invalid UUID: %s" edge-id))

  (let ((edge (gethash edge-id (graph-edges graph))))
    (unless edge
      (error "Edge does not exist in graph: %s" edge-id))
    (with-graph-transaction (graph)
      (graph--edge-ref-remove graph edge)
      (remhash edge-id (graph-edges graph)))
    edge))

(cl-defmethod graph-edge-remove ((graph graph) (edge graph-edge))
  "Remove EDGE from GRAPH. Return the removed edge."
  (graph-edge-remove graph (edge-id edge)))

(cl-defgeneric graph-node-edges (graph node-or-id &optional incoming)
  "Return edges connected to NODE-OR-ID in GRAPH.
If INCOMING is non-nil, get incoming edges instead of outgoing.")

(cl-defmethod graph-node-edges ((graph graph) node-id &optional incoming)
  "Return edges connected to NODE-ID in GRAPH.
If INCOMING is non-nil, get incoming edges instead of outgoing."
  (unless (graph-uuid-p node-id)
    (error "Invalid UUID: %s" node-id))
  (unless (graph-node-p graph node-id)
    (error "Node %s does not exist" node-id))

  (let ((edge-ids (gethash node-id (if incoming
                                     (graph-incoming-edges graph)
                                     (graph-outgoing-edges graph)))))
    (mapcar (lambda (edge-id)
              (gethash edge-id (graph-edges graph)))
      (seq-into edge-ids 'list))))

(cl-defmethod graph-node-edges ((graph graph) (node graph-node) &optional incoming)
  "Return edges connected to NODE in GRAPH.
If INCOMING is non-nil, get incoming edges instead of outgoing."
  (graph-node-edges graph (node-id node) incoming))

(provide 'graphael-core)
;;; graphael-core.el ends here
