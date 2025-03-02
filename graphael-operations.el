;;; graphael-operations.el --- Higher-level graph operations -*- lexical-binding: t; -*-

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
;; Operations for graph manipulation - cloning, merging, subgraphs.

;;; Code:

(require 'graphael-core)

(cl-defmethod graph-clone ((graph graph))
  "Create and return a complete copy of GRAPH."
  (let ((new-graph (make-instance 'graph)))
    (with-graph-transaction (graph)
      ;; First copy all nodes
      (maphash (lambda (_id node)
                 (let ((new-node (make-instance 'graph-node
                                   :id (node-id node)
                                   :label (when (slot-boundp node 'label)
                                            (node-label node))
                                   :data (copy-tree (node-data node)))))
                   (graph-node-add new-graph :node new-node)))
        (graph-nodes graph))

      ;; Then copy all edges
      (maphash (lambda (_id edge)
                 (graph-edge-add new-graph
                   :id (edge-id edge)
                   :from (edge-from edge)
                   :to (edge-to edge)
                   :weight (edge-weight edge)
                   :label (edge-label edge)
                   :data (copy-tree (edge-data edge))))
        (graph-edges graph)))
    new-graph))

(cl-defmethod graph-merge ((graph1 graph) (graph2 graph) &optional modify-in-place)
  "Merge GRAPH1 and GRAPH2, returning a new merged graph.
When MODIFY-IN-PLACE is non-nil, modify GRAPH1 directly instead.
For nodes with the same ID, the node from GRAPH1 is preserved."
  (let ((result (if modify-in-place
                  graph1
                  (graph-clone graph1))))
    (with-graph-transaction (result)
      ;; Add all nodes from graph2
      (maphash (lambda (id node)
                 (unless (graph-node-p result id)
                   (let ((new-node (make-instance 'graph-node
                                     :id (node-id node)
                                     :label (when (slot-boundp node 'label)
                                              (node-label node))
                                     :data (copy-tree (node-data node)))))
                     (graph-node-add result :node new-node))))
        (graph-nodes graph2))

      ;; Add all edges from graph2
      (maphash (lambda (_id edge)
                 (unless (graph-edge-p result (edge-id edge))
                   (let ((from-id (edge-from edge))
                         (to-id (edge-to edge)))
                     ;; Ensure both nodes exist in result graph
                     (when (and (graph-node-p result from-id)
                             (graph-node-p result to-id))
                       (graph-edge-add result
                         :id (edge-id edge)
                         :from from-id
                         :to to-id
                         :weight (edge-weight edge)
                         :label (edge-label edge)
                         :data (copy-tree (edge-data edge)))))))
        (graph-edges graph2)))
    result))

(cl-defmethod graph-subgraph ((graph graph) node-filter &optional include-connected)
  "Create a new graph containing only nodes matching NODE-FILTER.
NODE-FILTER can be:
- A function that takes a node and returns non-nil if it should be included
- A list of node IDs (strings) to include
- A list of node objects to include
If INCLUDE-CONNECTED is non-nil, also include all nodes directly connected
to matching nodes."
  (let ((new-graph (make-instance 'graph))
        (node-ids (cond
                    ;; Function filter
                    ((functionp node-filter)
                      (let (ids)
                        (maphash (lambda (id node)
                                   (when (funcall node-filter node)
                                     (push id ids)))
                          (graph-nodes graph))
                        ids))

                    ;; List of nodes/IDs
                    ((listp node-filter)
                      (mapcar (lambda (node-or-id)
                                (if (cl-typep node-or-id 'graph-node)
                                  (node-id node-or-id)
                                  node-or-id))
                        node-filter))

                    ;; Invalid filter type
                    (t
                      (error "Invalid node-filter: must be a function or list of nodes/IDs")))))

    ;; Add connected nodes if requested
    (when include-connected
      (let ((additional-ids nil))
        (dolist (id node-ids)
          (dolist (neighbor (graph-get-neighbors graph id))
            (push (node-id neighbor) additional-ids))
          (dolist (neighbor (graph-get-neighbors graph id t))
            (push (node-id neighbor) additional-ids)))
        (setq node-ids (cl-union node-ids additional-ids :test #'equal))))

    ;; Add nodes to new graph
    (dolist (id node-ids)
      (let ((node (graph-node-get graph id)))
        (when node
          (graph-node-add new-graph
            :id (node-id node)
            :label (node-label node)
            :data (copy-tree (node-data node))))))

    ;; Add edges that connect included nodes
    (maphash (lambda (_id edge)
               (when (and (member (edge-from edge) node-ids)
                       (member (edge-to edge) node-ids))
                 (graph-edge-add new-graph
                   :id (edge-id edge)
                   :from (edge-from edge)
                   :to (edge-to edge)
                   :weight (edge-weight edge)
                   :label (edge-label edge)
                   :data (copy-tree (edge-data edge)))))
      (graph-edges graph))
    new-graph))

(cl-defmethod graph-difference ((graph1 graph) (graph2 graph))
  "Subtract GRAPH2 from GRAPH1, returning a new graph.
The new graph contains all nodes from GRAPH1 that are not in GRAPH2,
and all edges between those nodes from GRAPH1."
  (let ((g2-node-ids (hash-table-keys (graph-nodes graph2))))
    (graph-subgraph graph1
      (lambda (node)
        (not (member (node-id node) g2-node-ids))))))

(cl-defmethod graph-intersect ((graph1 graph) (graph2 graph))
  "Create a new graph containing only nodes present in both GRAPH1 and GRAPH2.
The new graph contains nodes with IDs present in both input graphs,
and edges between these nodes that exist in GRAPH1."
  (let ((g2-node-ids (hash-table-keys (graph-nodes graph2))))
    (graph-subgraph graph1
      (lambda (node)
        (member (node-id node) g2-node-ids)))))

(cl-defgeneric graph-add-path (graph nodes)
  "Add a path to GRAPH connecting all NODES in sequence.
NODES can be a sequence of node IDs or node objects.
Return a list of the created edges.")

(cl-defmethod graph-add-path ((graph graph) (nodes sequence))
  "Add a path to GRAPH connecting all NODES in sequence.
NODES should be a sequence of node UUID strings or node objects.
Return a list of the created edges."
  (let ((node-ids (seq-map (lambda (n)
                             (if (cl-typep n 'graph-node)
                               (node-id n)
                               n))
                    nodes)))
    (unless (>= (length node-ids) 2)
      (error "Path must contain at least two nodes"))

    (let (edges)
      (cl-loop for (from-id to-id) on node-ids
        by #'cdr
        while to-id
        do
        (unless (graph-node-p graph from-id)
          (error "Node %s does not exist" from-id))
        (unless (graph-node-p graph to-id)
          (error "Node %s does not exist" to-id))
        (push (graph-edge-add graph :from from-id :to to-id) edges))
      (nreverse edges))))

;;; Graph Analysis and Statistics

(cl-defstruct graph-stats-result
  nodes
  edges
  density
  avg-edge-weight
  size-kb)

(cl-defmethod graph-stats ((graph graph))
  "Return statistics about GRAPH as a struct."
  (let* ((nodes (graph-nodes graph))
         (edges (graph-edges graph))
         (edge-weights (mapcar #'edge-weight (hash-table-values edges)))
         (total-weight (apply #'+ edge-weights)))
    (make-graph-stats-result
      :nodes (hash-table-count nodes)
      :edges (hash-table-count edges)
      :density (if (> (hash-table-count nodes) 1)
                 (/ (float (hash-table-count edges))
                   (* (hash-table-count nodes)
                     (1- (hash-table-count nodes))))
                 0.0)
      :avg-edge-weight (if edge-weights
                         (/ total-weight (length edge-weights))
                         0.0)
      :size-kb (/ (+ (hash-table-size nodes)
                    (hash-table-size edges)
                    (hash-table-size (graph-outgoing-edges graph))
                    (hash-table-size (graph-incoming-edges graph)))
                 1024.0))))

(cl-defmethod graph-print-stats ((graph graph))
  "Print statistics about GRAPH."
  (let ((stats (graph-stats graph)))
    (princ (format "Graph Statistics:
 Nodes: %d
 Edges: %d
 Density: %.3f
 Avg Edge Weight: %.2f
 Memory Usage: %.2f KB\n"
             (graph-stats-result-nodes stats)
             (graph-stats-result-edges stats)
             (graph-stats-result-density stats)
             (graph-stats-result-avg-edge-weight stats)
             (graph-stats-result-size-kb stats)))))

(provide 'graphael-operations)
;;; graphael-operations.el ends here
