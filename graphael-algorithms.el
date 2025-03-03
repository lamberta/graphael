;;; graphael-algorithms.el --- Algorithms for graph data structures -*- lexical-binding: t; -*-

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
;; Algorithms for traversing and analyzing graphs - pathfinding, cycle detection,
;; and topological sorting.

;;; Code:

(require 'graphael-core)
(require 'cl-lib)

;;; Graph Traversal

(cl-defmethod graph-cycle-p ((graph graph))
  "Return t if GRAPH contains a cycle.
Uses depth-first search with visited and path tracking to detect cycles."
  (let ((visited (make-hash-table :test 'equal))
        (path (make-hash-table :test 'equal)))
    (cl-labels ((visit (node)
                  (puthash node t visited)
                  (puthash node t path)
                  (prog1
                    (cl-some (lambda (neighbor)
                               (or (gethash (node-id neighbor) path)
                                 (and (not (gethash (node-id neighbor) visited))
                                   (visit (node-id neighbor)))))
                      (graph-neighbors graph node))
                    (remhash node path))))
      (cl-some #'visit (hash-table-keys (graph-nodes graph))))))

(cl-defmethod graph-is-connected-p ((graph graph))
  "Return t if GRAPH is connected, nil otherwise.
A connected graph has a path between any two nodes.
This treats the graph as undirected.
Returns nil for empty graphs."
  (if (graph-empty-p graph)
    nil
    (let* ((nodes (hash-table-keys (graph-nodes graph)))
           (start-id (car nodes))
           (visited (make-hash-table :test 'equal)))

      ;; Run BFS from start node
      (let ((queue (list start-id)))
        (puthash start-id t visited)
        (while queue
          (let ((current (pop queue)))
            ;; Get both outgoing and incoming edges (treat as undirected)
            (dolist (neighbor (append
                                (graph-neighbors graph current)
                                (graph-neighbors graph current t)))
              (let ((neighbor-id (node-id neighbor)))
                (unless (gethash neighbor-id visited)
                  (puthash neighbor-id t visited)
                  (push neighbor-id queue)))))))

      ;; If all nodes were visited, the graph is connected
      (= (hash-table-count visited) (length nodes)))))

;;; Path Finding Algorithms

(cl-defgeneric graph-find-path (graph from to)
  "Find any path between FROM and TO in GRAPH.
Returns a list of nodes forming a valid route, ignoring edge weights.
This finds any valid path, not necessarily the shortest or optimal one.")

(cl-defmethod graph-find-path ((graph graph) (from-id string) (to-id string))
  "Find any path between FROM-ID and TO-ID in GRAPH using depth-first search.
Returns a list of nodes forming a valid route, ignoring edge weights.
This finds any valid path, not necessarily the shortest or optimal one."
  (unless (and (graph-uuid-p from-id) (graph-uuid-p to-id))
    (error "Invalid node UUID"))
  (unless (and (graph-node-p graph from-id) (graph-node-p graph to-id))
    (error "Source or target node does not exist"))

  (let ((visited (make-hash-table :test 'equal)))
    (cl-labels ((visit (node target path)
                  (puthash node t visited)
                  (if (string= node target)
                    (cons node path)
                    (cl-some (lambda (next)
                               (and (not (gethash (node-id next) visited))
                                 (visit (node-id next) target
                                   (cons node path))))
                      (graph-neighbors graph node)))))
      (let ((id-path (nreverse (visit from-id to-id nil))))
        (when id-path
          (mapcar (lambda (id) (graph-node-get graph id)) id-path))))))

(cl-defmethod graph-find-path ((graph graph) (from graph-node) (to graph-node))
  "Find any path between FROM and TO nodes in GRAPH.
Returns a list of nodes forming a valid route."
  (graph-find-path graph (node-id from) (node-id to)))

(cl-defgeneric graph-find-path-astar (graph from to &optional heuristic-fn)
  "Find shortest path between FROM and TO in GRAPH using A* algorithm.
Return multiple values: PATH and DISTANCE, where PATH is a list of nodes
and DISTANCE is the total accumulated weight.

Optional HEURISTIC-FN estimates distance between two nodes. It should accept
two node IDs and return a non-negative number. If nil, defaults to constant 0
(equivalent to Dijkstra's algorithm).")

(cl-defmethod graph-find-path-astar ((graph graph) (from-id string) (to-id string)
                                     &optional heuristic-fn)
  "Find shortest path between FROM-ID and TO-ID in GRAPH using A* algorithm.
Return multiple values: PATH and DISTANCE, where PATH is a list of nodes
and DISTANCE is the total accumulated weight."
  (unless (and (graph-uuid-p from-id) (graph-uuid-p to-id))
    (error "Invalid node UUID"))
  (unless (and (graph-node-p graph from-id) (graph-node-p graph to-id))
    (error "Source or target node does not exist"))

  ;; Default heuristic function
  (unless heuristic-fn
    (setq heuristic-fn (lambda (_n1 _n2) 0)))

  (let ((open-set (list from-id))
        (closed-set (make-hash-table :test 'equal))
        (g-score (make-hash-table :test 'equal))
        (f-score (make-hash-table :test 'equal))
        (came-from (make-hash-table :test 'equal))
        (result-path nil)
        (result-distance nil))

    ;; Initialize scores
    (puthash from-id 0 g-score)
    (puthash from-id (funcall heuristic-fn from-id to-id) f-score)

    (while (and open-set (not result-path))
      ;; Get node with lowest f-score
      (let* ((current (cl-reduce
                        (lambda (a b)
                          (if (< (or (gethash a f-score) most-positive-fixnum)
                                (or (gethash b f-score) most-positive-fixnum))
                            a b))
                        open-set))
             (current-g (gethash current g-score)))

        ;; Goal reached
        (when (string= current to-id)
          (let ((path nil)
                (node current))
            (while node
              (push node path)
              (setq node (gethash node came-from)))
            ;; Convert IDs to node objects
            (setq result-path (mapcar (lambda (id) (graph-node-get graph id)) path))
            (setq result-distance current-g)))

        (unless result-path
          ;; Move current from open to closed set
          (setq open-set (delete current open-set))
          (puthash current t closed-set)

          ;; Check each neighbor
          (dolist (edge (graph-node-edges graph current))
            (let* ((neighbor (edge-to edge))
                   (tentative-g (+ current-g (edge-weight edge))))

              ;; Skip if neighbor is in closed set and we can't improve its score
              (unless (and (gethash neighbor closed-set)
                        (>= tentative-g (gethash neighbor g-score
                                          most-positive-fixnum)))
                ;; Add to open set if not there
                (unless (member neighbor open-set)
                  (push neighbor open-set))

                ;; This path is better than any previous one
                (when (< tentative-g (gethash neighbor g-score
                                       most-positive-fixnum))
                  (puthash neighbor current came-from)
                  (puthash neighbor tentative-g g-score)
                  (puthash neighbor
                    (+ tentative-g
                      (funcall heuristic-fn neighbor to-id))
                    f-score))))))))

    ;; Return multiple values
    (cl-values result-path result-distance)))

(cl-defmethod graph-find-path-astar ((graph graph) (from graph-node) (to graph-node)
                                     &optional heuristic-fn)
  "Find shortest path between FROM and TO nodes in GRAPH using A* algorithm.
Return multiple values: PATH and DISTANCE, where PATH is a list of nodes
and DISTANCE is the total accumulated weight."
  (graph-find-path-astar graph (node-id from) (node-id to) heuristic-fn))

(cl-defgeneric graph-find-path-shortest (graph from to)
  "Find shortest path between FROM and TO in GRAPH.
Return multiple values: PATH and DISTANCE, where PATH is a list of nodes
and DISTANCE is the total accumulated weight.

This uses A* algorithm with a zero heuristic, making it equivalent to Dijkstra's
algorithm. For more advanced pathfinding, use `graph-find-path-astar` directly.")

(cl-defmethod graph-find-path-shortest ((graph graph) (from-id string) (to-id string))
  "Find shortest path between FROM-ID and TO-ID in GRAPH.
Return multiple values: PATH and DISTANCE, where PATH is a list of nodes
and DISTANCE is the total accumulated weight."
  (graph-find-path-astar graph from-id to-id nil))


(cl-defmethod graph-find-path-shortest ((graph graph) (from graph-node) (to graph-node))
  "Find shortest path between FROM and TO nodes in GRAPH.
Return multiple values: PATH and DISTANCE, where PATH is a list of nodes
and DISTANCE is the total accumulated weight."
  (graph-find-path-shortest graph (node-id from) (node-id to)))

;;; Heuristic Generators for A*

(defun graph-astar-euclidean-distance (graph coord-fn)
  "Return a heuristic function that calculates Euclidean distance.
GRAPH is the graph being searched.
COORD-FN function takes a node and returns a cons (x . y) of coordinates."
  (lambda (from to)
    (let* ((from-node (if (cl-typep from 'graph-node)
                        from
                        (graph-node-get graph from)))
           (to-node (if (cl-typep to 'graph-node)
                      to
                      (graph-node-get graph to)))
           (p1 (funcall coord-fn from-node))
           (p2 (funcall coord-fn to-node)))
      (if (and p1 p2)
        (sqrt (+ (expt (- (car p1) (car p2)) 2)
                 (expt (- (cdr p1) (cdr p2)) 2)))
        0))))

(defun graph-astar-manhattan-distance (graph coord-fn)
  "Return a heuristic function that calculates Manhattan distance.
GRAPH is the graph being searched.
COORD-FN function takes a node and returns a cons (x . y) of coordinates."
  (lambda (from to)
    (let* ((from-node (if (cl-typep from 'graph-node)
                        from
                        (graph-node-get graph from)))
           (to-node (if (cl-typep to 'graph-node)
                      to
                      (graph-node-get graph to)))
           (p1 (funcall coord-fn from-node))
           (p2 (funcall coord-fn to-node)))
      (if (and p1 p2)
        (+ (abs (- (car p1) (car p2)))
           (abs (- (cdr p1) (cdr p2))))
        0))))

;;; Topological Sort

(cl-defmethod graph-topological-sort ((graph graph))
  "Return a topological ordering of nodes in GRAPH.
Return nil if GRAPH contains a cycle (not a DAG).
A topological sort is a linear ordering of vertices such that for
every directed edge (u,v), vertex u comes before v in the ordering."
  (when (graph-cycle-p graph)
    (error "Cannot perform topological sort on a graph with cycles"))

  (let ((sorted-nodes nil)
        (in-degree (make-hash-table :test 'equal))
        (queue nil))

    ;; Initialize in-degree counts for all nodes
    (maphash (lambda (node-id _)
               (puthash node-id
                 (length (graph-node-edges graph node-id t))
                 in-degree))
      (graph-nodes graph))

    ;; Find all nodes with in-degree zero (no incoming edges)
    (maphash (lambda (node-id count)
               (when (zerop count)
                 (push node-id queue)))
      in-degree)

    ;; Process queue in topological order
    (cl-loop while queue
      for current-id = (pop queue)
      for current-node = (graph-node-get graph current-id)
      do (push current-node sorted-nodes)
      ;; Process outgoing edges
      do (dolist (edge (graph-node-edges graph current-id))
           (let* ((neighbor-id (edge-to edge))
                  (new-degree (1- (gethash neighbor-id in-degree))))
             ;; Update in-degree and queue if needed
             (puthash neighbor-id new-degree in-degree)
             (when (zerop new-degree)
               (push neighbor-id queue)))))

    ;; Verify all nodes were processed (no cycles)
    (when (/= (length sorted-nodes) (hash-table-count (graph-nodes graph)))
      (error "Graph contains a cycle, cannot perform topological sort"))

    ;; Return nodes in topological order
    (nreverse sorted-nodes)))

(provide 'graphael-algorithms)
;;; graphael-algorithms.el ends here
