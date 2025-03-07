;;; graphael-operations-test.el --- Tests for graphael-operations.el -*- lexical-binding: t -*-

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
;; Tests for graphael-operations.el

(require 'ert)
(require 'graphael-core)
(require 'graphael-operations)

(ert-deftest graphael-clone-test ()
  "Test graph cloning functionality."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (n3 (graph-node-add g))
         (e1 (graph-edge-add g :from n1 :to n2))
         ;; Using the edge we create
         (_e2 (graph-edge-add g :from n2 :to n3)))

    ;; Set some attrs to test deep copying
    (setf (node-attrs n1) '(:label "Node 1"))
    (setf (edge-weight e1) 5.0)

    ;; Clone the graph
    (let* ((g2 (graph-clone g)))
      ;; Check node counts
      (should (= (hash-table-count (graph-nodes g))
                (hash-table-count (graph-nodes g2))))

      ;; Check edge counts
      (should (= (hash-table-count (graph-edges g))
                (hash-table-count (graph-edges g2))))

      ;; Check that nodes with same IDs exist in both graphs
      (should (graph-node-p g2 (node-id n1)))
      (should (graph-node-p g2 (node-id n2)))
      (should (graph-node-p g2 (node-id n3)))

      ;; Check that attrs was properly copied
      (should (equal (node-attrs (graph-node-get g2 (node-id n1)))
                (node-attrs n1)))

      ;; Modify cloned graph and verify original is unchanged
      (graph-node-remove g2 (node-id n3))
      (should (graph-node-p g (node-id n3)))
      (should-not (graph-node-p g2 (node-id n3))))))

(ert-deftest graphael-merge-test ()
  "Test merging of two graphs."
  (let* (;; Create first graph
         (g1 (make-instance 'graph))
         (n1-1 (graph-node-add g1))
         (n1-2 (graph-node-add g1))
         (n1-3 (graph-node-add g1))
         ;; Create edges in first graph
         (e1-1 (graph-edge-add g1 :from n1-1 :to n1-2))
         (e1-2 (graph-edge-add g1 :from n1-2 :to n1-3))
         ;; Create second graph
         (g2 (make-instance 'graph))
         (n2-1 (graph-node-add g2))
         (n2-2 (graph-node-add g2))
         ;; Also create a node with same ID as in g1
         (n2-3 (graph-node-add g2 :id (node-id n1-3)))
         ;; Create edges in second graph
         (e2-1 (graph-edge-add g2 :from n2-1 :to n2-2))
         (e2-2 (graph-edge-add g2 :from n2-2 :to n2-3)))

    ;; Before merge: g1 has 3 nodes and 2 edges
    (should (= (hash-table-count (graph-nodes g1)) 3))
    (should (= (hash-table-count (graph-edges g1)) 2))
    ;; Before merge: g2 has 3 nodes and 2 edges
    (should (= (hash-table-count (graph-nodes g2)) 3))
    (should (= (hash-table-count (graph-edges g2)) 2))
    ;; Test 1: Merge creates a new graph by default
    (let* ((merged (graph-merge g1 g2)))
      ;; Merged graph should have 5 nodes (3 original + 2 new, 1 overlap)
      (should (= (hash-table-count (graph-nodes merged)) 5))
      ;; Merged graph should have 4 edges (2 original + 2 new)
      (should (= (hash-table-count (graph-edges merged)) 4))
      ;; Original graphs should be unchanged
      (should (= (hash-table-count (graph-nodes g1)) 3))
      (should (= (hash-table-count (graph-edges g1)) 2))
      (should (= (hash-table-count (graph-nodes g2)) 3))
      (should (= (hash-table-count (graph-edges g2)) 2))
      ;; Merged graph should contain all nodes from both graphs
      (should (graph-node-p merged (node-id n1-1)))
      (should (graph-node-p merged (node-id n1-2)))
      (should (graph-node-p merged (node-id n1-3)))
      (should (graph-node-p merged (node-id n2-1)))
      (should (graph-node-p merged (node-id n2-2)))
      ;; Merged graph should contain all edges
      (should (graph-edge-p merged (edge-id e1-1)))
      (should (graph-edge-p merged (edge-id e1-2)))
      (should (graph-edge-p merged (edge-id e2-1)))
      (should (graph-edge-p merged (edge-id e2-2))))

    ;; Test 2: In-place modification when requested
    (graph-merge g1 g2 t)
    ;; After in-place merge: g1 should have 5 nodes
    (should (= (hash-table-count (graph-nodes g1)) 5))
    ;; After in-place merge: g1 should have 4 edges
    (should (= (hash-table-count (graph-edges g1)) 4))
    ;; g1 should contain all nodes from both graphs
    (should (graph-node-p g1 (node-id n1-1)))
    (should (graph-node-p g1 (node-id n1-2)))
    (should (graph-node-p g1 (node-id n1-3)))
    (should (graph-node-p g1 (node-id n2-1)))
    (should (graph-node-p g1 (node-id n2-2)))
    ;; g1 should contain all edges
    (should (graph-edge-p g1 (edge-id e1-1)))
    (should (graph-edge-p g1 (edge-id e1-2)))
    (should (graph-edge-p g1 (edge-id e2-1)))
    (should (graph-edge-p g1 (edge-id e2-2)))
    ;; g2 should be unchanged
    (should (= (hash-table-count (graph-nodes g2)) 3))
    (should (= (hash-table-count (graph-edges g2)) 2))))

(ert-deftest graphael-subgraph-test ()
  "Test creating a subgraph."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g :label "Node 1"))
         (n2 (graph-node-add g :label "Node 2"))
         (n3 (graph-node-add g :label "Node 3"))
         (n4 (graph-node-add g :label "Node 4"))
         (e1 (graph-edge-add g :from n1 :to n2))
         (e2 (graph-edge-add g :from n2 :to n3))
         (e3 (graph-edge-add g :from n3 :to n4)))
    ;; Test with explicit node list
    (let* ((node-ids (list (node-id n1) (node-id n2)))
           (sub1 (graph-subgraph g node-ids)))
      ;; Should contain 2 nodes
      (should (= (hash-table-count (graph-nodes sub1)) 2))
      ;; Should contain 1 edge
      (should (= (hash-table-count (graph-edges sub1)) 1))
      (should (graph-edge-p sub1 (edge-id e1))))
    ;; Test with function filter
    (let* ((sub2 (graph-subgraph g (lambda (node)
                                     (string= (node-label node) "Node 3")))))
      ;; Should contain 1 node
      (should (= (hash-table-count (graph-nodes sub2)) 1))
      ;; Should contain 0 edges (no edges between Node 3 and itself)
      (should (= (hash-table-count (graph-edges sub2)) 0)))

    ;; Test with include-connected
    (let* ((sub3 (graph-subgraph g (list (node-id n3)) t)))
      ;; Should contain 3 nodes: n2, n3, n4
      (should (= (hash-table-count (graph-nodes sub3)) 3))
      ;; Should contain 2 edges: e2, e3
      (should (= (hash-table-count (graph-edges sub3)) 2))
      (should (graph-edge-p sub3 (edge-id e2)))
      (should (graph-edge-p sub3 (edge-id e3)))
      (should-not (graph-edge-p sub3 (edge-id e1))))))

(ert-deftest graphael-subgraph-node-objects-test ()
  "Test creating a subgraph with node objects instead of IDs."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g :label "Node 1"))
         (n2 (graph-node-add g :label "Node 2"))
         (n3 (graph-node-add g :label "Node 3"))
         (n4 (graph-node-add g :label "Node 4"))
         (e1 (graph-edge-add g :from n1 :to n2))
         (_e2 (graph-edge-add g :from n2 :to n3))
         (_e3 (graph-edge-add g :from n3 :to n4)))

    ;; Test with list of node objects
    (let* ((nodes (list n1 n2))
           (sub1 (graph-subgraph g nodes)))
      ;; Should contain 2 nodes
      (should (= (hash-table-count (graph-nodes sub1)) 2))
      ;; Should contain 1 edge
      (should (= (hash-table-count (graph-edges sub1)) 1))
      (should (graph-edge-p sub1 (edge-id e1))))

    ;; Test with mixed list (node objects and IDs)
    (let* ((mixed-list (list n1 (node-id n3)))
           (sub2 (graph-subgraph g mixed-list)))
      ;; Should contain 2 nodes
      (should (= (hash-table-count (graph-nodes sub2)) 2))
      ;; Should contain 0 edges (no edges between n1 and n3)
      (should (= (hash-table-count (graph-edges sub2)) 0))
      (should (graph-node-p sub2 (node-id n1)))
      (should (graph-node-p sub2 (node-id n3)))
      (should-not (graph-node-p sub2 (node-id n2)))
      (should-not (graph-node-p sub2 (node-id n4))))))

(ert-deftest graphael-graph-set-operations-test ()
  "Test graph set operations (difference and intersection)."
  (let* ((g1 (make-instance 'graph))
         (n1-1 (graph-node-add g1 :label "A"))
         (n1-2 (graph-node-add g1 :label "B"))
         (n1-3 (graph-node-add g1 :label "C"))
         (n1-4 (graph-node-add g1 :label "D"))
         ;; Use the edges we create with a leading underscore
         (_e1-1 (graph-edge-add g1 :from n1-1 :to n1-2))
         (_e1-2 (graph-edge-add g1 :from n1-2 :to n1-3))
         (_e1-3 (graph-edge-add g1 :from n1-3 :to n1-4))
         ;; Create second graph with some overlapping nodes
         (g2 (make-instance 'graph))
         (n2-1 (graph-node-add g2 :id (node-id n1-2))) ; Same as B in g1
         (n2-2 (graph-node-add g2 :id (node-id n1-3))) ; Same as C in g1
         ;; Use the edge with a leading underscore
         (_e2-1 (graph-edge-add g2 :from n2-1 :to n2-2)))

    ;; Test difference (g1 - g2)
    (let* ((result (graph-difference g1 g2)))
      ;; Result should have 2 nodes (A and D)
      (should (= (hash-table-count (graph-nodes result)) 2))
      ;; Result should have 0 edges (no edges connect A to D directly)
      (should (= (hash-table-count (graph-edges result)) 0))
      ;; Result should include n1-1 and n1-4 specifically
      (should (graph-node-p result (node-id n1-1)))
      (should (graph-node-p result (node-id n1-4)))
      ;; Result should NOT include n1-2 or n1-3
      (should-not (graph-node-p result (node-id n1-2)))
      (should-not (graph-node-p result (node-id n1-3))))

    ;; Test intersection (g1 ∩ g2)
    (let* ((result (graph-intersect g1 g2)))
      ;; Result should have 2 nodes (B and C)
      (should (= (hash-table-count (graph-nodes result)) 2))
      ;; Result should have 1 edge (B->C)
      (should (= (hash-table-count (graph-edges result)) 1))
      ;; Result should include n1-2 and n1-3 specifically
      (should (graph-node-p result (node-id n1-2)))
      (should (graph-node-p result (node-id n1-3)))
      ;; Result should NOT include n1-1 or n1-4
      (should-not (graph-node-p result (node-id n1-1)))
      (should-not (graph-node-p result (node-id n1-4))))))

(ert-deftest graphael-add-path-test ()
  "Test adding a path to a graph."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (n3 (graph-node-add g))
         (n4 (graph-node-add g))
         (id1 (node-id n1))
         (id2 (node-id n2))
         (id3 (node-id n3))
         (id4 (node-id n4)))

    ;; Add a path n1 -> n2 -> n3 -> n4
    (let* ((edges (graph-add-path g (list id1 id2 id3 id4))))
      ;; Should create 3 edges
      (should (= (length edges) 3))

      ;; Each node should have correct edge counts
      (should (= (length (graph-node-edges g id1)) 1))
      (should (= (length (graph-node-edges g id2)) 1))
      (should (= (length (graph-node-edges g id3)) 1))
      (should (= (length (graph-node-edges g id4)) 0))

      (should (= (length (graph-node-edges g id1 t)) 0))
      (should (= (length (graph-node-edges g id2 t)) 1))
      (should (= (length (graph-node-edges g id3 t)) 1))
      (should (= (length (graph-node-edges g id4 t)) 1)))))

(ert-deftest graphael-stats-test ()
  "Test graph statistics functions."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (n3 (graph-node-add g))
         ;; Use the edges with leading underscores
         (_e1 (graph-edge-add g :from n1 :to n2 :weight 2.0))
         (_e2 (graph-edge-add g :from n2 :to n3 :weight 3.0)))

    ;; Basic stats
    (let* ((stats (graph-stats g)))
      (should (= (graph-stats-result-nodes stats) 3))
      (should (= (graph-stats-result-edges stats) 2))
      ;; Density for 3 nodes, 2 edges = 2/(3*2) = 0.333
      (should (< (abs (- (graph-stats-result-density stats) 0.333)) 0.01))
      ;; Average weight = (2.0 + 3.0)/2 = 2.5
      (should (= (graph-stats-result-avg-edge-weight stats) 2.5)))))

(provide 'graphael-operations-test)
;;; graphael-operations-test.el ends here
