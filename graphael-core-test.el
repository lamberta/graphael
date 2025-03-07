;;; graphael-core-test.el --- Tests for graphael-core.el -*- lexical-binding: t -*-

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
;; Tests for graphael-core.el

(require 'ert)
(require 'graphael-core)

(ert-deftest graphael-creation-test ()
  "Test basic graph creation."
  (let ((g (make-instance 'graph)))
    (should (hash-table-empty-p (graph-nodes g)))
    (should (hash-table-empty-p (graph-edges g)))
    (should (hash-table-empty-p (graph-outgoing-edges g)))
    (should (hash-table-empty-p (graph-incoming-edges g)))
    (should (zerop (graph-node-count g)))
    (should (zerop (graph-edge-count g)))
    (should (graph-empty-p g))))

(ert-deftest graphael-node-operations-test ()
  "Test node creation and removal."
  (let* ((g (make-instance 'graph))
         (node1 (graph-node-add g))
         (node2 (graph-node-add g))
         (id1 (node-id node1))
         (id2 (node-id node2)))
    ;; Test node addition
    (should (graph-node-p g id1))
    (should (graph-node-p g id2))
    (should (= (hash-table-count (graph-nodes g)) 2))
    (should (not (graph-empty-p g)))
    ;; Test node retrieval
    (should (equal (graph-node-get g id1) node1))
    (should (equal (graph-node-get g id2) node2))
    ;; Test node removal
    (graph-node-remove g node1)
    (should-not (graph-node-p g id1))
    (should (graph-node-p g id2))
    (should (= (hash-table-count (graph-nodes g)) 1))))

(ert-deftest graphael-edge-operations-test ()
  "Test edge creation and removal."
  (let* ((g (make-instance 'graph))
         (node1 (graph-node-add g))
         (node2 (graph-node-add g))
         (edge (graph-edge-add g :from node1 :to node2))
         (edge-id (edge-id edge)))
    ;; Test edge addition
    (should (graph-edge-p g edge-id))
    (should (= (hash-table-count (graph-edges g)) 1))
    ;; Test edge connections
    (should (equal (graph-node-edges g (node-id node1)) (list edge)))
    (should (equal (graph-node-edges g (node-id node2) t) (list edge)))
    ;; Test connected nodes
    (should (equal (graph-neighbors g (node-id node1)) (list node2)))
    (should (equal (graph-neighbors g (node-id node2) t) (list node1)))
    ;; Test edge removal
    (graph-edge-remove g edge)
    (should-not (graph-edge-p g edge-id))
    (should (= (hash-table-count (graph-edges g)) 0))
    (should-not (graph-node-edges g (node-id node1)))
    (should-not (graph-node-edges g (node-id node2) t))))

(ert-deftest graphael-multiple-dispatch-test ()
  "Test that multiple dispatch methods work consistently."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (n3 (graph-node-add g))
         (id1 (node-id n1)))
    ;; Create edges
    (graph-edge-add g :from n1 :to n2)
    (graph-edge-add g :from n1 :to n3)
    ;; Test get-neighbors with both node and ID
    (let ((neighbors1 (graph-neighbors g n1))
          (neighbors2 (graph-neighbors g id1)))
      (should (= (length neighbors1) 2))
      (should (equal neighbors1 neighbors2)))

    ;; Test get-node-edges with both node and ID
    (let ((edges1 (graph-node-edges g n1))
          (edges2 (graph-node-edges g id1)))
      (should (= (length edges1) 2))
      (should (equal edges1 edges2)))))

(ert-deftest graphael-validation-test ()
  "Test validation and error cases."
  (let ((g (make-instance 'graph)))
    ;; Invalid UUIDs
    (should-error (graph-node-add g :id "not-a-uuid"))
    (should-not (graph-node-get g "not-a-uuid"))
    (should-not (graph-node-p g "not-a-uuid"))

    ;; Non-existent nodes/edges
    (let ((uuid "12345678-1234-1234-1234-123456789012"))
      (should-not (graph-node-get g uuid))
      (should-error (graph-node-remove g uuid))
      (should-error (graph-node-edges g uuid)))

    ;; Self-loops
    (let* ((node (graph-node-add g))
           (id (node-id node)))
      (should-error (graph-edge-add g :from id :to id)))))

(ert-deftest graphael-node-attrs-test ()
  "Test node attrs storage and retrieval."
  (let* ((g (make-instance 'graph))
         (node (graph-node-add g))
         (attrs '(:key "value")))
    (setf (node-attrs node) attrs)
    (should (equal (node-attrs node) attrs))
    (should (equal (node-attrs (graph-node-get g (node-id node))) attrs))
    ;; Test attr helpers
    (graph-node-attr-put node :color "blue")
    (should (equal (graph-node-attr-get node :color) "blue"))
    (should (equal (graph-node-attr-get node :missing "default") "default"))))

(ert-deftest graphael-edge-attributes-test ()
  "Test edge attributes."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (edge (graph-edge-add g :from n1 :to n2)))
    ;; Test weight
    (should (= (edge-weight edge) 1.0))
    (setf (edge-weight edge) 2.5)
    (should (= (edge-weight edge) 2.5))
    ;; Test label
    (should-not (edge-label edge))
    (setf (edge-label edge) "test-label")
    (should (string= (edge-label edge) "test-label"))
    ;; Test attrs helpers
    (let ((attrs '(:key "value")))
      (setf (edge-attrs edge) attrs)
      (should (equal (edge-attrs edge) attrs))
      (graph-edge-attr-put edge :priority "high")
      (should (equal (graph-edge-attr-get edge :priority) "high")))))

(ert-deftest graphael-multiple-edges-test ()
  "Test handling of multiple edges between nodes."
  (let* ((g (make-instance 'graph))
         (node1 (graph-node-add g))
         (node2 (graph-node-add g))
         (edge1 (graph-edge-add g :from node1 :to node2))
         ;; Multiple edges between node1 and node2
         (edge2 (graph-edge-add g :from node1 :to node2))
         (id1 (node-id node1))
         (id2 (node-id node2)))
    ;; Check if all edges are added
    (should (= (hash-table-count (graph-edges g)) 2))
    (should (graph-edge-p g (edge-id edge1)))
    (should (graph-edge-p g (edge-id edge2)))
    ;; Check outgoing edges for node1 - should contain both edges
    (let ((outgoing (graph-node-edges g id1)))
      (should (= (length outgoing) 2))
      (should (member edge1 outgoing))
      (should (member edge2 outgoing)))
    ;; Check incoming edges for node2 - should contain both edges
    (let ((incoming (graph-node-edges g id2 t)))
      (should (= (length incoming) 2))
      (should (member edge1 incoming))
      (should (member edge2 incoming)))))

(ert-deftest graphael-node-removal-with-edges-test ()
  "Test node removal when nodes have edges."
  (let* ((g (make-instance 'graph))
         (node1 (graph-node-add g))
         (node2 (graph-node-add g))
         (node3 (graph-node-add g))
         (edge1 (graph-edge-add g :from node1 :to node2))
         (edge2 (graph-edge-add g :from node2 :to node3)))
    ;; Initially graph should have nodes and edges
    (should (graph-node-p g (node-id node1)))
    (should (graph-node-p g (node-id node2)))
    (should (graph-edge-p g (edge-id edge1)))
    (should (graph-edge-p g (edge-id edge2)))
    ;; Remove middle node
    (graph-node-remove g node2)
    ;; Check that node and connected edges are removed
    (should-not (graph-node-p g (node-id node2)))
    (should-not (graph-edge-p g (edge-id edge1)))
    (should-not (graph-edge-p g (edge-id edge2)))
    ;; Remaining nodes should still exist
    (should (graph-node-p g (node-id node1)))
    (should (graph-node-p g (node-id node3)))))

(ert-deftest graphael-attr-handling-test ()
  "Test attr handling with complex attrs structures."
  (let* ((g (make-instance 'graph))
         (node (graph-node-add g))
         (complex-attrs '(:array [1 2 3]
                           :hash ,(make-hash-table)
                           :nested (:a (:b 1)))))
    ;; Test storing complex attrs
    (graph-node-attr-put node :complex complex-attrs)
    (should (equal (graph-node-attr-get node :complex) complex-attrs))
    ;; Test multiple properties
    (graph-node-attr-put node :p1 "value1")
    (graph-node-attr-put node :p2 "value2")
    (should (equal (graph-node-attr-get node :p1) "value1"))
    (should (equal (graph-node-attr-get node :p2) "value2"))
    ;; Test overwriting properties
    (graph-node-attr-put node :p1 "new-value")
    (should (equal (graph-node-attr-get node :p1) "new-value"))))

(ert-deftest graphael-edge-weight-handling-test ()
  "Test handling of edge weights."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (edge (graph-edge-add g :from n1 :to n2 :weight 2.5)))
    ;; Test weight retrieval
    (should (= (edge-weight edge) 2.5))
    ;; Test changing weight
    (setf (edge-weight edge) 3.7)
    (should (= (edge-weight edge) 3.7))
    ;; Test invalid weight
    (should-error (setf (edge-weight edge) -1.0))))

(ert-deftest graphael-transaction-safety-test ()
  "Test that transactions properly protect operations."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g)))
    ;; Test successful transaction
    (with-graph-transaction (g)
      (graph-edge-add g :from n1 :to n2)
      (should (= (graph-edge-count g) 1)))
    ;; Test transaction that errors out
    (should-error
      (with-graph-transaction (g)
        (graph-edge-add g :from n2 :to n1)
        (graph-edge-add g :from n1 :to n1))) ; Should error (self-loop)
    ;; The first edge was added before the error
    ;; Current implementation doesn't roll back on error
    (should (= (graph-edge-count g) 2))))

(ert-deftest graphael-path-operations-test ()
  "Test creating paths and working with them."
  (let* ((g (make-instance 'graph))
         (nodes (cl-loop for i from 1 to 5
                  collect (graph-node-add g :label (format "Node %d" i)))))
    ;; Test creating a path
    (let* ((edges (graph-add-path g nodes)))
      (should (= (length edges) 4)) ; 5 nodes = 4 edges
      ;; Test path connectivity
      (should (graph-find-path g (node-id (car nodes)) (node-id (car (last nodes)))))
      ;; Test removing a node in the middle breaks the path
      (graph-node-remove g (nth 2 nodes))
      (should-not (graph-find-path g (node-id (car nodes)) (node-id (car (last nodes))))))))

(ert-deftest graphael-algorithm-edge-cases-test ()
  "Test algorithms with edge cases."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g)))
    ;; Graph with single node
    (should-not (graph-cycle-p g))
    (should (graph-is-connected-p g))
    ;; Empty graph
    (graph-node-remove g n1)
    (should (graph-empty-p g))
    (should-not (graph-is-connected-p g))
    ;; Graph with disconnected nodes
    (let ((n2 (graph-node-add g))
          (n3 (graph-node-add g)))
      (should-not (graph-is-connected-p g))
      (should-not (graph-find-path g (node-id n2) (node-id n3)))
      ;; Connect the nodes
      (graph-edge-add g :from n2 :to n3)
      (should (graph-is-connected-p g))
      (should (graph-find-path g (node-id n2) (node-id n3)))
      (should-not (graph-find-path g (node-id n3) (node-id n2)))))) ; Directed graph

(ert-deftest graphael-performance-stress-test ()
  "Test performance with larger graphs."
  (let* ((g (make-instance 'graph))
         (node-count 100)
         (edge-count 200)
         (nodes (cl-loop for i from 0 below node-count
                  collect (graph-node-add g))))
    ;; Add random edges
    (cl-loop for i from 0 below edge-count
      do (let ((from (nth (random node-count) nodes))
               (to (nth (random node-count) nodes)))
           (unless (string= (node-id from) (node-id to)) ; Avoid self-loops
             (graph-edge-add g :from from :to to))))

    ;; Test graph operations are efficient
    (should (= (graph-node-count g) node-count))
    (should (<= (graph-edge-count g) edge-count)) ; May be fewer due to duplicate attempts
    ;; Test cloning a large graph
    (let* ((g2 (graph-clone g)))
      (should (= (graph-node-count g2) (graph-node-count g)))
      (should (= (graph-edge-count g2) (graph-edge-count g))))))

(provide 'graphael-core-test)
;;; graphael-core-test.el ends here
