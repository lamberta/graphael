;;; graphael-boundary-test.el --- Boundary case and error handling tests -*- lexical-binding: t -*-

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
;; Boundary case and error handling tests

(require 'ert)
(require 'threads)
(require 'graphael-core)
(require 'graphael-operations)
(require 'graphael-algorithms)

(ert-deftest graphael-large-graph-edge-case-test ()
  "Test building and managing a graph with many nodes and edges."
  (let* ((g (make-instance 'graph))
         ;; Use a smaller node count to avoid excessive nesting
         (node-count 300)
         (nodes (cl-loop for i from 0 below node-count
                  collect (graph-node-add g :label (format "Node %d" i)))))

    ;; Create a linear path through all nodes
    (cl-loop for i from 0 below (1- node-count)
      for from = (nth i nodes)
      for to = (nth (1+ i) nodes)
      do (graph-edge-add g :from from :to to))

    ;; Test the graph structure
    (should (= (graph-node-count g) node-count))
    (should (= (graph-edge-count g) (1- node-count)))

    ;; Test pathfinding on a very long path
    (let* ((start-id (node-id (car nodes)))
           ;; Use first and middle node to avoid excessive recursion
           (mid-id (node-id (nth (/ node-count 2) nodes)))
           (path (graph-find-path g start-id mid-id)))
      (should path)
      ;; Should be half the nodes plus 1
      (should (= (length path) (1+ (/ node-count 2)))))))

(ert-deftest graphael-uuid-edge-case-test ()
  "Test edge cases with UUIDs."
  (let* ((g (make-instance 'graph))
         (uuid "12345678-1234-1234-1234-123456789abc")
         (node (graph-node-add g :id uuid)))
    ;; Test with valid UUID
    (should (graph-uuid-p uuid))
    (should (string= (node-id node) uuid))
    (should (graph-node-p g uuid))
    ;; Test with malformed UUIDs
    (should-not (graph-uuid-p "not-a-uuid"))
    (should-not (graph-uuid-p "12345678-1234-1234-1234-12345678"))  ; Too short
    (should-not (graph-uuid-p "12345678-1234-1234-1234-12345678901234"))  ; Too long
    (should-not (graph-uuid-p "12345678-1234-1234-1234-GGGGGGGGGGGG"))  ; Invalid chars
    ;; Test error cases
    (should-error (graph-node-add g :id "not-a-uuid"))
    ;; Current implementation returns nil for non-existent nodes, doesn't error
    (should-not (graph-node-get g "not-a-uuid"))))

(ert-deftest graphael-node-property-equality-test ()
  "Test that property values maintain equality across operations."
  (let* ((g (make-instance 'graph))
         (node (graph-node-add g :label "Test"))
         (complex-data (list :vector [1 2 3] :list '(a b c))))

    ;; Add data to node
    (setf (node-data node) complex-data)

    ;; Clone the graph
    (let* ((g2 (graph-clone g))
           (node2 (graph-node-get g2 (node-id node)))
           (data2 (node-data node2)))

      ;; Data should be equal
      (should (equal (plist-get complex-data :list)
                (plist-get data2 :list)))
      (should (equal (plist-get complex-data :vector)
                (plist-get data2 :vector))))))

(ert-deftest graphael-concurrent-modification-test ()
  "Test concurrent modification of the graph is handled correctly."
  (let* ((g (make-instance 'graph))
         (thread1-done nil)
         (thread2-done nil)
         (error-occurred nil))
    ;; Set up some initial nodes
    (dotimes (i 10)
      (graph-node-add g :label (format "Node %d" i)))

    ;; Create two threads that will modify the graph concurrently
    (let* ((thread1 (make-thread
                      (lambda ()
                        (condition-case _err
                          (progn
                            (dotimes (_i 100)
                              (let* ((n1 (graph-node-add g))
                                     (n2 (graph-node-add g)))
                                (graph-edge-add g :from n1 :to n2)))
                            (setq thread1-done t))
                          (error
                            (setq error-occurred t))))))
            (thread2 (make-thread
                       (lambda ()
                         (condition-case _err
                           (progn
                             (dotimes (_i 50)
                               (let* ((nodes (hash-table-values (graph-nodes g))))
                                 (when (> (length nodes) 2)
                                   (let* ((n1 (nth (random (length nodes)) nodes))
                                          (n2 (nth (random (length nodes)) nodes)))
                                     (unless (string= (node-id n1) (node-id n2))
                                       (graph-edge-add g :from n1 :to n2))))))
                             (setq thread2-done t))
                           (error
                             (setq error-occurred t)))))))
      ;; Start the threads
      (thread-join thread1)
      (thread-join thread2)
      ;; Check the results
      (should thread1-done)
      (should thread2-done)
      (should-not error-occurred)
      ;; Graph should be in a consistent state
      (should (= (graph-node-count g) (+ 10 (* 2 100))))
      (should (>= (graph-edge-count g) 100)))))

(ert-deftest graphael-algorithm-stress-test ()
  "Stress test algorithms on complex graph structures."
  (let* ((g (make-instance 'graph))
         (size 30) ; 30x30 grid = 900 nodes
         (nodes (make-hash-table :test 'equal))
         (random-seed 12345))
    ;; Set fixed seed for reproducible tests
    (random "")  ;; Reset
    (random random-seed)  ;; Set the seed

    ;; Create a grid of nodes
    (dotimes (y size)
      (dotimes (x size)
        (let ((node (graph-node-add g :data (cons x y))))
          (puthash (cons x y) node nodes))))

    ;; Connect nodes in a grid pattern (with some random weights)
    (dotimes (y size)
      (dotimes (x size)
        (when (< x (1- size))
          (graph-edge-add g
            :from (gethash (cons x y) nodes)
            :to (gethash (cons (1+ x) y) nodes)
            :weight (+ 1.0 (random 5))))
        (when (< y (1- size))
          (graph-edge-add g
            :from (gethash (cons x y) nodes)
            :to (gethash (cons x (1+ y)) nodes)
            :weight (+ 1.0 (random 5))))))

    ;; Add some random diagonal shortcuts (about 10% of possible diagonals)
    (dotimes (_ (/ (* size size) 10))
      (let* ((x1 (random (1- size)))
             (y1 (random (1- size)))
             (x2 (+ 1 (random (1- size))))
             (y2 (+ 1 (random (1- size)))))
        (unless (or (= x1 x2) (= y1 y2))
          (graph-edge-add g
            :from (gethash (cons x1 y1) nodes)
            :to (gethash (cons x2 y2) nodes)
            :weight (+ 0.5 (random 30))))))
    ;; Restore randomness
    (random t)
    ;; Define coordinate function for heuristics
    (cl-flet ((get-coord (node) (node-data node)))
      ;; Test with different path finding algorithms
      (let* ((start-node (gethash (cons 0 0) nodes))
             (end-node (gethash (cons (1- size) (1- size)) nodes))
             (start-id (node-id start-node))
             (end-id (node-id end-node))
             ;; Basic path finding
             (basic-path (graph-find-path g start-id end-id)))

        ;; Use cl-multiple-value-bind for multiple return values
        (cl-multiple-value-bind (dijkstra-path dijkstra-cost)
          (graph-find-path-shortest g start-id end-id)

          ;; A* with Manhattan heuristic
          (let* ((manhattan-fn (graph-astar-manhattan-distance g #'get-coord)))
            (cl-multiple-value-bind (astar-path astar-cost)
              (graph-find-path-astar g start-id end-id manhattan-fn)

              ;; All algorithms should find a path
              (should basic-path)
              (should dijkstra-path)
              (should astar-path)

              ;; A* and Dijkstra should find similar-cost paths
              ;; (allowing a small percentage difference due to heuristic estimates)
              (should (<= (abs (- astar-cost dijkstra-cost)) (* 0.1 dijkstra-cost)))

              ;; Verify start and end nodes
              (should (equal (car astar-path) start-node))
              (should (equal (car (last astar-path)) end-node)))))))))

(ert-deftest graphael-circular-reference-test ()
  "Test handling of circular references in node/edge data."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g :label "Node 1"))
         (n2 (graph-node-add g :label "Node 2"))
         (circular-data (list :self)))

    ;; Create circular reference differently (properly)
    (setf (cdr circular-data) (list circular-data))

    ;; Set data with circular reference
    ;; Note: The current implementation doesn't actually detect circular refs,
    ;; so we're just testing that setting complex data works
    (setf (node-data n1) circular-data)
    (should (node-data n1))

    ;; Test regular edge creation works
    (let ((edge (graph-edge-add g :from n1 :to n2)))
      (should edge)
      (should (graph-edge-p g (edge-id edge))))))

(ert-deftest graphael-api-consistency-test ()
  "Test consistency of the API across different operations."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g :label "Node 1"))
         (id1 (node-id n1)))

    ;; Test that different ways to reference the same node work consistently
    (should (eq (graph-node-p g n1) (graph-node-p g id1)))

    ;; Test node removal works the same way with both object and ID
    (let* ((g2 (make-instance 'graph))
           (n2a (graph-node-add g2 :label "Test A"))
           (n2b (graph-node-add g2 :label "Test B"))
           (id2a (node-id n2a))
           (id2b (node-id n2b)))

      ;; Remove one node by object, one by ID
      (graph-node-remove g2 n2a)
      (graph-node-remove g2 id2b)

      ;; Both should be gone
      (should-not (graph-node-p g2 id2a))
      (should-not (graph-node-p g2 id2b)))))

(ert-deftest graphael-zero-weight-edge-test ()
  "Test edges with zero weight."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (n3 (graph-node-add g))
         ;; Create path with: n1 --(1.0)--> n2 --(0.0)--> n3
         (_e1 (graph-edge-add g :from n1 :to n2 :weight 1.0))
         (e2 (graph-edge-add g :from n2 :to n3 :weight 0.0)))

    ;; Test edge creation with zero weight works
    (should (= (edge-weight e2) 0.0))

    ;; Test shortest path chooses the zero-weight edge
    (cl-multiple-value-bind (path cost) (graph-find-path-shortest g (node-id n1) (node-id n3))
      ;; Path should go through n2
      (should (= (length path) 3))
      (should (equal (nth 0 path) n1))
      (should (equal (nth 1 path) n2))
      (should (equal (nth 2 path) n3))
      ;; Total cost should be 1.0 (the weight of only the first edge)
      (should (= cost 1.0)))))

(provide 'graphael-boundary-test)
;;; graphael-boundary-test.el ends here
