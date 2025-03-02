;;; graphael-algorithms-test.el --- Tests for graphael-algorithms.el -*- lexical-binding: t -*-

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
;; Tests for graphael-algorithms.el

(require 'ert)
(require 'graphael-core)
(require 'graphael-operations)
(require 'graphael-algorithms)

(ert-deftest graphael-cycle-detection-test ()
  "Test cycle detection functionality."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (n3 (graph-node-add g)))

    ;; No cycle initially
    (should-not (graph-cycle-p g))

    ;; Create cycle: n1 -> n2 -> n3 -> n1
    (graph-edge-add g :from n1 :to n2)
    (graph-edge-add g :from n2 :to n3)
    (graph-edge-add g :from n3 :to n1)

    (should (graph-cycle-p g))))

(defun graphael-setup-test-grid ()
  "Set up a 3x3 grid for pathfinding tests."
  (let* ((g (make-instance 'graph))
         (nodes (cl-loop for y from 0 to 2
                  append (cl-loop for x from 0 to 2
                           collect (graph-node-add g))))
         (coords (make-hash-table :test 'equal)))

    ;; Assign coordinates to nodes
    (cl-loop for node in nodes
      for i from 0
      for x = (mod i 3)
      for y = (/ i 3)
      do (setf (node-data node) (cons x y))
      do (puthash (node-id node) (cons x y) coords))

    ;; Horizontal connections
    (graph-edge-add g :from (nth 0 nodes) :to (nth 1 nodes))
    (graph-edge-add g :from (nth 1 nodes) :to (nth 2 nodes))
    (graph-edge-add g :from (nth 3 nodes) :to (nth 4 nodes))
    (graph-edge-add g :from (nth 4 nodes) :to (nth 5 nodes))
    (graph-edge-add g :from (nth 6 nodes) :to (nth 7 nodes))
    (graph-edge-add g :from (nth 7 nodes) :to (nth 8 nodes))

    ;; Vertical connections
    (graph-edge-add g :from (nth 0 nodes) :to (nth 3 nodes))
    (graph-edge-add g :from (nth 3 nodes) :to (nth 6 nodes))
    (graph-edge-add g :from (nth 1 nodes) :to (nth 4 nodes))
    (graph-edge-add g :from (nth 4 nodes) :to (nth 7 nodes))
    (graph-edge-add g :from (nth 2 nodes) :to (nth 5 nodes))
    (graph-edge-add g :from (nth 5 nodes) :to (nth 8 nodes))

    ;; Add diagonal shortcut
    (let ((diagonal-edge
            (graph-edge-add g :from (nth 0 nodes) :to (nth 4 nodes))))
      (setf (edge-weight diagonal-edge) 0.9))

    ;; Return values
    (list g nodes coords)))

(ert-deftest graphael-basic-pathfinding-test ()
  "Test basic pathfinding with depth-first search."
  (let* ((setup (graphael-setup-test-grid))
         (g (nth 0 setup))
         (nodes (nth 1 setup))
         (start-node (nth 0 nodes))
         (goal-node (nth 8 nodes))
         (start-id (node-id start-node))
         (goal-id (node-id goal-node)))

    ;; Test DFS path finding
    (let ((path (graph-find-path g start-id goal-id)))
      ;; Path should exist
      (should path)
      ;; Path should start at start node and end at goal node
      (should (equal (car path) start-node))
      (should (equal (car (last path)) goal-node)))))

(ert-deftest graphael-dijkstra-pathfinding-test ()
  "Test Dijkstra's shortest path algorithm."
  (let* ((setup (graphael-setup-test-grid))
         (g (nth 0 setup))
         (nodes (nth 1 setup))
         (start-node (nth 0 nodes))
         (goal-node (nth 8 nodes))
         (start-id (node-id start-node))
         (goal-id (node-id goal-node)))

    ;; Test Dijkstra's algorithm using multiple values
    (cl-multiple-value-bind (path cost) (graph-find-path-shortest g start-id goal-id)
      ;; Path should exist
      (should path)
      ;; Path should start at start node and end at goal node
      (should (equal (car path) start-node))
      (should (equal (car (last path)) goal-node))
      ;; Path should have a reasonable cost
      (should (numberp cost)))))

(ert-deftest graphael-astar-pathfinding-test ()
  "Test A* pathfinding algorithm."
  (let* ((setup (graphael-setup-test-grid))
         (g (nth 0 setup))
         (nodes (nth 1 setup))
         (coords (nth 2 setup))
         (start-node (nth 0 nodes))
         (goal-node (nth 8 nodes))
         (start-id (node-id start-node))
         (goal-id (node-id goal-node))
         (get-coord (lambda (node)
                      (if (stringp node)
                        (gethash node coords)
                        (node-data node))))
          (manhattan (graph-astar-manhattan-distance g get-coord)))

    ;; Test A* with Manhattan distance heuristic
    (cl-multiple-value-bind (path cost) (graph-find-path-astar g start-id goal-id manhattan)
      ;; Path should exist
      (should path)
      ;; Path should start at start node and end at goal node
      (should (equal (car path) start-node))
      (should (equal (car (last path)) goal-node))

      ;; Compare with Dijkstra cost
      (cl-multiple-value-bind (_ dijkstra-cost) (graph-find-path-shortest g start-id goal-id)
        (should (= cost dijkstra-cost))))))

(ert-deftest graphael-pathfinding-error-test ()
  "Test error handling in pathfinding functions."
  (let* ((setup (graphael-setup-test-grid))
         (g (nth 0 setup))
         (nodes (nth 1 setup))
         (coords (nth 2 setup))
         (start-node (nth 0 nodes))
         (start-id (node-id start-node))
         (manhattan (graph-astar-manhattan-distance g
                      (lambda (node)
                        (if (stringp node)
                          (gethash node coords)
                          (node-data node))))))

    ;; Test invalid cases
    (should-error (graph-find-path g start-id "invalid-id"))
    (should-error (graph-find-path-shortest g start-id "invalid-id"))
    (should-error (graph-find-path-astar g start-id "invalid-id" manhattan))

    ;; Test nodes with no path between them
    (let ((isolated-node (graph-node-add g)))
      (should-not (graph-find-path g start-id (node-id isolated-node)))

      ;; Test shortest path with no solution
      (cl-multiple-value-bind (path cost)
        (graph-find-path-shortest g start-id (node-id isolated-node))
        (should-not path)
        (should-not cost))

      ;; Test A* with no solution
      (cl-multiple-value-bind (path cost)
        (graph-find-path-astar g start-id (node-id isolated-node) manhattan)
        (should-not path)
        (should-not cost)))))

(ert-deftest graphael-topological-sort-test ()
  "Test topological sorting of a directed acyclic graph."
  (let* ((g (make-instance 'graph))
         (n1 (graph-node-add g))
         (n2 (graph-node-add g))
         (n3 (graph-node-add g))
         (n4 (graph-node-add g))
         (n5 (graph-node-add g))
         (id1 (node-id n1))
         (id2 (node-id n2))
         (id3 (node-id n3))
         (id4 (node-id n4))
         (id5 (node-id n5)))

    ;; Create a DAG with dependencies:
    ;; n5 depends on n2 and n4
    ;; n4 depends on n1
    ;; n2 depends on n1 and n3
    (graph-edge-add g :from n1 :to n2)  ; n1 -> n2
    (graph-edge-add g :from n1 :to n4)  ; n1 -> n4
    (graph-edge-add g :from n3 :to n2)  ; n3 -> n2
    (graph-edge-add g :from n2 :to n5)  ; n2 -> n5
    (graph-edge-add g :from n4 :to n5)  ; n4 -> n5

    ;; No cycles should exist
    (should-not (graph-cycle-p g))

    ;; Get topological order
    (let* ((sorted-nodes (graph-topological-sort g))
           (order (mapcar #'node-id sorted-nodes)))

      ;; Helper function to check if node a comes before node b in order
      (cl-flet ((comes-before (a b)
                  (< (seq-position order a)
                    (seq-position order b))))

        ;; Check all edge constraints
        (should (comes-before id1 id2)) ; n1 -> n2
        (should (comes-before id1 id4)) ; n1 -> n4
        (should (comes-before id3 id2)) ; n3 -> n2
        (should (comes-before id2 id5)) ; n2 -> n5
        (should (comes-before id4 id5)) ; n4 -> n5

        ;; All nodes should be present in the result
        (should (= (length order) 5))
        (should (member id1 order))
        (should (member id2 order))
        (should (member id3 order))
        (should (member id4 order))
        (should (member id5 order))))

    ;; Add a cycle and verify that topological sort errors
    (graph-edge-add g :from n5 :to n1)  ; Creates cycle: n1 -> n2 -> n5 -> n1
    (should (graph-cycle-p g))
    (should-error (graph-topological-sort g))))

(ert-deftest graphael-connectivity-test ()
  "Test graph connectivity detection."
  (let ((g (make-instance 'graph)))
    ;; Empty graph is not connected
    (should-not (graph-is-connected-p g))

    ;; Single node graph
    (let ((n1 (graph-node-add g)))
      (should (graph-is-connected-p g))

      ;; Add disconnected components
      (let* ((n2 (graph-node-add g))
             (n3 (graph-node-add g))
             (n4 (graph-node-add g)))
        ;; Nodes exist but aren't connected - should not be connected
        (should-not (graph-is-connected-p g))

        ;; Connect some nodes but still have disconnected components
        (graph-edge-add g :from n1 :to n2)
        (graph-edge-add g :from n3 :to n4)
        (should-not (graph-is-connected-p g))

        ;; Connect all components to make a connected graph
        (graph-edge-add g :from n2 :to n3)
        (should (graph-is-connected-p g))))))

(provide 'graphael-algorithms-test)
;;; graphael-algorithms-test.el ends here
