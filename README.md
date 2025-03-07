# Graphael - Graph Library for Emacs

Graphael is an object-based graph data structure library for Emacs Lisp
that provides a useful API to create, manipulate, and analyze directed
graphs. Features include:

- UUID-based node and edge identification
- Directed graph representation with weighted edges
- Store arbitrary metadata on nodes and edges
- Efficient adjacency lookup with pre-computed indices
- Graph operations to merge, clone, and extract a subgraph
- Algorithms such as pathfinding, cycle detection, and topological sort
- Extensive test coverage and usage examples

## Install

Clone the repository:

```bash
$ git clone https://github.com/lamberta/graphael.git
```

Load the library in your Emacs Lisp project:

```elisp
(add-to-list 'load-path "/path/to/graphael")
(require 'graphael)
```

## Basic usage

### Create a graph and add nodes

```elisp
;; Create a new graph
(setq g (make-instance 'graph))

;; Add nodes
(setq node1 (graph-node-add g :label "Start"))
(setq node2 (graph-node-add g :label "Middle"))
(setq node3 (graph-node-add g :label "End"))

;; Add edges between nodes
(setq edge1 (graph-edge-add g :from node1 :to node2))
(setq edge2 (graph-edge-add g :from node2 :to node3 :weight 2.5))

;; Add metadata to nodes/edges
(graph-node-attr-put node1 :category "entry-point")
(graph-edge-attr-put edge2 :requires-permission t)
```

### Get node information

```elisp
;; Get a node by its ID
(setq retrieved-node (graph-node-get g (node-id node1)))

;; Get node properties
(node-label retrieved-node)  ; => "Start"
(graph-node-attr-get retrieved-node :category)  ; => "entry-point"

;; Get connected nodes (neighbors)
(graph-neighbors g (node-id node1))    ; => List of successor nodes
(graph-neighbors g (node-id node2) t)  ; => List of predecessor nodes
```

## Advanced usage

### Find shortest path

```elisp
(cl-multiple-value-bind (path distance)
  ;; Use Dijkstra's algorithm
  (graph-find-path-shortest g node1 node3)
  (message "Length: %d nodes, Distance: %.1f" (length path) distance))
```

### Find path between nodes using A*

```elisp
;; Assume grid layout and set (x . y) coordinates for each node
(graph-node-attr-put node1 :coords '(0 . 0))
(graph-node-attr-put node2 :coords '(2 . 3))
(graph-node-attr-put node3 :coords '(5 . 3))

;; Create the Manhattan distance heuristic function
(let ((heuristic (graph-astar-manhattan-distance g
                   (lambda (node)
                     "Helper function to extract coordinates from a node"
                     (graph-node-attr-get node :coords)))))
  (cl-multiple-value-bind (path distance)
    (graph-find-path-astar g node1 node3 heuristic)
    (message "Length: %d nodes, Distance: %.1f" (length path) distance)))
```

### Graph operations

```elisp
;; Create a copy of a graph
(setq g2 (graph-clone g))

;; Create a subgraph containing only specific nodes
(setq subgraph (graph-subgraph g2 (list node1 node2)))

;; Create a subgraph using a filter function
(setq filtered-graph (graph-subgraph g
                       (lambda (node)
                         (string-prefix-p "Start" (node-label node)))))

;; Merge two graphs (returns a new graph by default)
(setq merged-graph (graph-merge g g2))

;; Merge graphs in-place (modifies first graph)
(graph-merge g g2 t)
```

### Graph analysis

```elisp
;; Detect cycles in a graph
(when (graph-cycle-p g)
  (message "Graph contains at least one cycle"))

;; Check if graph is connected
(graph-is-connected-p g)

;; Perform topological sort (for DAGs)
(graph-topological-sort g)

;; Get graph statistics
(graph-print-stats g)
```

### Transaction safety

```elisp
;; Execute multiple operations atomically
(with-graph-transaction (g)
  (graph-node-remove g node1)
  (graph-node-add g :label "Replacement"))
```

## Developer commands

Byte-compile Elisp files for compilation warnings:

```bash
$ make
```

Run tests:

```bash
$ make test
```

Use a different version of Emacs:

```bash
$ EMACS=/path/to/bin/emacs make test
```

## License

This project is licensed under the GNU General Public License v3.0 -
see the LICENSE file for details.
