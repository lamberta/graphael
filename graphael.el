;;; graphael.el --- Object-based graph data structure library -*- lexical-binding: t; -*-

;; Author: Billy Lamberta
;; URL: https://github.com/lamberta/graphael
;; Version: 0.1.0
;; Created: Feb 2025
;; Updated: Mar 2025
;; Package-Requires: ((emacs "26.2"))
;; Keywords: data, graph, network

;; This file is not part of GNU Emacs

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

;; Graphael provides an EIEIO-based graph data structure to represent and
;; manipulate directed graphs. Each node and edge has a UUID identifier and can
;; store arbitrary metadata. See README.md for more examples.
;;
;; Basic usage:
;;   ;; Create a new graph
;;   (setq g (make-instance 'graph))
;;
;;   ;; Add nodes and connect them with an edge
;;   (setq n1 (graph-node-add g))
;;   (setq n2 (graph-node-add g))
;;   (graph-edge-add g :from n1 :to n2)
;;
;;   ;; Find paths between nodes
;;   (graph-find-path-shortest g n1 n2)

;;; Code:

(require 'graphael-core)
(require 'graphael-operations)
(require 'graphael-algorithms)

(defgroup graphael nil
  "Graph data structure for Emacs."
  :group 'data
  :prefix "graph-")

(provide 'graphael)
;;; graphael.el ends here
