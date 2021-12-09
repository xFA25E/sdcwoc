;;; sdcwoc.el --- Sdcv interface                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: stardict, sdcv, docs, processes
;; URL: https://github.com/xFA25E/sdcwoc
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; View SDCV dictionaries

;;; Code:

;;;; REQUIRES

(require 'ewoc)
(require 'map)
(require 'seq)
(require 'subr-x)

;;;; CUSTOMIZATION OPTIONS

(defgroup sdcwoc nil
  "Query SDCV dictionary."
  :group 'external
  :group 'processes
  :group 'applications)

;;;;; VARIABLES

(defcustom sdcwoc-buffer-name
  "*SDCWOC*"
  "Default buffer name for Sdcwoc mode."
  :type 'string
  :group 'sdcwoc)

;;;;; FACES

(defface sdcwoc-dictionary
  '((t :inherit font-lock-constant-face))
   "SDCV dictionary face."
   :group 'sdcwoc)

(defface sdcwoc-word
  '((t :inherit font-lock-string-face))
   "SDCV word face."
   :group 'sdcwoc)

;;;; VARIABLES

(defvar sdcwoc--history
  nil
  "`sdcwoc' query history.")

(defvar-local sdcwoc--ewoc
  nil
  "SDCWOC ewoc data.")

;;;; UTILS

(defun sdcwoc--read-query ()
  "Read SDCV query with word at point."
  (let* ((default-query (thing-at-point 'word))
         (default-part (if default-query (format " (default %s)" default-query) ""))
         (prompt (format "SDCV search {/fuzzy,|full,?*}%s: " default-part)))
    (read-string prompt nil 'sdcwoc--history default-query)))

(defun sdcwoc--shift-string (string &optional n)
  "Shift STRING to the right by N spaces.
N by default is 2."
  (replace-regexp-in-string (rx (or bos bol)) (make-string (or n 2) ?\ ) string))

(defun sdcwoc--set-state-at-point (state)
  "Set state of entry at point to STATE."
  (when-let ((node (ewoc-locate sdcwoc--ewoc)))
    (setf (cdr (ewoc-data node)) state)
    (with-silent-modifications
      (ewoc-invalidate sdcwoc--ewoc node))
    node))

(defun sdcwoc--set-all-states (state)
  "Set all states of all entries to STATE."
  (ewoc-map (lambda (data) (setf (cdr data) state)) sdcwoc--ewoc))

(defun sdcwoc--make-node-visible (node)
  "Make ewoc NODE visible if part if it is hidden after screen."
  (let ((next-node-location (if-let ((next-node (ewoc-next sdcwoc--ewoc node)))
                                (ewoc-location next-node)
                              (point-max))))
    (unless (pos-visible-in-window-p next-node-location)
          (let ((node-line-number (line-number-at-pos (ewoc-location node)))
                (next-node-line-number (line-number-at-pos next-node-location)))
            (recenter (- (min (- next-node-line-number node-line-number) (window-height))))))))

(defun sdcwoc--search (query)
  "Search QUERY in SDCV dictionaries."
  (with-temp-buffer
    (save-excursion
      (call-process "sdcv" nil t nil "-jn" "--utf8-input" "--utf8-output" query))
    (json-parse-buffer)))

(defun sdcwoc--draw (data)
  "Draw ewoc DATA with SDCV entry."
  (pcase-let* ((`(,entry . ,state) data)
               ((map ("dict" dictionary) ("word" word)) entry))
    (insert "--> " (propertize dictionary 'face 'sdcwoc-dictionary) " (" (propertize word 'face 'sdcwoc-word) ")")
    (pcase state
      (:hidden (insert "..."))
      ((let definition (sdcwoc--shift-string (string-trim (map-elt entry "definition"))))
       (insert "\n\n")
       (pcase state
         (:full (insert definition))
         (:partial (insert (string-join (seq-take (split-string definition "\n" t) 2) "\n") "...")))))
    (insert "\n")))

;;;; BINDINGS

(easy-mmode-defmap sdcwoc-mode-map
  '(("e" . sdcwoc-expand)
    ("E" . sdcwoc-expand-all)
    ("t" . sdcwoc-partial)
    ("T" . sdcwoc-partial-all)
    ("d" . sdcwoc-hide)
    ("D" . sdcwoc-hide-all)
    ("n" . sdcwoc-next-entry)
    ("p" . sdcwoc-previous-entry))
  "Keymap for `sdcwoc-mode'.")

;;;; COMMANDS

(define-derived-mode sdcwoc-mode special-mode "SDCWOC"
  "Mode used to draw SDCV entries."
  :group 'sdcwoc)

(defun sdcwoc (query)
  "Search QUERY in sdcv dictionaries."
  (interactive (list (sdcwoc--read-query)))
  (let ((buffer (get-buffer sdcwoc-buffer-name)))
    (unless buffer
      (with-current-buffer (setq buffer (get-buffer-create sdcwoc-buffer-name))
        (with-silent-modifications
          (sdcwoc-mode)
          (setq sdcwoc--ewoc (ewoc-create #'sdcwoc--draw)))))
    (with-current-buffer buffer
      (with-silent-modifications
        (ewoc-filter sdcwoc--ewoc #'ignore)
        (seq-do (lambda (entry) (ewoc-enter-last sdcwoc--ewoc (cons entry :partial)))
                (sdcwoc--search query))))
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-same-window)))))

(defun sdcwoc-expand ()
  "Expand SDCV entry at point."
  (interactive)
  (sdcwoc--make-node-visible (sdcwoc--set-state-at-point :full)))

(defun sdcwoc-expand-all ()
  "Expand all SDCV entries."
  (interactive)
  (sdcwoc--set-all-states :full))

(defun sdcwoc-partial ()
  "Expand partially SDCV entry at point."
  (interactive)
  (sdcwoc--make-node-visible (sdcwoc--set-state-at-point :partial)))

(defun sdcwoc-partial-all ()
  "Expand partially all SDCV entries."
  (interactive)
  (sdcwoc--set-all-states :partial))

(defun sdcwoc-hide ()
  "Hide SDCV entry at point."
  (interactive)
  (sdcwoc--set-state-at-point :hidden))

(defun sdcwoc-hide-all ()
  "Hide all SDCV entries."
  (interactive)
  (sdcwoc--set-all-states :hidden))

(defun sdcwoc-next-entry (n)
  "Go to next Nth SDCV entry."
  (interactive "p")
  (sdcwoc--make-node-visible (ewoc-goto-next sdcwoc--ewoc n)))

(defun sdcwoc-previous-entry (n)
  "Go to previous Nth SDCV entry."
  (interactive "p")
  (ewoc-goto-prev sdcwoc--ewoc n))

;;;; PROVIDE

(provide 'sdcwoc)
;;; sdcwoc.el ends here
