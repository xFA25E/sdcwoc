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

;; View SDCV dictionaries.  M-x sdcwoc

;;; Code:

;;;; REQUIRES

(require 'ewoc)
(require 'map)
(require 'seq)
(require 'shr)
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

(defcustom sdcwoc-mode-hook nil
  "Hook run after entering SDCWOC mode."
  :type 'hook
  :group 'sdcwoc)

(defcustom sdcwoc-categories
  (let* ((data (with-temp-buffer
                 (call-process "sdcv" nil t nil "-jl" "--utf8-output")
                 (goto-char (point-min))
                 (unwind-protect (json-parse-buffer)
                   (kill-buffer (current-buffer))))))
    (list (cons "all" (seq-map (lambda (m) (map-elt m "name")) data))))
  "Dictionary categories.
It is an alist which contains names associated with dictionary
lists.  A user is prompted for category before search."
  :type '(alist :tag "Categories"
                :key-type (string :tag "Category")
                :value-type (repeat :tag "Dictionaries"
                                    (string :tag "Dictionary")))
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

(defface sdcwoc-header
  '((t :inherit font-lock-variable-name-face))
  "SDCV header face."
  :group 'sdcwoc)

(defface sdcwoc-dots
  '((t :inherit font-lock-type-face :underline t))
  "Face used to visualize sdcwoc dots."
  :group 'sdcwoc)

;;;; VARIABLES

(defvar sdcwoc--dots
  (propertize "..." 'face 'sdcwoc-dots)
  "Dots.")

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
         (prompt (format-prompt "SDCV {/fuzzy,|full,?*}" default-query)))
    (read-string prompt nil 'sdcwoc--history default-query)))

(defun sdcwoc--read-category ()
  "Read category to query.
See `sdcwoc-categories'."
  (completing-read "SDCV category: " sdcwoc-categories nil t))

(defun sdcwoc--search (query &optional category)
  "Search QUERY in SDCV dictionaries.
Use CATEGORY for a list of dictionaries.  See
`sdcwoc-categories'."
  (set-process-sentinel
   (apply #'start-process "sdcv" (generate-new-buffer "*sdcwoc*")
          "sdcv" "-jn" "--utf8-input" "--utf8-output" query
          (seq-map (apply-partially #'concat "--use-dict=")
                   (map-elt sdcwoc-categories category)))
   (lambda (process event)
     (when (string= "finished\n" event)
       (let ((entries (with-current-buffer (process-buffer process)
                        (goto-char (point-min))
                        (unwind-protect (json-parse-buffer)
                          (kill-buffer (current-buffer))))))
         (with-current-buffer (get-buffer-create sdcwoc-buffer-name)
           (sdcwoc-mode)
           (let ((header (propertize query 'face 'sdcwoc-header)))
             (ewoc-set-hf sdcwoc--ewoc (concat header "\n") ""))
           (seq-doseq (entry entries)
             (map-put! entry "state" :partial)
             (ewoc-enter-last sdcwoc--ewoc entry))
           (pop-to-buffer (current-buffer))))))))

(defun sdcwoc--set-state-at-point (state)
  "Set state of entry at point to STATE."
  (when-let ((node (ewoc-locate sdcwoc--ewoc)))
    (map-put! (ewoc-data node) "state" state)
    (ewoc-invalidate sdcwoc--ewoc node)))

(defun sdcwoc--set-all-states (state)
  "Set all states of all entries to STATE."
  (ewoc-map (lambda (data) (map-put! data "state" state)) sdcwoc--ewoc))

(defun sdcwoc--format-definition (definition)
  "Format DEFINITION before drawing."
  (let ((fill-column (window-width))
        (sentence-end-double-space nil))
    (with-temp-buffer
      (insert (string-trim definition) "\n")
      (indent-rigidly (point-min) (point-max) 2)
      (goto-char (point-min))
      (cl-loop for start = (point)
               do (end-of-line)
               for end = (point)
               if (< fill-column (- end start))
               do (fill-region start end nil t)
               do (forward-line)
               until (eobp))
      (buffer-string))))

(defun sdcwoc--format-definition-as-html (definition)
  "Format DEFINITION as html before drawing."
  (with-temp-buffer
    (insert definition)
    (let ((shr-indentation 2))
      (shr-render-region (point-min) (point-max)))
    (buffer-string)))

;;;; DRAW

(defun sdcwoc--draw-header (data)
  "Draw header with ewoc DATA."
  (pcase-let (((map ("header" header)) data))
    (unless header
      (pcase-let (((map ("dict" dict) ("word" word)) data))
        (setq header (concat "--> " (propertize dict 'face 'sdcwoc-dictionary)
                             " (" (propertize word 'face 'sdcwoc-word) ")")))
      (map-put! data "header" header))
    (insert header)))

(defun sdcwoc--draw-full (data)
  "Draw full word definition with ewoc DATA."
  (pcase-let (((map ("full" full)) data))
    (unless full
      (pcase-let (((map ("definition" definition)) data))
        (setq full (sdcwoc--format-definition definition)))
      (map-put! data "full" full))
    (insert "\n\n" full)))

(defun sdcwoc--draw-partial (data)
  "Draw partial word definition with ewoc DATA."
  (pcase-let (((map ("partial" partial)) data))
    (unless partial
      (pcase-let (((map ("full" full)) data))
        (unless full
          (pcase-let (((map ("definition" definition)) data))
            (setq full (sdcwoc--format-definition definition)))
          (map-put! data "full" full))
        (if-let* ((pos1 (string-match-p "\n" full))
                  (pos2 (string-match-p "\n" full (1+ pos1)))
                  (pos3 (string-match-p "\n" full (1+ pos2))))
            (setq partial (concat (substring full 0 pos2) "\n  " sdcwoc--dots))
          (setq partial full)))
      (map-put! data "partial" partial))
    (insert "\n\n" partial)))

(defun sdcwoc--draw (data)
  "Draw ewoc DATA with SDCV entry."
  (sdcwoc--draw-header data)
  (pcase (map-elt data "state")
    (:hidden (insert sdcwoc--dots))
    (:full (sdcwoc--draw-full data))
    (:partial (sdcwoc--draw-partial data)))
  (insert "\n"))

;;;; BINDINGS

(easy-mmode-defmap sdcwoc-mode-map
  '(("e" . sdcwoc-expand)
    ("E" . sdcwoc-expand-all)
    ("t" . sdcwoc-partial)
    ("T" . sdcwoc-partial-all)
    ("d" . sdcwoc-hide)
    ("D" . sdcwoc-hide-all)
    ("n" . sdcwoc-next-entry)
    ("p" . sdcwoc-previous-entry)
    ("m" . sdcwoc-format-as-html))
  "Keymap for `sdcwoc-mode'.")

;;;; COMMANDS

(define-derived-mode sdcwoc-mode special-mode "SDCWOC"
  "Mode used to draw SDCV entries."
  :group 'sdcwoc
  (buffer-disable-undo)
  (with-silent-modifications (erase-buffer))
  (setq sdcwoc--ewoc (ewoc-create #'sdcwoc--draw)))

;;;###autoload
(defun sdcwoc (query &optional category)
  "Search QUERY in sdcv dictionaries.
Prompt for CATEGORY by default.  If prefix argument is present,
search all dictionaries.  See `sdcwoc-categories' for CATEGORY."
  (interactive
   (let ((category (unless current-prefix-arg (sdcwoc--read-category))))
     (list (sdcwoc--read-query) category)))
  (sdcwoc--search query category))

(defun sdcwoc-next-entry (n)
  "Go to next Nth SDCV entry."
  (interactive "p")
  (ewoc-goto-next sdcwoc--ewoc n)
  (recenter 0))

(defun sdcwoc-previous-entry (n)
  "Go to previous Nth SDCV entry."
  (interactive "p")
  (ewoc-goto-prev sdcwoc--ewoc n))

(defun sdcwoc-expand ()
  "Expand SDCV entry at point."
  (interactive)
  (sdcwoc--set-state-at-point :full))

(defun sdcwoc-expand-all ()
  "Expand all SDCV entries."
  (interactive)
  (sdcwoc--set-all-states :full))

(defun sdcwoc-partial ()
  "Expand partially SDCV entry at point."
  (interactive)
  (sdcwoc--set-state-at-point :partial))

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

(defun sdcwoc-format-as-html ()
  "Format current word definition as html.
With prefix argument, return to previous formatting."
  (interactive)
  (when-let* ((node (ewoc-locate sdcwoc--ewoc))
              (data (ewoc-data node))
              (definition (map-elt data "definition")))
    (let ((full (unless current-prefix-arg
                  (sdcwoc--format-definition-as-html definition))))
      (map-put! data "full" full)
      (map-put! data "partial" nil))
    (ewoc-invalidate sdcwoc--ewoc node)))

;;;; PROVIDE

(provide 'sdcwoc)
;;; sdcwoc.el ends here
