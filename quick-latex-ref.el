;;; quick-latex-ref.el --- Efficient LaTeX referencing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/quick-latex-ref
;; Version: 0.2.2
;; Keywords: convenience

;; Package-Requires: ((emacs "27.1") (auctex "12.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; While writing in LaTeX, you often want to reference (through the
;; "\ref" macro) things just above or just below point.  Some LaTeX
;; packages offer special macros that are always expanded in this
;; sense (for example, a macro "\Next" that expands into a
;; "\ref{LABEL}" if the first label macro after "\Next" is
;; "\label{LABEL}").  But you have to be mindful, when rewriting
;; things, not to compromise this dependency (for example, by
;; inserting another "\label" macro in between), otherwise you will
;; end up with wrong references.

;; This package gives you an efficient way to avoid these problems, by
;; providing commands that let you insert a "\ref" macro with the same
;; label as the first "\label" macro preceding or following point.
;; There are three entry points (three interactive commands):

;; + quick-latex-ref
;; + quick-latex-ref-previous
;; + quick-latex-ref-next

;; All of these commands can be called with the prefix argument, in
;; which case only the label itself (i.e., the argument of the
;; "\label" macro) is inserted at point.  Furthermore, if the variable
;; quick-latex-ref-only-label-if-in-argument is set to t, only the
;; label (and not a full "\ref" macro) is inserted if point is already
;; inside the braces that determine the argument of a "\ref" macro.

;; All of these three commands enter a loop where you can cycle
;; through the "\label" macros in the buffer, in order of proximity to
;; point.  The last two commands enter the loop starting with the
;; first preceding and following "\label", respectively.  Once you are
;; in the loop, you move up and down with the characters defined as
;; the values of

;; + quick-latex-ref-previous-key ("p" by default)
;; + quick-latex-ref-next-key ("n" by default).

;; "\label" macros that are in comments are completely ignored.  If
;; the variable quick-latex-ref-show-context is non-nil (as per
;; default), the context of the current "\label" candidate is shown in
;; the echo area, to help you determine whether that is indeed the
;; label you are interested in.  The context is the three lines
;; (visual lines, if you are in visual-line-mode) where the candidate
;; "\label" is in.

;; The buffer is immediately updated with the appropriate "\ref" macro
;; at point as you walk up and down the list of labels.  Both the
;; "\ref" macro at point and the currently selected "\label" macro are
;; highlighted with the faces quick-latex-ref-at-point and
;; quick-latex-current-target respectively (by default, they inherit
;; the same face that the theme you are using uses to highlight the
;; active region).

;; There is a third action you can take while being in the loop,
;; through the character specified as the value of
;; quick-latex-ref-goto-key ("." by default).  This will move point to
;; the currently selected label, save the original position (from
;; which you entered the loop) in the mark ring, and undo any
;; insertion there.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'tex)
(require 'latex)

(defgroup quick-latex-ref nil
  "Quick-latex-ref configuration options."
  :prefix "quick-latex-ref"
  :group 'convenience)

(defcustom quick-latex-ref-previous-key ?p
  "Select previous label in the loop.

The value of this variable must be a character to select the
previous label in the loop entered by `quick-latex-ref-previous',
`quick-latex-ref-next' or `quick-latex-ref'."
  :type 'character)

(defcustom quick-latex-ref-next-key ?n
  "Select next label in the loop.

The value of this variable must be a character to select the next
label in the loop entered by `quick-latex-ref-previous',
`quick-latex-ref-next' or `quick-latex-ref'."
  :type 'character)

(defcustom quick-latex-ref-goto-key ?.
  "Go to currently selected label.

The value of this variable must be a character."
  :type 'character)

(defcustom quick-latex-ref-show-context t
  "If t, show context of candidate label in the echo area."
  :type 'boolean)

(defcustom quick-latex-ref-only-label-if-in-argument t
  "If t, insert no label unless point is between curly braces."
  :type 'boolean)

(defface quick-latex-ref-current-target '((nil :inherit region))
  "Face for highlighting the currently selected \\label macro.

See Info node `(emacs) Face Customization' for more information
about customizing faces and `list-faces-display' for a list of
all faces."
  :group 'quick-latex-ref)

(defface quick-latex-ref-at-point '((nil :inherit region))
  "Face for highlighting the \\ref macro at point.

See Info node `(emacs) Face Customization' for more information
about customizing faces and `list-faces-display' for a list of
all defined faces."
  :group 'quick-latex-ref)

(defun quick-latex-ref--get (buf direction)
  "Get information from the first label in direction DIRECTION.

DIRECTION is either \\='backward\\=' or \\='forward\\='.

BUF is the buffer in which the search is performed.

The return value is a list of four elements:

+ the label (the argument of the \"\\label\" macro, a string)

+ the beginning of the \"\\label\" macro (a buffer position)

+ the end of the \"\\label\" macro (a buffer position)

+ the context of the \"\\label\" macro (a string): an empty
string, if `quick-latex-ref-show-context' is nil, otherwise the
lines containing the \"\\label\" macro (visual lines, if
`visual-line-mode' is non nil)."
  (let ((fn (lambda ()
              (TeX-search-unescaped "\\(\\\\label\\){[[:space:]]*[^}]"
                                    direction t nil t)))
        inv lab found targ-b targ-e)
    (with-current-buffer buf
      (widen)
      (when outline-minor-mode (outline-show-all))
      (setq found (funcall fn))
      (while (TeX-in-comment)
        (setq found (funcall fn)))
      (when found
        (setq targ-b (match-beginning 0))
        (goto-char (1- (match-end 0)))
        (setq lab (string-trim
                   (buffer-substring (point)
                                     (save-excursion (forward-sexp)
                                                     (setq targ-e (point))))
                   "{" "}"))
        (list lab targ-b targ-e
              (when quick-latex-ref-show-context
                (let ((context-b (save-excursion (beginning-of-visual-line 0)))
                      (context-e (save-excursion (end-of-visual-line 2)
                                                 (point))))
                  (thread-last (buffer-substring context-b context-e)
                               (string-trim)
                               (replace-regexp-in-string "%" "%%")
                               (concat "\n\n")))))))))

(defun quick-latex-ref (&optional direction only-label)
  "Insert a reference at point.

If the prefix argument ONLY-LABEL is non-nil, insert just the
string that acts as a tag (the argument of the \"\\label{}\"
macro), otherwise insert the full \"\\ref{}\" macro.

Only the tag string is inserted regardless of the value of
ONLY-LABEL if `quick-latex-ref-only-label-if-in-argument' is
non-nil and point is inside the argument of a \"\\ref{}\" macro.

By calling this function the user enters a loop where they can
choose the label for the \"\\ref\" macro starting from the ones
closest to point.  If DIRECTION is nil, the loop waits for the
user to choose the direction to move to (with the keys specified
as the values of `quick-latex-ref-previous-key' and
`quick-latex-ref-next-key').  Otherwise, start from the first
\"\\label\" macro preceding, or following, point, depending on
whether DIRECTION is \\='backward\\=' or \\='forward\\='
respectively.

`quick-latex-ref-previous-key' and `quick-latex-ref-next-key'
can be repeated as much as needed to target the desired
\"\\label\" macro."
  (interactive "P")
  (push-mark)
  (let* ((instr (format "%s to go back, %s to go forward\n"
                        (key-description
                         (if (vectorp quick-latex-ref-previous-key)
                             quick-latex-ref-previous-key
                           (vector quick-latex-ref-previous-key)))
                        (key-description
                         (if (vectorp quick-latex-ref-next-key)
                             quick-latex-ref-next-key
                           (vector quick-latex-ref-next-key)))))
         (ind-buffer (clone-indirect-buffer nil nil t))
         (index 0)
         (between-braces (and quick-latex-ref-only-label-if-in-argument
                              (save-excursion
                                (let ((beg (line-beginning-position))
                                      (p (point))
                                      par)
                                  (when (search-backward "\\ref" beg t)
                                    (unless (TeX-escaped-p)
                                      (search-forward "{")
                                      (forward-char -1)
                                      (setq par (point))
                                      (ignore-errors (forward-sexp 1))
                                      (< par p (point))))))))
         (message-log-max 0)
         (dir (or direction
                  (let ((k (read-key instr)))
                    (cond ((eq k quick-latex-ref-previous-key) 'backward)
                          ((eq k quick-latex-ref-next-key) 'forward)
                          (t (user-error (format "%s is an invalid choice"
                                                 (char-to-string k))))))))
         (reading-chars t)
         (count-fn (lambda (i) (format "%s labels %s"
                                       (abs i)
                                       (if (< i 0) "up" "down"))))
         (b (point-marker)) e lab-left-marker lab-right-marker)
    (insert (if (or only-label between-braces) " " "\\ref{} "))
    (setq e (point-marker))
    (forward-char -1)
    (unless (or only-label between-braces)
      (setq lab-right-marker (point-marker))
      (setq lab-left-marker
            (save-excursion (forward-sexp -1) (point-marker))))
    (let ((at-point-ol (make-overlay b e)) targ-ol)
      (overlay-put at-point-ol 'face 'quick-latex-ref-at-point)
      (unwind-protect
          (while reading-chars
            (let* ((res (ignore-errors (quick-latex-ref--get ind-buffer dir)))
                   (lab (nth 0 res))
                   (targ-b (nth 1 res))
                   (targ-e (nth 2 res))
                   (ctxt (or (nth 3 res) "")))
              (if (not lab)
                  (message (concat instr (funcall count-fn index)
                                   ", no " (if (eq dir 'backward)
                                               "previous"
                                             "next")
                                   " label" ctxt))
                (setq targ-ol (make-overlay targ-b targ-e))
                (overlay-put targ-ol 'face 'quick-latex-ref-current-target)
                (setq index (if (eq dir 'forward) (1+ index) (1- index)))
                (when (= index 0)
                  (if (eq dir 'forward) (setq index 1) (setq index -1)))
                (message (concat instr (funcall count-fn index) ctxt))
                (if (or only-label between-braces)
                    (progn (delete-region b (point))
                           (insert lab))
                  (save-excursion
                    (delete-region (1+ lab-left-marker) (1- lab-right-marker))
                    (goto-char (1+ lab-left-marker))
                    (insert lab))))
              (let* ((ch (read-key))
                     (gt (eq ch quick-latex-ref-goto-key))
                     (pr (eq ch quick-latex-ref-previous-key))
                     (nx (eq ch quick-latex-ref-next-key)))
                (when targ-ol (delete-overlay targ-ol))
                (cond (gt (setq reading-chars nil)
                          (goto-char (nth 2 res))
                          (when (invisible-p (nth 2 res)) (outline-show-entry))
                          (delete-region b e))
                      (pr (setq dir 'backward))
                      (nx (setq dir 'forward))
                      (t (setq reading-chars nil)
                         (when (and (characterp ch)
                                    (eq 'self-insert-command
                                        (lookup-key global-map
                                                    (char-to-string ch))))
                           (funcall #'self-insert-command 1 ch)))))))
        (kill-buffer ind-buffer)
        (delete-region (point) (1+ (point)))
        (delete-overlay at-point-ol)))))

(defun quick-latex-ref-previous (&optional only-label)
  "Call `quick-latex-ref' starting from the first label before point.

If the prefix argument ONLY-LABEL is non-nil, insert just the
string that acts as a tag (the argument of the \"\\label{}\"
macro), otherwise insert the full \"\\ref{}\" macro.

Only the tag string is inserted regardless of the value of
ONLY-LABEL if `quick-latex-ref-only-label-if-in-argument' is
non-nil and point is inside the argument of a \"\\ref{}\" macro."
  (interactive "P")
  (quick-latex-ref 'backward only-label))

(defun quick-latex-ref-next (&optional only-label)
  "Call `quick-latex-ref' starting from the first label after point.

If the prefix argument ONLY-LABEL is non-nil, insert just the
string that acts as a tag (the argument of the \"\\label{}\"
macro), otherwise insert the full \"\\ref{}\" macro.

Only the tag string is inserted regardless of the value of
ONLY-LABEL if `quick-latex-ref-only-label-if-in-argument' is
non-nil and point is inside the argument of a \"\\ref{}\" macro."
  (interactive "P")
  (quick-latex-ref 'forward only-label))

(provide 'quick-latex-ref)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; quick-latex-ref.el ends here
