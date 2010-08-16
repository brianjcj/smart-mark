;;; smart-mark.el --- Smart Mark for GNU Emacs

;; Copyright (C) 2010  Brian Jiang

;; Author: Brian Jiang <brianjcj@gmail.com>
;; Keywords: completion, convenience
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Simulate some vim command. Mark context in the (), <>, [], "", '', html tag,
;; mark line, mark....
;;
;;; Uage:
;; (require 'smart-mark)
;; type C-M-m and then following the prompt.
;;

;;; Code:


(provide 'smart-mark)

(defvar smart-mark-chars-open  (list ?\( ?\< ?\{ ?\[ ))
(defvar smart-mark-chars-close (list ?\) ?\> ?\} ?\] ))
(defvar smart-mark-chars-quote (list ?\" ?\' ))
(defvar smart-mark-chars (append smart-mark-chars-open smart-mark-chars-close smart-mark-chars-quote))
(defvar smart-mark-chars-pairs (mapcar* 'cons smart-mark-chars-close smart-mark-chars-open))


(defvar myutils-temp-syntax-table (make-char-table 'syntax-table nil))
(modify-syntax-entry ?\' "\"" myutils-temp-syntax-table)
(modify-syntax-entry ?\" "\"" myutils-temp-syntax-table)
(modify-syntax-entry ?\{ "(" myutils-temp-syntax-table)
(modify-syntax-entry ?\} ")" myutils-temp-syntax-table)
(modify-syntax-entry ?\( "(" myutils-temp-syntax-table)
(modify-syntax-entry ?\) ")" myutils-temp-syntax-table)
(modify-syntax-entry ?\< "(" myutils-temp-syntax-table)
(modify-syntax-entry ?\> ")" myutils-temp-syntax-table)
(modify-syntax-entry ?\[ "(" myutils-temp-syntax-table)
(modify-syntax-entry ?\] ")" myutils-temp-syntax-table)


(defun smart-mark-1 (&optional adjust pos)
  (let* ((c (char-after))
         (range (mouse-start-end (point) (point) 1))
         (b (car range))
         (e (nth 1 range))
         start end)
    
    (unless (or (and (< pos b) (< pos e)) ;; out of range
                (and (> pos b) (> pos e)) ;; out of range
                )
      (when (and adjust (not (eq major-mode 'emacs-lisp-mode))
                 (not (eq major-mode 'lisp-mode)))
        (cond ((< b e)
               (setq b (1+ b)
                     e (1- e)))
              (t
               (setq b (1- b)
                     e (1+ e))))
        (when (or (memq c smart-mark-chars-open)
                  (memq c smart-mark-chars-close))
          
          (goto-char (min b e))
          (when (looking-at "[\s\t]*$")
            (forward-line))
          (setq start (point))
          (goto-char (max b e))
          (skip-chars-backward "\s\t")
          (when (looking-at "^")
            (backward-char))
          (setq end (point))
          (cond ((< b e)
                 (setq b start
                       e end))
                (t
                 (setq e start
                       b end)))))
      (unless (= b e)
        (set-mark b)
        (goto-char e)))))

(defun smart-mark-line ()
  (interactive)
  (set-mark (line-beginning-position))
  (goto-char (line-end-position)))

(defun smart-mark-to-line-start ()
  (interactive)
  (set-mark (point))
  (goto-char (line-beginning-position)))

(defun smart-mark-to-line-end ()
  (interactive)
  (set-mark (point))
  (goto-char (line-end-position)))

(defun smart-mark-mark-current-list ()
  (interactive)
  (when (memq (char-after) smart-mark-chars-open)
    (forward-char))
  (backward-up-list)
  (smart-mark-1 t (point)))


(defun smart-mark ()
  (interactive)
  (let ((cc (char-after))
        (pos (point))
        c c-t search? done?)
    (setq c
          (read-char
           "Select a command (', \", (, ), {, }, [, ], <, >, t, l, a, e, m, n, RET; type ? for help) : "))
    (unless (or (= c cc)
                (= c ?n)
                (and (setq c-t (cdr (assoc c  smart-mark-chars-pairs)))
                     (= cc c)))
      (setq search? t))

    (cond ((= c ?t)
           (unless (smart-mark-tag)
             (goto-char pos)))
          ((= c ?l)
           (smart-mark-line))
          ((= c ?a)
           (smart-mark-to-line-start))
          ((= c ?e)
           (smart-mark-to-line-end))
          ((or (= c ?\r) (= c ?w))
           (when (looking-at "[\n\t\s]")
             (skip-chars-backward "[\t\s]")
             (unless (looking-at "^")
               (backward-char)))
           (smart-mark-1 nil (point)))
          ((= c ?m)
           (smart-mark-mark-current-list))
          ((= c ?f)
           (setq c-t (read-char "find char: "))
           (unless mark-active
             (set-mark pos))
           (search-forward (string c-t)))
          ((= c ?F)
           (setq c-t (read-char "find char: "))
           (unless mark-active
             (set-mark pos))
           (search-backward (string c-t)))
          ((= c ?\?)
           (with-output-to-temp-buffer "*smart-mark*"
             (princ "()[]<>\"\'   as you know;
m   mark current list;
n   mark according to the char under cursor;
t   mark tag content;
l   mark line;
a   mark from current pos to beginning of the line;
e   mark from current pos to end of the line;
RET mark word;
w   mark word;
f   find a char forward and mark orig pos to new pos;
F   find a char backward and mark orig pos to new pos;
?   help")))
          (t
           (let ((cs (char-syntax c))
                 syntax-mod-need meta-char)
             (cond ((memq c smart-mark-chars-quote)
                    (setq syntax-mod-need (/= cs ?\")))
                   ((memq c smart-mark-chars-open)
                    (setq syntax-mod-need (/= cs ?\( )))
                   ((memq c smart-mark-chars-close)
                    (setq syntax-mod-need (/= cs ?\) )
                          c (cdr (assoc c smart-mark-chars-pairs)))))
             (flet ((s-n-m ()
                           (cond (search?
                                  (cond ((memq c smart-mark-chars-quote)
                                         (forward-sexp)
                                         (unless (smart-mark-1 t pos)
                                           (goto-char pos)))
                                        (t
                                         (ignore-errors
                                           (while (progn (backward-up-list) (/= c (char-after)))))
                                         (if (= c (char-after))
                                             (unless (smart-mark-1 t pos)
                                               (goto-char pos))
                                           (goto-char pos)))))
                                 (t
                                  (smart-mark-1 t pos)))))
               (cond (syntax-mod-need
                      (set-char-table-parent myutils-temp-syntax-table (syntax-table))
                      (with-syntax-table
                          myutils-temp-syntax-table
                        (s-n-m)))
                     (t
                      (s-n-m)))))))))

(defun smart-mark-tag ()
  (interactive)
  (require 'sgml-mode)
  (let ((pos (point)) s e tag rs re)
    (when (looking-at "<[^/]")
        (forward-char))
    (mysgml-goto-open-tag-backward-1 1)
    (setq s (point))
    (sgml-skip-tag-forward 1)
    (setq e (point))
    (goto-char s)
    (when (looking-at "<\\([^\s\t>]+\\)[^>]*>")
      (setq tag (match-string 1))
      (goto-char (match-end 0))
      (setq rs (point))
      (goto-char e)
      (when (re-search-backward "</\\([^\s\t>]+\\)[^>]*>" nil t)
        (when (string= tag (match-string 1))
          (setq re (match-beginning 0))
          (when (>= e pos)
            (set-mark rs)
            (goto-char re)))))))


(defun mysgml-goto-open-tag-backward-1 (arg)
  "Skip to beginning of tag or matching opening tag if present.
With prefix argument ARG, repeat this ARG times.
Return non-nil if we skipped over matched tags."
  (interactive "p")
  ;; FIXME: use sgml-get-context or something similar.
  (let ((return t)  (pos (point)) pos1)
    (when (search-backward "<")
        (cond ((= ?/ (char-after (1+ (point))))
               ;; end tags
               (setq pos1 (point))
               (forward-char)
               (when (search-forward ">")
                 (when (> (point) pos)
                   (goto-char pos1))))
              (t
               (setq arg (1- arg)))))
    (while (>= arg 1)
      (search-backward "<" nil t)
      (if (looking-at "</\\([^ \n\t>]+\\)")
          ;; end tag, skip any nested pairs
          ;; (setq level (1+ level))
          (let ((case-fold-search t)
                (re (concat "</?" (regexp-quote (match-string 1))
                            ;; Ignore empty tags like <foo/>.
                            "\\([^>]*[^/>]\\)?>")))
            (while (and (re-search-backward re nil t)
                        (eq (char-after (1+ (point))) ?/))
              (forward-char 1)
              (sgml-skip-tag-backward 1))
            (setq arg (1+ arg)))
        (setq return nil))
      (setq arg (1- arg)))
    return))


(global-set-key "\C-\M-m" 'smart-mark)

