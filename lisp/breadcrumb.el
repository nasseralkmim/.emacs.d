;;; breadcrumb.el --- imenu-based breadcrumb paths   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Version: 0.0.1alpha
;; Keywords:

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
;;;
;;; M-x breadcrumb-mode in any buffer where you have imenu capability
;;; (which are a lot of them, though said capability varies)
;;;
;;; Works even better for a recent Eglot (I think Eglot 1.14+),
;;; because it adds extra region info the the traditional imenu
;;; struct, `imenu--index-alist'.  But there should be interesting
;;; stuff in older Eglot too.
;;;
;;; This _should_ be faster than which-func.el due to good caching
;;; strategies.  But I haven't measured.
;;;
;;; This also takes care not to over-call `imenu-make-index-alist',
;;; which could be slow.  The variable `breadcrumb-idle-delay'
;;; controls that.
;;;
;;; Relies a lot on this double-dashed imenu function, but that is
;;; really not a double-dashed function.
;;;

;;; Code:
(require 'cl-lib)
(require 'imenu)

(cl-defun bc-bisect (a x &key (from 0) (to (length a)) key from-end)
  "Compute index to insert X in sequence A, keeping it sorted.
If X already in A, the resulting index is the leftmost such
index, unless FROM-END is t.  KEY is as usual in other CL land."
  (cl-macrolet ((search (from-end key)
                  `(cl-loop while (< from to)
                            for mid = (/ (+ from to) 2)
                            for p1 = (elt a mid)
                            for p2 = ,(if key `(funcall key p1) `p1)
                            if (,(if from-end '< '<=) x p2)
                            do (setq to mid) else do (setq from (1+ mid))
                            finally return from)))
    (if from-end (if key (search t key) (search t nil))
      (if key (search nil key) (search nil nil)))))

(defun bc--path-1 (index-alist pos)
  (cl-labels
      ((search (nodes &optional path)
         (cl-loop
          for n in nodes
          for reg = (get-text-property 0 'breadcrumb-region (car n))
          when (<= (car reg) pos (cdr reg))
          return (search (cdr n) (cons (car n) path))
          finally (cl-return path))))
    (nreverse (search index-alist))))

(defvar-local bc--path-2-cache nil)
(defun bc--path-2 (index-alist pos)
  (cl-labels ((dfs (n &optional path)
                (setq path (cons (car n) path))
                (if (consp (cdr n))
                    (mapc (lambda (n) (dfs n path)) (cdr n))
                  (setq bc--path-2-cache
                        (vconcat bc--path-2-cache
                                 `[,(cons (cdr n) path)])))))
    (unless bc--path-2-cache
      (mapc #'dfs index-alist)
      (setq bc--path-2-cache (cl-sort bc--path-2-cache #'< :key #'car)))
    (unless (< pos (car (aref bc--path-2-cache 0)))
      (let ((res (bc-bisect bc--path-2-cache pos :key #'car :from-end t)))
        (unless (zerop res) (reverse (cdr (elt bc--path-2-cache (1- res)))))))))

(defun bc-path (index-alist pos)
  "Get breadcrumb for position POS given INDEX-ALIST."
  (if (get-text-property 0 'breadcrumb-region (caar index-alist))
      (bc--path-1 index-alist pos)
    (bc--path-2 index-alist pos)))

(defvar-local bc--last-update-tick 0)

(defvar bc--header-line-key [header-line mouse-1])

(defun bc--format-node (p)
  (let ((reg (get-text-property 0 'breadcrumb-region p)))
    (if reg
        (propertize p
                    'mouse-face 'header-line-highlight
                    'help-echo "Go here"
                    'keymap (let ((m (make-sparse-keymap)))
                              (define-key m bc--header-line-key
                                          (lambda (&rest _e)
                                            (interactive)
                                            (push-mark)
                                            (goto-char (car reg))))
                              m))
      p)))

(defvar bc-idle-time 1
  "Control idle time before requesting new breadcrumbs.")

(defvar-local bc--idle-timer nil)

(defun bc--alist ()
  (let ((nochangep (= (buffer-chars-modified-tick) bc--last-update-tick))
        (buf (current-buffer)))
    (cond ((and nochangep imenu--index-alist) imenu--index-alist)
          (t
           (setq bc--last-update-tick (buffer-chars-modified-tick))
           (when bc--idle-timer (cancel-timer bc--idle-timer))
           (setq bc--idle-timer
                 (run-with-idle-timer
                  bc-idle-time nil
                  (lambda ()
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (setq bc--last-update-tick (buffer-chars-modified-tick))
                        (let ((non-essential t)
                              (imenu-auto-rescan t))
                          (imenu--make-index-alist t)
                          (setq bc--path-2-cache nil)
                          (force-mode-line-update t)))))))))))

(defun bc-path-for-header-line ()
  (cl-loop with alist = (bc--alist)
           for (p . more) on (bc-path alist (point))
           collect (bc--format-node p) when more collect " > "))

(defvar bc-header-line-format
  '(:eval (bc-path-for-header-line)))

(define-minor-mode bc-mode
  "Header lines with breadcrumbs."
  :init-value nil
  (if bc-mode (add-to-list 'header-line-format bc-header-line-format)
    (setq header-line-format (delq bc-header-line-format header-line-format))))

(defun bc-jump ()
  "Like M-x `imenu', but breadcrumb-powered."
  (interactive)
  (let (cands choice)
    (cl-labels
        ((fmt (strs)
           (mapconcat #'identity strs " > "))
         (dfs (nodes &optional path)
           (cl-loop
            for n in nodes
            for newpath = (cons (car n) path)
            for pos = (or (car (get-text-property 0 'breadcrumb-region (car n)))
                          (and (number-or-marker-p (cdr n)) (cdr n)))
            when pos do (push (cons (fmt (reverse newpath)) pos)
                              cands)
            do (dfs (cdr n) newpath))))
      (imenu--make-index-alist)
      (dfs imenu--index-alist)
      (unless cands (user-error "Sorry, no breadcrumb items to jump to."))
      (setq choice (cdr (assoc (completing-read "Index item? " cands nil t)
                               cands #'string=)))
      (push-mark)
      (goto-char choice))))

(provide 'breadcrumb)
;;; breadcrumb.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("bc-" . "breadcrumb-"))
;; End:
