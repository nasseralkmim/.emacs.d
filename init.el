;;;; Code:
(require 'package)

;;;; Packages archives:
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(setq package-enable-at-startup nil)

; Activate all the packages
(package-initialize)


; packages
(setq package-list '(god-mode
		     auctex
		     latex-preview-pane
		     ido
		     neotree
		     projectile
		     zenburn-theme
		     helm
		     helm-projectile))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;
;;; ESS
;;;
;; Load ESS and activate the nifty feature showing function arguments
;; in the minibuffer until the call is closed with ')'.
(require 'ess-site)

;;;
;;; org
;;;
;; Load recent version of org-mode.
(require 'org-install)


(tool-bar-mode -1) ; Remove tool bar
(menu-bar-mode -1) ; Remove menu bar
(load-theme 'solarized-light t) ; load theme
(desktop-save-mode 1) ; Save desktop opened previously

(require 'ido) ; Interactive mode
(ido-mode t)

(global-visual-line-mode 1) ; Line wrapping
(global-hl-line-mode 1) ; Highlight current line
(setq make-backup-files nil) ; Don't save backup files


;;;
;;; AUCTeX
;;;
;; We assume that MiKTeX (http://www.miktek.org) is used for
;; TeX/LaTeX. Otherwise, change the (require ...) line as per the
;; AUCTeX documentation.
;; this from AUCTeX documentation to open multiple files page1
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-PDF-mode t)

;; the wrapping up of the two loads make sure
;; auctex is loades only when editing tex files
(load "auctex.el" nil t t)
(load  "preview-latex.el" nil t t)

;; to get doc-view-mode
(setq doc-view-ghostscript-program "gswin64c")

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(setq TeX-PDF-mode t) ; compile pdflatex
(latex-preview-pane-enable) ; automatically enable preview pane

;; God mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode) ; Toggle between god-mode and normal using Esc


;; change cursor to indicate wheather in god-mode or not
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; Line numbers display
(require 'linum)
(global-linum-mode 1)

;; Loading refTex
(require 'reftex)

;; turn reftex on for all tex files
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; Integrate refTex with auctex
(setq reftex-plug-into-AUCTeX t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(doc-view-ghostscript-program "gswin64c"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; neotrees - file navigation with f8
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; projectile -
(projectile-global-mode)

;; helm
(require 'helm-config)
(helm-mode 1)
