;;;; Code:
(require 'package)

; packages I want
(setq package-list '(god-mode
		     auctex
		     reftex
		     latex-preview-pane
		     ido
		     neotree
		     projectile
		     zenburn-theme
		     helm
		     helm-projectile
		     jedi
		     helm-ls-git
		     magit
		     auto-complete
		     company
		     yasnippet
		     org-download
		     powerline
		     theme-looper
		     material-theme))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; show the stat up time
(setq message-log-max t)

;;
;; Change default font
;;
(set-face-attribute 'default nil :font  "Fantasque Sans Mono-11" ))
(set-frame-font  "Fantasque Sans Mono-11" nil t)


;Configure theme-looper
(theme-looper-set-theme-set '(deeper-blue
                              wheatgrass
                              wombat
			      material
			      material-light
			      zenburn))
(theme-looper-set-customizations 'powerline-reset)
(global-set-key (kbd "C-0") 'theme-looper-enable-next-theme)
;;
;;powerline
;;
(require 'powerline)
(powerline-center-theme)
(setq powerline-default-separator 'wave)


;;
;; Yassnippet
;;
;(require 'yasnippet)
;(yas-global-mode)


;;
;;org download - images on org
;;
;(require 'org-download)
;; paste from clipboard
(defun my-org-insert-clipboard ()
  (interactive)
  (let* ((image-file (concat (buffer-file-name)
			     "_"
			     (format-time-string "%Y%m%d_%H%M%S_.png")))
	 (exit-status
	  (call-process "convert" nil nil nil
			"clipboard:" image-file)))
    (org-insert-link nil (concat "file:" image-file) "")
    (org-display-inline-images)))
(global-set-key (kbd "C-c y") 'my-org-insert-clipboard)

;;
;; Compnay mode
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)



;;
;;magi latex buffer
;;
(add-to-list 'load-path "C:/Users/Nasser/.emacs.d/elpa/magic-latex-buffer-master")
(require 'magic-latex-buffer)
(add-hook 'latex-mode-hook 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
(setq magic-latex-enable-block-highlight nil
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil)

;;
;;autocomplete
;;
(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)

;; delete trailing white space automatically
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; Projectile
;;
(require 'projectile)
(projectile-global-mode)
;; (setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)
;;Using Emacs Lisp for indexing files is really slow on Windows. To enable external indexing, add this setting:
(setq projectile-indexing-method 'alien)
;; The default is projectile-find-file. My suggestion is to bind it to helm-projectile-find-file, as it provides the same thing as projectile-find-file but with more feature
(setq projectile-switch-project-action 'helm-projectile-find-file)

;;
;; God mode
;;
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode) ; Toggle between god-mode and normal using Esc
;; change cursor to indicate wheather in god-mode or not
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

; Interactive mode
(require 'ido)
(ido-mode t)

;; neotrees - file navigation with f8
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically.
;(setq projectile-switch-project-action 'neotree-projectile-action)




;;;
;;; org
;;;
;; Load recent version of org-mode.
(require 'org-install)
;; make lists collapsed by default
(setq org-cycle-include-plain-lists 'integrate)

;; helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
; helm find files
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; helm ls git
(require 'helm-ls-git)
(global-set-key (kbd "C-<f6>") 'helm-ls-git-ls)


;; Jedi
(require 'jedi)
;; hook up to autocomplete (hook adds funcions)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
;; enable python mode
(add-hook 'python-mode-hook 'jedi:setup)

;;
;; configure emacs window
;;
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Minibuffer line and column
(setq line-number-mode t)
(setq column-number-mode t)

;; Smoother mouse scrolling
(setq mouse-wheel-scroll-amount '(0.01))

;; Show buffer name in title bar
(setq frame-title-format '("%b"))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

(load-theme 'material t) ; load theme
;;(desktop-save-mode 1) ; Save desktop opened previously

(global-visual-line-mode 1) ; Line wrapping
(global-hl-line-mode 1) ; Highlight current line
;(set-face-background 'hl-line "#5F5F5F")
(setq make-backup-files nil) ; Don't save backup files

;;;
;;; AUCTeX
;;;
;; We assume that MiKTeX (http://www.miktek.org) is used for
;; TeX/LaTeX. Otherwise, change the (require ...) line as per the
;; AUCTeX documentation.
;; this from AUCTeX documentation to open multiple files page1
(load "auctex.el" nil t t)
(setq TeX-auto-save t)

(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-PDF-mode t)

;; to get doc-view-mode
(setq doc-view-ghostscript-program "gswin64c")

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(setq TeX-PDF-mode t) ; compile pdflatex

;(latex-preview-pane-enable) ; automatically enable preview pane
;(add-hook 'doc-view-mode-hook 'doc-view-fit-width-to-window)

;;
;; Reftex
;;
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;;(add-hook 'prog-mode-hook 'linum-on);; Line numbers display

;;
;; Aspell
;; path to Aspell
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
;; use aspell
(setq ispell-program-name "aspell")
;; where the dictionay is
(setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")

;; turn it on
(require 'ispell)

(global-set-key [C-whell-up]  'doc-view-enlarge)
(global-set-key  [C-mouse-wheel-down-event] 'doc-view-shrink)

(show-paren-mode 1)

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(global-set-key (kbd "C-<wheel-up>") 'doc-view-enlarge)
(global-set-key (kbd "C-<wheel-down>") 'doc-view-shrink)
(setq doc-view-continuous t)

;; use shift-arrows to move between windows
(windmove-default-keybindings)

;; suppressing ad-handle warnings
(setq ad-redefinition-action 'accept)

;; recent for save last opened files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; open emacs full screen
;(add-to-list 'default-frame-alist '(fullscreen . maximized))
(put 'upcase-region 'disabled nil)

;; MAGIT
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
