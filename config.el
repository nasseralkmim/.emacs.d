(setq user-full-name "Nasser Alkmim"
      user-mail-address "nasser.alkmim@gmail.com")

(use-package moe-theme
  :ensure t
  :defer t
  :init
  (require 'moe-theme)
)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package anti-zenburn-theme
  :ensure t
  :init
  (load-theme 'anti-zenburn t))

(use-package material-theme
  :defer t
  :ensure t)  

(use-package cyberpunk-theme
  :defer t
  :ensure t)

(use-package zenburn-theme
  :defer t
  :ensure t)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package theme-looper
  :defer t
  :ensure t
  :bind ("S-<f12>" . theme-looper-enable-next-theme)
  :init
  (theme-looper-set-theme-set '(moe-light
                                ;moe-dark
                                leuven
                                anti-zenburn
                                ;material
                                ;material-light
                                ;zenburn
                                cyberpunk
                                ;solarized-light
                                ;sanityinc-tomorrow-day
                                ))
  (theme-looper-set-customizations 'powerline-reset))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 100)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
;; from Sacha page
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq-default indent-tabs-mode nil)

;; use shift-arrows to move between windows
(windmove-default-keybindings)

;; highlight current line
(global-hl-line-mode 1)

; wrap lines
(global-visual-line-mode)
(diminish 'visual-line-mode)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)


(column-number-mode t)


(setq uniquify-buffer-name-style 'forward)


;; Don't beep at me
(setq visible-bell t)

;; Don't create backups
(setq make-backup-files nil)

(use-package recentf
  :defer 10
  :config
  (progn
    (recentf-mode t)
    (setq recentf-max-saved-items 200
          recentf-max-menu-items 15)))

(use-package org
  :ensure t
  :defer t
  :bind(("C-c a" . org-agenda)
        ("C-c l" . org-store-link)
        ("C-c c" . org-capture))
  :config
  (add-hook 'org-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'flycheck-mode))

(setq org-modules '(org-habit))

(eval-after-load 'org
 '(org-load-modules-maybe t))

(setq org-habit-graph-column 80)
;(setq org-habit-show-habits-only-for-today nil)

(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/OneDrive/Org/notes.org")))

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/OneDrive/Org/gtd.org"
                      "~/OneDrive/Org/notes.org"
                      "~/OneDrive/Org/culture.org"
                      "~/OneDrive/ANAC/anac.org"
))))

(custom-set-variables
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-if-done t))

(setq org-default-notes-file "~/OneDrive/Org/gtb.org")

(setq org-capture-templates
      '(
("t" "Todo" entry (file+datetree "~/OneDrive/Org/gtd.org") 
"* TODO %?

Added: %U")

("n" "Notes" entry (file+datetree "~/OneDrive/Org/notes.org") 
"* %^{Description} %^g 

%?
 
Added: %U")

("b" "Books" entry (file+headline "~/OneDrive/Org/culture.org" "Books")
"* STRT %^{Title} 
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a +1d>>\")
:PROPERTIES:
:STYLE:    habit
:END:


*Author(s):* %^{Author}
*Pages/Day:* %^{P/D}
*Review/Comments:*

%?

*Added*: %U" )

("m" "Movies" entry (file+headline "~/OneDrive/Org/culture.org" "Movies")
"* %^{Title}

*Review/Comments:*

%?

*Added*: %U"
)

("p" "Post" plain (file  (blog-post-new))
 "Title: %^{Title}
Date: %<%Y-%m-%d %H:%M>
Category: %^{Category}
Tags: %^{Tags}

%?

"
)
      
))

(setq org-cycle-include-plain-lists 'integrate)

(setq org-startup-with-inline-images t)

(setq org-use-speed-commands t)

(setq org-special-ctrl-a/e t)
(transient-mark-mode nil)
(setq org-log-done 'time) ;Log the time a task is completed.
(setq org-habit-graph-column 50) ;position the habit graph on the agenda to the right of the default
(setq org-hide-emphasis-markers nil)
(setq org-src-fontify-natively t)
(setq inhibit-splash-screen t)
(setq org-indent-mode t) ;indent the headings for clean view
(diminish 'org-indent-mode)
(setq org-startup-indented t)
(setq org-tags-column -96) ;where the tags are places

(setq org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "DONE(d)")))
(setq org-todo-keyword-faces 
      '(
         ("TODO" :background "tomato" :foreground "#5f5f5f" :weight bold )
         ("STRT" :background "#edd400" :foreground "#5f5f5f" :weight bold )
         ("DONE" :background "#6ac214" :foreground "#5f5f5f" :weight bold )))

(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(setq org-cycle-separator-lines 0)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(defun my/org-insert-clipboard ()
  (interactive)
  ;make the img directory
  (setq myvar/folder-path (concat default-directory "img/"))
  ;create the directory if it doesn't exist
  (if (not (file-exists-p myvar/folder-path))
      (mkdir myvar/folder-path))

  (let* ((image-file (concat 
                      myvar/folder-path
                      (buffer-name)
                      "_"
                      (format-time-string "%Y%m%d_%H%M%S_.png")))

	 (exit-status
	  (call-process "convert" nil nil nil
			"clipboard:" image-file)))

    (org-insert-link nil (concat "file:" image-file) "")

    (org-display-inline-images)))

(global-set-key (kbd "C-c y") 'my/org-insert-clipboard)

(custom-set-faces

  '(org-level-1 ((t (:background nil :bold t :overline nil))))

  '(org-level-2 ((t (:background nil :bold t :overline nil)))))

(use-package org-pomodoro
  :ensure t
  :bind ("<f12>" . org-pomodoro))

(defun today-date ()
  (format-time-string "%Y-%m-%d"))

(defun blog-title ()
  (interactive)
  (read-string "Blog title: "))

(defun blog-post-new ()
  (let ((title
         (blog-title)))
  (concat (file-name-as-directory "C:/Users/Nasser/Onedrive/nasseralkmim.github.io/blog/content")
          (today-date) "-" title ".md")))

(bind-key "C-x m" 'shell)
(bind-key "C-x M" 'ansi-term)

(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :commands ace-jump-mode
  :bind ("C-x C-x" . ace-jump-mode))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (ace-window-display-mode)
  :bind ("C-o" . ace-window))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)
  :config 
  (helm-mode)
  (helm-autoresize-mode t)
  (setq helm-display-header-line nil)
  (setq helm-split-window-in-side-p t)
  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)

  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("C-x f"   . helm-multi-files)
         ("C-`" . helm-resume)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package swiper-helm
  :bind (("C-s" . swiper-helm)
         ("C-r" . swiper-helm))
  :ensure t
  :config
  (setq swiper-helm-display-function 'helm-default-display-buffer))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package ein
  :defer t
  :disabled t
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-global-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (setq projectile-indexing-method 'alien))
  (projectile-global-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :ensure t
  :config
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (use-package smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  (sp-local-pair 'org-mode "_" "_" )
  (sp-local-pair 'org-mode "*" "*" ))

(use-package smooth-scrolling
  :defer t
  :disabled t
  :ensure t)

(use-package tex
  :ensure auctex
  :defer t
  :config
  (load "auctex.el" nil nil t)
  (setq global-font-lock-mode t)
  (add-hook 'TeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'flycheck-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
  (setq reftex-plug-into-AUCTeX t)
  (setq-default TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package latex-preview-pane
  :ensure t
  :bind ("M-p" . latex-preview-pane-mode)
  :config
  (setq doc-view-ghostscript-program "gswin64c")
  (custom-set-variables
   '(shell-escape-mode "-shell-escape")))

;; activate fold mode
;(add-hook 'LaTeX-mode-hook (lambda ()
;                             (TeX-fold-mode 1)))
; hide foldable items automatically
;(add-hook 'find-file-hook 'TeX-fold-buffer t)

;(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

(use-package reftex
  :ensure t)

(use-package magic-latex-buffer
  :load-path ("C:/Users/Nasser/.emacs.d/elpa/magic-latex-buffer-master")
  :config
  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
  (setq magic-latex-enable-block-highlight nil
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil))

(use-package flycheck
  :ensure t
  :bind ("S-<f5>" . flycheck-mode))

(use-package flyspell
  :ensure t
  :bind ("S-<f6>" . flyspell-mode)
  :config
  ;; path to Aspell
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  ;; use aspell
  (setq ispell-program-name "aspell")
  ;; where the dictionay is
  (setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")
  ;; change dictionaries
  (defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "brasileiro") "english" "brasileiro")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))
  (global-set-key (kbd "<f6>")   'fd-switch-dictionary)
  (global-set-key (kbd "C-<f1>") 'flyspell-correct-word-before-point))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 2))

(use-package undo-tree
  :defer t
  :ensure t
  :bind ("C-z" . undo-tree-undo)
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)))

(use-package workgroups2
  :ensure t
  :bind (("C-c 1" . wg-reload-session)
         ("C-c 2" . wg-switch-to-workgroup)
         ("C-c z z" . wg-save-session))
  :config
  (workgroups-mode 1))

(use-package centered-cursor-mode
  :ensure t
  :diminish centered-cursor-mode
;  :disabled t
  :config
  (global-centered-cursor-mode +1))

(use-package doc-view
  :config
  (add-hook 'doc-view-mode-hook (lambda () (centered-cursor-mode -1)))
  (define-key doc-view-mode-map (kbd "<right>") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "<left>") 'doc-view-previous-page)
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

  (global-set-key (kbd "C-<wheel-up>") 'doc-view-enlarge)
  (global-set-key (kbd "C-<wheel-down>") 'doc-view-shrink)

  (setq doc-view-continuous t))

(setq ad-redefinition-action 'accept)
