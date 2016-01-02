(setq user-full-name "Nasser Alkmim"
      user-mail-address "nasser.alkmim@gmail.com")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(use-package moe-theme
  :ensure t
  :config
  (setq moe-light-pure-white-background-in-terminal t))

(use-package material-theme
  :defer t
  :ensure t
  :init
  (load-theme 'material t))

(use-package cyberpunk-theme
  :defer t
  :ensure t)

(use-package zenburn-theme
  :defer t
  :ensure t)

(use-package theme-looper
  :defer t
  :ensure t
  :init
  (theme-looper-set-theme-set '(moe-light
                                moe-dark
                                material
                                material-light
                                zenburn
                                cyberpunk
                                ))
  (theme-looper-set-customizations 'powerline-reset)
  (global-set-key (kbd "S-<f12>") 'theme-looper-enable-next-theme))

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

;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(setq-default indent-tabs-mode nil)

;; use shift-arrows to move between windows
(windmove-default-keybindings)

;; highlight current line
(global-hl-line-mode 1)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

(show-paren-mode t)

(column-number-mode t)

(global-visual-line-mode)
(diminish 'visual-line-mode)

(setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

;; Don't create backups
(setq make-backup-files nil)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(bind-key "M-s o" 'occur-dwim)

(use-package recentf
  :commands ido-recentf-open
  :init
  (progn
    (recentf-mode t)
    (setq recentf-max-saved-items 200)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))

    (bind-key "C-x C-r" 'ido-recentf-open)))

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c a" 'org-agenda)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/OneDrive/Agenda"))))

(bind-key "C-c c" 'org-capture)
(setq org-default-notes-file "~/OneDrive/Notes/notes.org")

(setq org-use-speed-commands t)

(setq org-image-actual-width 550)

(setq org-special-ctrl-a/e t)
(transient-mark-mode nil)
(setq org-log-done 'time) ;Log the time a task is completed.
(setq org-habit-graph-column 50) ;position the habit graph on the agenda to the right of the default
(setq org-hide-emphasis-markers nil)
(setq org-src-fontify-natively t)

(setq org-tags-column 45)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

;; paste from clipboard
(defun my-org-insert-clipboard ()
  (interactive)
  (let* ((image-file (concat 
                      (buffer-file-name)
                      "_"
                      (format-time-string "%Y%m%d_%H%M%S_.png")))
         (exit-status
          (call-process "convert" nil nil nil
                        "clipboard:" image-file)))
    (org-insert-link nil (concat "file:" image-file) "")
    (org-display-inline-images)))
(global-set-key (kbd "C-c y") 'my-org-insert-clipboard)

(custom-set-faces

  '(org-level-1 ((t (:background nil :bold t :overline nil))))

  '(org-level-2 ((t (:background nil :bold t :overline nil)))))

(bind-key "C-x m" 'shell)
(bind-key "C-x M" 'ansi-term)

(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :commands ace-jump-mode
  :bind ("C-S-s" . ace-jump-mode))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (ace-window-display-mode)
  :bind ("C-o" . ace-window))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (use-package helm-projectile
            :ensure t
            :commands helm-projectile
            :bind ("C-c p h" . helm-projectile)
            :init
            ;; helm with projectile
            (projectile-global-mode)
            (setq projectile-completion-system 'helm)
            (helm-projectile-on)
            (setq projectile-switch-project-action 'helm-projectile-find-file)
            (setq projectile-indexing-method 'alien))
          (use-package helm-ag :ensure t)
          (setq helm-locate-command "mdfind -interpret -name %s %s"
                helm-ff-newfile-prompt-p nil
                helm-M-x-fuzzy-match t)
          (helm-mode)
          (helm-autoresize-mode t)
          (setq helm-split-window-in-side-p t))
  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
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
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)))

(use-package perspective
  :ensure t
  :config (persp-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-mode
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-enable-caching t)
    (use-package ag
      :commands ag
      :ensure t)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :ensure t)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config (progn (require 'smartparens-config)
                 (smartparens-global-mode t)))

(sp-local-pair 'org-mode "~" "~" :actions '(wrap))
(sp-local-pair 'org-mode "/" "/" :actions '(wrap))
(sp-local-pair 'org-mode "*" "*" :actions '(wrap))

(use-package smooth-scrolling
  :defer t
  :ensure t)

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (progn
    (setq TeX-PDF-mode t)
    (setq-default TeX-master nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq global-font-lock-mode t)))

(use-package latex-preview-pane
  :ensure t
  :bind ("M-p" . latex-preview-pane-mode)
  :config
  (setq doc-view-ghostscript-program "gswin64c"))

(use-package reftex
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(add-to-list 'load-path "C:/Users/Nasser/.emacs.d/elpa/magic-latex-buffer-master")
(require 'magic-latex-buffer)
(add-hook 'latex-mode-hook 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
(setq magic-latex-enable-block-highlight nil
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil)

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
  (global-set-key (kbd "<f6>")   'fd-switch-dictionary))

(use-package company
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'wave)
  (setq powerline-default-separator 'utf-8))

(setq display-time-default-load-average nil)

(use-package doc-view
  :config
  (define-key doc-view-mode-map (kbd "<right>") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "<left>") 'doc-view-previous-page)
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

  (global-set-key (kbd "C-<wheel-up>") 'doc-view-enlarge)
  (global-set-key (kbd "C-<wheel-down>") 'doc-view-shrink)
  (setq doc-view-continuous t))
