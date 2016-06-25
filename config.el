(setq user-full-name "Nasser Alkmim"
      user-mail-address "nasser.alkmim@gmail.com")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(use-package moe-theme
  :ensure t
  :disabled t
  :defer t
  :init
  (require 'moe-theme)
)

(use-package tao-theme
  :ensure t
  :defer t)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package anti-zenburn-theme
  :ensure t
  :defer t )

(use-package cyberpunk-theme
  :defer t
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package theme-looper
  :defer t
  :bind ("S-<f12>" . theme-looper-enable-next-theme)
  :init
  (theme-looper-set-theme-set '(                               
                                leuven
                                tao-yang
                                anti-zenburn
                                 zenburn
                                ;cyberpunk
                                ;solarized-light
                                sanityinc-tomorrow-day
                                ))
  (theme-looper-set-customizations 'powerline-reset))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(set-frame-font "Source Code Pro 10")
;(set-frame-font "Monospace 10")

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(set-language-environment "UTF-8")
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

;; convert symbols like greek letter into its unicode character
(global-prettify-symbols-mode)


(setq uniquify-buffer-name-style 'forward)


;; Don't beep at me
(setq visible-bell t)

;; Don't create backups
(setq make-backup-files nil)

(defun my/delete-config-el ()
  "Delete ~/.emacs.d/config.el when the current buffer is ~/.emacs.d/config.org"
  (setq configel "~/.emacs.d/config.el")
  (if (s-suffix? ".emacs.d/config.org" buffer-file-name)
      (if (file-exists-p configel)
          (delete-file "~/.emacs.d/config.el"))))

(add-hook 'after-save-hook 'my/delete-config-el)

;(setq debug-on-error t)
;(setq debug-on-quit t)

(use-package recentf
  :defer 10
  :config
  (progn
    (recentf-mode t)
    (setq recentf-max-saved-items 200
          recentf-max-menu-items 15)))

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind(("C-c a" . org-agenda)
        ("C-c l" . org-store-link)
        ("C-c c" . org-capture))
  :config
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook 'company-mode)


  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ipython .t)))

  ;; beamer export
  (eval-after-load "org"
  '(require 'ox-beamer))

   ;; This is for remove the annoying background color on the headings, level 1 and level 2, when using the material-theme.
  (custom-set-faces
  '(org-level-1 ((t (:background nil :bold t :overline nil))))
  '(org-level-2 ((t (:background nil :bold t :overline nil)))))

  (setq org-modules '(org-habit))

  (eval-after-load 'org
    '(org-load-modules-maybe t)))

(use-package org
  :config
  (setq org-agenda-files
        (delq nil
              (mapcar (lambda (x) (and (file-exists-p x) x))
                      '("~/OneDrive/Org/gtd.org"
                        "~/OneDrive/Org/notes.org"
                        "~/OneDrive/Org/culture.org"
                        "~/OneDrive/ANAC/anac.org"))))
   (custom-set-variables
  '(org-agenda-skip-scheduled-if-done t)
  '(org-agenda-skip-deadline-if-done t))

   (setq org-default-notes-file "~/OneDrive/Org/gtb.org")

   (global-set-key (kbd "C-c o") 
                   (lambda () (interactive) (find-file "~/OneDrive/Org/notes.org"))))

(use-package org
  :defer t
  :config
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
  )))

(use-package org
  :defer t 
  :config
  (setq org-cycle-include-plain-lists 'integrate)
  (setq org-image-actual-width nil)
  (setq org-startup-with-inline-images t))

(use-package org
  :config
  (setq org-special-ctrl-a/e t)
  (transient-mark-mode nil)
  (setq org-log-done 'time) ;Log the time a task is completed.
  (setq org-habit-graph-column 50) ;position the habit graph on the agenda to the right of the default
  (setq org-hide-emphasis-markers nil)
  (setq inhibit-splash-screen t)
  (setq org-indent-mode t) ;indent the headings for clean view
  (setq org-hide-leading-stars t) 
  (setq org-hide-leading-stars-before-indent-mode t)
  (setq org-odd-levels-only t)
  (diminish 'org-indent-mode)
  (setq org-startup-indented t)
  (setq org-tags-column -96) ;where the tags are places
  (setq org-use-speed-commands t)) ; speed up commands

(use-package org
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "DONE(d)")))

  (setq org-todo-keyword-faces 
        '(("TODO" :background "tomato" :foreground "#5f5f5f" :weight bold )
          ("STRT" :background "#edd400" :foreground "#5f5f5f" :weight bold )
          ("DONE" :background "#6ac214" :foreground "#5f5f5f" :weight bold )))
  
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-cycle-separator-lines 0))

(use-package org
  :config
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persistence-insinuate t)
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STRT")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t))

(use-package org
  :config
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-export-babel-evaluate nil
        org-confirm-babel-evaluate nil) ; doesn't ask for confirmation

  ;;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(use-package org
  :bind ("C-c y" . my/org-insert-clipboard)
  :config
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

      (org-display-inline-images))))

(use-package org-page
  :ensure t
  :bind (("C-x C-a p" . op/do-publication-and-preview-site)
         ("C-x C-a C-p" . op/do-publication)
         ("C-x C-a C-n" . op/new-post))
  :config
  (setq op/repository-directory "c:/Users/Nasser/OneDrive/nasseralkmim.github.io/")
  (setq op/site-domain "http://nasseralkmim.github.io/")
  (setq op/personal-disqus-shortname "nasseralkmim")
  (setq op/site-main-title "Nasser Alkmim")
  (setq op/site-sub-title "~/-")
  (setq op/personal-github-link "https://github.com/nasseralkmim")
  (setq op/personal-google-analytics-id "74704246"))

(bind-key "C-x m" 'shell)
(bind-key "C-x M" 'ansi-term)

(use-package avy
  :ensure t 
  :diminish avy-mode
  :bind (("C-x C-SPC" . avy-goto-char)
         ("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line)))

(use-package ace-window
  :ensure t 
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (ace-window-display-mode)
  :bind ("C-o " . ace-window))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package magit
  :ensure t 
  :bind ("C-x g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  
  ;;This setting is needed to use ivy completion:
  (setq magit-completing-read-function 'ivy-completing-read)
  
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package projectile
  :ensure t 
  :diminish projectile-mode
  :commands projectile-global-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config

  (use-package helm-projectile
    :disabled t
    :ensure t 
    :defer t)

  ;(setq projectile-completion-system 'helm)
  ;(helm-projectile-on)

  ;So projectile works with ivy
  (setq projectile-completion-system 'ivy)

  (setq projectile-indexing-method 'alien)
  (projectile-global-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config

  (add-hook 'python-mode-hook 'elpy-mode)

  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
 
  ;; Sets the python interpreter to be ipython. To trick emacs into
  ;; thinking we're still running regular python, we run ipython in
  ;; classic mode.
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "-i --classic"))

(use-package elpy
  :ensure t
  :defer t
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (setq jedi:complete-on-dot t))

(use-package smartparens
  :ensure t 
  :defer t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  (sp-local-pair 'org-mode "_" "_" )
  (sp-local-pair 'org-mode "*" "*" )
  (sp-local-pair 'latex-mode "$" "$" )
  (sp-local-pair 'latex-mode "\\left(" "\\right)" :trigger "\\l("))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (magic-latex-buffer)
              (LaTeX-math-mode)
              (rainbow-delimiters-mode)
              (flyspell-mode)
              (company-mode)
              (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (setq TeX-PDF-mode t)
              (setq global-font-lock-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;https://github.com/politza/pdf-tools/issues/187

;; to use pdfview with auctex
(add-hook 'LaTeX-mode-hook 'pdf-tools-install)
(setq TeX-view-program-selection '((output-pdf "pdf-tools")))
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

; language specific hooks in auctex
(add-hook 'TeX-language-dk-hook
      (lambda () (ispell-change-dictionary "brasileiro"))))

(use-package company-auctex
  :ensure t
  :defer t
  :config
  (company-auctex-init))

(use-package latex-preview-pane
  :disabled t
  :bind ("M-p" . latex-preview-pane-mode)
  :config
  (setq doc-view-ghostscript-program "gswin64c")
  
  (custom-set-variables
   '(shell-escape-mode "-shell-escape")
   '(latex-preview-pane-multifile-mode (quote auctex))))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

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
  :diminish flycheck-mode
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
  :defer t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 3)
  (delete 'company-capf company-backends)
  (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-statistics
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t 
  :bind ("C-z" . undo-tree-undo)
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)))

(use-package rainbow-delimiters
  :ensure t 
  :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package pdf-tools
  :ensure t
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))

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

(use-package key-chord
  :ensure t
  :after (org tex-site) 
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define-global "]]" "\\")
  (key-chord-define-global ";;" "/")
  (key-chord-define-global "::" "?"))

(global-auto-revert-mode t)

;; * Colored src blocks
;; based on patches from Rasmus <rasmus@gmx.us>

;; This function overwrites the org-src function to make src blocks be colored again.
(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
LANG is the language of the block.  START and END are positions of
the block.  This function is called by Emacs automatic
fontification, as long as `org-src-fontify-natively' is non-nil."
  (let ((lang-mode (org-src--get-lang-mode lang)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
            (modified (buffer-modified-p))
            (org-buffer (current-buffer))
            (block-faces (let ((face-name (intern (format "org-block-%s" lang))))
                           (append (and (facep face-name) (list face-name))
                                   '(org-block)))))
        (remove-text-properties start end '(face nil))
        (with-current-buffer
            (get-buffer-create
             (format " *org-src-fontification:%s*" lang-mode))
          (erase-buffer)
          (insert string " ") ;; so there's a final property change
          (unless (eq major-mode lang-mode) (funcall lang-mode))
          (org-font-lock-ensure)
          (let ((pos (point-min)) next)
            (while (setq next (next-single-property-change pos 'face))
              (let ((new-face (get-text-property pos 'face)))
                (put-text-property
                 (+ start (1- pos)) (1- (+ start next)) 'face
                 (list :inherit (append (and new-face (list new-face))
                                        block-faces))
                 org-buffer))
              (setq pos next))
            ;; Add the face to the remaining part of the font.
            (put-text-property (1- (+ start pos))
                               end 'face
                               (list :inherit block-faces) org-buffer)))
        (add-text-properties
         start end
         '(font-lock-fontified t fontified t font-lock-multiline t))
        (set-buffer-modified-p modified)))))

(defun org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (if (re-search-forward
         "^\\([ \t]*#\\(\\(\\+[a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
         limit t)
        (let ((beg (match-beginning 0))
              (block-start (match-end 0))
              (block-end nil)
              (lang (match-string 7))
              (beg1 (line-beginning-position 2))
              (dc1 (downcase (match-string 2)))
              (dc3 (downcase (match-string 3)))
              end end1 quoting block-type ovl)
          (cond
           ((and (match-end 4) (equal dc3 "+begin"))
            ;; Truly a block
            (setq block-type (downcase (match-string 5))
                  quoting (member block-type org-protecting-blocks))
            (when (re-search-forward
                   (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
                   nil t)  ;; on purpose, we look further than LIMIT
              (setq end (min (point-max) (match-end 0))
                    end1 (min (point-max) (1- (match-beginning 0))))
              (setq block-end (match-beginning 0))
              (when quoting
                (org-remove-flyspell-overlays-in beg1 end1)
                (remove-text-properties beg end
                                        '(display t invisible t intangible t)))
              (add-text-properties
               beg end '(font-lock-fontified t font-lock-multiline t))
              (add-text-properties beg beg1 '(face org-meta-line))
              (org-remove-flyspell-overlays-in beg beg1)
              (add-text-properties      ; For end_src
               end1 (min (point-max) (1+ end)) '(face org-meta-line))
              (org-remove-flyspell-overlays-in end1 end)
              (cond
               ((and lang (not (string= lang "")) org-src-fontify-natively)
                (org-src-font-lock-fontify-block lang block-start block-end)
                (add-text-properties beg1 block-end '(src-block t)))
               (quoting
                (add-text-properties beg1 (min (point-max) (1+ end1))
                                     (let ((face-name (intern (format "org-block-%s" lang))))
                                       (append (and (facep face-name) (list face-name))
                                               '(face org-block))))) ; end of source block
               ((not org-fontify-quote-and-verse-blocks))
               ((string= block-type "quote")
                (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-quote)))
               ((string= block-type "verse")
                (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-verse))))
              (add-text-properties beg beg1 '(face org-block-begin-line))
              (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
                                   '(face org-block-end-line))
              t))
           ((member dc1 '("+title:" "+author:" "+email:" "+date:"))
            (org-remove-flyspell-overlays-in
             (match-beginning 0)
             (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
            (add-text-properties
             beg (match-end 3)
             (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
                 '(font-lock-fontified t invisible t)
               '(font-lock-fontified t face org-document-info-keyword)))
            (add-text-properties
             (match-beginning 6) (min (point-max) (1+ (match-end 6)))
             (if (string-equal dc1 "+title:")
                 '(font-lock-fontified t face org-document-title)
               '(font-lock-fontified t face org-document-info))))
           ((equal dc1 "+caption:")
            (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(display t invisible t intangible t))
            (add-text-properties (match-beginning 1) (match-end 3)
                                 '(font-lock-fontified t face org-meta-line))
            (add-text-properties (match-beginning 6) (+ (match-end 6) 1)
                                 '(font-lock-fontified t face org-block))
            t)
           ((member dc3 '(" " ""))
            (org-remove-flyspell-overlays-in beg (match-end 0))
            (add-text-properties
             beg (match-end 0)
             '(font-lock-fontified t face font-lock-comment-face)))
           (t ;; just any other in-buffer setting, but not indented
            (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(display t invisible t intangible t))
            (add-text-properties beg (match-end 0)
                                 '(font-lock-fontified t face org-meta-line))
            t))))))



(defface org-block-emacs-lisp
  `((t (:background "GhostWhite")))
  "Face for elisp src blocks")

(defface org-block-python
  `((t (:background "WhiteSmoke")))
  "Face for python blocks")

(defface org-block-ipython
  `((t (:background "AliceBlue")))
  "Face for python blocks") 

(defface org-block-sh
  `((t (:background "MintCream")))
  "Face for python blocks")

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
