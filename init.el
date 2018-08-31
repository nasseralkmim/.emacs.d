(defvar my-start-time (current-time)
  "Time when Emacs was started")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq user-full-name "Nasser Alkmim"
      user-mail-address "nasser.alkmim@gmail.com")

;; UTF-8 please
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Package management
;; set load-path manually
;; don't call package-initialize
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t    ; so it doesn't call package initialize
        package-enable-at-startup nil)  ; do not automatically load packages

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t)))

(setq use-package-verbose t
      use-package-always-defer nil      ;I'm not used to that
      use-package-minimum-reported-time 0.01)

;; Manually set loat-path
(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Initialize package management
(eval-when-compile                      ; when byte compiled skip this
  (require 'package)

  ;; add aditional package archives
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

  ;; initialize packages and ensure that use-package is installed
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))     ; install if it's missing
  (require 'use-package)
  (setq use-package-always-ensure t))

;; set a default font Iosevka, Hack, PragmataPro
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 100
                    :weight 'normal
                    :width 'normal)
;; ;; specify font for all unicode characters
(set-fontset-font t
                  'unicode
                  (font-spec :family "Dejavu Sans mono"
                             :width 'normal
                             :height 100
                             :weight 'normal) nil 'prepend)
;; For testing purposes: →„Σ"←


;; Don't create backups
(setq make-backup-files nil)
(set-fringe-mode '(6 . 0))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

;; Don't beep at me
(setq visible-bell t)

(use-package color-identifiers-mode
  :defer t)
(use-package recentf
  :defer 10
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 500
          recentf-max-menu-items 15
          recentf-auto-cleanup 60))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-s" . counsel-grep-or-swiper)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file))
  :config
  (use-package smex :ensure t))
(use-package ivy
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  ;; Disable ido
  (with-eval-after-load 'ido
    (ido-mode -1)
    ;; Enable ivy
    (ivy-mode 1))
  (setq ivy-display-style 'fancy)
  (setq ivy-dynamic-exhibit-delay-ms 200)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  ;; C-M-j imediate done ivy
  ;; ;; Show recently killed buffers when calling ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths
  ;; ;; Do not show "./" and "../" in the counsel-find-file completion list
  (setq ivy-extra-directories nil))
(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode))
(use-package smartparens
  :diminish smartparens-mode  
  :commands smartparens-mode
  :init
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'lisp-interaction-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  :config
  (sp-local-pair 'org-mode "_" "_" )
  (sp-local-pair 'org-mode "*" "*" )
  (sp-local-pair 'latex-mode "$" "$" )
  (sp-local-pair 'latex-mode "\\left(" "\\right)" :trigger "\\l(")
  ;; highligh matching brackets
  (show-paren-mode 1)
  (show-smartparens-global-mode 0)
  ;; so that paren highlights do not override region marking (aka selecting)
  (setq show-paren-priority -1)
  (setq show-paren-style 'mixed))
(use-package evil
  :diminish evil-mode
  :defer 1
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (key-chord-define evil-insert-state-map "pk" 'sp-up-sexp)
  (key-chord-define evil-normal-state-map "xc" 'avy-goto-char)
  (key-chord-define evil-normal-state-map "xs" 'save-buffer)
  (evil-define-key 'normal 'global "s" 'avy-goto-char-timer)
  (evil-define-key 'normal 'global "j" 'evil-next-visual-line)
  (evil-define-key 'normal 'global "k" 'evil-previous-visual-line)
  (evil-define-key 'normal 'global ";" 'evil-search-forward)
  (setq
   lazy-highlight-cleanup nil
   lazy-highlight-max-at-a-time nil
   lazy-highlight-initial-delay 0))
(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.4)   
  (setq-default evil-escape-unordered-key-sequence t))
(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC"))
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
      "s" 'counsel-imenu
      "q" 'counsel-org-goto-all
      "a" 'evil-append-line
      "i" 'evil-insert-line
      "h" 'evil-first-non-blank
      "l" 'evil-last-non-blank
      "d" 'downcase-word
      "c" 'capitalize-word
      "u" 'upcase-word
      "t" 'xah-toggle-letter-case
      "m" 'cfw:open-org-calendar
      "p" 'evil-jump-item
      "g m" 'my-magit-stage-all-and-commit)

  ;; function to toggle case
  (defun xah-toggle-letter-case ()
    "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq $p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0))))))
(use-package evil-snipe
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode)
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible
	evil-snipe-smart-case t))
(use-package evil-numbers
  :after evil org
  :bind (:map evil-normal-state-map
              ("C-c =" . 'evil-numbers/inc-at-pt)
              ("C-c -" . 'evil-numbers/dec-at-pt))
  :diminish evil-numbers-mode)
(use-package evil-multiedit
  :after evil
  :bind (:map evil-normal-state-map
              ("C-;" . evil-multiedit-match-all))
  :config
  (evil-multiedit-default-keybinds))
(use-package evil-goggles
  :after evil
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse nil)
  (setq evil-goggles-duration 0.1)
  (evil-goggles-use-diff-faces))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list nil)
  (setq evil-collection-mode-list '(anaconda-mode
                                    realgud
                                    ivy
                                    dired
                                    company
                                    minibuffer
                                    paren
                                    magit
                                    reftex
                                    calendar
                                    eww))
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-outline-bind-tab-p nil)
  (evil-collection-init))
(use-package evil-org
  :diminish evil-org-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-mc
  :disabled
  :after evil
  :diminish evil-mc-mode
  :bind (:map evil-normal-state-map
              ("C-n" . evil-mc-make-and-goto-next-match)
              ("C-c n" . evil-mc-make-cursor-move-next-line))
  :config
  (global-evil-mc-mode 1))
(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)  
  (setq key-chord-one-key-delay 0.5) 
  (key-chord-define evil-insert-state-map "]]" "\\")
  (key-chord-define evil-insert-state-map ";;" "/")
  (key-chord-define evil-insert-state-map "::" "?")
  (key-chord-define evil-insert-state-map "}}" "|")
  (key-chord-define evil-insert-state-map "==" "+")
  (key-chord-define evil-insert-state-map "99" "(")
  (key-chord-define evil-insert-state-map "--" "_")
  (key-chord-define evil-insert-state-map "''" "^")
  (key-chord-define evil-insert-state-map "[[" "{"))
(use-package beacon
  :diminish beacon-mode
  :defer 10
  :config
  (setq beacon-blink-delay .5)
  (setq beacon-size 4)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-duration .5)
  (setq beacon-blink-when-window-scrolls t)
  (beacon-mode 1))
(use-package undo-tree
  :diminish (undo-tree-mode)
  :defer t)
(use-package eldoc
  :ensure nil
  :defer t
  :diminish (eldoc-mode))
(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  
  ;;This setting is needed to use ivy completion:
  (setq magit-completing-read-function 'ivy-completing-read)
  
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun my-magit-stage-all-and-commit(message)
    (interactive "sCommit Message: ")
    (magit-stage-modified)
    (magit-commit (list "-m" message)))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))
(use-package iedit
  :defer t)
(use-package rainbow-mode
  :diminish rainbow-mode
  :config (rainbow-mode))
(use-package org
  :ensure org-plus-contrib
  :diminish org-indent-mode
  :mode (("\\.org$" . org-mode))
  :bind(("C-c c" . org-capture)
        ("C-c a" . org-agenda)
        :map org-mode-map
             ("C-c l" . org-store-link)
             ("M-p" . org-previous-item)
             ("M-n" . org-next-item))
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  :config
  (transient-mark-mode -1)
  (setq org-special-ctrl-a/e t
	org-log-done 'time         ;Log the time a task is completed.
	org-habit-following-days 4
	org-hide-emphasis-markers t 
	inhibit-splash-screen t
	org-startup-indented nil
	org-hide-leading-stars t 
	org-startup-align-all-tables t ;align tables on startup
	org-hide-leading-stars-before-indent-mode nil
	org-odd-levels-only t
	org-use-speed-commands t
	org-edit-src-content-indentation 0
	org-support-shift-select t
	line-spacing '0.1 
	org-ellipsis "…"
	org-modules '(org-habit)
	org-cycle-include-plain-lists t
	org-image-actual-width nil
	org-goto-interface 'outline-path-completion ;; org goto play nice with ivy
	org-goto-max-level 4
	org-outline-path-complete-in-steps nil
	org-startup-with-inline-images t
	org-cycle-separator-lines 0)
	
  (set-face-attribute 'org-ellipsis nil :underline nil)
  (eval-after-load 'org
    '(org-load-modules-maybe t))
  
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf::\\([0-9]+\\)\\'" . "sumatrapdf \"%s\" -page %1")
                        ("\\.pdf\\'" . default)))

  ;; (set-face-attribute 'org-block-begin-line nil :foreground "#005f87")
  ;; (set-face-attribute 'org-block-end-line nil :foreground "#3a3a3a")
  ;; org markups meta line --> change to grey100 when presenting
  (set-face-attribute 'org-meta-line nil :height 0.7)
  (set-face-attribute 'org-special-keyword nil :height  0.7)
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "REVW" "|" "DONE(d)")))
  ;; ;; change todo faces
  (setq org-todo-keyword-faces 
        '(("NEXT" :foreground "#edd400" :weight bold)
	  ("TODO" :foreground "tomato" :weight bold)
	  ("DONE" :foreground "medium sea green" :weight bold)
          ("REVW" :foreground "deep sky blue" :weight bold)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (latex . t)
     (plantuml . t)
     (shell . t)))

  ;; plantuml jar file path
  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/plantuml.jar"))
  (setq org-babel-default-header-args:python
        '((:exports . "both")
          (:results . "output")))

  ;; Org babel and source blocks
  (setq org-src-fontify-natively t
        org-highlight-latex-and-related '(latex)
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t  ; preserve indentation in code
        org-adapt-indentation nil ; Non-nil means adapt indentation to outline node level.
        org-src-tab-acts-natively t
        org-export-babel-evaluate nil
        org-confirm-babel-evaluate nil) ; doesn't ask for confirmation

  ;; dont guess the indent offset
  (setq python-indent-guess-indent-offset nil)
 ;;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
                                 "~/OneDrive/Org/notes.org"
                                 "~/OneDrive/Org/journal.org"
                                 "~/OneDrive/Org/gcal.org"
                                 "~/OneDrive/Concurso/Notas/notas_concurso.org")))
  (setq org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-timestamp-if-done nil
	org-agenda-use-time-grid nil)

  (setq org-imenu-depth 2)
  (setq org-default-notes-file "~/OneDrive/Org/notes.org")

  ;; Which days are weekend?
  ;; (setq org-agenda-weekend-days nil)
  ;; start agenda on current day
  (setq org-agenda-start-on-weekday 1)
  ;; min and max percentages of agenda window height
  (setq org-agenda-window-frame-fractions '(0 . 1))

  ;; Days on the overview display
  (setq org-agenda-span 1)

  ;; (setq org-agenda-time-grid
  ;;     '((daily today require-timed)
  ;;       (800 1000 1200 1400 1600 1800 2000)
  ;;       "......" "----------------"))
  
  (setq org-agenda-prefix-format '(
  ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
  (agenda  . " %i %-16:c%?-16t% s")
  (timeline  . "  % s")
  (todo  . " %i %-12:c")
  (tags  . " %i %-12:c")
  (search . " %i %-12:c")))

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (todo "TODO")))))
  
  ;; (global-set-key (kbd "C-c o") 
  ;;                 (lambda () (interactive) 
  ;;                   (find-file "~/OneDrive/Org/notes.org")))

  (setq org-capture-templates
        '(("t" "Todo" entry (file+datetree "~/OneDrive/Org/gtd.org") 
           "* TODO %? \n\n Added: %T")
          ("n" "Notes" entry (file+datetree "~/OneDrive/Org/notes.org") 
           "* %^{Description} %^g \n\n %? \n\n Added: %T")
          ("j" "Journal" entry (file+datetree "~/OneDrive/Org/journal.org") 
           "* %T \n\n%?"))))
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
(use-package org-clock
  :after org
  :ensure nil
  :config
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persistence-insinuate t
	org-clock-persist t
	org-clock-in-resume t
	org-clock-mode-line-total 'current
	org-duration-format (quote h:mm))

  (setq org-clocktable-defaults
        '(:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize t :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns 3 :formatter nil))

  ;; remove schedule tag on agenda
  (setq org-agenda-scheduled-leaders '("" ""))
  (setq org-agenda-block-separator "")
  ;; Save clock data and notes in the LOGBOOK drawer
  ;; (setq org-clock-into-drawer t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-clock-into-drawer 1)
  
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t))
(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'reversed))
(use-package flyspell
  :diminish flyspell-mode
  :after flyspell-lazy
  :commands flyspell-mode
  :init
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  :config
  (setq ispell-program-name "hunspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (setq ispell-dictionary "en_US,pt_BR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,pt_BR"))
(use-package flyspell-lazy
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'flyspell-lazy-mode)
  (add-hook 'org-mode-hook 'flyspell-lazy-mode)
  :config
  (flyspell-lazy-mode 1))
(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c C-SPC" . flyspell-correct-word-generic)))
(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.1
        company-echo-delay 0 ; remove annoying blinking)
        company-minimum-prefix-length 2
        company-show-numbers t 
        company-require-match 'never  ; 'company-explicit-action-p
	company-selection-wrap-around t
        company-tooltip-flip-when-above t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  ;; display inline
  (setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend)))
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))
(use-package company-childframe
  :diminish company-childframe-mode
  :after company
  :config
  (company-childframe-mode 1)
  ;; let desktop.el not record the company-childframe-mode
  (require 'desktop) ;this line is needed.
  (push '(company-childframe-mode . nil)
        desktop-minor-mode-table))
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :bind ("C-S-f" . forward-whitespace)
  :init
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (prettify-symbols-mode)
              (LaTeX-math-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)
              (turn-off-auto-fill)))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  :config
  (setq TeX-save-query nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)       ;enable document parsing
  (setq-default TeX-master nil) ;make auctex aware of multi-file documents
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq TeX-electric-escape t)
  (setq global-font-lock-mode t)
  (font-lock-add-keywords 'latex-mode
                          (list (list "\\(«\\(.+?\\|\n\\)\\)\\(+?\\)\\(»\\)"
                                      '(1 'font-latex-string-face t)
                                      '(2 'font-latex-string-face t)
                                      '(3 'font-latex-string-face t))))

  ;; Method for enabling forward and inverse search 
  (setq TeX-source-correlate-method 'synctex)
  ;; inhibit the question to start a server process
  (setq TeX-source-correlate-start-server t)

  ;; Use pdf tools
  ;;
  ;;
  ;; add "PDF Tools" to the list of possible PDF tools  
  ;; (unless (assoc "PDF Tools" TeX-view-program-list  
  ;;                (add-to-list 'TeX-view-program-list  
  ;;                             '("PDF Tools" TeX-pdf-tools-sync-view)))
  ;;   (add-to-list 'TeX-view-program-selection  
  ;;                '(output-pdf "PDF Tools")))
  
  ;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  
  ;;   ;; Update PDF buffers after successful LaTeX runs  
  ;;   (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook  
  ;;             'TeX-revert-document-buffer)

  
  ;; use sumatra to view pdf
  ;;
  ;; 
  ;; http://stackoverflow.com/questions/14448606/sync-emacs-auctex-with-sumatra-pdf
  ;; -set-color-range #fdf4c1 #282828
  (setq TeX-view-program-list
        '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                          (mode-io-correlate " -forward-search %b %n ") " %o"))))

  (eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))
  ;; jump to source
  (setq TeX-source-correlate-mode t)
  
  ;; Custom functions
  ;;
  ;; 
  (defun my/tex-insert-clipboard ()
    (interactive)
                                        ;make the img directory
    (setq myvar/folder-path (concat default-directory "img/"))
                                        ;create the directory if it doesn't exist
    (if (not (file-exists-p myvar/folder-path))
        (mkdir myvar/folder-path))
    (setq my/image-path (concat 
                         myvar/folder-path
                         "img_"
                         (format-time-string "%Y%m%d_%H%M%S_.png")))
    (let* ((image-file (concat 
                        "img/img_"
                        (format-time-string "%Y%m%d_%H%M%S_.png")))
           (exit-status
            (call-process "convert" nil nil nil
                          "clipboard:" my/image-path)))
      (insert (format "
\\begin{figure}[ht!]
  \\centering
  \\includegraphics[width=.5\\textwidth]{%s}
\\end{figure}" image-file))
      )))
(use-package reftex
  :after latex
  :bind ("C-c =" . reftex-toc)
  :config
  (setq reftex-cite-prompt-optional-args t) ; Prompt for empty optional arguments in cite
  ;; https://www.gnu.org/software/emacs/manual/html_mono/reftex.html
  (setq reftex-enable-partial-scans t)
  (setq reftex-keep-temporary-buffers nil)
  (setq reftex-save-parse-info t)
  (setq reftex-trust-label-prefix '("fig:" "eq:"))
  (setq reftex-default-bibliography "C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib"))
(use-package company-bibtex
  :after latex
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography
	'("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib")))
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTex-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode))
(use-package dired
  :ensure nil
  :commands dired
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (setq dired-hide-details-mode t)
  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup))
(use-package spacemacs-common
  :disabled
  :ensure spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t)
  (custom-theme-set-faces
   'user
     `(org-level-5 ((t (:height 0.95 :slant italic))))
     `(org-level-7 ((t (:height 0.9 :slant italic))))  
     `(org-level-9 ((t (:height 0.9 :slant italic))))  
     `(org-table ((t (:family "Monospace Serif" :height 0.8))))
     `(org-formula ((t (:family "Monospace Serif" :height 0.8 :foreground "chocolate"))))))
(use-package darktooth-theme
  :disabled
  :config
  (load-theme 'darktooth t))
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line nil)
  (setq solarized-use-less-bold nil)
  (setq solarized-use-more-italic t)
  (setq solarized-emphasize-indicators nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))
(use-package challenger-deep-theme
  :disabled
  :config
  (load-theme 'challenger-deep t)
  (custom-theme-set-faces
   'user
     `(org-level-5 ((t (:height 0.95 :slant italic))))
     `(org-level-7 ((t (:height 0.9 :slant italic))))  
     `(org-level-9 ((t (:height 0.9 :slant italic))))  
     `(org-table ((t (:family "Monospace Serif" :height 0.8))))
     `(org-formula ((t (:family "Monospace Serif" :height 0.8))))))
(use-package smart-mode-line
  :defer 2
  :config
  (setq sml/name-width 20)
  (setq sml/shorten-directory t)
  (setq sml/theme 'respectful)
  (setq after-save-hook nil)
  (sml/setup))
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; change commenting foreground
  (setq warning-suppress-types '((python)
                                 (emacs)))
  ;; suppress the warning "python.el: native completion setup failed"
  (with-eval-after-load 'python
    (defun python-shell-completion-native-try ()
      "Return non-nil if can trigger native completion."
      (let ((python-shell-completion-native-enable t)
            (python-shell-completion-native-output-timeout
             python-shell-completion-native-try-output-timeout))
        (python-shell-completion-native-get-completions
         (get-buffer-process (current-buffer))
         nil "_"))))
  (setenv "PYTHONIOENCODING" "utf-8")
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil))
(use-package highlight-indent-guides
  :after python
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'lisp-interaction-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))
(use-package adaptive-wrap
  :ensure adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))
(use-package calfw
  :commands cfw:open-org-calendar)
(use-package calfw-org
  :after org calfw)
(use-package toc-org
  :after org
  :config  (add-hook 'org-mode-hook 'toc-org-enable))
(use-package hydra
  :defer 5
  :bind (("C-c C-w" . hydra-window-resize/body)
         ("C-c C-u" . hydra-outline/body)
         ("C-x C-m " . multiple-cursors-hydra/body)
         ("C-x C-'" . hydra-fold/body))
  :config
  (defhydra hydra-fold (:pre (hs-minor-mode 1))
    "fold"
    ("t" fold-dwim-toggle "toggle")
    ("h" fold-dwim-hide-all "hide-all")
    ("s" fold-dwim-show-all "show-all")
    ("q" nil "quit"))
  
  (defun my-funcs/resize-window-down ()
    "Resize a window downwards."
    (interactive)
    (if (window-in-direction 'below)
        (enlarge-window 1)
      (shrink-window 1)))
  (defun my-funcs/resize-window-up ()
    "Resize a window upwards."
    (interactive)
    (if (window-in-direction 'above)
        (enlarge-window 1)
      (shrink-window 1)))
  (defun my-funcs/resize-window-left ()
    "Resize a window leftwards."
    (interactive)
    (if (window-in-direction 'left)
        (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1)))
  (defun my-funcs/resize-window-right ()
    "Resize a window rightwards."
    (interactive)
    (if (window-in-direction 'right)
        (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1)))
  (defhydra hydra-window-resize (global-map "C-c w")
    "Window resizing"
    ("j" my-funcs/resize-window-down "down")
    ("k" my-funcs/resize-window-up "up")
    ("l" my-funcs/resize-window-right "right")
    ("h" my-funcs/resize-window-left "left"))
  
  (defhydra hydra-outline (:color pink :hint nil)
    "
 ^Hide^             ^Show^           ^Move
 ^^^^^^------------------------------------------------------
 _q_: sublevels     _a_: all         _u_: up
 _t_: body          _e_: entry       _n_: next visible
 _o_: other         _i_: children    _p_: previous visible
 _c_: entry         _k_: branches    _f_: forward same level
 _l_: leaves        _s_: subtree     _b_: backward same level
 _d_: subtree   _<tab>_: cycle

 "
    ;; Hide
    ("q" hide-sublevels)  ; Hide everything but the top-level headings
    ("t" hide-body)    ; Hide everything but headings (all body lines)
    ("o" hide-other)   ; Hide other branches
    ("c" hide-entry)   ; Hide this entry's body
    ("l" hide-leaves)  ; Hide body lines in this entry and sub-entries
    ("d" hide-subtree) ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" show-all)                      ; Show (expand) everything
    ("e" show-entry)                    ; Show this heading's body
    ("i" show-children) ; Show this heading's immediate child sub-headings
    ("k" show-branches) ; Show all sub-headings under this heading
    ("s" show-subtree) ; Show (expand) everything in this heading & below
    ("<tab>" org-cycle)
    ;; Move
    ("u" outline-up-heading)               ; Up
    ("n" outline-next-visible-heading)     ; Next
    ("p" outline-previous-visible-heading) ; Previous
    ("f" outline-forward-same-level)       ; Forward - same level
    ("b" outline-backward-same-level)      ; Backward - same level
    ("z" nil "leave"))
  
  (defhydra multiple-cursors-hydra (:hint nil)
    "
      ^Up^            ^Down^        ^Other^
 ----------------------------------------------
 [_p_]   Next    [_n_]   Next    [_l_] Edit lines
 [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
 [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
 ^ ^             ^ ^             [_q_] Quit
 "
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))
  (defhydra hydra-origami (:color red)
    "
  _o_pen node    _n_ext fold       toggle _f_orward    _t_oggle recursively
  _c_lose node   _p_revious fold   toggle _a_ll 
  "
    ("o" origami-open-node)
    ("t" origami-recursively-toggle-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))

  (defhydra hydra-move-previous
     (:body-pre (previous-line))
     "move"
     ("n" next-line)
     ("p" previous-line)
     ("<tab>" org-cycle)
     ("q" nil))
  
   (defhydra hydra-move-next
     (:body-pre (next-line))
     "move"
     ("n" next-line)
     ("p" previous-line)
     ("<tab>" org-cycle)
     ("q" nil)))
(use-package goto-last-change
  :bind ("C-x C-j" . goto-last-change))
(use-package avy
  :diminish avy-mode
  :bind (("C-x C-SPC" . avy-goto-char)
         ("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.4))
;; abbrev for speed and less strain
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(setq save-abbrevs 'silently)

(bind-key "C-x C-o" 'next-multiframe-window)

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
