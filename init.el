(defvar my-start-time (current-time)
  "Time when Emacs was started")

(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; development branch of straight
(setq straight-repository-branch "develop")

;; bootstrap straight.el
;; straight automatically checks if it needs to be rebuilt
;; straight generates autoloads 
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install `use-package`
;; `alway-defer` means that for a package to load we need a `:hook` or using a `:general` keybinding 
;; if there is none, we need to explicitly add `:demand` to load the package
;; can also load with `:defer time`
(straight-use-package 'use-package)
(setq use-package-verbose nil		; don't print anything
      use-package-compute-statistics nil; compute statistics about package initialization
      use-package-expand-minimally t	; minimal expanded macro
      use-package-always-defer t)	; always defer, don't "require", except when :demand

;; install package with same name expect specified otherwise
;; use-package expands to straight-use-package (excepts when :straight nil)
(setq straight-use-package-by-default t)

;; prevent Emacs-provided Org from being loaded
(straight-register-package 'org)
(straight-register-package 'org-contrib)

;; general for kybinding
(use-package general
  :demand)

;; control minor-mode indication in the mode-lie
(use-package diminish
  :demand)

;; minimizes GC interferecen with user activity
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 0.5
	gcmh-high-cons-threshold (* 16 1024 1024)) 
  (gcmh-mode 1))

;; basics and better default
(use-package emacs
  :general
  ('normal "gy" 'revert-buffer)
  ("C-<tab>" 'next-window-any-frame)
  ("<backtab>" 'previous-window-any-frame)
  ("C-c w" 'enlarge-window-horizontally)
  ("C-x C-M-e" 'pp-macroexpand-last-sexp)
  ("C-x C-e" 'eval-defun)
  ("C-x e" 'eval-last-sexp)
  ("C-h j" 'describe-keymap)
  :init
  (global-hl-line-mode t) ; highlight current line
  (winner-mode t)	  ; move between windows configuration
  (setq-default fill-column 80)	  ; column length
  (column-number-mode t)  ; show column number in the mode line

  ;; name on top of window
  (setq-default frame-title-format '("%b [%m]"))

  (setq warning-minimum-level :error)		 ;avoid warning buffer

  ;; scroll
  (setq auto-window-vscroll nil) 		;avoid next-line to trigger line-move-partial
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-follow-mouse 't)
  (setq scroll-step 1)

  ;; other basiscs
  (setq ring-bell-function 'ignore)
  (setq inhibit-startup-screen t)

  ;; UTF-8 encoding
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; create backups in separate folder
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq create-lockfiles nil)		; files with # problem with onedrive...

  ;; answering just 'y' or 'n' will do
  (defalias 'yes-or-no-p 'y-or-n-p)

  (if (display-graphic-p)
      (blink-cursor-mode 1)
    (progn
      (blink-cursor-mode -1)
      (setq visible-cursor nil)))
  
  (setq-default
   completion-cycle-threshold 3	    ; TAB cycle if there are only few candidates
   completions-detailed t	    ; add details in completions as prefix/sufix
   idle-update-delay 1.1  ; Slow down the UI being updated to improve performance
   enable-recursive-minibuffers t	; Enable recursive minibuffers
   resize-mini-windows nil		; Avoid grow and shrink minibuffer
   visible-bell t			; Don't beep at me
   kill-buffer-query-functions nil) ; don't ask if it is ok to kill a process when killing a buffer

  ;; do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; don't need to confirm to revert buffer
  (setq revert-without-query t)

  (setq-default
   indent-tabs-mode nil	     ; don't insert tab when indent
   ;; this is giving me problems when creating new lines in org-mode source blocks
   tab-always-indent 'complete ; tab indents first, then tries to complete
   help-window-select t	    ; focus on help window when openend
   window-combination-resize t)) ; resize windows proportionaly

;; custom emacs theme
(use-package custom-theme
  :straight (emacs :type built-in)
  :when (display-graphic-p)
  :custom-face 
  ;; latex
  (font-latex-sectioning-1-face ((t (:weight bold :slant italic :box t))))
  (font-latex-sectioning-2-face ((t (:weight bold))))
  (font-latex-sectioning-3-face ((t (:weight bold :underline t))))
  (font-latex-sectioning-4-face ((t (:weight normal :slant normal))))
  (font-latex-sectioning-5-face ((t (:weight normal :slant italic :underline t))))
  (font-latex-string-face ((t (:foreground "SaddleBrown"))))
  ;; general
  (font-lock-comment-face ((t (:foreground "gray60"))))
  ;; auto-dim-other-buffers
  (auto-dim-other-buffers-face ((t (:background "gray94"))))
  ;; org
  (org-block ((t (:background "gray97"))))
  (org-meta-line ((t (:height 0.9 :inherit 'font-lock-comment-face))))
  (org-drawer ((t (:inherit 'font-lock-comment-face :height 0.9))))
  (org-verbatim ((t (:box t)))))

;; change typeface size font
(use-package emacs
  :general
  ("C-M-=" 'zoom-frame)
  ("C-M--" 'zoom-frame-out)
  :init
  ;; setting typeface size
  (defun zoom-frame (&optional amt frame)
    "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
    (interactive "p")
    (let* ((frame (or frame (selected-frame)))
           (font (face-attribute 'default :font frame))
           (size (font-get font :size))
           (amt (or amt 1))
           (new-size (+ size amt)))
      (set-frame-font (font-spec :size new-size) t `(,frame))))

  (defun zoom-frame-out (&optional amt frame)
    "Call `zoom-frame' with negative argument."
    (interactive "p")
    (zoom-frame (- (or amt 1)) frame)))

(use-package abbrev
  :straight (:type built-in)
  :config
  ;; abbrev for speed and less strain
  (setq-default abbrev-mode t)
  (diminish 'abbrev-mode)
  (setq save-abbrevs 'silently))

(use-package color-identifiers-mode)

;; save recent visited files
(use-package recentf
  :defer 5
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 50
	recentf-auto-cleanup 'mode))

(use-package autorevert
  :if (eq system-type 'gnu/linux)
  :defer 1
  :config
  (setq auto-revert-interval 5)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h d" 'helpful-at-point)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key))

;; enhances default minibuffer and completions buffer
;; easily navigate from minibuffer into completions buffer
;; problems: no history sorting, no reverse
(use-package mct
  :disabled
  :straight (mct :type git :host gitlab :repo "protesilaos/mct")
  :init
  (mct-mode)
  :config
  (setq mct-live-update-delay 0
        mct-minimum-input 1))

;; completion UI (vertical list in minibuffer)
(use-package vertico
  :straight (vertico :type git :host github :repo "minad/vertico"
        	     :includes (vertico-buffer
        			vertico-directory
                                vertico-flat
        			vertico-repeat)
        	     :files (:defaults "extensions/vertico-buffer.el"
        			       "extensions/vertico-directory.el"
        			       "extensions/vertico-flat.el"
        			       "extensions/vertico-repeat.el"))
  :general
  ('insert vertico-map "C-k" 'vertico-exit-input)
  :init
  (vertico-mode)
  (setq vertico-resize t))

;; improves behavior when dealing with directories in the minibuffer
(use-package vertico-directory
  :straight nil
  :after vertico
  :general
  (vertico-map "RET" 'vertico-directory-enter
	       "DEL" 'vertico-directory-delete-char
	       "M-DEL" 'vertico-directory-delete-word)
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; repeat last vertico session
(use-package vertico-repeat
  :straight nil
  :after vertico
  :general
  ("M-r" 'vertico-repeat))

;; use vertico to complete in region with orderless in terminal
(use-package vertico
  :straight nil
  :unless (display-graphic-p)
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; display completions in anotherbuffer instead of minibuffer
;; avoids window been pushed up
(use-package vertico-buffer
  :straight nil
  :after vertico
  :init
  (vertico-buffer-mode)
  (setq vertico-buffer-display-action '(display-buffer-in-side-window
                                        (window-height . 13)
                                        (side . bottom))))

;; `completion STYLE` with flexible candidate filtering
;; filter with space-separated components and match components in any order
;; filter means how a input string is matched against candidates
(use-package orderless
  :init
  ;; partial completion for files to allows path expansion
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        read-file-name-completion-ignore-case t
        completion-category-overrides '((file (styles . (partial-completion)))
                                        ;; navigate files with initials
                                        (minibuffer (initials)))))

;; save the search history
(use-package savehist
  :init
  (savehist-mode))

;; minibuffer annotations details
(use-package marginalia
  :if (eq system-type 'gnu/linux)
  :general
  (minibuffer-local-map "M-A" 'marginalia-cycle)
  :init (marginalia-mode))

;; practical navigation and search commands 
(use-package consult
  :general
  ;; m/f/b <SPC> for bookmarks/files/buffers narrowing
  ("C-x b" 'consult-buffer)		; enhanced switch to buffer
  ("M-s" 'consult-outline)		; navigation by headings
  ("C-c o" 'consult-imenu)		; navigation by "imenu" items
  ("M-y" 'consult-yank-pop)		; editing cycle through kill-ring
  ("C-s" 'consult-line)			; search lines with preview
  ("C-c m" 'consult-mark)
  ;; two parts: search  and filter
  ;; #<search string>#<filter terms> filtering with orderless! amazing!
  ("C-c r" 'consult-ripgrep)		; search file contents
  ("C-c f" 'consult-find-fd)		; search files in directories
  ;; (minibuffer-local-completion-map "<tab>" 'minibuffer-force-complete)
  :hook
  ;; hook for using default completion mode
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; configure preview behavior to `any` key and specific time delay
  (consult-customize consult-buffer consult-bookmark consult-ripgrep consult-find-fd
                     :preview-key '(:debounce 5 any))
  (consult-customize consult-line :preview-key '(:debounce 0 any))

  ;; use 'fd' instead of 'find'
  (defun consult-find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))

  ;; set project root from vc.el
  ;; available when a project file is visited (project-switch-project)
  (setq consult-project-root-function #'vc-root-dir))

;; insert recent openend directories in prompt
(use-package consult-dir
  :straight (consult-dir :type git :host github :repo "karthink/consult-dir") 
  :general
  ("C-x C-d" 'consult-dir)
  (vertico-map "C-x C-d" 'consult-dir))

;; context menu/action at point or inside the minibuffer (on the top candidate)
;; or in a region
;; `embark-collect` allows acting on a `set of targets` (snapshot or live)
;; `live` works like a completion narrowing that Vertico does 
;; `export` the set of targets are shown in an appropriate major-mode
;; embark-mixed-indicator: if no action is selected, buffer will pop up
(use-package embark
  :demand                               ; load it independently of bind and hook
  :general
  ("M-a" 'embark-act)
  ("C-S-z" 'embark-dwim)
  ("C-h B" 'embark-bindings)
  :commands embark-prefix-help-command
  :hook
  ;; use embark to help find command after prefix (C-h after a prefix key)
  ;; if this is used before `embark-act`, load the package (because of `:commands`)
  ;; `which-key` changes this variable as well, so we need to change it after `which-key`
  (which-key-mode . (lambda ()
		      (setq prefix-help-command #'embark-prefix-help-command))))

;; allows consult previews as you move around an auto-updating embark collect
;; buffer
;; `exbark-collects` grep results to a grep buffer
(use-package embark-consult
  :demand				;necessary for consult preview
  :hook (embark-collect-mode . embark-consult-preview-minor-mode)
  :after (embark consult))

;; utility for using icons fonts
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 0.8))

;; automatic insert matching pairs and navigation
;; highlight matching parens
;; for wrap/unwrap I use evil-surround
;; expand/contract (slurp) is good for elisp
(use-package smartparens
  :straight (:includes smartparens-config)
  :diminish smartparens-mode  
  :general
  ('normal smartparens-mode-map "M-l" 'sp-next-sexp)
  ('normal smartparens-mode-map "M-h" 'sp-previous-sexp)
  ('normal smartparens-mode-map "M-k" 'sp-up-sexp)
  ('normal smartparens-mode-map "M-j" 'sp-down-sexp)
  ('normal smartparens-mode-map "C-M-l" 'sp-forward-sexp)
  ;; binding all modes for Latex
  ('insert '(prog-mode-map LaTeX-mode-map org-mode-map) "C-<tab>" 'sp-forward-sexp)
  :hook
  (prog-mode . smartparens-mode)
  (LaTeX-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  (smartparens-mode . smartparens-strict-mode) ; enforce pairs to be balanced
  (smartparens-mode . show-smartparens-mode) ; instead of default show-paren-mode
  :config
  (setq sp-show-pair-delay 0
	sp-show-pair-from-inside t))

(use-package smartparens-config
  :demand
  :after smartparens)

;; experimenting with built in flymake
(use-package flycheck
  :disabled
  :hook
  (python-mode . flycheck-mode))

(use-package evil-multiedit
  :after evil
  :custom-face
  (iedit-occurrence ((t (:background "plum1"))))
  :general
  ('visual "R" 'evil-multiedit-match-all)
  ("M-d" 'evil-multiedit-match-and-next)
  ("M-C-d" 'evil-multiedit-match-and-prev)
  (evil-multiedit-state-map "<tab>" 'evil-multiedit-toggle-or-restrict-region) ;RET will toggle the region under the cursor
  (evil-multiedit-state-map "C-j" 'evil-multiedit-next) 
  (evil-multiedit-state-map "C-k" 'evil-multiedit-prev)
  ('visual "C-S-d" 'evil-multiedit-restore))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode
  :general
  ;; autoload keymap, `g s` will trigger the loading of `evil-mc` library
  ;; change prefix for `cursors-map`
  ('(normal visual) "g s" '(:keymap evil-mc-cursors-map))
  ('(normal visual) evil-mc-key-map "g s a" 'evil-mc-make-cursor-in-visual-selection-beg)
  ;; evil-mc-cursors-map is accessed with evil-mc-cursors-map
  (evil-mc-cursors-map
    "n" 'evil-mc-make-and-goto-next-match
    "p" 'evil-mc-make-and-goto-prev-match
    "N" 'evil-mc-skip-and-goto-next-match
    "P" 'evil-mc-skip-and-goto-prev-match)
  :config
  (global-evil-mc-mode 1)
  (push '(evil-org-delete . ((:default . evil-mc-execute-default-evil-delete)))
        evil-mc-known-commands)
  (push '(evil-digit-argument-or-evil-org-beginning-of-line . ((:default . evil-mc-execute-default-call)))
        evil-mc-known-commands)
  )

(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-keybinding nil ; preference for evil-collection
        evil-want-minibuffer t); evil in minibuffer
  (evil-mode 1)
  :general
  (evil-motion-state-map "C-i" nil)     ; avoid conflicting with tab in terminal
  ('normal ";" 'evil-search-forward)
  ('normal "M-p" 'evil-paste-from-register)
  ('(normal visual) :prefix "SPC" "l" 'evil-last-non-blank)
  ('(normal visual) :prefix "SPC" "h" 'evil-first-non-blank)
  ('normal :prefix "SPC" "a" 'evil-append-line)
  ('normal :prefix "SPC" "x s" 'save-buffer)
  ('(normal visual) "[ ]" 'evil-next-close-paren)
  ('(normal visual) "] [" 'evil-previous-open-paren)
  ('normal "j" 'evil-next-visual-line)
  ('normal "k" 'evil-previous-visual-line)
  ('normal "z q" 'evil-scroll-line-to-top)
  ('normal "C-c r" nil)
  :config
  (setq
   lazy-highlight-cleanup nil
   lazy-highlight-max-at-a-time nil
   lazy-highlight-initial-delay 0)
  (evil-set-undo-system 'undo-redo)	; use native redo function
  
  ;; fix tab behavior in org-mode source block
  (defun evil-org-insert-state-in-edit-buffer (fun &rest args)
    "Bind `evil-default-state' to `insert' before calling FUN with ARGS."
    (let ((evil-default-state 'insert)
	  ;; Force insert state
	  evil-emacs-state-modes
	  evil-normal-state-modes
	  evil-motion-state-modes
	  evil-visual-state-modes
	  evil-operator-state-modes
	  evil-replace-state-modes)
      (apply fun args)
      (evil-refresh-cursor)))

  (advice-add 'org-babel-do-key-sequence-in-edit-buffer
	      :around #'evil-org-insert-state-in-edit-buffer))

;; move around text
(use-package evil-easymotion
  :defer 1
  :after evil
  :config
  (evilem-default-keybindings "SPC"))

;; move aroud text
(use-package evil-snipe
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode)
  :general ('normal "f" 'evil-snipe-f)
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible
	evil-snipe-smart-case t))

;; visualize evil commands
(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :defer 1
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse t)
  (setq evil-goggles-duration 0.2)
  (evil-goggles-use-diff-faces))

;; unimpaired is a collection of commands with '[' or ']'
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :init
  (setq evil-collection-setup-minibuffer nil ; does not play nice with vertico
	evil-collection-company-use-tng nil) ; makes company works betters I think
  (evil-collection-init))

;; navigation: gh, gj, gk, gl
;; cycling headings: tab
;; headings: M-ret
(use-package evil-org
  :straight (:includes evil-org-agenda)
  :diminish evil-org-mode
  :after evil org
  :hook (org-mode . evil-org-mode))

;; included in evil-org
;; load it when using agenda
(use-package evil-org-agenda
  :requires evil-org
  :config
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :after evil
  :general
  ('normal "g c" 'evil-surround-change)
  ('visual "S" 'evil-Surround-region)
  :init
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :after evil
  :general ('normal "g x" 'evil-exchange)
  :config (evil-exchange-install))

(use-package evil-matchit
  :after evil python
  :config
  (global-evil-matchit-mode 4))

(use-package beacon
  :defer 1
  :diminish beacon-mode
  :config
  (setq beacon-blink-delay 0)
  (setq beacon-size 40)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-duration .3)
  (setq beacon-blink-when-window-scrolls nil)
  (beacon-mode 1))

(use-package undo-propose
  :after evil
  :general
  ('normal 'global "C-c u" 'undo-propose)
  ('normal 'global "u" 'undo-only)
  :config
  (setq undo-propose-pop-to-buffer t))

(use-package magit
  :general
  (magit-mode-map "C-<tab>" nil)
  ("C-x g" 'magit-status)
  :config
  (setq magit-diff-hide-trailing-cr-characters t)
  ;; open commit in insert mode
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

;; show colors
(use-package rainbow-mode
  :commands rainbow-mode 
  :diminish rainbow-mode)

(use-package org-contrib)

(use-package org
  :straight (:includes (org-id oc ob org-clock org-src org-agenda
                               ox-latex ob-shell ob-python ox-html))
  :diminish org-indent-mode
  :mode (("\\.org$" . org-mode))
  :general
  (org-mode-map "C-c C-l" 'org-insert-link)
  (org-mode-map "C-c ," 'org-insert-structure-template)
  ('normal org-mode-map "TAB" 'org-cycle)
  ('normal org-mode-map "z n" 'org-toggle-narrow-to-subtree)
  ('normal org-mode-map "z k" 'org-previous-visible-heading)
  ('normal org-mode-map "z j" 'org-next-visible-heading)
  ('normal org-mode-map "g k" 'org-backward-heading-same-level)
  ('normal org-mode-map "g j" 'org-forward-heading-same-level)
  ('normal org-mode-map :prefix "z"
	   "s j" 'org-babel-next-src-block
	   "s k" 'org-babel-previous-src-block)
  :hook
  (org-mode . visual-line-mode)
  :custom
   (org-hide-emphasis-markers nil)        ; avoid noisy //,__, **(makes anoying to edit) 
   (org-startup-indented nil)		; start collapsed
   (org-startup-folded t)               ; folded in "overview" state
   (org-hide-leading-stars t)           ; don't show a  bunch of '*'
   (org-edit-src-content-indentation 0)
   (org-outline-path-complete-in-steps nil)
   (org-startup-with-inline-images t)
   (org-special-ctrl-a/e t)       ; when jump to beginning of line be aware of *
   (org-cycle-separator-lines 0)  ; no empty lines between headings
   (org-fontify-quote-and-verse-blocks t) ; yes syntax highlighting
   (org-insert-heading-respect-content t) ; insert heading after current tree
   (org-catch-invisible-edits 'smart)
   (org-html-htmlize-output-type 'inline-css)   ; nil to export as plain text
   (org-image-actual-width nil)     ; if width is specified use that, otherwise keep original size
  :config
  (transient-mark-mode -1)

  ;; remove org-cycle-hide-drawers from cycle hook
  ;; so it shows the plots inside a "results drawer" when the heading is opened
  (setq org-cycle-hook
        '(org-cycle-hide-archived-subtrees
          org-cycle-hide-drawers
          org-cycle-show-empty-lines
          org-optimize-window-after-visibility-change))

  ;; display images after executing
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(use-package ox-extra
  :after org
  :demand
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package ox-html
  :after org
  :config
  ;; don't scale svg images
  (setq org-html-head "<style> .org-svg {width: auto} </style>"))

;; load ob-python only when executing python block
(use-package ob-python
  :after org
  :commands org-babel-execute:python
  :init
  (setq org-babel-python-command "python3") ; python3 please!
  (when (eq system-type 'windows-nt)
    (setq org-babel-python-command "python")) ; windows uses python for versions > 3, argh... 
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:exports . "results")))) ; export only plots by default

(use-package org-clock
  :general
  ('normal org-mode-map :prefix "z x"
           "i" 'org-clock-in
           "o" 'org-clock-out
	   "x" 'org-clock-in-last)
  :after org
  :config
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persistence-insinuate t
	org-clock-persist t
	org-clock-in-resume t
	org-clock-out-remove-zero-time-clocks t
	org-clock-mode-line-total 'current
	org-duration-format (quote h:mm)))

(use-package org-src
  :general
  ('normal org-mode-map "z e" 'org-edit-special)
  ('normal org-src-mode-map "z e" 'org-edit-src-exit)
  :after org
  :init
  ;; babel and source blocks
  (setq org-src-fontify-natively t
	org-src-window-setup 'current-window ; don't move my windows around!
	org-src-preserve-indentation t  ; preserve indentation in code
	org-adapt-indentation nil ; no extra whitespace!
	org-src-tab-acts-natively t	; if t, it is slow!
	org-confirm-babel-evaluate nil)) ; doesn't ask for confirmation

(use-package org-agenda
  :after org
  :init
  (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
				 "~/OneDrive/Org/notes.org"
				 "~/OneDrive/Org/journal.org"
				 "~/OneDrive/Org/gcal.org"))))

(use-package ox-latex
  :if (eq system-type 'gnu/linux)
  :after org
  :init
  ;; change scale of latex preview in org-mode
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1)
	org-startup-with-latex-preview t
        org-preview-latex-image-directory "~/.cache/ltximg/")

  ;; minted code pdf export org
  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("newfloat" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(use-package ob-async
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python")))

(use-package ob-shell
  :after org
  :commands org-babel-execute:sh
  :init
  (setq org-babel-default-header-args:shell
	'((:results . "output")
          ;; always get my .bashrc aliases
          (:shebang . "#!/bin/bash -i"))))

;; insert web links with better description
(use-package org-cliplink
  :commands org-cliplink
  :general (org-mode-map "C-c l" 'org-cliplink)
  :after org)

;; copy image from clipboard, save it and insert it
(use-package org-download
  :after org
  :general
  (org-mode-map "C-M-y" 'org-download-screenshot)
  :config
  (setq
   org-download-image-dir "./images"
   org-download-image-html-width 350)
  ;; for wsl
  (when (string-match "-[Mm]icrosoft" operating-system-release)
    (setq org-download-screenshot-method "convert.exe clipboard: %s")) ; add .exe to work within wsl2
  
  (setq org-download-screenshot-file "./screenshot.png")) ; where temporary screenshot will be saved so convert can work

;; languages spell checker
(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :hook
  (LaTeX-mode . flyspell-mode)
  (org-mode . flyspell-mode)
  (prorg-mode . flyspell-prog-mode)
  :config
  ;; husnpell is alternative to aspell
  (setq ispell-program-name "hunspell")	; dictionary /usr/share/hunspell
  (ispell-set-spellchecker-params)

  (setq flyspell-issue-message-flag nil ; don't emit messages
	ispell-personal-dictionary "~/.dotfiles/hunspell/.personal"))

;; flyspell uses `hooks` and `sit-for` to delay
;; this uses `idle-timers`
(use-package flyspell-lazy
  :if (eq system-type 'gnu/linux)
  :if (string-match "-[Mm]icrosoft" operating-system-release)
  :hook
  (flyspell-mode . flyspell-lazy-mode)
  :config
  (setq flyspell-lazy-idle-seconds 1))

;; convenient functions for correctiong
(use-package flyspell-correct
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-correct-wrapper)
  ('normal flyspell-mode-map "[ ," 'flyspell-correct-wrapper)
  :after flyspell)

;; completion in region manually summoned with <tab> (no auto pop up)
;; allows space between filter words (combined with oderless)
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  :init
  (corfu-global-mode)
  :general
  (corfu-map "<tab>" 'corfu-next
	     "<backtab>" 'corfu-previous
	     "C-n" 'corfu-next
	     "C-p" 'corfu-previous)
  ('normal corfu-map "<escape>" 'corfu-quit)
  ('insert "C-n" nil
	   "C-p" nil)
  :hook
  ;; after `np.` quit the completion
  ;; if nil: allow space separated orderless completion style
  ;; if t: quit at boundary
  (python-mode . (lambda ()
                   (setq-local corfu-quit-at-boundary t)))
  :config
  (setq corfu-auto t                    ; enables timer-based completion
        corfu-auto-delay 0.1
        corfu-quit-at-boundary nil
	corfu-auto-prefix 1
	corfu-quit-no-match t))

(use-package +capf
  :straight nil
  :load-path "./lisp"
  :after corfu
  :demand
  :config
  (add-to-list 'completion-at-point-functions
               '+file-completion-at-point-function)
  (add-to-list 'completion-at-point-functions
               '+dabbrev-completion-at-point-function))

;; completion any text based on buffer contents
(use-package dabbrev
  :general
  ("M-/" 'dabbrev-completion)           ; this can be completed with corfu
  ("C-M-/" 'dabbrev-expand)
  :config
  ;; don't change case
  (setq dabbrev-case-replace nil))

;; completion in region
(use-package company
  :disabled
  :diminish company-mode
  :hook
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (org-mode . company-mode)
  :config
  (setq company-idle-delay .2
	company-tooltip-align-annotations t
	company-dabbrev-downcase nil	; don't replace case
	company-format-margin-function #'company-vscode-dark-icons-margin
	company-minimum-prefix-length 1))

;; Company use prescient.el
;; used to sorts and filter list of candidates
(use-package company-prescient
  :disabled
  :after company
  :init
  (company-prescient-mode))

;; folding
;; note: evil collection also sets a bunch of keybindings
(use-package outline
  :straight (:type built-in)
  ;; :diminish outline-minor-mode
  :hook
  (prog-mode . outline-minor-mode)
  (markdown-mode . outline-minor-mode)
  (conf-mode . outline-minor-mode)
  (LaTeX-mode . outline-minor-mode)
  :general
  ('normal outline-mode-map "C-j" nil)
  ('normal outline-mode-map "z j" 'outline-next-visible-heading)
  ('normal outline-mode-map "z b" 'outline-show-branches)
  ('normal outline-mode-map "z t" 'outline-show-subtree)
  ('normal outline-mode-map "z o" 'outline-show-children)
  ('normal outline-mode-map "z h" 'outline-hide-sublevels)
  ('normal outline-mode-map "z a" 'outline-show-all)
  ('normal outline-mode-map "<tab>" 'outline-cycle)
  :config
  ;; need to rebind after loading outline
  ;; because general uses `after-load-functions' and evil-collection uses `eval-after-load'
  ;; evil-collection end up binding last...
  (general-def 'normal outline-mode-map "z k" 'outline-previous-visible-heading)
  (setq outline-minor-mode-cycle t
	outline-minor-mode-highlight 'append))  

(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :general
  ('normal outline-mode-map
    "g j" nil
    "g k" nil
    "C-j" nil)
  ('normal LaTeX-mode-map "g f" '(:keymap TeX-fold-keymap))
  (TeX-fold-keymap
   "b" 'TeX-fold-buffer
   "c" 'TeX-fold-clearout-buffer
   "f" 'TeX-fold-dwim)
  :hook
  (LaTeX-mode . (lambda ()
                  (prettify-symbols-mode) ; for greek letters and other math symbols
                  (LaTeX-math-mode)       ; easy to type greek letters
                  (TeX-fold-mode) ; fold (reduce clutter) footnotes, comments etc (C-c C-o C-o DWIM)
                  (reftex-isearch-minor-mode)
                  (visual-line-mode)
                  (outline-hide-sublevels 1) ; start folded
                  (yas-minor-mode)
                  (turn-off-auto-fill)))
  :config
  ;; basics configs
  (setq TeX-save-query nil
        TeX-auto-save t
        TeX-parse-self t       ;enable document parsing
        reftex-plug-into-AUCTeX t
        TeX-PDF-mode t			; output pdf 
        TeX-electric-escape t
        TeX-master nil) ;make auctex aware of multi-file documents

  ;; start latex buffer folded
  (add-hook 'find-file-hook 'TeX-fold-buffer t)
  
  ;; specific config
  (setq LaTeX-command "latex -shell-escape") ;; -shell-escape for minted (syntax highlight)
  
  ;; variables for jumping between source and pdf
  (setq TeX-source-correlate-method 'synctex ;; Method for enabling forward and inverse search 
        TeX-source-correlate-start-server t ;; inhibit the question to start a server process
        TeX-source-correlate-mode t) ;; jump to source

  ;; update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook  
	    'TeX-revert-document-buffer) 

  ;; nomenclature compilation option for latex
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list 
		  '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
		    (lambda (name command file)
		      (TeX-run-compile name command file)
		      (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
		    nil t :help "Create nomenclature file")))

  ;; using "Zathura" or "PDF Tools" on WSL
  ;; one advantage of "PDF Tools" is "pdf-view-set-slice-from-bounding-box"
  ;; PDF Toll is good when I'm with just one screen
  (add-to-list 'TeX-view-program-selection
	       '(output-pdf "Okular")))

;; preview in latex
(use-package preview
  :straight nil
  :after latex
  :general
  ('normal LaTeX-mode-map "g p" '(:keymap preview-map))
  (preview-map
   "b" 'preview-buffer
   "c" 'preview-clearout-buffer
   "s" 'preview-section
   "p" 'preview-at-point)
  :init
  ;; preview latex config
  (setq preview-default-option-list '("displaymath" "textmath" "showlabels")
	preview-scale-function 1.1
	preview-auto-cache-preamble t))

;; latex function
(use-package latex-clipboard
  :straight nil
  :after latex
  :general
  (LaTeX-mode-map "C-M-y" 'my-tex-insert-clipboard)
  :init
  ;; function definition, not the best...
  (defun my-tex-insert-clipboard ()
    (interactive)
    (setq folder-path (concat default-directory "img/"));make the img directory
					;create the directory if it doesn't exist
    (if (not (file-exists-p folder-path))
	(mkdir folder-path))
    ;; correct path to use convert.exe inside wsl and create file
    (if (string-match "-[Mm]icrosoft" operating-system-release)
      (setq wsl-folder-path (replace-regexp-in-string "\n\\'" ""
                                                      (shell-command-to-string (concat "wslpath -w "
                                                                                       folder-path))))
      (setq wsl-folder-path folder-path)) 
    (setq image-path (concat wsl-folder-path
                             ;; wsl adjustment :/
			     (if (string-match "-[Mm]icrosoft" operating-system-release)
                                 "\\img_"
                               "\img_")
			     (format-time-string "%Y%m%d_%H%M%S_.png")))
    (let* ((image-file (concat 
			"img/img_"
			(format-time-string "%Y%m%d_%H%M%S_.png")))
	   (exit-status
            ;; for wsl to use windows imagemagick :/
            (if (string-match "-[Mm]icrosoft" operating-system-release)
                (call-process "convert.exe" nil nil nil
                              "clipboard:" image-path)
              (call-process "import" nil nil nil image-path))))
      (insert (format "
\\begin{figure}[ht!]
  \\centering
  \\includegraphics[width=.5\\linewidth]{%s}
\\end{figure}" image-file)))))


  (use-package evil-tex
  :after latex
  :hook (LaTeX-mode . evil-tex-mode))

;; labels, references, citations and indices in LaTeX
;; usually: C-c RET -> eqref -> TAB -> select label with completion
;; disabled: prefer now just using completions
(use-package reftex
  :disabled
  :after latex
  :commands reftex-toc
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)))
  :general
  ('normal reftex-mode-map :prefix "g r"
	   "t" 'reftex-toc
	   "v" 'reftex-view-crossref
	   "g" 'reftex-goto-label
	   "r" 'reftex-reference
	   "c" 'reftex-citation)
  :config
  (setq reftex-cite-prompt-optional-args t ; Prompt for empty optional arguments in cite
        ;; https://www.gnu.org/software/emacs/manual/html_mono/reftex.html
        reftex-enable-partial-scans t
        reftex-keep-temporary-buffers nil
        reftex-save-parse-info t
        reftex-trust-label-prefix '("fig:" "eq:") ; speed up parsing of labels
        ;; don't ask which refecence macro after `reftex-referene'
        reftex-ref-macro-prompt nil
        ;; show just equations in the label menu
        reftex-label-menu-flags '(nil nil nil nil nil nil nil nil)))

(use-package dired
  :straight (:type built-in)
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :general
  (dired-mode-map "C-c C-d" 'mkdir)
  ('normal dired-mode-map "h" 'dired-up-directory)
  (dired-mode-map "M-o" 'dired-omit-mode)
  ('normal dired-mode-map "l" 'dired-find-alternate-file)
  ('normal dired-mode-map "SPC" nil)
  ("C-x C-j" 'dired-jump-other-window)
  ("C-x j" 'dired-jump)
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$"
	dired-auto-revert-buffer t
	dired-listing-switches "-alh"	; human readable format when in detail
	dired-kill-when-opening-new-dired-buffer t ; kill when changing dir
	delete-by-moving-to-trash t)	; move to trash

  ;; kill the dired buffer eand enters the current line file or directory
  (put 'dired-find-alternate-file 'disabled nil))

;; open dired as a sidebar
(use-package dired-sidebar
  :general
  ("C-x C-j" 'dired-sidebar-jump)
  ('normal dired-sidebar-mode-map
           "l" 'dired-sidebar-find-file
           "h" 'dired-sidebar-up-directory)
  :hook
  (dired-sidebar-mode . visual-line-mode)
  ;; avoid fixing window size
  (dired-sidebar-mode . (lambda () (setq window-size-fixed nil)))
  :config
  (setq dired-sidebar-one-instance-p t)      ; just sidebar per frame
  (defun dired-sidebar-jump ()
    (interactive)
    (dired-sidebar-show-sidebar)
    (dired-sidebar-jump-to-sidebar)))

;; icons for dired sidebar
(use-package vscode-icon
  :disabled
  : (display-graphic-p)
  :commands (vscode-icon-for-file)
  :custom
  (vscode-icon-size 15))

;; load modus in terminal
(use-package modus-themes
  :unless (display-graphic-p)
  :init
  (setq modus-themes-org-blocks 'tinted-background
	modus-themes-prompts '(intense italic)
	modus-themes-hl-line '(accented)
	modus-themes-diffs 'desaturated
	modus-themes-completions 'opinionated
	modus-themes-paren-match '(bold underline)
	modus-themes-no-mixed-fonts nil
	modus-themes-variable-pitch-ui nil
	modus-themes-syntax '(faint)
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-fringes 'subtle
        modus-themes-headings '((t . (rainbow)))
	modus-themes-mode-line '(borderless accented moody))

  ;; hook to enforce change when theme is toggled (which loads the theme)
  (add-hook 'modus-themes-after-load-theme-hook
	    (lambda ()
	      (progn 
                (eval-after-load 'auto-dim-other-buffers
                  '(set-face-attribute 'auto-dim-other-buffers-face nil
                                      :foreground (modus-themes-color 'fg-dim)))
                (eval-after-load 'smartparens
                  '(set-face-attribute 'sp-show-pair-match-content-face nil
                                      :background (modus-themes-color 'bg-paren-expression))))))
  (modus-themes-load-vivendi)
  :general
  ("<f5>"  'modus-themes-toggle))

;; dim other buffer so we know what is the current working one.
(use-package auto-dim-other-buffers
  :disabled
  :init
  (auto-dim-other-buffers-mode t))

;; syntax highlight in html export of org-mode source blocks
;; does not work well with modus-themes and tree-sitter
(use-package htmlize)

;; `:includes` so straight can recognize dap-python.el and dap-cpptools
(use-package dap-mode
  :after lsp-mode
  :straight (dap-mode :includes (dap-python dap-cpptools)
		      :type git
		      :host github
		      :repo "emacs-lsp/dap-mode") 
  :general
  (lsp-mode-map "<f6>" 'dap-hydra))

(use-package dap-cpptools
  :after dap-mode
  :demand)

(use-package dap-python
  :after dap-mode python
  :demand ; so it loads, "requires", dap-python
  :init
  (setq dap-python-debugger 'debugpy))

;; change python virtual envirnment
;; obs: lsp-restart-workspace if change virtualenv 
(use-package pyvenv
  :commands pyvenv-activate pyvenv-workon
  :config
  (setenv "WORK_HOME" "~/.virtualenvs"))

(use-package c++-mode
  :straight (:type built-in)
  :mode ("\\.cpp\\'" . c++-mode)
  :general
  (c++-mode-map "C-x c" 'compile))

(use-package lsp-mode
  :disabled
  :commands (lsp lsp-deferred)
  :general
  (org-mode-map :prefix "C-l" "o" 'lsp-org)
  (org-mode-map :prefix "C-l" "d" 'lsp-virtual-buffer-disconnect)
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq read-process-output-max (* 1024 1024))
  :hook
  (python-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-idle-delay 0.5
	lsp-enable-folding nil		;potential to be slow
	lsp-enable-text-document-color nil ;potential to be slow
	lsp-keep-workspace-alive nil; terminate server if last workspace buffer is closed
	lsp-enable-on-type-formatting nil  ;don't format automatically
	lsp-headerline-breadcrumb-enable nil)  ;disable breadcrumb

  ;; using corfu, not company
  (setq lsp-completion-provider :none))

;; language server for type checker/completion/doc string
;; alternative to lsp-python-ms
(use-package lsp-pyright
  :disabled
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred)))
  :config
  ;; https://github.com/microsoft/pyright/discussions/1466
  ;; don't infer types from library source
  ;; treat all as unknowns when type is nonpresent
  (setq lsp-pyright-use-library-code-for-types nil)
  (setq lsp-pyright-python-executable-cmd "python3"))

;; lsp mode on org-edit-special
(use-package ob-python
  :disabled
  :after org lsp
  :commands org-babel-execute:python
  :init
  (defun org-babel-edit-prep:python (babel-info)
    (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
    (lsp)))

;; Microsoft python language server
;; it seems to be faster than pyls
;; does not have formating
(use-package lsp-python-ms
  :disabled
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))  ; or lsp-deferred

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook ((python-mode . toggle-truncate-lines)
	 (python-mode . display-fill-column-indicator-mode))
  :config
  ;; dont guess the indent offset
  (setq python-indent-guess-indent-offset nil)

  ;; make indentation aware of docstring
  (defun my-python-indent-line ()
    (if (eq (car (python-indent-context)) :inside-docstring)
	'noindent
      (python-indent-line)))
  ;; change default function to identify docstring
  (defun my-python-mode-hook ()
    (setq indent-line-function #'my-python-indent-line))
  (add-hook 'python-mode-hook #'my-python-mode-hook))

;; formatting python code
(use-package python-black
  :after python
  :commands python-black-buffer)

(use-package goto-last-change
  :general ('normal "g b" 'goto-last-change))

;; easy select region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; key chord hint
(use-package which-key
  :defer 1
  :diminish which-key-mode
  :init
  (which-key-mode t))

;; highligh TODO keyword everywhere
(use-package hl-todo
  :demand
  :hook
  (python-mode . hl-todo-mode)
  (LaTeX-mode . hl-todo-mode)
  (org-mode . hl-todo-mode)
  :general
  ('normal hl-todo-mode-map "g t" 'hl-todo-done)
  :config
  (defun hl-todo-done ()
    "Change text from TODO to DONE"
    (interactive)
    (if (string= (thing-at-point 'word 'no-properties) "TODO")
	(let ((bound (bounds-of-thing-at-point 'word)))
	  (replace-string "TODO" "DONE" 1 (car bound) (cdr bound))))))

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode))

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))

;; dependency of consult-bibtex
(use-package bibtex-completion
  :after consult-bibtex
  :straight (bibtex-completion :host github
                               :repo "tmalsburg/helm-bibtex"
                               :files (:defaults (:exclude "helm-bibtex.el" "ivy-bibtex.el")))
  :init
  (setq bibtex-completion-bibliography "~/.bibliography.bib"
        bibtex-completion-library-path "~/SeaDrive/My Libraries/PhD/bibliography/pdf/"
        bibtex-completion-pdf-open-function (lambda (fpath)
                                              (call-process "xdg-open" nil 0 nil fpath)))

  ;; windows wsl config for opening with default windows pdf reader
  ;; depends on `wslview` to be installed in wsl
  (when (string-match "-[Mm]icrosoft" operating-system-release)
    (setq bibtex-completion-pdf-open-function
          (lambda (fpath) (shell-command (concat
                                          "wslview "	; version 3.2.1 works with spaces in path
                                          (shell-quote-argument fpath))))))
  :config
  ;; dont prompt for anything, just insert the citation please.
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil))


;; search bibtex bibliography with consult
;; depends on helm-bibtex
(use-package consult-bibtex
  :if (eq system-type 'gnu/linux)
  :straight (consult-bibtex :host github
                            :repo "mohkale/consult-bibtex")
  :general
  ("C-c b" 'consult-bibtex)
  :config
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map))))

;; search bibliography by keyword or doi
;; helm-bibtex dependency
(use-package biblio
  :commands biblio-lookup)

(use-package server
  :straight (:type built-in)
  :demand
  :config (or (server-running-p)
	    (server-start)))

(use-package table
  :after org)

;; needs to be added manually to .emacs.d/lisp folder
(use-package wsl-path
  :disabled
  :if (not (string-match "-[Mm]icrosoft" operating-system-release))
  :straight nil
  :load-path "./lisp"
  :commands (wsl-path-activate
	     wsl-path-convert-file-name)
  :init
  (wsl-path-activate))

(use-package yasnippet
  :commands yas-insert-snippet
  :config
  (yas-reload-all)
  (yas-global-mode))

;; ensures environment variables inside Emacs is the same in the user's shell
;; emacs' exec-path is not automatically updated from PATH
;; to run jupyter which is installed in ~/.local/bin, not in the (print exec-path)
;; added ~/.local/bin to exec path solves the problem with jupyter
;; no need for this package, for now, defer with `:commands`
(use-package exec-path-from-shell
  :config
  ;; non interative shell start up faster
  ;; (setq exec-path-from-shell-arguments nil)
  :commands (exec-path-from-shell-initialize))

;; browser the web inside emacs
(use-package eww
  :straight (:type built-in)
  :general
  ("<f12>" 'eww)
  :hook (eww-mode-hook . (lambda () (eww-readable)))
  :config
  (setq shr-use-fonts  nil                          ; No special fonts
	shr-use-colors t                          ;  colours
	shr-inhibit-images nil			  ; inhibit images
	shr-indentation 2                           ; Left-side margin
	shr-color-visible-luminance-min 80
	eww-search-prefix "https://www.google.com/search?q="
	shr-width 70))                                ; Fold text to 70 columns

(use-package pdf-tools
  :if (eq system-type 'windows-nt)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :general
  (pdf-view-mode-map "M-h" 'pdf-history-backward)
  :init
  (pdf-loader-install)
  :config
  ;; sync pdf in different frame
  (setq pdf-sync-forward-display-action
        '(display-buffer-reuse-window (reusable-frames . t)))
  (setq pdf-sync-backward-display-action
        '(display-buffer-reuse-window (reusable-frames . t))))

(use-package hydra)

;; terminal emulator based on libvterm (in C)
(use-package vterm
  :general
  (vterm-mode-map "<f9>" nil))

;; manage multiple vterm's buffers
(use-package vterm-toggle
  :general
  ("<f9>" 'vterm-toggle-cd) 	; opens term in current cd including remote
  (vterm-mode-map "s-n" 'vterm-toggle-forward
		  "s-p" 'vterm-toggle-backward))


(use-package keycast
  :commands keycast-mode keycast-log-mode)

(use-package gif-screencast
  :commands gif-screencast
  :general
  ("<f8>" 'gif-screencast)
  (gif-screencast-mode-map "<f8>" 'gif-screencast-stop)
  :config
  ;; change the function gif-screencast--generate-gif to generate file without ":"
  ;; (format-time-string "output-%F-%H-%M-%S.gif" (current-time))
  (setq gif-screencast-output-directory "./gif/"))

;; update time stamp of org files
(use-package time-stamp
  :after org :demand				; load after org
  :hook (org-mode . (lambda ()
		      (add-hook 'before-save-hook 'time-stamp)))
  :config
  (setq time-stamp-active t
	time-stamp-format "%Y-%m-%d %H:%M:%S"
	time-stamp-end "$"		; regex for end of line
	time-stamp-start "#\\+lastmod:[ \t]*"))

;; terminal emacs with evil cursor indication
(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :init
  (evil-terminal-cursor-changer-activate))

(use-package repeat
  :if (string-greaterp emacs-version "28") ; need emacs > 28
  :straight (:type built-in)
  :init
  ;; built-in command repeater (like hydra)
  (repeat-mode t))

;; built in windows resize functions
(use-package window
  :straight (:type built-in)
  :general
  ("C-x C-o" 'other-window)
  (resize-window-repeat-map "j" 'shrink-window)
  (resize-window-repeat-map "k" 'enlarge-window)
  (resize-window-repeat-map "h" 'shrink-window-horizontally)
  (resize-window-repeat-map "l" 'enlarge-window-horizontally))

;; create backlinks when linking org-mode headings
(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links")
  :after org
  :general
  (org-mode-map :prefix "C-c s"
                "s" 'org-super-links-link
                "l" 'org-super-links-store-link
                "p" 'org-super-links-insert-link
                "d" 'org-super-links-quick-insert-drawer-link
                "i" 'org-super-links-quick-insert-inline-link
                "C-d" 'org-super-links-delete-link))

;; loads the org-id library from org repository
;; for creating org-ids for more robust linking, avoid referencing issues
(use-package org-id
  :after org
  :demand                               ; explicitly require org-id
  :init
  ;; automatic generate id for headings
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; citations support in org-mode
(use-package oc
  :after org
  :general
  (org-mode-map "C-c C-b" 'org-cite-insert)
  :config
  (setq org-cite-global-bibliography '("~/.bibliography.bib")))

;; highligh indentation
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; suppress error on tui
(use-package highlight-indent-guides
  :unless (display-graphic-p)
  :config
  (setq highlight-indent-guides-suppress-auto-error t))

;; emacs built in version control
(use-package vc-git
  :straight (:type built-in)
  :hook
  (diff-mode . outline-minor-mode)
  (vc-git-region-history-mode . outline-minor-mode))

;; manage remote files access and manipulations
(use-package tramp
  :straight (:type built-in)
  :config
  (setq tramp-default-method "ssh"
	tramp-default-host "138.232.83.174"
	tramp-verbose 4))

;; shows git information on fringe
(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  ;; (dired-mode . diff-hl-dired-mode)
  ;; integration with magit
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; built in substitute for `list-buffer`
(use-package ibuffer
  :general
  ("C-x C-b" 'ibuffer))

;; better code highlight and fold
(use-package tree-sitter
  :diminish tree-sitter-mode
  :hook
  (python-mode . tree-sitter-mode)
  (python-mode . tree-sitter-hl-mode))

;; langage bundle for `tree-sitter`
(use-package tree-sitter-langs
  :demand                               ; require it after loading tree-sitter
  :after tree-sitter)

;; use tree sitter as evil text objects
(use-package evil-textobj-treesitter
  :straight (el-patch :type git
                      :host github
                      :repo "meain/evil-textobj-treesitter"
                      :files (:defaults "queries"))
  :after tree-sitter
  :config
  (define-key evil-outer-text-objects-map "s" (evil-textobj-treesitter-get-textobj "statement.outer"))
  (define-key evil-inner-text-objects-map "n" (evil-textobj-treesitter-get-textobj "scopename.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-treesitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-treesitter-get-textobj "call.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-treesitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-treesitter-get-textobj "function.inner")))

(use-package hide-comnt
  :general ('normal "g h c" 'hide/show-comments-toggle)
  :commands hide/show-comments-toggle)

(use-package wgrep)

;; config for windows
(use-package emacs
  :if (eq system-type 'windows-nt)
  :init
  (setq w32-get-true-file-attributes nil
        recentf-auto-cleanup 'never))

;; use emacs to edit text within chrome
(use-package atomic-chrome
  :commands atomic-chrome-start-server)

;; easily change windows
(use-package ace-window
  :general
  ('normal "C-w C-w" 'ace-window))

;; highlight parensthesis
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

;; moving cursor around fast and efficiently
(use-package avy
  :general
  ('normal "s" 'avy-goto-char-timer))

(use-package lsp-grammarly
  :disabled
  :if (eq system-type 'gnu/linux)
  :commands lsp-grammarly-start
  :init
  (defun lsp-grammarly-start ()
    (interactive)
    (require 'lsp-grammarly)
    (lsp-deferred))
  :config
  (setq lsp-grammarly-audience "expert"))

;; grammarly as a flychecker
(use-package flycheck-grammarly
  :disabled
  :demand
  :after flycheck)

(use-package lsp-ltex
  :disabled
  :hook (LaTeX-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred))))

;; icons for completion candidates
(use-package all-the-icons-completion
  :when (display-graphic-p)
  :after marginalia
  :init
  (all-the-icons-completion-mode))

;; icons for dired
(use-package all-the-icons-dired
  :when (display-graphic-p)
  :hook
  (dired-mode . (lambda ()
                  ;; don't enable in dired-sidebar
                  (unless (eq major-mode 'dired-sidebar-mode)
                    (all-the-icons-dired-mode))))
  :init
  (setq all-the-icons-dired-monochrome nil))

;; simple LSP client
;; alternative to lsp, too many dependencies
;; python language server: pyls (need to install)
(use-package eglot
  :general
  ('normal eglot-mode-map :prefix "gl"
           "l" 'eglot-code-actions)
  :hook
  (python-mode . eglot-ensure))

;; set up for ltex language server
;; need to manually download the language server: wget https://github.com/valentjn/ltex-ls/releases/download/15.1.0/ltex-ls-15.1.0-linux-x64.tar.gz -P ~/tmp
(use-package eglot-ltex
  :straight (eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex")
  :hook
  (LaTeX-mode . (lambda ()
                  (require 'eglot-ltex)
                  (call-interactively #'eglot)))
  :init
  (setq eglot-languagetool-server-path "~/.opt/ltex-ls-15.1.0/"))

(use-package svg-lib
  :straight (svg-lib :type git :host github :repo "rougier/svg-lib"))

;; icons for completion in region
;; still in development
(use-package kind-icon
  :straight (kind-icon :type git :host github :repo "jdtsmith/kind-icon")
  :after corfu 
  :custom
  (kind-icon-default-face 'corfu-background)
  :demand
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; change font on windows wsl gui
(use-package emacs
  :when (and (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
             (display-graphic-p))
  :config
  (set-face-attribute 'default nil :height 98))
 
(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
