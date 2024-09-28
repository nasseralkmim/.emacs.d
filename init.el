; -*- coding: utf-8; lexical-binding: t -*-

(defvar my-start-time (current-time)
  "Time when Emacs was started")

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install 'use-package' support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode))

;; 'always-defer' means that for a package to load we need a ':hook' or using a ':general' keybinding
;; if there is none, we need to explicitly add ':demand' to load the package
;; can also load with ':defer time'
(setq use-package-verbose nil		; don't print anything
      use-package-compute-statistics nil ; compute statistics about package initialization
      use-package-minimum-reported-time 0.0001
      use-package-enable-imenu-support t
      use-package-always-ensure t	; always ensure the package is installed, unless :ensure nil
      use-package-expand-minimally t	; minimal expanded macro
      use-package-always-defer t)	; always defer, don't "require", except when :demand

;; Block until current queue processed.
(elpaca-process-queues)

;; general for keybinding
(use-package general :disabled
  :ensure (:wait t)
  :demand t
  :config
  ;; keybinding on 'override' keymap are not overridden by a minor-mode. 
  (general-override-mode))

;; control minor-mode indication in the mode-line
(use-package diminish :disabled
  :ensure (:wait t)
  :demand t)

;; ':general' and ':diminish' add keywords to 'use-package'
;; need to process before continue
(elpaca-process-queues)

;; basics and better default
(use-package emacs
  :ensure nil
  :defer 1
  :config
  (setq-default fill-column 88)	  ; column length (88 python black default, I think is good)
  (column-number-mode t)  ; show column number in the mode line
  (setq-default indicate-empty-lines nil) ; cleaner

  (setq warning-minimum-level :error)		 ;avoid warning buffer

  ;; scroll
  (setq auto-window-vscroll nil  ; avoid next-line to trigger line-move-partial
        mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)) ; 1 line at a time
        mouse-wheel-progressive-speed nil  ; proportional to scroll speed
        fast-but-imprecise-scrolling t
        jit-lock-defer-time nil
        jit-lock-stealth-time nil
        mouse-wheel-follow-mouse 't
        ;; scroll-conservatively 0
        ;; scroll-step 0
        )

  (setq ring-bell-function 'ignore)

  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  ;; create backups in separate folder
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq create-lockfiles nil)		; files with # problem with onedrive...

  ;; ;; answering just 'y' or 'n' will do
  (setq use-short-answers t)

  (setq-default
   completion-cycle-threshold nil    ; show all candidates
   completions-detailed t	    ; add details in completions as prefix/sufix
   enable-recursive-minibuffers t	; Enable recursive minibuffers
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
   help-window-select t	    ; focus on help window when openend, then quit with `q'
   window-combination-resize t) ; resize windows proportionaly

  (setq yank-pop-change-selection t)    ;change selection when using yank pop
  ;; overwrite over active region, useful when pasting to substitute text
  (delete-selection-mode 1)

  (blink-cursor-mode -1)                ; don't blink the cursor.

  ;; each line on its own, otherwise use 'visual-line-mode'
  (setq-default truncate-lines t)

  ;; narrow to region 'C-x n n' and widen with 'C-x n w'
  (put 'narrow-to-region 'disabled nil)

  ;; Window divider and continuous line
  (unless (display-graphic-p)
    ;; (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
    ;; emacs can not figure out that in the terminal the default BG is dark
    (setq frame-background-mode 'light)
    (mapc 'frame-set-background-mode (frame-list))))

(use-package bookmark
  :ensure nil
  :defer 1
  :config
  (setq bookmark-file "~/Sync/news/bookmarks"))

(use-package pixel-scroll
  :defer 1
  :ensure nil
  :if (string-greaterp emacs-version "29") ; need emacs > 29
  :custom
  (pixel-scroll-precision-interpolate-page t)
  :config
  ;; text scroll pixel by pixel
  (pixel-scroll-precision-mode t)
  ;; never recenter point when scrolling, but keep in view
  (setq scroll-conservatively 1000))

;; My custom emacs theme
(use-package seralk-theme :disabled
  :ensure nil
  :general
  ("<f5>"'toggle-dark-theme)
  :init
  (load-theme 'seralk t)
  (defun toggle-dark-theme ()
    (interactive)
    (if (eq frame-background-mode 'dark)
        (setq frame-background-mode 'light)
      (setq frame-background-mode 'dark))
    (invert-face 'default)))

;; Move between windows configuration
(use-package winner
  :ensure nil
  :defer 1
  :config
  (setq winner-ring-size 10)
  (winner-mode))

;; Visit file from where you left
(use-package saveplace :disabled        ; slow to quit emacs
  :ensure nil
  :defer 1
  :config
  (save-place-mode))

;; typeface
(use-package custom-typefaces :disabled
  ;; :when (not (display-graphic-p))
  :ensure nil
  :preface
  (setq default-monospace '("Commit Mono"))
  (setq default-unicode '("Symbols Nerd Font Mono"))
  (setq default-proportional '("Input Sans"))
  (setq default-comments '("Monaspace Radon"))
  :custom-face 
  ;; "Victor Mono" sometimes is nice for comments or "Recursive Mono Casual Static".
  ;; Monospace favorites are "JetBrains Mono NF", "MesloLGS Nerd Font Mono" and "Iosevka NF", or "Recursive Mono Linear Static".
  ;; Variable pitch favorites "Iosevka Etoile", "Recursive Sans Linear Static"
  ;; 'constant'
  ;; (default  ((t (:family ,(car default-monospace)))))
  (variable-pitch ((t (:family ,(car default-proportional)))))
  (variable-pitch-text ((t (:inherit variable-pitch))))
  ;; comment
  (font-lock-comment-face ((t (:family ,(car default-comments) :slant italic))))
  (font-lock-constant-face ((t (:family ,(car default-monospace)))))
  ;; (font-lock-function-name-face ((t (:family ,(car default-monospace) :weight regular))))
  ;; outline 4 inherits from comment face... make it oblique instead of italic
  (outline-4 ((t (:inherit font-lock-doc-face))))
  ;; so summary line aligned
  (gnus-summary-normal-unread  ((t (:family ,(car default-monospace)))))
  ;; when using variable pitch in org mode, use monospace for code blocks
  (tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face))))
  :init
  (set-fontset-font  t 'unicode (car default-unicode) nil 'prepend)
  (set-frame-font (car default-monospace) nil t))

(use-package variable-pitch-typeface
  :ensure nil
  :after (:or org latex)
  :preface
  (setq default-proportional '("Input Sans Narrow"))
  :custom-face
  (variable-pitch ((t (:family ,(car default-proportional)))))
  (variable-pitch-text ((t (:inherit variable-pitch)))))

(use-package org-typeface-when-variable-pitch
  :ensure nil
  :after (:or org latex)
  :preface
  (setq default-monospace '("Monaspace Neon Light"))
  :custom-face
  (org-block ((t (:family ,(car default-monospace)))))
  (org-table ((t (:family ,(car default-monospace)))))
  (org-meta-line ((t (:family ,(car default-monospace)))))
  (org-verbatim ((t (:family ,(car default-monospace)))))
  (org-code ((t (:slant italic :inherit org-verbatim :box nil)))))

;; change typeface size font
;; note: `global-text-scale-adjust' do that
(use-package emacs-frame-zoom :disabled
  :ensure nil
  :general
  ("C-M-=" 'zoom-frame)
  ("C-M--" 'zoom-frame-out)
  :init
  ;; ref: https://stackoverflow.com/questions/24705984/increase-decrease-font-size-in-an-emacs-frame-not-just-buffer
  ;; setting typeface size
  (defun zoom-frame (&optional amt frame)
    "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
    (interactive "p")
    (let* ((frame (or frame (selected-frame)))
           (font (face-attribute 'default :font frame))
           (size (font-get font :size))
           (amt (or amt 1))
           (new-size (+ size amt))
           (scale (if (boundp 'org-format-latex-options)
                      (plist-get org-format-latex-options :scale)
                    1.0))
           (new-scale (* scale 1.15)))
      ;; change size of images on org buffers
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (string-equal major-mode "org-mode")
            (org-zoom-inline-images))))
      (set-frame-font (font-spec :size new-size) t t)
      ;; latex preview in org
      (if (boundp 'org-format-latex-options)
          (setq org-format-latex-options (plist-put org-format-latex-options :scale new-scale)))))

  (defun zoom-frame-out (&optional amt frame)
    "Call `zoom-frame' with negative argument."
    (interactive "p")
    (zoom-frame (- (or amt 1)) frame)
    ;; change size of images on org buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-equal major-mode "org-mode")
          (org-zoom-out-inline-images))))
    ;; latex scale to 1.1
    (if (boundp 'org-format-latex-options)
        (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1)))))

;; controls the behavior of windows
(use-package emacs-display-windows :disabled
  :ensure nil
  :init
  ;; reuse existing windows including other frames
  (customize-set-variable
   'display-buffer-base-action          ; define a priorized list of actions
   '((display-buffer--maybe-same-window
      display-buffer-reuse-window     ; reuse help buffers for instance
      display-buffer--maybe-pop-up-frame-or-window ; pop up transient buffers
      display-buffer-in-previous-window
      ;; display-buffer-same-window     ; problems with magit popup
      display-buffer-below-selected     ; magit pop up at bottom of selected
      display-buffer-at-bottom)
     (reusable-frames . t)))
  ;; avoid resizing when a pop up window
  (customize-set-variable 'even-window-sizes nil))

(use-package abbrev
  :ensure nil
  :config
  ;; abbrev for speed and less strain
  (setq-default abbrev-mode t)
  (setq save-abbrevs 'silently))

(use-package color-identifiers-mode)

;; save recent visited files
(use-package recentf
  :ensure nil
  :defer 5
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 25
        recentf-auto-cleanup 'mode))

;; Enable autorevert on specific modes
(use-package autorevert
  :ensure nil
  :if (eq system-type 'gnu/linux)
  :hook
  (text-mode . auto-revert-mode)
  (dired-mode . auto-revert-mode)
  :config
  (setq auto-revert-interval 1
        auto-revert-verbose nil
        ;; maybe slow
        auto-revert-check-vc-info nil
        ;; maybe slow, but useful
        auto-revert-remote-files nil))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h d" . helpful-at-point)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key))
  ;; :init
  ;; (defvar read-symbol-positions-list nil) ; fix bug in upstream emacs
  :config
  ;; don't create multiple buffers, just switch the current
  (defun pop-or-switch-to-buffer (buffer-name)
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-name)
      (pop-to-buffer buffer-name)))
  (setq helpful-switch-buffer-function 'pop-or-switch-to-buffer))

;; completion UI (vertical list in minibuffer)
(use-package vertico
  :ensure (vertico :type git :host github :repo "minad/vertico"
                   :files (:defaults "extensions/*"))
  :bind
  (:map vertico-map
        ("M-<return>" . vertico-exit-input))
  :defer 0.5
  :config
  (vertico-mode)
  (setq vertico-resize t))

;; improves behavior when dealing with directories in the minibuffer
(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; repeat last vertico session
(use-package vertico-repeat :disabled
  :ensure nil
  :after vertico
  :bind
  ("M-r" . vertico-repeat))

;; use vertico to complete in region with orderless in terminal
(use-package vertico-terminal
  :ensure nil
  ;; :unless (display-graphic-p)
  :after vertico
  :init
  ;; Use `consult-completion-in-regionegion' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq-default completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package vertico-terminal-completion-window-placement-hack :disabled
  :ensure nil
  :unless (display-graphic-p)
  :after vertico-multiform
  :init
  (add-to-list 'vertico-multiform-commands
               '(consult-completion-in-region grid (vertico-grid-annotate . 25)))
  (setf (alist-get "^\\Completions\\$"
                   display-buffer-alist
                 nil nil #'string=)
        ;; reuse window, otherwise open bellow
      '((display-buffer-reuse-window
        display-buffer-below-selected))))

;; allows different completion UI configuration
(use-package vertico-multiform :disabled
  :ensure nil
  :after vertico
  :init
  (vertico-multiform-mode)
  ;; for spell checker
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 25))))

;; `completion STYLE` with flexible candidate filtering
;; filter with space-separated components and match components in any order
;; filter means how a input string is matched against candidates
(use-package orderless
  :defer 1
  :demand
  :config
  ;; partial completion for files to allows path expansion
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-ignore-case t        ; ignore case (useful in c++ for instance)
        read-file-name-completion-ignore-case t
        completion-category-overrides '((file (styles . (partial-completion)))
                                        ;; navigate files with initials
                                        (minibuffer (initials)))))

;; default completion framework
(use-package simple
  :ensure nil
  :config
  ;; first TAB shows candidates
  ;; second TAB switches to the candidates buffer
  (setq completion-auto-select 'second-tab
        ;; Just one column is better.
        completions-format 'one-column
        completions-max-height 20
        completions-header-format nil
        ;; use C-SPC after C-u C-SPC to go jump to marks
        set-mark-command-repeat-pop t))

;; save the search history
(use-package savehist
  :ensure nil
  :defer 1
  :config
  (savehist-mode))

;; minibuffer annotations details
(use-package marginalia
  :if (eq system-type 'gnu/linux)
  :defer 1
  :config
  (marginalia-mode))

;; enhances multiple commands based on completion
;; practical navigation and search commands 
(use-package consult
  :bind
  (;; m/f/b <SPC> for bookmarks/files/buffers narrowing
  ("C-x C-b" . consult-buffer)		; enhanced switch to buffer
  ("C-x b" . consult-buffer)		; enhanced switch to buffer
  ("C-M-s" . consult-line)
  ("M-s" . consult-outline)		; navigation by headings
  ("C-c C-o" . consult-imenu)		; navigation by "imenu" items
  ("M-y" . consult-yank-pop)		; editing cycle through kill-ring
  ("C-M-s" . consult-line)	; search lines with preview
  ("C-c C-f" . consult-focus-lines)	; show only matching results
  ("C-c m" . consult-mark)
  ;; two parts: search  and filter
  ;; #<search string>#<filter terms> filtering with orderless! amazing!
  ;; command line can be specified after "--", example: #<search string> -- -C 10 for context!! WHAT!
  ("C-c r" . consult-ripgrep)		; search file contents
  ("C-c f" . consult-find-fd)		; search files in directories
  ("M-e" . consult-isearch-history))
  :hook
  ;; hook for using default completion mode
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  (consult-customize consult-buffer
                     consult-bookmark
                     consult-outline
                     consult-line
                     :preview-key '(:debounce 1 any))
  ;; use a key for preview from ripgrep and find
  (consult-customize consult-ripgrep consult-find-fd
                     :preview-key '("M-."))

  ;; use 'fd' instead of 'find'
  (defun consult-find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))

  ;; set project root from vc.el
  ;; available when a project file is visited (project-switch-project)
  (setq consult-project-root-function nil)

  ;; #'vc-root-dir ; using current folder as default
  ;; added --no-ignore-vcs to avoid skipping files in gitignore
  (setq consult-ripgrep-args 
        "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --no-ignore-vcs --with-filename")

  ;; previous consult line 
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (consult-customize consult-line :keymap my-consult-line-map)
  (defvar my-consult-outline-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\M-s" #'previous-history-element)
      map))
  (consult-customize consult-outline :keymap my-consult-outline-map)

  ;; narrow to org buffers
  (defvar org-source
    (list :name     "Org"
          :category 'buffer
          :narrow   ?o
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              (insert "#+title: " name "\n\n")
              (org-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (eq (buffer-local-value 'major-mode x) 'org-mode))
                     (buffer-list))))))
  (add-to-list 'consult-buffer-sources 'org-source 'append)

  ;; narrow to tramp buffers
  (defvar  tramp-source
    (list :name     "Remote"
          :category 'buffer
          :narrow   ?r
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              ;;(insert "#+title: " name "\n\n")
              (tramp-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (file-remote-p (buffer-local-value 'default-directory x)))
                     (buffer-list))))))
  (add-to-list 'consult-buffer-sources 'tramp-source 'append)

  ;; narrow to local buffers only
  (defvar  host-source
    (list :name     "Host"
          :category 'buffer
          :narrow   ?h
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              ;;(insert "#+title: " name "\n\n")
              (tramp-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (not (file-remote-p (buffer-local-value 'default-directory x))))
                     (buffer-list))))))
  (add-to-list 'consult-buffer-sources 'host-source 'append)

  ;; narrow to dired buffers
  (defvar  dired-source
    (list :name     "Dired"
          :category 'buffer
          :narrow   ?d
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              ;;(insert "#+title: " name "\n\n")
              (tramp-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (eq (buffer-local-value 'major-mode x) 'dired-mode))
                     (buffer-list))))))
  (add-to-list 'consult-buffer-sources 'dired-source 'append))

;; insert recent openend directories in prompt
(use-package consult-dir :disabled
  :ensure (consult-dir :type git :host github :repo "karthink/consult-dir") 
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
  :ensure (embark :files (:defaults "embark-org.el"))
  ;; :demand                               ; load it independently of bind and hook
  :bind (("C-z" . embark-act)
         :map embark-function-map
         ("h" . helpful-symbol)
         :map embark-variable-map
         ("h" . helful-symbol))
  :commands embark-prefix-help-command
  :config
  (add-to-list 'display-buffer-alist
               ;; hide the mode line of the Embark live/completions buffers
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'display-buffer-alist
               ;; show export in the side
               '("\\*Embark Export: .*"
                 (display-buffer-in-side-window)))
  ;; change 'describe-symbol' to 'helpful-symbol'
  (add-to-list 'embark-default-action-overrides '(describe-symbol . helpful-symbol)))

;; allows consult previews as you move around an auto-updating embark collect
;; buffer
;; `exbark-collects` grep results to a grep buffer
(use-package embark-consult
  :demand				;necessary for consult preview
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :after (embark consult))

;; targets for org mode
(use-package embark-org
  :ensure nil
  :after embark
  :demand)

;; utility for using icons fonts
(use-package all-the-icons :disabled
  :custom
  (all-the-icons-scale-factor 1))

(use-package nerd-icons
  :defer 1
  :demand t)

(use-package nerd-icons-dired
  :ensure (nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  ;; need to load after marginalia 
  ;; https://github.com/rainstormstudio/nerd-icons-completion/issues/4
  :after (nerd-icons marginalia)
  :init
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; automatic insert matching pairs and navigation
;; highlight matching parens
;; for wrap/unwrap I use evil-surround
;; expand/contract (slurp) is good for elisp
(use-package smartparens :disabled
  ;; :ensure (:includes smartparens-config)
  :custom-face
  (sp-show-pair-match-content-face ((t (:inherit show-paren-match))))
  :hook
  (prog-mode . smartparens-mode)
  (LaTeX-mode . smartparens-mode)
  (nxml-mode . smartparens-mode)
  ;; (org-mode . smartparens-mode) ; messing with org-mode
  ;; (smartparens-mode . smartparens-strict-mode) ; enforce pairs to be balanced
  ;; (smartparens-mode . show-smartparens-mode) ; instead of default show-paren-mode
  :config
  (setq sp-show-pair-delay 0.125
        sp-max-prefix-length 25         ; reduces work
        sp-max-pair-length 4))

(use-package paren
  :ensure nil
  :hook
  (prog-mode . show-paren-mode)
  :config
  ;; show context (echo area) when closing delimiter is off screen
  (setq show-paren-context-when-offscreen 'overlay
        show-paren-style 'expression
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren nil))

(use-package hack-sp-face-in-org
  :ensure nil
  :after org
  :init
  (defun change-sp-face-when-in-org ()
    "remove background to avoid annoying color when drawing with edraw in
org-mode"
    (face-remap-add-relative 'show-paren-match '(:background 'unspecified)))
  (add-hook 'org-mode-hook #'change-sp-face-when-in-org))

(use-package smartparens-config
  :ensure nil
  :demand
  :after smartparens
  :config
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p)))

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error) 
        ("M-N" . flymake-goto-prev-error))
  :config
  ;; delay check, check only on save
  (setq flymake-no-changes-timeout 1                 ;only when saved
        flymake-show-diagnostics-at-end-of-line nil ; just use "M-n"
        flymake-mode-line-lighter "Fly"))

;; 'flymake' just for C++ in org edit special
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/Example_002d_002d_002dConfiguring-a-tool-called-directly.html
(use-package flymake-cpp-hack :disabled                             ;not working with org edit special
  :ensure nil
  :after flymake
  :init
  (defun flymake-cc-init ()
    (let* (;; Create temp file which is copy of current file
           (temp-file   (flymake-proc-init-create-temp-buffer-copy
                         'flymake-proc-create-temp-inplace))
           ;; Get relative path of temp file from current directory
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))

      ;; Construct compile command which is defined list.
      ;; First element is program name, "g++" in this case.
      ;; Second element is list of options.
      ;; So this means "g++ -Wall -Wextra -fsyntax-only tempfile-path"
      (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

  (defun flymake-in-org-edit-special (&optional ARG PRED)
    (when (eq major-mode 'c++-mode)
      (flymake-mode)
      ;; Enable above flymake setting for C++ files(suffix is '.cpp')
      (add-to-list 'flymake-proc-allowed-file-name-masks
                   '("\\.cpp$" flymake-cc-init))))

  (add-hook 'org-src-mode-hook 'flymake-in-org-edit-special))

;; Useful:
;; 1. kill without copying, use 'delete-region' from emacs: https://github.com/meow-edit/meow/discussions/474
(use-package meow
  :demand
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-kill)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . "C-s")
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("V" . meow-visit)
     '("v" . isearch-forward)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-visual-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("`" . "C-c @")                   ; for hide-show
     '("/" . "C-.")                    ; for avy
     '("<escape>" . meow-cancel-selection)))
  (meow-setup)
  (add-to-list 'meow-mode-state-list '(gnus-article-mode . normal))
  (meow-global-mode)
  (setq meow-replace-state-name-list
        '((normal . "🟢 N")
          (motion . "🟠 M")
          (insert . "🔵 I")
          (keypad . "🔴 K")
          (beacon . "🟣 B")))
  (meow-setup-indicator)
  (setq meow-use-clipboard t)
  :config
  (meow-thing-register 'block
                       '(regexp "^[ \\|\t]*\\(#\\+begin_\\|```\\)[^\n]*\n" "^[ \\|\t]*\\(#\\+end_[^\n]*\\|```\\)$")
                       '(regexp "^[ \\|\t]*\\(#\\+begin_\\|```\\)[^\n]*\n" "^[ \\|\t]*\\(#\\+end_[^\n]*\\|```\\)$"))
  (add-to-list 'meow-char-thing-table '(?o . block)))

;; Show-hide selected with 'C-\'' after 'iedit-mode'
;; with prefix "C-u 1", selects just first occurrence, to add more use "M-n" 'iedit-expand-down-to-occurrence'
(use-package iedit
  :custom-face
  (iedit-occurrence ((t (:box (:line-width (-1 . -1)) :inherit nil))))
  :bind
  (("C-;" . iedit-mode)
   ("C-c ;" . iedit-mode)               ; for mosh/tmux
   ("M-d" . my-iedit-expand-down-to-occurrence)
   :map iedit-mode-keymap
   ("M-'" . iedit-show/hide-context-lines))
  :init
  (defun my-iedit-expand-down-to-occurrence ()
  ;; https://www.reddit.com/r/emacs/comments/rpwdb9/creating_multiple_cursors_from_symbol_under_point/
    (interactive)
    (if (bound-and-true-p iedit-mode)
        (iedit-expand-down-to-occurrence)
      (iedit-mode 1)))
  :config
  (setq iedit-search-invisible t)       ; use visual line to narrow candidates

  ;; Change the face for terminal
  (when (not (display-graphic-p))
    (set-face-attribute 'iedit-occurrence nil :weight 'bold :underline t :italic t)))

(use-package lisp
  :ensure nil
  :bind
  ("C-M-[" . insert-pair)
  ("M-{" . insert-pair)
  ("M-\"" . insert-pair)
  ("M-\'" . insert-pair)
  ("C-M-s" . delete-pair)
  ("C-M-l" . back-up-list-and-forward-sexp)
  (:repeat-map move-repeat-map
               ("f" . forward-sexp)
               ("n" . forward-list)
               ("b" . backward-sexp)
               ("p" . backward-list)
               ("l" . back-up-list-and-forward-sexp)
               ("u" . backward-up-list))
  :init
  (defun back-up-list-and-forward-sexp ()
    (interactive)
    (backward-up-list)
    (forward-sexp)))

;; Need to be explicitly required for magit
;; https://emacs.stackexchange.com/questions/50592/whats-this-slot-missing-invalid-slot-name-transient-prefix-transient-pref
(use-package transient
  :demand t)

(use-package magit
  :ensure (magit :files (:defaults "git-commit.el"))
  :config
  (setq magit-diff-hide-trailing-cr-characters t
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  ;; auto refresh magit
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; show colors
(use-package rainbow-mode
  :defer 1
  :commands rainbow-mode)

;; Add-ons to Org https://git.sr.ht/~bzg/org-contrib
;; some file produces a bug with inline images (can not toggle) so it is better
;; to use just what is needed
(use-package org-contrib
  ;; build only selected (symlink to `build/' and make an pre-compiled '.elc')
  :ensure (org-contrib :files ("lisp/org-eldoc.el" ; show src block arguments
                               "lisp/ox-bibtex.el" ; export latex properly
                               "lisp/ox-extra.el" ; ignore headlines (need to config)
                               ))
  :init
  (with-eval-after-load 'org
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))
    (require 'org-eldoc)
    (org-eldoc-load))
  (with-eval-after-load 'ox
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines)))) 

(use-package org
  ;; Since elpaca queue fist before loading, we need to wait here.
  ;; So we load the correct version of org instead of built-in when exporting async.
  ;; :ensure (org :repo "https://code.tecosaur.net/tec/org-mode.git")
  :mode (("\\.org$" . org-mode))
  :custom-face
  (org-block ((t (:inherit org-agenda-restriction-lock))))
  :bind
  (("M-o" . org-open-at-point-global)
   ("C-c s s" . org-store-link)
   ("C-c s i" . org-insert-link-global)
   :map org-mode-map
   ("C-c ," . org-insert-structure-template)
   ("C-," . nil)                        ; use for xref
   ("C-c ;" . nil)                        ; use iedit
   ("C-c M-;" . org-toggle-comment)
   ("C-c C-v C-g" . org-fold-hide-block-all)
   ("M-<return>" . org-meta-return)
   :repeat-map org-babel-map
   ("n" . org-babel-next-src-block)
   ("p" . org-babel-previous-src-block))
  :hook
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . turn-on-org-cdlatex)      ; easy to type greek letters "`a" for \alpha
  ;; (org-mode . org-indent-mode)          ; align with heading, sometimes slow
  :config
  (setq
   org-startup-folded t               ; folded in "overview" state
   org-edit-src-content-indentation 0
   org-cycle-separator-lines 0  ; no empty lines between headings
   org-fontify-quote-and-verse-blocks nil ; no special fortification for those blocks 
   org-indent-indentation-per-level 1         ; indent just 1 space
   org-use-sub-superscripts nil               ; don't need that in tty
   org-highlight-latex-and-related '(latex)  ; highlight latex fragments
   org-image-actual-width nil)     ; if width is specified use that, otherwise keep original size

  ;; remove org-cycle-hide-drawers from cycle hook
  ;; so it shows the plots inside a "results drawer" when the heading is opened
  (setq org-cycle-hook
        '(org-cycle-hide-archived-subtrees
          org-cycle-hide-drawers
          org-cycle-show-empty-lines
          org-optimize-window-after-visibility-change))

  :init
  ;; Make windmove work in Org mode:
  ;; [[info:org#Conflicts][org#Conflicts]]
  (setq org-replace-disputed-keys t))

;; Quick insert latex macros
;; for example, "` a" inserts "\alpha"
;; org-mode already contains a version of cdlatex, 'org-cdlatex-mode'
(use-package cdlatex :after latex)

;; bug when display image using :dir
;; https://lists.gnu.org/archive/html/emacs-orgmode/2021-04/msg00246.html
;; [[gnus:nntp+news:gmane.emacs.orgmode#8735w3kshh.fsf@ddoherty.net][Email from Daniel E. Doherty: Bug: Display Inline Images from Subdirectory [9.4.4 (9.4.4-33-g5450d6-elpaplus @ /home/ded/.emacs.d/elpa/org-plus-contrib-20210322/)]​]]
;; TODO: does not work if the block is executed with ':async'
(use-package org-display-inline-image-hack :disabled
  :ensure nil
  :after org
  :init
  (require 'subr-x)
  (defun ded:org-babel-inline-display-subtree ()
    "Redisplay inline images in subtree if cursor in source block with :result 
graphics."

    (org-babel-when-in-src-block
     (let (beg end
               (default-directory (if-let ((fname (buffer-file-name)))
                                      (file-name-directory fname)
                                    default-directory)))
       (save-mark-and-excursion
         (org-mark-subtree)
         (setq beg (point))
         (setq end (mark)))
       (when-let ((info (org-babel-get-src-block-info t))
                  (params (org-babel-process-params (nth 2 info)))
                  (result-params (cdr (assq :result-params params)))
                  ((member "file" result-params)))
         ;; (org-display-inline-images nil nil beg end)
         (org-display-inline-images)))))

  (add-hook 'org-babel-after-execute-hook 
            #'ded:org-babel-inline-display-subtree))

;; To refresh the image after it changed in disk
;; problem with cache image when included as link
;; need to clear cache
;; https://list.orgmode.org/alpine.DEB.2.22.394.2212112203210.121316@shell3.miskatonic.org/T/#t
(use-package org-refresh-inline-image-linked-hack :disabled
  :ensure nil
  :after org
  :hook (org-babel-after-execute . org-refresh-inline-images)
  :init
  (defun org-refresh-inline-images ()
    (interactive)
    (clear-image-cache))
  ;; clear cache before redisplay, maybe slow with a lot of images
  (advice-add 'org-redisplay-inline-images :before #'clear-image-cache))

;; problem with babel execute subtree and showing images outside the subtree
(use-package org-display-inline-image-after-execute-subtreee-hack :disabled
  :ensure nil
  :after org
  :general
  (org-mode-map "C-c C-v C-s" 'org-redisplay-after-execute-subtree )
  :init
  (defun org-redisplay-after-execute-subtree ()
    "Redisplay images after execute subtree"
    (interactive)
    (org-babel-execute-subtree)
    (org-display-inline-images)))

;; For some reason I need to redisplay images twice
(use-package org-toggle-inline-images-hack :disabled
  :ensure nil
  :after org
  :general (org-mode-map "<f10>" '(lambda ()
                                    (interactive)
                                    (org-redisplay-inline-images)
                                    (org-redisplay-inline-images))))

;; org image remote
(use-package org-display-inline-image-remote
  :ensure nil
  :after org
  :init
  ;; maybe too slow when there are many images
  (setq org-display-remote-inline-images 'cache))

;; org export
(use-package ox
  :ensure nil
  :after org
  :init
  (setq org-export-in-background t))

(use-package ox-html
  :ensure nil
  :after org
  :init
  ;; don't scale svg images
  (setq org-html-head "<style> .org-svg {width: auto} </style>"))

;; export to markdown
(use-package ox-md :disabled
  :ensure nil
  :demand                               ; demand after some time after org is loaded
  :defer 1
  :after org)

;; async export only loads 'ox', so we require 'ox-md' explicitly after 'ox'
(use-package hack-ox-md-async-export
  :ensure nil
  :after ox
  :init
  (require 'ox-md))

;; better support for code blocks
(use-package ox-gfm
  :demand                               ; demand after some time after org is loaded
  :defer 1
  :after org)

;; Deals with "<>" delimiters in org mode source blocks
;; https://emacs.stackexchange.com/a/68321
(use-package org-<>-delimiter-hack
  :ensure nil
  :after org
  :init
  (defun org-syntax-table-modify ()
    "Modify `org-mode-syntax-table' for the current org buffer."
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  (add-hook 'org-mode-hook #'org-syntax-table-modify))

;; load ob-C when executing C++ source block
(use-package ob-C
  :ensure nil
  :after org
  :commands
  (org-babel-execute:C++
   org-babel-execute:C)
  :config
  (setq org-babel-default-header-args:C++
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:flags . "-Wall -Werror")    ; avoid all warnings
          (:exports . "results")))
  ;; same for C
  (setq org-babel-default-header-args:C
        '((:results . "output")
          (:noweb . "no-export")
          (:eval . "never-export")
          (:exports . "results"))))

;; load ob-python only when executing python block
(use-package ob-python
  :ensure nil
  :after org
  :demand
  :config
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:exports . "results")))) ; export only plots by default

(use-package ob-core
  :ensure nil
  :after org
  ;; :general
  ;; ('normal org-mode-map "g s" (general-simulate-key "C-u C-u C-c C-v C-t"))
  :init
  ;; mkdirp allows creating the :dir if it does not exist
  (add-to-list 'org-babel-default-header-args '(:mkdirp . "yes"))
  (add-to-list 'org-babel-default-header-args '(:noweb . "no-export")))

;; custom org function
(use-package org-zoom-inline-image-hack
  :ensure nil
  :after org
  :bind
  (:map org-mode-map  
        ("C-+" . org-zoom-inline-images)
        ("C-_" . org-zoom-out-inline-images))
  :init
  (defun org-zoom-inline-images (&optional scale)
    (interactive "p")
    ;; get size specified or start with 300
    (let* ((size (if org-image-actual-width
                     org-image-actual-width
                   100))
           ;; amount can be specified with prefix argument
           ;; or use default value
           (scale (if (eq current-prefix-arg nil)
                    1.1
                  current-prefix-arg))
           (new-size (floor (* size scale))))
      (setq org-image-actual-width new-size)
      ;; don't redisplay if in tramp
      (unless (file-remote-p default-directory)
          (org-toggle-inline-images)
          (org-toggle-inline-images))))

  (defun org-zoom-out-inline-images ()
    (interactive)
    (setq org-image-actual-width nil)
    ;; don't redisplay if in tramp
    (unless (file-remote-p default-directory)
          (org-toggle-inline-images)
          (org-toggle-inline-images))))

;; for windows
(use-package ob-python
  :ensure nil
  :after org
  :when (eq system-type 'windows-nt)
  :init
  ;; windows uses python for versions > 3, argh... 
  (setq org-babel-python-command "python"))

(use-package org-clock
  :ensure nil
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
  :ensure nil
  :custom-face
  (org-block ((t (:inherit org-agenda-restriction-lock))))
  :after org
  :init
  ;; babel and source blocks
  (setq org-src-fontify-natively t
        org-src-window-setup 'split-window-right ; don't move my windows around!
        org-src-preserve-indentation t  ; preserve indentation in code
        org-adapt-indentation nil ; no extra space... better use indent mode (virtual)
        org-edit-src-content-indentation 0 ; dont indent source code
        org-src-tab-acts-natively nil	; if t, it is slow!
        org-confirm-babel-evaluate nil) ; doesn't ask for confirmation

  ;; hide source blocks
  ;; https://emacs.stackexchange.com/a/7366 
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks-visibility ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden)))
  
  ;; https://tecosaur.github.io/emacs-config/config.html#prettier-highlighting
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

;; Use unicode for delimiter of noweb syntax
;; this avoids problems with sntax highlight inside the block
;; https://emacs.stackexchange.com/a/73720
(use-package org-source-noweb-delimiter
  :ensure nil
  :after org
  :init
  (defun org-babel-noweb-wrap (&optional regexp)
    "Return regexp matching a Noweb reference.

Match any reference, or only those matching REGEXP, if non-nil.

When matching, reference is stored in match group 1."
    (rx-to-string
     `(and (or "<<" "«")
           (group
            (not (or " " "\t" "\n"))
            (? (*? any) (not (or " " "\t" "\n"))))
           (or ">>" "»")))))

;; library of babel
(use-package ob-lob :disabled
  :ensure nil
  :after org
  :init
  (org-babel-lob-ingest "~/.emacs.d/lisp/scripts.org"))

;; Agenda
;; 'org-agenda-view-mode-dispatch' to view month/week 
;; 'org-agenda-todo' change todo state
;; 'org-todo-list' go to todo view (not scheduled), org just use the agenda view that include both ('C-c a n')
;; Some concepts:
;; 1. 'task': some todo item.
;; 2. 'event': something taking place at a time and optionally at a place.
;; An event has a _plain time stamp_. If you missed, it just stays in the past. If
;; marked as 'done' it disappear from the agenda (default behavior).
;; A task can be:
;; 1. 'scheduled': start the 'task' on a given date. It is shown until marked as 'done'.
;; 2. 'deadline': finish a 'task' by this date. It can have warnings before the deadline.
(use-package org-agenda
  :ensure nil
  :bind
  ("C-c a" . org-agenda)
  :hook
  (org-agenda-finalize . (lambda ()
                           (setq-local resize-mini-windows nil)))
  :config
  (setq org-agenda-files '("~/Sync/notes/log-notes/")
        org-agenda-window-setup 'current-window ; don't change my windows
        org-agenda-skip-scheduled-if-done t     ; after I mark done, I don't want to see anymore
        ;; when timestamp is in the same line as the todo entry
        org-agenda-skip-timestamp-if-done t)

  ;; configs from 'org-modern' recomendation
  (setq  org-agenda-tags-column 0
         org-agenda-block-separator ?─
         org-agenda-time-grid
         '((daily today require-timed)
           (800 1000 1200 1400 1600 1800 2000)
           " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
         org-agenda-current-time-string
         "⭠ now"))

(use-package ox-latex
  :ensure nil
  :if (eq system-type 'gnu/linux)
  :after org
  :init
  ;; Change scale of latex preview in org-mode and make it transparent
  (plist-put org-format-latex-options :scale 1.0)
  (plist-put org-format-latex-options :background "Transparent")
  (setq 
        ;; org-startup-with-latex-preview t
        org-latex-image-default-width nil ; don't scale my images!
        org-latex-images-centered t
        org-preview-latex-image-directory "~/.cache/ltximg/"
        ;; use svg images for latex preview
        org-preview-latex-default-process 'dvisvgm
        ;; precompile not working command
        org-latex-precompile nil)

  ;; mint and svg export require '-shell-escape' option
  (setq org-latex-listings 'minted
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; extra latex packages for every header
  (setq org-latex-packages-alist '(("newfloat" "minted" nil)
                                   ("inkscapelatex=false" "svg"  nil)
                                   ("" "mathrsfs" nil) ; for command mathscr (Calligraphic letters)
                                   ("" "amsmath")
                                   ("" "bm, upgreek")
                                   ("" "tikz" nil)
                                   ("" "algorithm, algpseudocode"))))

(use-package org-latex-preview
  :when (display-graphic-p)
  :ensure nil
  :after org
  :init
  (when (string-greaterp org-version "9.8")
    (add-hook 'org-mode-hook 'org-latex-preview-auto-mode))
  :config
  (setq org-latex-preview-auto-ignored-commands '(next-line previous-line)))

(use-package ox-beamer
  :ensure nil
  :init
  ;; allows async export
  (with-eval-after-load 'ox
    (require 'org-element)
    (require 'oc)
    (require 'ox-beamer)
    (add-to-list 'org-beamer-environments-extra '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
    (add-to-list 'org-beamer-environments-extra '("overlayarea" "Y" "\\begin{overlayarea}%r%a" "\\end{overlayarea}"))))

;; adds keyword `async' to source blocks
(use-package ob-async :disabled         ; error in process sentinel: async-when-done: Invalid read syntax: "#"
  :after org
  ;; :init (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  :init
  ;; ob-python defines its own `async' keyword (which needs a session)
  (setq ob-async-no-async-languages-alist '("python"))

  ;; problem with sh async (does not work)
  ;; https://github.com/astahlman/ob-async/issues/75
  (defun no-hide-overlays (orig-fun &rest args)
    (setq org-babel-hide-result-overlays nil))
  (advice-add 'ob-async-org-babel-execute-src-block :before #'no-hide-overlays))

(use-package ob-shell
  :ensure nil
  :after org
  :commands org-babel-execute:shell org-babel-execute:sh
  :demand
  :config
  (setq org-babel-default-header-args:sh
        '((:results . "output")
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:noweb . "no-export")
          ;; 
          ;; always get my .bashrc aliases ;; not always
          ;; does not play nice with dtache
          ;; (:shebang . "#!/bin/bash -i")
          ;; 
          ;; print stderr output (like C++ compiling errors)
          ;; https://emacs.stackexchange.com/questions/59875/org-src-block-does-not-return-any-output
          ;; (:prologue . "exec 2>&1")
          ;; (:epilogue . ":")
          )))

;; for UML diagrams in org-mode
;; need to install `yay plantuml`
(use-package ob-plantuml
  :ensure nil
  :commands org-babel-execute:plantuml
  :config
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-babel-default-header-args:plantuml
        '((:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:noweb . "no-export"))))

;; insert web links with better description
(use-package org-cliplink :disabled
  :commands org-cliplink
  :general (org-mode-map "C-c l" 'org-cliplink)
  :after org)

;; copy image from clipboard, save it and insert it
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        ("C-M-y" . org-download-screenshot)
        ("C-M-S-y" . org-download-clipboard))
  :init
  (setq
   ;; apparently I need to manually create the images folder
   org-download-image-dir "./figures"
   org-download-image-html-width 350
   org-download-image-latex-width 10)
  ;; can resize screen shot
  ;; (setq org-download-screenshot-method "flameshot gui --raw > %s")
  ;; for Wayland (need to install grim slurp and wl-clipboard, and swappy)
  (setq org-download-screenshot-method "grim -g \"$(slurp -w 0 -d)\" - | swappy -f - -o %s"))

;; wsl specific config
(use-package org-download-windows :disabled
  :after org-download
  :when (string-match "-[Mm]icrosoft" operating-system-release)
  :init
  ;; add .exe to work within wsl2
  (setq org-download-screenshot-method "convert.exe clipboard: %s"))

;; Languages spell checker
;; apparently, Aspell is faster than Hunspell http://aspell.net/test/cur/
;; aspell need to install dictionaries with 'yay aspell-us' ('pt' and 'de')
;; available dicts: 'aspell dump dicts'
(use-package flyspell :disabled
  :if (eq system-type 'gnu/linux)
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode) 
  (message-send . flyspell-mode)
  :config
  (setq flyspell-issue-message-flag nil ; don't emit messages
        ;; '--camel-case' check camel case variables... maybe useful
        ;; '--sug-mode' suggestion mode 'ultra' is the fastest
        ;; '--ignore-case' when checking
        ;; '--extra-dicts' use extra dictionaries
        ;; the user dictionary files must be defaults in '~/'
        ispell-extra-args '("--sug-mode=ultra" "--camel-case=true" "--ignore-case=true")
        ;; use this mixed language as default dictionary
        ispell-local-dictionary-alist '((nil "[A-Za-z]" "[^A-Za-z]" "[']" nil ("--lang=en_US,pt_BR,de_DE") nil utf-8))))

;; Attempt to use multiple dictionaries with 'aspell'
(use-package ispell-multi :disabled)

(use-package ispell
  :ensure nil
  :config
  (setq ispell-alternate-dictionary "/home/nasser/.personal"))

;; 'husnpell' is alternative to 'aspell' that accepts multiple simultaneous dictionaries
;; download 'hunspell' and the dictionaries 'yay hunspell hunspell-en (de, pt)
;; run 'hunspell -D' to check where dictionaries are
;; https://emacs.stackexchange.com/a/21379
(use-package flyspell :disabled
  :ensure nil
  :defer 1 ; add hook for 'text-mode' after 1s
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode) 
  (add-hook 'message-send-hook 'flyspell-mode)
  (setq ispell-program-name "hunspell")	; dictionary /usr/share/hunspell
  (setq ispell-dictionary "en_US,de_DE,pt_BR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,de_DE,pt_BR")
  (setq ispell-personal-dictionary "~/.personal"))

;; 'flyspell' uses `hooks` and `sit-for` to delay
;; this uses `idle-timers`
(use-package flyspell-lazy :disabled
  :after flyspell
  :hook
  (flyspell-mode . flyspell-lazy-mode)
  :config
  (setq flyspell-lazy-idle-seconds 1))

;; Convenient functions for correcting with 'flyspell'.
(use-package flyspell-correct :disabled
  :after flyspell
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-correct-wrapper)
  ('normal flyspell-mode-map "[ ," 'flyspell-correct-wrapper))

;; completion in region manually summoned with <tab> (no auto pop up)
;; allows space (separator M-SPC) between filter words (combined with oderless)
(use-package corfu :disabled
  :when (display-graphic-p)
  :ensure (corfu :type git :host github :repo "minad/corfu"
                   :files (:defaults "extensions/*"))
  :bind
  (:map corfu-map
        ("<tab>" . corfu-next)
        ("<backtab>" . corfu-previous)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :defer 1
  :hook
  (text-mode . (lambda () (setq-local corfu-auto-prefix 3))) ; increase prefix for text
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)                ; show doc -> using eldoc box, sometimes they overlap
  (setq corfu-auto t                    ; enables timer-based completion
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-quit-no-match 'separator))

;; corfu extension
(use-package corfu-history :disabled
  :ensure nil
  :after corfu
  :config
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; `completion at point' extensions for specific candidates in `completion in region'
(use-package cape
  :ensure (cape :type git :host github :repo "minad/cape")
  :demand
  :bind
  ("C-c p" . cape-prefix-map)
  :config
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))))

;; TODO: maybe not needed anymore 
;; see [[orgit-log:~/.local/src/emacs/::("master")][~/.local/src/emacs/ (magit-log "master")]]
(use-package cape-eglot :disabled
  :ensure nil
  :after orderless eglot
  :init
  ;; use orderless style for completion (default is flex)
  ;; https://github.com/minad/corfu/wiki
  (setq completion-category-overrides '((eglot (styles orderless)))))

;; use corfu on terminal
(use-package corfu-terminal :disabled
  :ensure (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless (display-graphic-p)
  :after corfu
  :defer 1
  :config
  (corfu-terminal-mode)
  ;; does not play nicely with 'org-indent-mode'
  (with-eval-after-load 'org
    (remove-hook 'org-mode-hook 'org-indent-mode))) 

;; completion any text based on buffer contents
(use-package dabbrev
  :ensure nil
  :config
  ;; preserve the expansion's case pattern
  (setq dabbrev-case-replace nil))

;; Allows selectively display portions of program
(use-package hideshow
  :ensure nil
  ;; :bind
  ;; (:map hs-minor-mode-map
  ;;  ("C-c z h"  . hs-hide-block)
  ;;  ("C-c z s"  . hs-show-block)
  ;;  ("C-c z M-h". hs-hide-all)
  ;;  ("C-c z M-s". hs-show-allC-z)
  ;;  ("C-c z l"  . hs-hide-level)
  ;;  ("C-c z c"  . hs-toggle-hiding)
  ;;  ("C-c z a"  . hs-show-all)
  ;;  ("C-c z t"  . hs-hide-all)
  ;;  ("C-c z d"  . hs-hide-block)
  ;;  ("C-c z e"  . hs-toggle-hiding))
  :hook
  (emacs-lisp-mode . hs-minor-mode)
  (nxml-mode . hs-minor-mode)
  :config
  ;; remember internal blocks hiding status, e.g. if internal blocks are hidden, keep them hidden
  (setq hs-allow-nesting t))

(use-package latex-hideshow-hack :disabled
  :ensure nil
  :hook
  (LaTeX-mode . (lambda ()
                  (outline-minor-mode -1)
                  (hs-minor-mode)))
  :init
  (with-eval-after-load 'hideshow
    (add-to-list 'hs-special-modes-alist '(LaTeX-mode
                                           "\\(?:\\\\\\(?:paragraph\\|s\\(?:\\(?:ubs\\(?:ubs\\)?\\)?ection\\)\\)\\)" ; start
                                           ""            ; end
                                           "%"          ; comment-start
                                           latex-hideshow-forward-sexp-function ; forward-sexp-function
                                           ))
    (defun latex-hideshow-forward-sexp-function (_arg)
      "Go to end of the current block."
      (interactive)
      (latex-nav-end-of-block))
    (defun latex-info-looking-at-beginning-of-block ()
      "Check if point is at the beginning of block."
      (save-excursion
        (beginning-of-line)
        (looking-at "\\(?:\\\\\\(?:paragraph\\|s\\(?:\\(?:ubs\\(?:ubs\\)?\\)?ection\\)\\)\\)")))
    (defun latex-nav-beginning-of-block ()
      "Move to start of the current block."
      (interactive)
      (if (latex-info-looking-at-beginning-of-block)
          t
        (search-backward  "\\(?:\\\\\\(?:paragraph\\|s\\(?:\\(?:ubs\\(?:ubs\\)?\\)?ection\\)\\)\\)" nil t)))
    (defun latex-nav-end-of-block ()
      "Move to end of current block"
      (interactive)
      (when (latex-nav-beginning-of-block)
        (let ((current-block (current-word)))
          (forward-line)
          ;; if can not find another block on the same level, search any other block
          (unless (re-search-forward (concat "\\\\" current-block) nil t)
            (unless (re-search-forward "\\(?:\\\\\\(?:paragraph\\|s\\(?:\\(?:ubs\\(?:ubs\\)?\\)?ection\\)\\)\\)" nil t)
              (re-search-forward "\\\\end{document}")))
          (forward-line -1)
          (end-of-line))))))

;; folding
;; note: evil collection also sets a bunch of keybindings
(use-package outline
  :mode ("\\.inp\\'" . outline-minor-mode)
  :ensure nil
  :config
  (setq outline-minor-mode-cycle t))  

;; trying to make outline work with python docstring
(use-package outline-python-regex :disabled
  :ensure nil
  :after outline
  :init
  (add-hook 'python-mode-hook  '(lambda ()
                                  (setq outline-regexp
                                        (rx (group 
                                             ;; Heading level
                                             (group (* space)) ; 0 or more spaces
                                             bow
                                             ;; Keywords
                                             (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
                                             eow))))))

(use-package latex
  ;; version for solving problem with evil-tex https://github.com/progfolio/elpaca/issues/217
  :ensure (auctex :pre-build (("./autogen.sh")
                    ("./configure"
                     "--without-texmf-dir"
                     "--with-packagelispdir=./"
                     "--with-packagedatadir=./")
                    ("make"))
        :build (:not elpaca--compile-info) ;; Make will take care of this step
        :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
        :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :init
  ;; This commit add a remap from LaTeX-mode (which Auctex) uses to latex-mode
  ;; [[orgit-rev:~/.local/src/emacs/::1ea3b369021c90701c634c512426f75ce1291d77][~/.local/src/emacs/ (magit-rev 1ea3b369021)]]
  (setq major-mode-remap-defaults nil)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :commands TeX-command-sentinel
  :custom-face
  (font-latex-sectioning-1-face ((t (:slant oblique :box t :height 1.0))))
  (font-latex-sectioning-2-face ((t (:underline t :inherit outline-1 :height 1.0))))
  (font-latex-sectioning-3-face ((t (:slant italic  :inherit outline-4 :height 1.0))))
  (font-latex-sectioning-4-face ((t (:slant normal :inherit outline-5 :underline nil :height 1.0))))
  (font-latex-sectioning-5-face ((t (:slant normal :height 1.0))))
  :hook
  (LaTeX-mode . outline-minor-mode)
  :hook
  (LaTeX-mode . (lambda ()
                  (prettify-symbols-mode) ; for greek letters and other math symbols
                  (LaTeX-math-mode)       ; easy to type greek letters
                  (TeX-fold-mode) ; fold (reduce clutter) footnotes, comments etc (C-c C-o C-o DWIM)
                  (reftex-isearch-minor-mode)
                  (visual-line-mode)
                  (outline-hide-sublevels 1) ; start folded
                  (variable-pitch-mode)      ; use variable pitch font (not monospace)
                  (yas-minor-mode)
                  (turn-off-auto-fill)))
  :config
  ;; basics configs
  (setq TeX-save-query nil
        TeX-auto-save t   ; enable parse on save (to force parse use C-c C-n (TeX-normal-mode))
        TeX-parse-self t ; enable document parsing to get labels for completion
        TeX-PDF-mode t			; output pdf 
        TeX-electric-escape t
        TeX-insert-macro-default-style 'mandatory-args-only ; don't ask for optional argument afte "C-c m"
        TeX-master nil) ; make auctex aware of multi-file documents

  ;; start latex buffer folded
  ;; (add-hook 'find-file-hook 'TeX-fold-buffer t)

  (setq-default TeX-engine 'default
                ;; for xetex with shell escape
                TeX-command-extra-options "-shell-escape")
  
  ;; variables for jumping between source and pdf
  (setq TeX-source-correlate-method 'synctex ;; Method for enabling forward and inverse search 
        TeX-source-correlate-start-server t ;; inhibit the question to start a server process
        TeX-source-correlate-mode t) ;; jump to source

  ;; update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions  
            'TeX-revert-document-buffer) 

  ;; TODO: currently does not work
  ;; remove intermediate files
  ;; file argument because of `TeX-after-compilation-finished-functions'
  (defun TeX-clean-intermediate (file)
    (let ((buf (find-buffer-visiting file)))
      (when buf
        (with-current-buffer buf
          (setq TeX-clean-confirm nil)          ; don't ask to confirm
          (TeX-clean t)))))
  ;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-clean-intermediate) 

  ;; nomenclature compilation option for latex
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list 
                  '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                    (lambda (name command file)
                      (TeX-run-compile name command file)
                      (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
                    nil t :help "Create nomenclature file")))

  ;; PDF Toll is ok when I'm with just one screen
  ;; one advantage of PDF Tools is 'pdf-view-set-slice-from-bounding-box', good for small screens
  ;; actually, Okular can trim margins as well...
  ;; Okular has better: continuous scrolling, zoom, more responsive
  ;; In the 'Editor Options', in Okular, need to add the 'Emacs client' option
  ;; for jumping to source functionality
  ;; To jump to source, from Okular, one needs to use the 'Browser tool'
  ;; (Ctrl-1) and click on the text with shift key pressed on.
  ;; (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))

;; preview in latex
(use-package preview
  :ensure nil
  :after latex
  :custom-face
  (preview-face  ((t (:background unspecified))))
  :config
  ;; preview latex config
  ;; only preview displaymath and not textmath which can be anoying when inside a table or algorithm
  ;; It is ok if we can fold the table or algorithm.
  (setq preview-default-option-list '("displaymath" "showlabels" "textmath")
        preview-auto-cache-preamble t
        preview-scale-function (/ 1 1.01659593) ; make the math approximately same height as text
        ;; preview-LaTeX-command-replacements '(preview-LaTeX-disable-pdfoutput)
        )
  (add-to-list 'preview-auto-reveal-commands 'meow-left)
  (add-to-list 'preview-auto-reveal-commands 'meow-right))

;; Better math preview in latex
(use-package preview-auto
  :ensure (preview-auto :type git :host github :repo "ultronozm/preview-auto.el")
  :after latex
  :demand t
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1))

;; latex function
(use-package latex-insert-figure-from-clipboard-hack
  :ensure nil
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("C-M-y" . my-tex-insert-clipboard))
  :init
  ;; function definition, not the best...
  (defun my-tex-insert-clipboard ()
    (interactive)
    (setq folder-path (concat default-directory "figures/"));make the img directory
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
                        "figures/img_"
                        (format-time-string "%Y%m%d_%H%M%S_.png")))
           (exit-status
            ;; for wsl to use windows imagemagick :/
            (if (string-match "-[Mm]icrosoft" operating-system-release)
                (call-process "convert.exe" nil nil nil
                              "clipboard:" image-path)
              (shell-command (concat "flameshot gui --raw > " (shell-quote-argument image-path))))))
      (insert (format "
\\begin{figure}[ht!]
  \\centering
  \\includegraphics[width=.5\\linewidth]{%s}
\\end{figure}" image-file)))))

(use-package tex-fold
  :ensure nil
  :after (:any latex org)
  :custom-face
  (TeX-fold-unfolded-face ((t (:background unspecified))))
  :config
  ;; add tables and figure to fold
  (add-to-list 'TeX-fold-env-spec-list '("[table]" ("table")))  
  (add-to-list 'TeX-fold-env-spec-list '("[figure]" ("figure")))
  (add-to-list 'TeX-fold-env-spec-list '("[frame]" ("frame")))
  (add-to-list 'TeX-fold-env-spec-list '("[minipage]" ("minipage")))
  (add-to-list 'TeX-fold-env-spec-list '("[tikzpicture]" ("tikzpicture")))
  (add-to-list 'TeX-fold-env-spec-list '("[algorithm]" ("algorithm")))

  ;; fold comments as well
  (add-to-list 'TeX-fold-type-list 'comment)

  ;; fold document class and document macros
  (add-to-list 'TeX-fold-macro-spec-list '("[class]" ("documentclass")))
  (add-to-list 'TeX-fold-macro-spec-list '("[note]" ("note")))

  (setq TeX-fold-ellipsis "…")

  ;; make sure meow keybindings auto reveal folded regions
  (add-to-list 'TeX-fold-auto-reveal-commands 'meow-left)
  (add-to-list 'TeX-fold-auto-reveal-commands 'meow-right))

;; fake headers for latex
;; https://emacs.stackexchange.com/a/3103
(use-package latex-fake-header
  :ensure nil
  :after latex
  :init
  ;; extra outline headers 
  (setq TeX-outline-extra
        '(("%paragraph" 5)))

  ;; add font locking to the headers
  (font-lock-add-keywords
   'latex-mode
   '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)" 0 'font-lock-keyword-face t)
     ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t))))

;; Creates UNIQUE labels, helps referencing them (not so good)
;; AUCTeX defaut: C-c RET -> eqref -> prompts for label (can be with completion)
;; RefTeX interface: C-c RET -> eqref -> TAB -> select label with completion
(use-package reftex :disabled
  :after latex
  :commands reftex-toc
  :hook
  (LaTeX-mode . (lambda () (turn-on-reftex))) ; reftex-mode
  :general
  ('normal reftex-mode-map :prefix "g r"
           "t" 'reftex-toc
           "v" 'reftex-view-crossref
           "g" 'reftex-goto-label
           "r" 'reftex-reference
           "c" 'reftex-citation)
  :init
  (setq reftex-cite-prompt-optional-args t ; Prompt for empty optional arguments in cite
        ;; https://www.gnu.org/software/emacs/manual/html_mono/reftex.html
        reftex-enable-partial-scans t
        reftex-keep-temporary-buffers nil
        reftex-save-parse-info t
        reftex-plug-into-AUCTeX t       ; integrate reftex with auctex
        reftex-trust-label-prefix '("fig:" "eq:") ; speed up parsing of labels
        ;; don't ask which refecence macro after `reftex-referene'
        reftex-ref-macro-prompt nil
        ;; show just equations in the label menu
        reftex-label-menu-flags '(nil nil nil nil nil nil nil nil)))

(use-package dired
  :ensure nil
  :commands dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :bind (("C-x j" . dired-jump)
         :map dired-jump-map
         ("j" . nil)
         :map dired-mode-map
         ("l" . dired-find-alternate-file)
         ("h" . dired-up-directory)
         ("o" . dired-sort-toggle-or-edit)
         ("s" . isearch-forward)
         ("C-c C-d" . mkdir)
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$"
        dired-auto-revert-buffer t
        dired-listing-switches "-AGhlv -lt --group-directories-first --time-style=long-iso"	; human readable format when in detail
        dired-kill-when-opening-new-dired-buffer t ; kill when changing dir
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; manjaro: ~/.local/share/Trash/
        delete-by-moving-to-trash t	; move to trash (problem with naming and tramp)
        remote-file-name-inhibit-delete-by-moving-to-trash t ; when in remote, just delete
        )

  ;; kill the dired buffer and enters the current line file or directory
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Open pdf in dired with `!` and the default application 
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "okular")
          ("\\.png\\'" "feh --auto-reload")
          ("\\.svg\\'" "feh --auto-reload"))))

(use-package dired-image-window-placement-hack
  :ensure nil
  :after dired
  :init
  (setf (alist-get "^\\*image-dired\\*$"
                 display-buffer-alist
                 nil nil #'string=)
        ;; reuse window, otherwise open bellow
      '((display-buffer-reuse-window
        display-buffer-below-selected))))

(use-package dired-subtree
  :after dired
  :custom-face
  (dired-subtree-depth-1-face ((t (:background unspecified))))
  (dired-subtree-depth-2-face ((t (:background unspecified))))
  (dired-subtree-depth-3-face ((t (:background unspecified))))
  :bind
  (:map dired-mode-map
      ("<TAB>" . dired-subtree-toggle)))

;; open dired as a sidebar
(use-package dired-sidebar
  :bind
  (("C-x C-j" . dired-sidebar-jump)
   :map dired-sidebar-mode-map
   ("l" . dired-sidebar-find-file) ; use 'C-u' to select specific window
   ("h" . dired-sidebar-up-directory))
  :hook
  (dired-sidebar-mode . visual-line-mode)
  ;; avoid fixing window size
  (dired-sidebar-mode . (lambda () (setq window-size-fixed nil)))
  :config
  (setq dired-sidebar-one-instance-p t      ; just sidebar per frame
        dired-sidebar-use-magit-integration nil) ; open dwim, not parent git
  (defun dired-sidebar-jump ()
    (interactive)
    (dired-sidebar-show-sidebar)        ;show the side bar
    (dired-sidebar-toggle-with-current-directory) ; hide it and re opening with current dir
    (dired-sidebar-toggle-with-current-directory)))

;; Load modus in terminal, it is very clever to figure out the colors there
(use-package modus-themes
  :defer 1
  :bind
  ("<f5>" . modus-themes-toggle)
  :config
  (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-prompts '(intense italic)
        modus-themes-diffs 'desaturated
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-headings '((t . (rainbow))))
  ;; hook to enforce change when theme is toggled (which loads the theme)
  ;; (defun my-modus-tweaks ()
  ;;   (progn 
  ;;     ;; Adjust some org faces
  ;;     (eval-after-load 'org
  ;;       ;; make org source blocks headers with same main background, so there is no different background when collapsed
  ;;       '(set-face-attribute 'org-block-begin-line nil :background (modus-themes-get-color-value 'bg-main) :slant 'italic))
  ;;     ;; adjust org modern if GUI
  ;;     (eval-after-load 'org-modern
  ;;       '(global-org-modern-mode))
  ;;     ;; reset icons cache to match theme
  ;;     (eval-after-load 'kind-icon
  ;;       '(kind-icon-reset-cache))
  ;;     ;; recompute face for indentation guide
  ;;     (eval-after-load 'hl-indent-scope
  ;;       '(hl-indent-scope--auto-color-calc))
  ;;     ;; make inside of parenthesis different background
  ;;     (eval-after-load 'smartparens
  ;;       '(set-face-attribute 'sp-show-pair-match-content-face nil :background (modus-themes-get-color-value 'bg-paren-expression)))))
  ;; (add-hook 'modus-themes-after-load-theme-hook 'my-modus-tweaks)

  ;; load the theme automatically in the terminal and disable others automatically
  ;; (if (not (display-graphic-p))
  ;;     (modus-themes-load-theme 'modus-vivendi-tinted))
  )

;; change backgroud of other windows
;; when with custom theme and GUI
(use-package highlight-current-window :disabled
  :when (display-graphic-p)
  :ensure nil
  :init
  (defun highlight-selected-window ()
    "Highlight selected window with a different background color.
Only if there is more than one window opened."
    (when (> (length (window-list)) 1)
      (walk-windows (lambda (w)
                      (unless (eq w (selected-window)) 
                        (with-current-buffer (window-buffer w)
                          (buffer-face-set '(:background "gray94"))))))
      (buffer-face-set 'default)))

  (add-hook 'buffer-list-update-hook 'highlight-selected-window)
  (defun disable-window-highlight (theme &rest args)
    "Disable window highlight"
    (remove-hook 'buffer-list-update-hook 'highlight-selected-window))
  ;; disable this when changing theme
  ;; only use when default theme
  (advice-add 'disable-theme :after #'disable-window-highlight))

;; dimm other buffers
(use-package dimmer
  :commands dimmer-mode
  :config
  (setq dimmer-fraction 0.3)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  ;; make it compatible to corfu
  ;; https://github.com/gonewest818/dimmer.el/issues/62
  (defun advise-dimmer-config-change-handler ()
    "Advise to only force process if no predicate is truthy."
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (unless ignore
        (when (fboundp 'dimmer-process-all)
          (dimmer-process-all t)))))

  (defun corfu-frame-p ()
    "Check if the buffer is a corfu frame buffer."
    (string-match-p "\\` \\*corfu" (buffer-name)))

  (defun dimmer-configure-corfu ()
    "Convenience settings for corfu users."
    (add-to-list
     'dimmer-prevent-dimming-predicates
     #'corfu-frame-p))

  (advice-add
   'dimmer-config-change-handler
   :override 'advise-dimmer-config-change-handler)

  (dimmer-configure-corfu))

;; this mode is used to highlight current window
(use-package face-remap :disabled
  :ensure nil)

;; syntax highlight in html export of org-mode source blocks
;; does not work well with modus-themes and tree-sitter
(use-package htmlize :disabled)

;; `:includes` so elpaca can recognize dap-python.el and dap-cpptools
(use-package pyvenv
  :commands pyvenv-activate pyvenv-workon
  :config
  (setenv "WORK_HOME" "~/.virtualenvs"))

;; mode for C++
(use-package c++-mode :disabled
  :ensure nil
  :mode ("\\.cpp\\'" . c++-mode)
  :general
  (c++-mode-map "C-x c" 'compile)
  (c-mode-map "C-x c" 'compile)
  :hook
  (c++-mode . setup-c++-style)
  :config
  ;; Linux style: keeps brackets on their own line and aligned
  ;; and it uses by default 8 spaces (too much, 4 is ok)
  ;; if(foo)
  ;; {
  ;;     bar++;
  ;; }
  (setq c-default-style "linux"
        c-basic-offset 4)
  :init
  (defun setup-c++-style ()
    "Function to set documentation comment style."
    ;; need to set up in a hook
    (setq c-doc-comment-style '(doxygen autodoc javadoc gtkdoc))
    (c-setup-doc-comment-style)         ; need to call after changing the style
    )) 

;; Specific for c++ with treesit
;; not ready yet
(use-package c++ts-mode
  :ensure nil
  :bind
  (:map c++-ts-mode-map
        ("C-x c" . compile))
  :init
  ;; From the documentation, substitute to 'tree-sitter' based modes
  ;; [[help:c-ts-mode][help:c-ts-mode]] 
  ;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist
  ;;              '(c-or-c++-mode . c-or-c++-ts-mode))
  :config
  (setq c-ts-mode-indent-style "linux"
        c-ts-mode-indent-offset 4)) 

(use-package highlight-doxygen
  :custom-face
  (highlight-doxygen-comment  ((t (:background unspecified :italic nil))))
  :hook
  (c++-mode . highlight-doxygen-mode)
  (c++-ts-mode . highlight-doxygen-mode))

(use-package ob-python :disabled
  :after org lsp
  :commands org-babel-execute:python
  :init
  (defun org-babel-edit-prep:python (babel-info)
    (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
    (lsp)))

;; Microsoft python language server
;; it seems to be faster than pyls
;; does not have formating
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook ((python-mode . visual-line-mode)
         (inferior-python-mode . visual-line-mode)
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

(use-package python-ts
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
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

(use-package numpydoc :disabled
  :after python
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :general
  (python-mode-map "C-c C-n" 'numpydoc-generate))

(use-package goto-last-change :disabled ; use the evil one "g ;" and "g ,"
  :general ('normal "g b" 'goto-last-change))

;; easy select region
(use-package expand-region :disabled
  :bind ("C-=" . er/expand-region))

;; key chord hint
(use-package which-key
  :defer 1
  :config
  (which-key-mode t))

;; highligh TODO keyword everywhere
(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode)
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

(use-package citar
  :bind
  ("C-c b" . citar-open)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :custom
  (citar-bibliography '("~/.bibliography.bib"))
  :config
  (setq citar-library-paths '("~/Sync/bibliography/"))
  ;; open xournalpp and pdf externally
  (add-to-list 'citar-file-open-functions '("xopp" . (lambda (file) (call-process "xournalpp" nil 0 nil file))))
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external)))

(use-package citar-embark
  :after citar embark
  :no-require
  :defer nil
  :config (citar-embark-mode))

(use-package biblio
  :commands biblio-lookup)

(use-package server
  :ensure nil
  :defer 1
  :config
  (or (server-running-p)
      (server-start)))

(use-package table
  :ensure nil
  :after org)

(use-package yasnippet
  :hook
  (LaTeX-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  (prog-mode . yas-minor-mode)
  :commands yas-insert-snippet
  :config
  (yas-reload-all)
  (yas-global-mode))

;; ensures environment variables inside Emacs is the same in the user's shell
;; emacs' exec-path is not automatically updated from PATH
;; to run jupyter which is installed in ~/.local/bin, not in the (print exec-path)
;; added ~/.local/bin to exec path solves the problem with jupyter
;; no need for this package, for now, defer with `:commands`
(use-package exec-path-from-shell :disabled
  :config
  ;; non interative shell start up faster
  ;; (setq exec-path-from-shell-arguments nil)
  :commands (exec-path-from-shell-initialize))

;; browser the web inside emacs
(use-package eww
  :ensure nil
  :bind
  ("<f12>" . eww)                        ; with C-u prefix, open new buffer
  :hook
  ;; (eww-after-render . (lambda () (eww-readable)))  ; does not work for all
  (eww-mode . visual-line-mode)
  :config
  (setq shr-use-fonts t                 ; change heading size
        shr-use-colors t
        shr-max-image-proportion .5
        shr-folding-mode t
        shr-bullet "• "
        eww-auto-rename-buffer t                    ; each page on its own buffer
        eww-search-prefix "https://www.google.com/search?hl=en&lr=lang_en&q="))

(use-package browse-url
  :ensure nil
  :config
  (setq browse-url-browser-function 'browse-url-default-browser))

;; jump to link
(use-package ace-link :disabled
  :general
  ('normal eww-mode-map "C-f" 'ace-link-eww)
  ('normal helpful-mode-map "C-f" 'ace-link-help)
  ('normal gnus-article-mode-map "C-f" 'ace-link-gnus))

(use-package pdf-tools :disabled
  ;; :if (eq system-type 'windows-nt)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :general
  ('normal pdf-view-mode-map "M-h" 'pdf-history-backward)
  ('normal pdf-view-mode-map "C" 'pdf-view-center-in-window)
  ;; use 'isearch' and before quitting use 'consult-isearch-forward'
  ('normal pdf-view-mode-map "/" 'isearch-forward-regexp)
  :init
  (pdf-loader-install)
  :config
  (setq pdf-view-midnight-colors '("white" . "black"))
  
  ;; sync pdf in different frame
  (setq pdf-sync-forward-display-action
        '(display-buffer-reuse-window (reusable-frames . t)))
  (setq pdf-sync-backward-display-action
        '(display-buffer-reuse-window (reusable-frames . t))))

;; Terminal emulator based on libvterm (in C)
(use-package vterm :disabled
  :commands vterm
  :config
  (setq vterm-max-scrollback 20000
        vterm-timer-delay 0)
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("scp" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("apptainer" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("docker" "/bin/bash")))

;; Quickly switch to 'vterm' buffer.
(use-package vterm-toggle :disabled
  :after vterm
  :bind
  (("<f9>" . vterm-toggle-cd) 	; opens term in current cd including remote
   ("C-<f9>" . vterm-toggle-insert-cd)
   :map vterm-mode-map
   ("s-n" . vterm-toggle-forward)
   ("s-p" . vterm-toggle-backward))
  :config
  ;; toggle terminal bellow the selected window (avoid messing with other windows)
  ;; https://github.com/jixiuf/vterm-toggle/issues/33#issuecomment-1100027238
  (add-to-list 'display-buffer-alist `(,vterm-buffer-name display-buffer-below-selected)))

;; multiple terminals
(use-package multi-vterm :disabled
  :bind
  ("S-<f9>" . multi-vterm)
  :commands multi-vterm
  :config
  (add-to-list 'display-buffer-alist `("^\\*vterminal.*" display-buffer-below-selected)))

(use-package terminal-here :disabled
  :general
  ('normal "C-<f7>" 'terminal-here-launch)
  :config
  ;; change terminal command
  (when (string= system-name "ryzen-ms7c37")
    (setq terminal-here-terminal-command 'gnome-terminal)))

(use-package keycast :disabled
  :commands keycast-mode keycast-log-mode)

(use-package gif-screencast :disabled
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
  :ensure nil
  :after org :demand				; load after org
  :hook (org-mode . (lambda ()
                      (add-hook 'before-save-hook 'time-stamp)))
  :config
  (setq time-stamp-active t
        time-stamp-format "%Y-%m-%d %H:%M:%S"
        time-stamp-end "$"		; regex for end of line
        time-stamp-start "#\\+lastmod:[ \t]*"))

(use-package repeat
  ;; :if (string-greaterp emacs-version "28") ; need emacs > 28
  :ensure nil
  :defer 1
  :demand
  :config
  ;; useful to resize windows
  (setq repeat-keep-prefix t)
  ;; built-in command repeater (like hydra)
  (repeat-mode t))

(use-package windmove
  :ensure nil
  :defer 1
  :bind
  ("C-c C-h" . windmove-left)
  ("C-c C-l" . windmove-right)
  ("C-c C-j" . windmove-down)
  ("C-c C-k" . windmove-up)
  :config
  (windmove-default-keybindings))

;; built in windows resize functions
(use-package window
  :ensure nil
  :bind-keymap ("C-c w" . resize-window-repeat-map)
  :bind (("C-c o" . other-window)
         :repeat-map resize-window-repeat-map
                     ("j" . enlarge-window)
                     ("k" . shrink-window)
                     ("h" . shrink-window-horizontally)
                     ("l" . enlarge-window-horizontally)))

(use-package ol
  :ensure nil
  :after org
  :bind
  (:map org-mode-map
        ("C-c s s" . org-store-link)))

;; create backlinks when linking org-mode headings
(use-package org-super-links :disabled
  :ensure (org-super-links :type git :host github :repo "toshism/org-super-links")
  ;; :after org  ; can use outside of org-mode, so use the keybindings to load
  :bind
  (("C-c s l" . org-super-links-store-link)
   ("C-c s P" . org-super-links-insert-link)
   :map org-mode-map 
   ("C-c s l" . org-super-links-store-link)
   ("C-c s P" . org-super-links-insert-link)
   ("C-c s d" . org-super-links-quick-insert-drawer-link)
   ("C-c s i" . org-super-links-quick-insert-inline-link)
   ("C-c s C-d" . org-super-links-delete-link))
  :config
  ;; just use plain link with no description, easy to edit
  (setq org-super-links-default-description-formatter ""))

;; loads the org-id library from org repository
;; for creating org-ids for more robust linking, avoid referencing issues
(use-package org-id
  :ensure nil
  :after org
  :demand                               ; explicitly require 'org-id'
  :config
  (setq org-id-locations-file "~/Sync/news/.org-id-locations")
  ;; automatic generate id for headings
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; citations support in org-mode
(use-package oc
  :ensure nil
  :init
  (with-eval-after-load 'org
   (setq org-cite-global-bibliography '("~/.bibliography.bib")))
  (with-eval-after-load 'ox
    (require 'oc)
    (setq org-cite-global-bibliography '("~/.bibliography.bib"))))

(use-package ox-icalendar
  :ensure nil
  :init
  ;; for async export
  (with-eval-after-load 'ox
    (require 'ox-icalendar)))

(use-package ox-beamer
  :ensure nil
  :init
  ;; for async export
  (with-eval-after-load 'ox
    (require 'oc)
    (require 'ox-beamer)))

;; emacs built in version control
(use-package vc-git
  :ensure nil
  :bind
  ("C-x v p" . vc-push)
  :hook
  (diff-mode . outline-minor-mode)
  (vc-git-region-history-mode . outline-minor-mode))

;; Manage remote files access and manipulations
;; to use without password, first create and copy the key to the remote
;; `ssh-keygen -b 4096'
;; `ssh-copy-id <remote-address>'
;; then add to `~/.ssh/config'
;;  Host <name>  
;;    HostName ip  
;;    IdentityFile path-to-ssh-key  
;;    User root
;; 
;; To use with docker containers created with sudo, one can use hops syntax:
;;
;; `sudo:|docker:containerID:/'
;; 
;; The container ID can be found with `sudo docker ps'
(use-package tramp
  :ensure nil
  :config
  ;; scp is faster than ssh for copying files, but scp is apparently deprecated
  ;; https://www.reddit.com/r/emacs/comments/xul3qm/how_to_make_tramp_faster/
  (setq tramp-default-method "scp"
        tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
        tramp-histfile-override nil
        tramp-verbose 1)                ; increase to 6 to debug

  ;; apparently makes it faster
  ;; https://www.gnu.org/software/tramp/#Frequently-Asked-Questions
  ;; https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh 
  (setq remote-file-name-inhibit-cache nil)
  ;; Ok, if different emacs sessions are not editing the same file
  (setq remote-file-name-inhibit-locks t)

  ;; ignore version control 
  ;; https://www.reddit.com/r/emacs/comments/gxhomh/help_tramp_connections_make_emacs_unresponsive_on/
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; make TRAMP use the remotes' PATH environment variable
  ;; https://github.com/emacs-pe/docker-tramp.el/blob/master/README.md#tramp-does-not-respect-remote-path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; enable apptainer method
  (tramp-enable-apptainer-method))

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
  :bind ("C-c C-b" . ibuffer)
  :ensure nil
  :config
  ;; Grouping
  ;; https://www.emacswiki.org/emacs/IbufferMode#h5o-6
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 ("outputs" (or
                             (name . "\\.out")))
                 ("python-sessions" (or
                             (mode . inferior-python-mode)))
                 ("dired" (or
                             (mode . dired-mode)))
                 ("gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))))))
  :hook (ibuffer . (lambda ()
                     (ibuffer-switch-to-saved-filter-groups "default")
                     (ibuffer-do-sort-by-alphabetic))))

;; group by Remote in ibuffer
(use-package ibuffer-tramp :disabled
  :general
  ('normal ibuffer-mode-map "s t" 'ibuffer-tramp-set-filter-groups-by-tramp-connection)
  ('normal ibuffer-mode-map "s r" 'ibuffer-switch-to-saved-filter-groups))

;; need to install language specific modules:
;; https://github.com/casouri/tree-sitter-module
;; and put in ./emacs.d/tree-sitter
(use-package treesit
  :ensure nil
  :custom-face
  (font-lock-function-call-face ((t :inherit outline-7)))
  ;; (font-lock-function-name-face ((t :inherit t :weight bold)))
  :config
  ;; maximum fontification
  (setq treesit-font-lock-level 4))

(use-package wgrep)

;; config for windows
(use-package emacs-windows-config :disabled
  :ensure nil
  :if (eq system-type 'windows-nt)
  :init
  (setq w32-get-true-file-attributes nil
        recentf-auto-cleanup 'never))

;; easily change windows
(use-package ace-window
  :commands aw-select aw-window-list                   ; for dired with C-u
  :bind
  (("C-c C-w" . ace-window))
  :init
  ;; Adapted from: https://stackoverflow.com/a/47624310
  (defun dired-find-alternate-file-ace ()
    "With prefix argument use ace window to select a window for
opening a file from dired. Otherwise just regular dired."
    (interactive)
    (if (eq current-prefix-arg nil)
        (dired-find-alternate-file)
      (let ((file (dired-get-file-for-visit)))
        (if (> (length (aw-window-list)) 1)
            (aw-select "" (lambda (window)
                            (aw-switch-to-window window)
                            (find-file file)))
          (find-file-other-window file)))))

  (defun find-file-or-switch-to-buffer (filename)
    "Open the file FILENAME or switch to the buffer if the file is already open."
    (let ((buffer (find-buffer-visiting filename)))
      (if buffer
          (switch-to-buffer buffer)
        (find-file filename))))

  (defun dired-find-alternate-file-ace-rem ()
    "With prefix argument use ace window to select a window for
opening a file from dired. Otherwise just regular dired."
    (interactive)
    (if (eq current-prefix-arg nil)
        (find-file-or-switch-to-buffer (dired-get-file-for-visit))
      (let ((file (dired-get-file-for-visit)))
        (if (> (length (aw-window-list)) 1)
            (aw-select "" (lambda (window)
                            (aw-switch-to-window window)
                            (find-file-or-switch-to-buffer file)))
          (find-file-or-switch-to-buffer file))))))

;; highlight parensthesis
(use-package highlight-parentheses :disabled
  :defer 1 
  :config
  (setq highlight-parentheses-colors nil
        highlight-parentheses-attributes '((:box  (:line-width (-1 . -1) :style nil))))
  ;; Since tty does not have box, use underline
  (if (not (display-graphic-p))
      (setq highlight-parentheses-attributes '((:underline  t :weight bold :slant italic))))
  (global-highlight-parentheses-mode))

;; moving cursor around fast and efficiently
(use-package avy
  :bind
  ("M-j" . avy-goto-char-timer)
  ("C-M-j" . avy-resume)
  ("C-c j" . avy-goto-line-below)       ; nice for org-mode buffers
  ("C-c k" . avy-goto-line-above)
  (:map isearch-mode-map 
        ("M-j" . avy-isearch))
  :config
  (setq avy-timeout-seconds 0.2         ; quicker
        avy-all-windows-alt t           ; allow all windows when `C-u`
        avy-background t
        avy-all-windows nil))           ; restrict to one window

(use-package all-the-icons-completion
  :ensure (all-the-icons-completion :type git :host github :repo "MintSoup/all-the-icons-completion")
  :when (display-graphic-p)
  :after marginalia all-the-icons
  :defer 1
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; icons for dired
(use-package all-the-icons-dired :disabled
  :after all-the-icons
  :when (display-graphic-p)
  :hook
  (dired-mode . (lambda ()
                  ;; don't enable in dired-sidebar
                  (unless (eq major-mode 'dired-sidebar-mode)
                    (all-the-icons-dired-mode))))
  :custom-face
  (all-the-icons-dired-dir-face ((t (:foreground "goldenrod"))))
  :init
  (setq all-the-icons-dired-monochrome nil))

;; Built in language server.
(use-package eglot
  :ensure nil
  :custom-face
  (eglot-highlight-symbol-face ((t (:underline t :weight bold))))
  :bind
  (:map eglot-mode-map
        ("C-c RET" . eglot-code-actions))
  :hook
  (eglot-managed-mode . eglot-inlay-hints-mode)
  ;; python "pyright" language server
  ;; sudo npm install --global pyright
  (python-mode . eglot-ensure) ; works if there is only one server available
  (python-ts-mode . eglot-ensure)
  (c++-mode . eglot-ensure) ; works if there is only one server available
  (c++-ts-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (js-mode . eglot-ensure) ; works if there is only one server available
  :config
  ;; add watch mode "-w" for performance
  ;; pyright only reanalyze the files that have been modified
  ;; (add-to-list 'eglot-server-programs
  ;;              '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio" "--watch")))
  ;; (add-to-list 'eglot-server-programs
  ;;              `((c++-mode c++-ts-mode) . ,(eglot-alternatives
  ;;                                           '(("clangd" "--clang-tidy")))))

  ;; Maybe improve performance
  ;; No event buffers, disable providers cause a lot of hover traffic. Shutdown unused servers.
  ;; https://github.com/joaotavora/eglot/discussions/993
  (setq eglot-events-buffer-config '(:size 0 :format full)
        eglot-sync-connect 0            ; don't block LSP connection attempts
        eglot-autoshutdown t)

  ;; Maybe improve performance
  ;; https://old.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/
  (fset #'jsonrpc--log-event #'ignore))

;; Use 'mypy' check for type in python
;; need to install 'pip install mypy'
;; for examples: https://mypy-lang.org/examples.html
(use-package flymake-mypy :disabled     ; for some reason not working, I'm
                                        ; relying on the language server for
                                        ; type check now
  :ensure (flymake-mypy :type git :host github :repo "com4/flymake-mypy")
  :hook
  (eglot-managed-mode . (lambda ()
                           (when (or (derived-mode-p 'python-mode)
                                     (derived-mode-p 'python-ts-mode))
                             ;; re-enable the default python.el checker
                             (add-hook 'flymake-diagnostic-functions 'python-flymake nil t)
                             (require 'flymake-mypy) ; load the package 
                             (flymake-mypy-enable)))))

;; add ltex language server to eglot
;; uses language tool for grammar, but there is no need to install it
;; allow configuration on the file: 
;; need to manually download the language server:
;; cd ~/.opt && wget https://github.com/valentjn/ltex-ls/releases/download/16.0.0/ltex-ls-16.0.0-linux-x64.tar.gz
;; tar -xvf ltex-ls-16.0.0-linux-x64.tar.gz
;; also need java (open JDK 11 development kit)
;; 
;; ltex-ls DOES NOT SUPPORT external files...
;; https://github.com/valentjn/ltex-ls/issues/157#:~:text=External%20files%20are%20not%20supported%20by%20ltex%2Dls
;; just disable spelling with a .dir-locals.el
;; ((nil
;;   (eglot-workspace-configuration
;;    . ((ltex . (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"])))))))
;; 
(use-package eglot-ltex :disabled
  :unless (string-match "-[Mm]icrosoft" operating-system-release) ; only in linux
  :ensure (eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex")
  :commands start-eglot-ltex
  :init
  ;; apparently the newer version does not work 
  ;; https://github.com/valentjn/ltex-ls/issues/262
  (setq eglot-languagetool-server-path "~/.opt/ltex-ls-15.2.0/")

  (defun start-eglot-ltex ()
    (interactive)
    (require 'eglot-ltex)
    ;; (setq-local eglot-stay-out-of '(eldoc))  ; in org-mode, standard eldoc behavior (for src blocks heading)
    ;; automatic set a dir locals to org-mode
    (call-interactively #'eglot)
    ;; set custom CAPE functions
    ;; (setq-local completion-at-point-functions
    ;;             `(cape-file
    ;;               cape-dabbrev
    ;;               eglot-completion-at-point
    ;;               pcomplete-completions-at-point
    ;;               cape-ispell))
    )

  ;; example: https://www.emacswiki.org/emacs/DirectoryVariables#:~:text=non%2Ddotted%20notation.-,Without%20a%20.dir%2Dlocals.el%20file,-You%20can%20also
  (dir-locals-set-class-variables
   'ltex-config
   '((nil                               ; all modes
      . ((eglot-workspace-configuration
          . (:ltex . (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"])))))))))

;; need to install grammarly-languageserver
(use-package eglot-grammarly
  :ensure (:host github :repo "emacs-grammarly/eglot-grammarly")
  :commands start-eglot-grammarly 
  :init
  (defun start-eglot-grammarly ()
    (interactive)
    (require 'eglot-grammarly)
    (if (derived-mode-p 'org-mode)
        (progn
          (setq-local eglot-stay-out-of '(eldoc))  ; in org-mode, standard eldoc behavior (for src blocks heading)
          (setq-local eldoc-documentation-functions '(org-eldoc-documentation-function
                                                      flymake-eldoc-function
                                                      eglot-signature-eldoc-function
                                                      eglot-hover-eldoc-function))))
    (call-interactively #'eglot)))

;; Use language tool with flymake
;; download latest version https://languagetool.org/download/
;; wget https://languagetool.org/download/LanguageTool-stable.zip -P ~/Downloads
;; unzip <download> -d ~/.opt/
(use-package flymake-languagetool :disabled ; not working
  :commands flymake-languagetool-start
  ;; better to do manually, sometimes slow to start
  ;; :hook (text-mode . flymake-languagetool-maybe-load)
  :init
  (defun flymake-languagetool-start ()
    (interactive)
    "Add languagetool as a diagnostic function and enable flymake-mode"
    (flymake-languagetool-load)
    (flymake-mode))
  :config
  ;; (setq flymake-languagetool-server-jar nil)
  ;; (setq flymake-languagetool-url "https://api.languagetool.org")
  (setq flymake-languagetool-server-jar "~/.opt/LanguageTool-6.4/languagetool-server.jar"))

(use-package languagetool :disabled
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8") ; accept file in UTF-8
        languagetool-console-command "~/.opt/LanguageTool-6.4/languagetool-commandline.jar"
        languagetool-server-command "~/.opt/LanguageTool-6.4/languagetool-server.jar"))

(use-package svg-lib :disabled
  :ensure (svg-lib :type git :host github :repo "rougier/svg-lib"))

;; Icons for completion in region.
(use-package kind-icon :disabled
  :ensure (kind-icon :type git :host github :repo "jdtsmith/kind-icon")
  :after corfu 
  :custom
  (kind-icon-default-face 'corfu-default)
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package image-mode
  :ensure nil
  :config
  (setq image-animate-loop t))

;; Shows which function in the mode line.
(use-package which-func :disabled ; using breadcrumb
  :ensure nil
  :hook
  (prog-mode . which-function-mode))

;; Work git servers (forges)
;; uses 'Ghub' to access github/gitlab
;; need to set 'git config --global github.user <>'
;; resolved issues are dimmed, to view them one need 'forge-visit-topic' or 'forge-visit-issue' with prefix-argument
(use-package forge
  :demand
  :after magit
  :init
  ;; use evil collection keybindings
  (setq forge-add-default-bindings nil))

;; Mass copy-paste or copy-move (analogous to cut-paste) for dired.
(use-package dired-ranger
  :after dired
  :bind
  (:map dired-mode-map
        ("C-C C-c" . dired-ranger-copy)
        ("C-c C-v" . dired-ranger-paste)
        ("C-c C-x" . dired-ranger-move)))

;; Open with external program.
(use-package openwith :disabled         ; too many problems
  :defer 1 
  :config
  (openwith-mode)
  (setq openwith-associations '(("\\.pdf\\'" "okular" (file))
                                ("\\.xopp\\'" "xournalpp" (file))
                                ("\\.svg\\'" "feh --auto-reload" (file))
                                ("\\.png\\'" "feh --auto-reload" (file))))
  ;; Problem with gnus send email with attachment pdf (it opens the pdf)
  ;; https://github.com/djcb/mu/issues/510
  (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)

  ;; problem with org-mode inline images, argh!
  ;; https://emacs.stackexchange.com/a/3181
  (defadvice org-display-inline-images
      (around handle-openwith
              (&optional include-linked refresh beg end) activate compile)
    (if openwith-mode
        (progn
          (openwith-mode -1)
          ad-do-it
          (openwith-mode 1))
      ad-do-it)))

(use-package org-file-apps
  :ensure nil
  :after org
  :init
  (setq org-file-apps
        '(("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "okular %s & disown")
          ("\\.pdf:::\\([0-9]+\\)?\\'" . "okular %s -p %1 & disown")
          ;; if file has ::<page> opens at this page
          ("\\.pdf::\\([0-9]+\\)?\\'" . "xournalpp %s -n %1 & disown")
          (auto-mode . emacs)
          (directory . emacs))))

;; Specific for terminal emacs to open images
(use-package org-file-apps-terminal
  :ensure nil
  :if (not (display-graphic-p))
  :after org
  :init
  (add-to-list 'org-file-apps '("\\.svg\\'" . "swayimg %s & disown"))
  ;; (add-to-list 'org-file-apps '("\\.svg\\'" . "feh -B white --auto-reload --auto-zoom %s & disown"))
  (add-to-list 'org-file-apps '("\\.png\\'" . "feh -B white --auto-reload --auto-zoom %s & disown")))

;; convert pdf to svg to display inline org image
;; requires pdf-tools
;; better to just use svg and use latex svg package
(use-package org-inline-pdf :disabled   ; just using svg
  :when (eq system-type 'gnu/linux)
  :hook (org-mode . org-inline-pdf-mode))

;; insert org link to specified page in pdf
;; uses regular org functions `org-insert-link-global'
(use-package org-pdftools :disabled t                             ;org file apps with regex is enough!
  :hook (org-mode . org-pdftools-setup-link))

(use-package doom-themes :disabled
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package eldoc
  :ensure nil
  :hook (org-mode . eldoc-mode)
  :config
  ;; never resize echo area display, use always 1 truncated line
  ;; use `eldoc-doc-buffer' for multiple lines (with popper is good)
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-idle-delay 0.5))

(use-package eldoc-buffer-window-hack
  :ensure nil
  :after eldoc
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc"
                 (display-buffer-below-selected)
                 (display-buffer-at-bottom)
                 (window-height . 0.25))))

(use-package inferior-python-buffer-window-hack :disabled
  :ensure nil
  :after python
  :init
  (add-to-list 'display-buffer-alist
               '((major-mode . inferior-python-mode)
                 (display-buffer-below-selected)
                 (display-buffer-at-bottom)
                 (window-height . 0.25))))

;; save windows configurations and use regular bookmarks file
(use-package burly :disabled
  :ensure (burly :type git :host github :repo "alphapapa/burly.el")
  :general
  ('normal "<f6>" 'burly-bookmark-windows)
  :config
  ;; integrate with tab-bar-mode, open windows in a new tab
  (burly-tabs-mode))

;; Instead of burly
;; restore windows as LAST-SEEN state
(use-package activities
  :ensure (activities :type git :host github :repo "alphapapa/activities.el")
  :after consult                        ; access with 'consult-buffer' and 'b <SPC>' for bookmarks
  :init
  (activities-mode)
  (activities-tabs-mode))

;; view large files
(use-package vlf :disabled
  :defer 1
  :config
  ;; to have 'vlf' as an option
  (require 'vlf-setup))

;; requires dtach `yay dtach'
;; run shell commands detached from emacs
(use-package detached :disabled
  :ensure (detached :type git :host nil :repo "https://git.sr.ht/~niklaseklund/detached.el")
  :general
  ([remap async-shell-command] 'detached-shell-command)
  :custom ((detached-show-output-on-attach t))
  :init
  (detached-init))

;; helps with windows popups
(use-package popper
  :hook (after-init . popper-mode)
  :bind
  (("C-`" . popper-toggle)
   :map popper-mode-map
   ("C-c C-`" . popper-toggle))
  :config
  ;; treat those as popups
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" 
          "output\\*$"           ;for preview latex error
          ".log$"              ;for dtache log
          ("^\\*Detached Shell.*" . hide)
          help-mode
          "Command\\*$"                   ;for shell command
          ("^\\*Async.*" . hide)                   ; async commands
          "\\*xref\\*"
          "^\\*gt-result\\*"               ; gt translate
          "^\\*py.*"
          "CAPTURE-.*"
          "^\\*Dicti.*"
          "\\*Pueue Log\\*"
          "^\\*eldoc\\*"
          ("\\*BBDB\\*" . hide)         ; when the database add an etry
          "\\*compilation\\*"
          compilation-mode))

  ;; only show the pop up and don't focus on its window
  (setq popper-display-function #'popper-display-popup-at-bottom
        popper-display-control 'user    ; control only explicitly marked popups
        ;; 1/3 of the screen height
        popper-window-height 0.33)

  (popper-echo-mode +1))

(use-package rainbow-delimiters
  :hook (smartparens-mode . rainbow-delimiters-mode))

;; eye candy for org
(use-package org-modern
  :when (display-graphic-p)             ;only when gui, there is a problem with tty
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star nil)
  ;; don't add fringe, does not play nicely with org indent
  (org-modern-block-fringe nil)
  :config
  ;; don't prettify table
  (setq org-modern-table nil))

;; query for org
(use-package org-ql :disabled
  :after org
  :ensure (org-ql :host github :repo "alphapapa/org-ql"
                    :files (:defaults (:exclude "helm-org-ql.el"))))

;; allows inline animations in org
(use-package org-inline-anim
  :after org
  :commands org-inline-anim-mode)

;; eldoc in childframe
;; sometimes it gets in the way
(use-package eldoc-box
  :if (display-graphic-p)
  :hook
  (prog-mode . eldoc-box-hover-mode)
  :config
  (setq eldoc-box-only-multi-line t
        eldoc-box-lighter t))

;; async support for dired
(use-package dired-async
  :ensure nil
  :hook (dired-mode . dired-async-mode))

;; function to run local command on remote file
;; https://emacs.stackexchange.com/questions/42252/run-local-command-on-remote-file-with-tramp
(use-package dired-local-command-on-remote :disabled
  :ensure nil
  :after dired
  :general
  ('normal dired-mode-map "\"" 'dired-do-local-command)
  :init
  (defun dired-do-local-command ()
    (interactive)
    (let* ((marked-files (dired-get-marked-files nil current-prefix-arg))
           (local-tmp-files (mapcar #'file-local-copy marked-files))
           (num-files (length local-tmp-files))
           (default-directory temporary-file-directory)
           (command (dired-read-shell-command "! on %s: " num-files marked-files)))
      (dired-do-shell-command command num-files local-tmp-files))))

;; Anther package to find synonyms
(use-package powerthesaurus 
  :bind
  ("C-c d s" . powerthesaurus-lookup-dwim))

;; org-mode toc heading
(use-package org-make-toc :disabled
  :after org
  :commands org-make-toc-insert org-make-toc)

;; more color in dired
(use-package diredfl :disabled
  :hook (dired-mode . diredfl-mode))

;; show org-babel error or warning when execute block
;; just using prologue command is sufficient
(use-package org-babel-eval-verbose :disabled
  :ensure nil
  :after org
  :init
  (defvar org-babel-eval-verbose nil
    "A non-nil value makes `org-babel-eval' display")

  (defun org-babel-eval (command query)
    "Run COMMAND on QUERY.
Writes QUERY into a temp-buffer that is processed with
`org-babel--shell-command-on-region'.  If COMMAND succeeds then return
its results, otherwise display STDERR with
`org-babel-eval-error-notify'."
    (let ((error-buffer (get-buffer-create " *Org-Babel Error*")) exit-code)
      (with-current-buffer error-buffer (erase-buffer))
      (with-temp-buffer
        (insert query)
        (setq exit-code
              (org-babel--shell-command-on-region
               command error-buffer))

        (if (or (not (numberp exit-code)) (> exit-code 0)
                (and org-babel-eval-verbose (> (buffer-size error-buffer) 0)))
            (progn
              (with-current-buffer error-buffer
                (org-babel-eval-error-notify exit-code (buffer-string)))
              (save-excursion
                (when (get-buffer org-babel-error-buffer-name)
                  (with-current-buffer org-babel-error-buffer-name
                    (unless (derived-mode-p 'compilation-mode)
                      (compilation-mode))
                    ;; Compilation-mode enforces read-only, but Babel expects the buffer modifiable.
                    (setq buffer-read-only nil))))
              nil)
          (buffer-string))))))

;; list 'imenu' entries in a buffer
;; better faces than 'consult-imenu'
;; 'imenu' gives the namespace, functions, classes and methods in a tree
(use-package imenu-list :disabled
  :bind
  ("C-c C-o" . imenu-list)
  :config
  (setq imenu-list-auto-resize t
        imenu-list-auto-update nil      ; I want to keep the list from a file
        imenu-list-position 'left
        imenu-list-size 0.1
        imenu-list-focus-after-activation t))

;; deal with ANSI escape sequences for coloring
(use-package ansi-color
  :ensure nil
  :init
  ;; for compile mode
  ;; https://www.reddit.com/r/emacs/comments/kbwkca/compile_buffer_show_weird_symbols/
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  ;; for org babel
  ;; https://emacs.stackexchange.com/questions/44664/apply-ansi-color-escape-sequences-for-org-babel-results
  (defun ek/babel-ansi ()
    (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
      (save-excursion
        (goto-char beg)
        (when (looking-at org-babel-result-regexp)
          (let ((end (org-babel-result-end))
                (ansi-color-context-region nil))
            (ansi-color-apply-on-region beg end))))))
  (add-hook 'org-babel-after-execute-hook 'ek/babel-ansi))

;; custom detangle function
(use-package org-babel-detangle :disabled
  :ensure nil
  :after org
  :commands org-babel-detangle-bg
  :general
  ("C-c C-t C-t" 'org-babel-detangle-bg)
  :init
  (defun org-babel-detangle-bg ()
    "Use `org-babel-detangle' but maintain focus on source code"
    (interactive)
    (org-babel-detangle)
    (find-file buffer-file-name)))

(use-package org-footnote
  :ensure nil
  :after org
  :init
  ;; put footnotes at the current section
  (setq org-footnote-section nil))

;; Emcas 'gdb' interface
;; 'gdb-mi.el' is the file where the variables are defined so statements in
;; ':config' would be evaluated after this file is loaded.
(use-package gdb-mi
  :ensure nil
  :bind
  (:map gud-global-map ("C-a" . gud-run))
  :config
  (setq gdb-locals-value-limit 1000
        ;; use gdb layout and just the info locals
        ;; 'gdb-display-locals-buffer' shows local variables
        gdb-many-windows nil
        ;; show main function file, good to follow execution of code.
        gdb-show-main t
        ;; don't pop up 'io' buffer, if I want it, I open it
        gdb-display-io-nopopup t)

  ;; https://www.emacswiki.org/emacs/GDB-MI
  ;; 'gdb-mi' sets all windows as "dedicated", if we try to switch to one of its
  ;; buffers, if will appear in a different window. Instead of the selected
  ;; window where the command was called.
  ;; 
  ;; Force gdb-mi to not dedicate any windows
  (advice-add 'gdb-display-buffer
              :around (lambda (orig-fun &rest r)
                        (let ((window (apply orig-fun r)))
                          (set-window-dedicated-p window nil)
                          window)))

  (advice-add 'gdb-set-window-buffer
              :around (lambda (orig-fun name &optional ignore-dedicated window)
                        (funcall orig-fun name ignore-dedicated window)
                        (set-window-dedicated-p window nil))))

;; Unified interface for 'secrets' backends
(use-package auth-source
  :ensure nil
  :commands auth-source-search
  :init
  (setq auth-sources '("~/Sync/secrets/.authinfo.gpg")))

;; hide everything except current heading
;; https://stackoverflow.com/a/28031539/20449842
(use-package org-show-current-tidyly-hack
  :ensure nil
  :after org
  :bind
  (:map org-mode-map
        ("C-x =" . org-show-current-heading-tidily))
  :init
  (defun org-show-current-heading-tidily ()
    (interactive)
    "Show next entry, keeping other entries closed."
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (show-children))
      (outline-back-to-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (let ((current-prefix-arg '(4))) (call-interactively 'org-reveal))
      (org-show-entry)
      (show-children)))
  (defun org-fold-hide-block-all-nonvisible ()
    (interactive)
    (org-fold-show-all)
    (org-fold-hide-block-all)
    (org-show-current-heading-tidily)))


(use-package elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  (text-mode . electric-pair-mode)
  :config
  (electric-pair-mode)
  (setq electric-pair-inhibit-predicate 'ignore)
  (setq electric-pair-skip-self t))

(use-package markdown-mode
  :hook
  (markdown-mode . variable-pitch-mode) ; use variable pitch fonts
  (markdown-mode . visual-line-mode))

;; for reading email lists
;; summary marks: [[info:gnus#Read Articles][gnus#Read Articles]]
;; Useful commands:
;; 'gnus-summary-refer-thread' to recover thread, with prefix-argumetn looks the whole server.
;; 'gnus-summary-very-wide-reply' reply to author and CC participants in the thread, including the mailing list.
;; 'gnus-summary-kill-thread' "collapse" a thread by marking it as-read. With prefix-argument do the opposite.
;; 'gnus-group-catchup-current' mark all messages in a group as-read.
;; 'gnus-summary-mark-as-processable' and then 'gnus-summary-universal-argument' with 'gnus-summary-mark-as-read-forward' to mark processable as read.
(use-package gnus
  :ensure nil
  :bind (("C-x C-m" . gnus))
  :hook
  (gnus-mode . turn-on-gnus-dired-mode )
  (gnus-article-mode . variable-pitch-mode)
  (gnus-article-mode . visual-line-mode)
  :config
  (setq user-mail-address "nasser.alkmim@gmail.com"
        ;; 'select method' means the 'backend': how gnus stores the messages 
        ;; IMAP backend also fetches the mail (that's why it takes a while)
        gnus-select-method '(nnnil)
        gnus-secondary-select-methods '((nntp "news"
                                              ;; news server with mailing lists gatewayed to the NNTP server [2].
                                              ;; one can read it, but post/follow is not straightforward.
                                              ;; substituted old '.org' [0].
                                              ;; there is no search engine [1].
                                              ;; 
                                              ;; [0] https://lars.ingebrigtsen.no/2020/01/15/news-gmane-org-is-now-news-gmane-io/
                                              ;; [1] https://stackoverflow.com/a/48214757
                                              ;; [2] [[info:gnus#Mail and Post][gnus#Mail and Post]]
                                              (nntp-address "news.gmane.io"))
                                        (nnimap "personal"
                                                (nnimap-address "imap.gmail.com"))
                                        (nnimap "work"
                                                (nnimap-address "exchange.uibk.ac.at")))
        ;; (info "(message)Mail Variables")
        ;; use an SMTP server to send email, setup with group properties
        message-send-mail-function 'smtpmail-send-it
        send-mail-function 'smtpmail-send-it
        ;; This is just aesthetic
        ;; (info "(gnus)Summary Buffer Lines")
        gnus-summary-line-format (concat
                                  "%U"  ; read status 'O' read in previous, 'R' read now 'r' (info "(gnus) Read Articles")
                                  "%R"  ; reply status
                                  "%O"   ; download if '-' is not downloaded, to download use 'gnus-agent-toggle-mark', downloaded have '+'
                                  ;; "%z "  ; score ; don't need it, higher score is bold
                                  "%-10,10&user-date; %*"  ; date
                                  "%B"     ; thread tree string 
                                  "%(%[%-20,20a%]%) " ; name
                                  "%I%s\n") 
        gnus-article-sort-functions '((not gnus-article-sort-by-number)  ; newer on top...
                                      (not gnus-article-sort-by-date)
                                      gnus-article-sort-by-score)
        gnus-thread-sort-functions  '((not gnus-thread-sort-by-number))
        gnus-use-full-window nil       ; don't use entire window!
        ;; Change location of newsrc file.
        ;; this file has information about the groups that I subscribe and the articles that I
        ;; have read.
        ;; use 'gnus-summary-increase-score' to increase score based on string
        gnus-home-directory "~/Sync/news/" ; easier to sync different machines with git
        gnus-home-score-file '(("Science" "~/Sync/news/papers.SCORE")
                               ("Wiley" "~/Sync/news/papers.SCORE")
                               ("arXiv" "~/Sync/news/papers.SCORE")
                               ("Springer" "~/Sync/news/papers.SCORE")
                               ("rss" "~/Sync/news/rssnews.SCORE"))
        gnus-kill-files-directory "~/Sync/news/" ; to store the score
        ;; Attempts to make it faster
        gnus-fetch-old-headers nil       ; build from already read mail, nil is faster, use '^' to get parent
        gnus-check-new-newsgroups nil  ; make start up faster, need to manually 'gnus-find-new-newsgroup' to look for others
        ;; When [[gnus:nntp+news:gmane.emacs.gnus.general#56aag99k3g.fsf@news.eternal-september.org][Email from Richard Riley: gnus-unplugged and non agent groups]]
        ;; only check this level and lower or lower on startup
        gnus-activate-level 1
        gnus-show-threads nil            ; if nil can make faster, threads again with T t
        gnus-use-cross-reference nil
        gnus-always-read-dribble-file t  ; don't ask, just use auto saved data 
        ;; (info "(gnus)Startup Files")
        gnus-read-active-file nil        ; only read '.newsrc', speeds up
        gnus-save-newsrc-file nil        ; I will not use anything other than gnus
        gnus-read-newsrc-file nil        ; speed up start
        ;; Maybe improve https://gluer.org/blog/2023/trying-gnus-as-an-email-client/
        gnus-asynchronous t
        ;; (info "(gnus) Article Caching")
        ;; default: "ticked" (indicated with "!") articles go to local cache, they are indicated with "*"
        gnus-use-cache 'passive
        gnus-use-header-prefetch t
        gnus-cache-directory "~/Sync/news/cache"
        gnus-agent-directory "~/Sync/news/agent" ; where messages will be stored
        nndraft-directory "~/Sync/news/drafts" 
        ;; search with generalized query syntax:
        ;; from:fulano body:"multi word" attachment:pdf
        gnus-search-use-parsed-queries t
        ;; search all groups in a server when constructing thread
        ;; Sent mails are in another group, this allows to find my sent messages when creating threads.
        ;; And, no need to GCC my sent messages to the inbox.
        gnus-refer-thread-use-search t
        ;; show images in gnus, except adds
        gnus-blocked-images "ads"
        nnrss-directory "~/Sync/news/rss"
        nnrss-use-local t
        ;; avoid duplicate messages/articles, specially in rss fields
        gnus-suppress-duplicates t
        ;; don't show messages with same ID
        gnus-summary-ignore-duplicates t)

  ;; a: name of the day of the weak abreviated
  ;; k: hour (blank-padded)
  ;; M: minutes
  ;; e: day of the month (blank-padded)
  ;; b: month name
  (setq gnus-user-date-format-alist '(((gnus-seconds-today) . "%a %k:%M")
                                      (604800 . "%a %k:%M")
                                      ((gnus-seconds-month) . "%e %b")
                                      (t . "%b %Y")))

  ;; Activate groups on idle, and not so important stuff get news manually with
  ;; 'gnus-topic-get-new-news-this-topic'
  ;; https://old.reddit.com/r/emacs/comments/18cbeel/anyone_using_gnus_in_2023/kcceopw/
  (defun my-gnus-group-activate-on-idle ()
    (run-with-idle-timer 3 nil (lambda () (gnus-activate-all-groups 2))))
  (add-hook 'gnus-group-mode-hook #'my-gnus-group-activate-on-idle))

(use-package nnrss
  :ensure nil
  :after gnus
  :config
  (add-to-list 'nnrss-ignore-article-fields 'pubDate)
  
  ;; Prefer 'text/plain' in general
  ;; Set the default value of ‘mm-discouraged-alternatives’.
  (with-eval-after-load "gnus-sum"
    (add-to-list 'gnus-newsgroup-variables
                 '(mm-discouraged-alternatives . '("text/html" "image/.*")))
    (add-to-list 'gnus-newsgroup-variables
       '(mm-automatic-display . (remove "text/html" mm-automatic-display))))

  ;; Display ‘text/html’ parts in ‘nnrss’ groups.
  ;; nnrss generates *always* text/plain and text/html [[info:gnus#RSS]]
  (add-to-list
   'gnus-parameters
   '("\\`nnrss:"
     (mm-discouraged-alternatives "text/plain")
     (mm-automatic-display (add-to-list 'mm-automatic-display "text/html")))))

;; Set parameter for each group
;; use email (and smpt server) according to group
(use-package gnus-group-parameters
  :ensure nil
  :after gnus
  :init
  ;; (info "(gnus) Group Parameters")
  ;; Modify group parameters, most affect when sending messages (emails)
  ;; one can check, e.g., GCC, X-Message-SMTP-Method, signature, etc
  (setq gnus-parameters '((".*"          ; all groups, including personal gmail
                           ;; https://www.bounga.org/tips/2020/05/03/multiple-smtp-accounts-in-gnus-without-external-tools/
                           ;; https://www.gnu.org/software/emacs/manual/html_node/message/Mail-Variables.html
                           (posting-style
                            (address "Nasser Alkmim <nasser.alkmim@gmail.com>")
                            (signature "Nasser Alkmim")
                            ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 nasser.alkmim@gmail.com")))
                          ("work"
                           ;; Messages (emails) are GCC to the this group so it collects my sent mails on the server
                           ;; Gmail does not need this, for some reason.
                           (gcc-self "nnimap+work:Sent Items")
                           (posting-style
                            (address "Nasser Alkmim <nasser.alkmim@uibk.ac.at>")
                            (signature-file "/home/nasser/Sync/documents/signature")
                            ("X-Message-SMTP-Method" "smtp smtp.uibk.ac.at 587 c8441205")))
                          ("\\`nnrss:"
                           ;; just for rss groups
                           (mm-discouraged-alternatives nil))))

  ;; So my own messages are not considered new
  (setq gnus-gcc-mark-as-read t))

;; Enable topic mode to make it more organized
(use-package gnus-topic
  :ensure nil
  :after gnus
  :demand
  :hook
  (gnus-group-mode . gnus-topic-mode))

(use-package gnus-open-external-browser-hack
  :ensure nil
  :after gnus
  :bind
  (:map gnus-summary-mode-map
        ("K H" . gnus-article-browse-html-article-external)
        ("K H" . gnus-article-browse-html-article-external))
  :init
  (defun gnus-article-browse-html-article-external (&optional arg)
    "Open on external browser"
    (interactive)
    (let ((browse-url-browser-function 'browse-url-default-browser))
      (gnus-article-browse-html-article arg))))

;; Contact manager package
;; 'ebdb-migrate-from-bbdb' converts from bbdb
;; 'ebdb-mua-update-records' can be used to add contacts
(use-package ebdb
  :after (:any gnus message)
  :init
  ;; load code for GNUs for reading and message for sending 
  (require 'ebdb-gnus)
  (require 'ebdb-message)
  ;; use complete at point interface to select email from contacts
  (setq ebdb-complete-mail 'capf
        ebdb-mua-pop-up nil             ; don't show any pop ups
        ;; save on exit
        ebdb-save-on-exit t
        ebdb-sources "~/Sync/secrets/ebdb")

  ;; when reading or sending with the "reader" in GNUS create contact if it does not exist
  ;; ;; (info "(ebdb) Auto-Updating Records")
  (setq ebdb-mua-auto-update-p
        (lambda ()
          (unless (ebdb-mua-message-header "Newsgroups")
            ;; only create when not in a newsgroup, there are too many people in newsgroups
            'create))))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook
  (prog-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  :config
  ;; only active window
  (setq hl-line-sticky-flag nil))

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("DEL" . isearch-edit-string)
        ("C-n" . isearch-repeat-forward)
        ("C-p" . isearch-repeat-backward)
        ("C-s" . isearch-toggle-invisible))
  :config
  ;; show the matching count
  (setq isearch-lazy-count t
        ;; make a whitespace work as regex ".*" which represents "anyting in between"
        ;; behavior similar to orderless
        search-whitespace-regexp ".*"))

;; highlight code in eww
(use-package shr-tag-pre-highlight :disabled
  :after shr
  :demand
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

;; https://panadestein.github.io/emacsd/#org602eb51
;; add indentation, background color https://github.com/chenyanming/shrface#hacking-the-shr-tag-pre-highlightel
(use-package shr-tag-pre-highlight-hack :disabled
  :ensure nil
  :after shrface
  :init
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       (propertize "#+END_SRC" 'face 'org-block-end-line ))
      (shr-ensure-newline)
      (setq end (point))
      (add-face-text-property start end '(:background "#1f2329" :extend t))
      (shr-ensure-newline)
      (insert "\n")))
  (add-to-list 'shr-external-rendering-functions
               '(pre . shrface-shr-tag-pre-highlight)))

;; extend eww/shr with org features
(use-package shrface :disabled
  :general
  ('normal shrface-mode-map "<tab>" (general-predicate-dispatch nil
                                            (outline-on-heading-p) 'outline-cycle))
  :hook
  (eww-after-render . shrface-mode)
  :config
  (shrface-basic)
  (shrface-trial)
  (setq
   ;; versatile is messing with links (adding a '_' on a new line)
   shrface-href-versatile nil        ; different face for different kinds of links (http, file, ...)
   shrface-toggle-bullets t        ; disable bullets (annoyingly breaking link line)
   shrface-bullets-bullet-list '("\*")))

(use-package speedbar 
  :ensure nil)

;; translation package
(use-package go-translate
  :bind
  (("C-c t t" . gt-do-translate)     ; overrides the tutorial, but ok...
   ("C-c t d" . gt-do-setup)
   ("C-c t c" . my-gt-cycle-translation))
  ;; (override "C-c t i" 'gt-do-translate-and-insert)
  ;; ;; only when there is a gt-result buffer 
  ;; ('(normal visual) override "C-t" (general-predicate-dispatch nil
  ;;                                    (when (get-buffer "*gt-result*") t) 'my-gt-cycle-translation))
  ;; ('(normal visual) "SPC t" (general-simulate-key "S-V C-c t t")) ; whole line
  :hook
  (gt-buffer-render-init . visual-line-mode)
  :init
  (defun my-gt-cycle-translation ()
    "Got to gt-result buffer and cycle translation."
    (interactive)
    (save-excursion
      (with-current-buffer "*gt-result*"
        (gt-buffer-render--cycle-next))))
  ;; :hook
  ;; Add 'visual-line-mode' to the translation buffer
  ;; (gt-after-buffer-prepared . (lambda () (visual-line-mode 1)))
  :config
  (setq gt-langs '(en de pt it)
        gt-default-translator (gt-translator
                               :engines (list (gt-deepl-engine)
                                              (gt-google-engine))
                               :render (gt-buffer-render)))
  (setq gt-chatgpt-key (funcall (plist-get (nth 0 (auth-source-search :host "api.openai.com")) :secret))
        gt-chatgpt-model "gpt-3.5")

  ;; function to translate and insert translation
  (defun gt-do-translate-and-insert ()
    (interactive)
    (interactive)
    (let ((translator (gt-translator
                       :engines (list (gt-deepl-engine))
                       :render (gt-insert-render :type 'after))))
      (gt-start translator)))

  (setq gt-preset-translators
        `((ts-pt-de . ,(gt-translator
                        :taker (gt-taker :langs '(pt de))
                        :engines (list (gt-deepl-engine)
                                       (gt-google-engine))
                        :render (gt-buffer-render)))
          (ts-pt-it . ,(gt-translator
                        :taker (gt-taker :langs '(pt it))
                        :engines (list (gt-deepl-engine)
                                       (gt-google-engine))
                        :render (gt-buffer-render)))
          (ts-en-de . ,(gt-translator
                        :taker (gt-taker :langs '(en de))
                        :engines (list (gt-deepl-engine)
                                       (gt-google-engine))
                        :render (gt-buffer-render))))))

(use-package go-translate-window-placement-hack
  :ensure nil
  :after go-translate
  :init
  (setf (alist-get "^\\*gt-result\\*$"
                 display-buffer-alist
                 nil nil #'string=)
        ;; reuse window, otherwise open bellow
      '((display-buffer-reuse-window
        display-buffer-below-selected))))

;; custom function to connect to vpn
(use-package connect-vpn
  :ensure nil
  :commands connect-uibk
  :init
  (defun connect-vpn ()
    "Connect via VPN."
    (let*
        ((psw (funcall (plist-get (nth 0 (auth-source-search :user "c8441205" :port "sudo")) :secret)))
         (command (format "echo %s | openconnect vpn.uibk.ac.at -u c8441205 -b --passwd-on-stdin" psw))
         (default-directory "/sudo::"))
      (shell-command command)))

  (defun insert-credentials-firewall-uibk ()
    "Insert firewall access credential."
    (interactive)
    (unwind-protect
       (progn
         (shr-next-link)
         (insert "c8441205")
         (shr-next-link)
         (insert (funcall (plist-get (nth 0 (auth-source-search :user "c8441205" :port "sudo")) :secret)))
         (shr-next-link)
         (eww-submit))
     ;; remove the hook after finish the inputs
     (remove-hook 'eww-after-render-hook 'insert-credentials-firewall-uibk)))

  (defun get-access-firewall-uibk ()
    (interactive)
    "Insert credentials after page renders."
    (add-hook 'eww-after-render-hook 'insert-credentials-firewall-uibk)
    (eww "https://fwauth-tech.uibk.ac.at/"))

  (defun connect-uibk ()
    "Connect through VPN and get firewall authorization."
    (interactive)
    (connect-vpn)
    (get-access-firewall-uibk))

  (defun disconnect-uibk ()
    "Disconnect VPN."
    (interactive)
    (let* ((psw (funcall (plist-get (nth 0 (auth-source-search :user "c8441205" :port "sudo")) :secret)))
           (command "killall openconnect")
           (default-directory "/sudo::"))
      (shell-command command))))

;; Shortcut to open notes directory
(use-package notes
  :ensure nil
  :bind
  ("<f8>" . (lambda ()
            (interactive)
            (find-file "/home/nasser/Sync/notes"))))

;; Sync between google calendar and org mode.
;; It is a bit tricky to set up
;; create a project here https://console.developers.google.com/project
;; setup consent in the API & Services -> Credentials, then get the id/secrect
;; in API & Services -> Library: enable Calendar API
(use-package org-gcal
  :after org
  :demand
  :init 
  (let ((id (plist-get (nth 0 (auth-source-search :host "gcal")) :user))
        (secret (funcall (plist-get (nth 0 (auth-source-search :host "gcal")) :secret))))
    (setq org-gcal-client-id id
          org-gcal-client-secret secret
          org-gcal-fetch-file-alist '(("nasser.alkmim@gmail.com" .  "~/Sync/notes/log-notes/gcal.org"))))
  ;; Uses asymmetric encryption with gnuPG
  ;; Need to setup a key and maybe edit '~/.gnupg/gnu-agent.conf' with 'pinentry-program /usr/bin/pinetry' (but maybe this is not necesssary on linux)
  ;; stores OAuth token
  (setq-default oauth2-auto-plstore "/home/nasser/Sync/secrets/oauth2-auto.plist")
  (require 'plstore)
  ;; Add key ID
  ;; 'plstore-encrypt-to' is a list of strings (documentation is wrong)
  ;; https://github.com/kidd/org-gcal.el/issues/225
  (add-to-list 'plstore-encrypt-to "C0FDC21258188852FFC70E2C3A3B897B81E89865")
  ;; Apparently new Gnupg does not work
;; https://github.com/kidd/org-gcal.el/issues/236
  (setq epg-gpg-program "~/.opt/gnupg-2.4.0/bin/gpg")
  ;; this avoids problem with hanging in "Contacting host: oauth2.googleapis.com:443"
  (fset 'epg-wait-for-status 'ignore)

  (defun sync-gcal-idle ()
    (run-with-idle-timer 3 nil (lambda () (org-gcal-sync))))
  (add-hook 'org-agenda-finalize-hook 'sync-gcal-idle))

;; Setup template for capture gcal 
(use-package org-capture-template
  :ensure nil
  :bind
  ("C-c c" . org-capture)
  :hook
  (org-capture-mode . evil-insert-state)
  :init
  ;; Don't delete other window when calling 'org-capture'
  ;; https://stackoverflow.com/a/54251825
  (defun my-org-capture-place-template-dont-delete-windows (oldfun &rest args)
    (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
      (apply oldfun args)))
  (with-eval-after-load "org-capture"
    (advice-add 'org-capture-place-template :around 'my-org-capture-place-template-dont-delete-windows))

  ;; For capturing in agenda view 'org-agenda-capture' with default file.
  ;; This is for quick TODOS, which don't need a schedule
  ;; In Agenda, they can be viewed: 'org-todo-list'
  (setq org-default-notes-file "~/Sync/notes/log-notes/tasks.org")

  (setq org-capture-templates '(;; 'Event' is something that happens in a time, it takes a timestamp.
                                ("e" "Event" entry ; type entry creates a headline
                                 (file+datetree "~/Sync/notes/log-notes/gcal.org")
                                 "* %?\n:org-gcal:\n%^T--%^T\n%a\n:END:")
                                ;; 'Task' is a 'TODO' entry and is scheduled,
                                ;; therefore it is shown continuously until
                                ;; marked as done.
                                ;; A 'Task' can be done before the schedule date.
                                ;; The '%^{SCHEDULED}p' prompts for (1) a
                                ;; scheduled value, useless, (2) the date, with
                                ;; the calendar, which is nice, and (3) another
                                ;; value, useless as well.
                                ;; Seen here: https://emacs.stackexchange.com/a/53586
                                ;; Would be nice to just have the date with calendar prompt.
                                ;; This is a solution: https://emacs.stackexchange.com/a/72326
                                ("t" "Task" entry
                                 (file+datetree "~/Sync/notes/log-notes/gcal.org")
                                 "* TODO %?\n:org-gcal:\n%a\n:END:\n%^{SCHEDULED}p"))))

;; Link to org commits
(use-package orgit
  :after org)

(use-package orgit-forge
  :after org)

(use-package hack-org-edraw-async-export
  :ensure nil
  :load-path "./elpaca/builds/edraw/"
  :after ox
  :init
  (require 'edraw-org)
  (edraw-org-setup-exporter))

;; Drawing link support in 'org-mode'
(use-package edraw
  ;; :when (display-graphic-p)
  :ensure (edraw :type git :host github :repo "misohena/el-easydraw")
  :hook
  (org-mode . (lambda ()
                (require 'edraw-org)
                (edraw-org-setup-default)))
  :config
  (setq edraw-editor-default-grid-visible nil
        edraw-editor-default-tool 'freehand
        edraw-editor-tool-freehand-smoothing-method 'bezier-fitting
        edraw-editor-default-transparent-bg-visible nil    ; always see transparent as white
        edraw-default-document-properties '((width . 800)
                                            (height . 600)
                                            (background . "none"))
        ;; make toolbar small
        ;; f - free hand 
        ;; z - undo
        ;; s - select
        ;; dc - display crop
        ;; + - zoom 
        ;; spc - move and then q to quit
        edraw-editor-toolbar-button-h 18
        edraw-editor-toolbar-button-w 16)
  (add-to-list 'edraw-default-shape-properties '(path
                                                  (stroke . "#707070")
                                                  (stroke-width . 1)
                                                  (fill . "none"))))

(use-package cmake
  :ensure nil
  :mode ("\\CMakeLists.txt\\'" . cmake-ts-mode))

;; LLM interface for emacs
;; For Ollama, need to download and execute "ollama"
;; Also need to run a model to pull manifest "ollama run mistral"
(use-package gptel
  :ensure (gptel :type git :host github :repo "karthink/gptel")
  :bind
  ("C-c C-g" . gptel-menu)
  :config
  (setq gptel-api-key (funcall
                       (plist-get (car (auth-source-search :host "api.openai.com"))
                                  :secret)))
  ;; make Ollama the default
  (setq-default gptel-model "llama3.1"
                gptel-backend (gptel-make-ollama
                               "Ollama"                               ;Any name of your choosing
                               :host "localhost:11434"                ;Where it's running
                               :models '("llama3.1" "llama3.1:70b")            ;Installed models (ollama pull "model")
                               :stream t)))

;; Alternative to 'mail-mode' and preferred mode for 'gnus'
(use-package message
  :ensure nil
  :hook
  ;; disable autofill mode on emails
  (message-mode . (lambda () (auto-fill-mode -1))))

;; fold for '.xml' with 'hide-show'
;; https://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
(use-package xml-hs-fold
  :ensure nil
  :after (hideshow nxml-mode)
  :init
  (add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil)))

;; Improved spell checker with 'libenchant'
;; It lazily checks, only to visible (includes folding), support multiple languages
;; 
;; It looks for words in '~/.config/enchant' in specific 'en.dic' dictionary (for english).
;; One can create a symlink ('dired-do-symlink') from the main list of words file to this 'en.dic' file.
;; Remember to disable 'flyspell'.
(use-package jinx
  :ensure (jinx :host github :repo "minad/jinx"
                :files (:defaults "*.c"))
  :hook
  (prog-mode . jinx-mode)
  (text-mode . jinx-mode)
  :bind
  ("M-," . jinx-correct)
  :config
  (setq jinx-languages "en de pt_BR it"
        jinx-delay 1))

(use-package ediff
  :ensure nil
  :config
  (setq
   ;; control panel on the same buffer
   ediff-window-setup-function 'ediff-setup-windows-plain
   ;; split windws horizontally (side-by-side)
   ediff-split-window-function 'split-window-horizontally)

  ;; Use both A and B into C when solving conflicts
  ;; https://stackoverflow.com/a/29757750
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "c" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

  ;; Restore window configuration after ediff
  ;; https://emacs.stackexchange.com/a/17089
  (defvar my-ediff-last-windows nil)

  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig))

;; Built in dictionary look up
;; freedict has dictionaries for multiple languages, to install:
;; 1. download: https://freedict.org/downloads/#gnu/linux
;; 2. copy the '.dict.dz' and '.index' to '/usr/share/dictd/' folder
;; 3. add the database in '/etc/dict/dictd.conf':
;; 
;; database eng-por {
;; 	data /usr/share/dictd/eng-por.dict.dz
;; 	index /usr/share/dictd/eng-por.index
;; }
(use-package dictionary
  :ensure nil
  :bind
  ("C-c d l" . my/dictionary-lookup-definition) ; search for word at a point
  :config
  (setq
   dictionary-server nil
  ;; use just one buffer, please.
   dictionary-use-single-buffer t)

  (defun my/dictionary-lookup-definition ()
      "Unconditionally lookup the word at point or selected."
    (interactive)
    (let ((word (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (current-word))))
      (unless word
        (user-error "No word at point"))
      (dictionary-new-search (cons word dictionary-default-dictionary)))))

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :config
  ;; avoid extra flags from 'ledger'
  (setq ledger-report-auto-width nil
        ledger-report--extra-args-marker " " ; don't use 'ledger' flags in 'hledger'
        ;; avoid error with --date-format
        ;; https://github.com/simonmichael/hledger/issues/367#issuecomment-927024529
        ledger-mode-should-check-version nil
        ;; https://github.com/simonmichael/hledger/issues/367#issuecomment-753455796
        ledger-report-native-highlighting-arguments nil))

(use-package org-compat
  :ensure nil
  :after org
  :init
  (setq org-imenu-depth 5))

(use-package pueue
  :ensure (pueue :host github :repo "xFA25E/pueue")
  :hook (pueue-mode . (lambda ()
                        (setq auto-revert-interval 0)
                        (auto-revert-mode)))
  :commands pueue
  :bind 
  ("<f7>" . pueue))

;; Matchs the cursor color when running emacs in terminal
;; makes it much more visible, but it does not change the foreground when over the text as in the GUI
(use-package term-cursor-color
  :ensure (term-cursor-color :host github :repo "CyberShadow/term-cursor-color")
  :if (not (display-graphic-p))
  :init
  (term-cursor-color-mode))

(use-package combobulate :disabled
  :ensure (combobulate :url "https://github.com/mickeynp/combobulate")
  :hook
  (python-ts-mode . combobulate-mode)
  (c++-ts-mode . combobulate-mode)
  (yaml-ts-mode . combobulate-mode))

;; copy from emacs terminal
(use-package clipetty
  :if (not (display-graphic-p))
  :hook (after-init . global-clippety-mode)
  :bind
  ("M-S-y" . clipetty-kill-ring-save)
  ;; for terminal support
  ("C-M-y" . clipetty-kill-ring-save))

;; Add text-mode to files without format extension
(use-package custom-files-auto-mode
  :ensure nil
  :init
  ;; files that end with ".out"
  ;; "\'" matches end of a string
  ;; "$" matches emptry string before a newline
  ;; "\." matches a period (escaped)
  (add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode)))

(use-package tab-bar
  :ensure nil
  :custom-face 
  (tab-bar ((t (:inherit unspecified))))
  :bind
  (:map tab-prefix-map
        ("t" . tab-bar-select-tab))
  :config
  (setq tab-bar-tab-hints t))

(use-package xref
  :ensure nil
  :bind
  ("C-," . xref-go-back)
  :config
  ;; Need to set both
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function 'xref-show-definitions-completing-read))

(use-package indent
  :ensure nil
  :bind
  (:repeat-map indent-rigidly-map
              ("h" . indent-rigidly-left)
              ("H" . indent-rigidly-left-to-tab-stop)
              ("l" . indent-rigidly-right)
              ("L" . indent-rigidly-right-to-tab-stop)))

;; Show guides on indentation level
(use-package indent-bars
  :ensure (indent-bars :url "https://github.com/jdtsmith/indent-bars")
  :hook
  (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-pattern "."
        indent-bars-width-frac 0.1
        indent-bars-highlight-current-depth '(:width 0.25))
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.75)
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-unspecified-fg-color "white"
   indent-bars-unspecified-bg-color "black"))

(use-package docview
  :ensure nil)

;; Use eglot when in org-edit-special (hack, experimental)
;; https://github.com/joaotavora/eglot/issues/216
(use-package org-edit-special-with-eglot-hack
  :ensure nil
  :after org 
  :bind
  ([remap org-edit-special] . mb/org-babel-edit)
  :init
  (defun mb/org-babel-edit ()
    "Edit python src block with lsp support by tangling the block and
then setting the org-edit-special buffer-file-name to the
absolute path. Finally load eglot."
    (interactive)

    ;; org-babel-get-src-block-info returns lang, code_src, and header
    ;; params; Use nth 2 to get the params and then retrieve the :tangle
    ;; to get the filename
    (setq mb/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))

    ;; tangle the src block at point 
    (org-babel-tangle '(4))
    (org-edit-special)

    ;; Now we should be in the special edit buffer with python-mode. Set
    ;; the buffer-file-name to the tangled file so that pylsp and
    ;; plugins can see an actual file.
    (setq-local buffer-file-name mb/tangled-file-name)
    ;; disable breadcrumb
    (eglot-ensure)
    (breadcrumb-mode -1)))

(use-package standard-themes :disabled
  :defer .5
  :init
  (standard-themes-load-dark))

;; use treesit modes in org edit special
(use-package org-treesit-src-blocks-hack
  :ensure nil
  :after org
  :init
  ;; https://old.reddit.com/r/emacs/comments/15yxdz3/weekly_tips_tricks_c_thread/jy03758/
  (defun change-mode-to-ts-mode (mode)
    (pcase (assoc mode major-mode-remap-alist)
      (`(,mode . ,ts-mode) ts-mode)
      (_ mode)))
  (advice-add 'org-src-get-lang-mode :filter-return #'change-mode-to-ts-mode))

(use-package immersive-translate :disabled
  :ensure (immersive-translate :url "https://github.com/Elilif/emacs-immersive-translate.git")
  :config
  (immersive-translate-setup)
  (setq immersive-translate-backend 'trans
        immersive-translate-trans-source-language "de"
        immersive-translate-trans-target-language "en"))

;; Call 'ediff' on marked dired files on different buffers
;; https://stackoverflow.com/a/25944631
(use-package ediff-dired-marked-files-hack
  :ensure nil
  :init
  (defun mkm/ediff-marked-pair ()
   "Run ediff-files on a pair of files marked in dired buffer"
   (interactive)
   (let* ((marked-files (dired-get-marked-files nil nil))
          (other-win (get-window-with-predicate
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (and (not (eq window (selected-window)))
                               (eq major-mode 'dired-mode))))))
          (other-marked-files (and other-win
                                   (with-current-buffer (window-buffer other-win)
                                     (dired-get-marked-files nil)))))
     (cond ((= (length marked-files) 2)
            (ediff-files (nth 0 marked-files)
                         (nth 1 marked-files)))
           ((and (= (length marked-files) 1)
                 (= (length other-marked-files) 1))
            (ediff-files (nth 0 marked-files)
                         (nth 0 other-marked-files)))
           ((= (length marked-files) 1)
            (let ((single-file (nth 0 marked-files))) 
              (ediff-files single-file
                           (read-file-name
                            (format "Diff %s with: " single-file)
                            nil (m (if (string= single-file (dired-get-filename))
                                       nil
                                     (dired-get-filename))) t))))
           (t (error "mark no more than 2 files")))))
(defun mkm/edirs-marked-pair ()
   "Run edirs on a pair of files marked in dired buffer"
   (interactive)
   (let* ((marked-files (dired-get-marked-files nil nil))
          (other-win (get-window-with-predicate
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (and (not (eq window (selected-window)))
                               (eq major-mode 'dired-mode))))))
          (other-marked-files (and other-win
                                   (with-current-buffer (window-buffer other-win)
                                     (dired-get-marked-files nil)))))
     (cond ((= (length marked-files) 2)
            (edirs (nth 0 marked-files)
                         (nth 1 marked-files) nil))
           (t (error "mark no more than 2 files"))))))

(use-package dape
  :ensure (dape :type git :host github :repo "svaante/dape")
  :commands dape
  :config
  ;; Add inline variable hints, this feature is highly experimental
  (setq dape-inline-variables nil))

;; Rust-based wrapper to speed interaction with LSP servers
;; Need to build and install rust binary "emacs-lsp-booster" which should be on the path
;; https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init (eglot-booster-mode))

(use-package calfw :disabled
  ;; make sure to "build" 'calfw-org.el' as well.
  :ensure (calfw :files ("calfw.el"
                         "calfw-org.el"))
  :init
  ;; autoload 'calfw-org' when opening calendar
  (unless (fboundp 'cfw:open-org-calendar)
    (autoload #'cfw:open-org-calendar "calfw-org" nil t))
  (bind-keys :package calfw ("C-c A" . cfw:open-org-calendar)))

(use-package dslide
  :ensure (dslide :host github
                  :repo "positron-solutions/dslide")
  :bind
  ("C-<f12>" . dslide-deck-start)
  :config
  (setq dslide-animation-duration 0))

(use-package treesit-fold
  :ensure (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :bind
  (:map treesit-fold-mode-map
   ("C-c @ C-h"  . treesit-fold-close)
   ("C-c @ C-s"  . treesit-fold-open)
   ("C-c @ C-e"  . treesit-fold-toggle)
   ("C-c @ C-a"  . treesit-fold-open-all)
   ("C-c @ C-t"  . treesit-fold-close-all))
  :hook 
  (c++-ts-mode . treesit-fold-mode)
  (python-ts-mode . treesit-fold-mode)
  :config

  ;; add support for "if" statement
  ;; TODO: would be nice to folde if...else..., and not just if...
  (push '(if_statement . ((lambda (node offset)
                            (treesit-fold-range-markers node offset ":")) 0 1))
        (alist-get 'python-ts-mode treesit-fold-range-alist)))

(use-package treesit-auto
  :defer 1
  :custom
  (treesit-auto-install 'prompt)
  :demand
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package hledger-rules-mode :disabled
  :ensure nil
  :mode ("\\.rules\\'" . hledger-rules-mode)
  :config
  (defun hledger-rules-mode-setup ()
    (font-lock-add-keywords nil 
                            '(;; Comments
                              ("^\\s-*#.*" . font-lock-comment-face)
                              ;; Keywords
                              ("\\<\\(if\\|fields\\|date-format\\|currency\\|account[0-9]+\\|amount[0-9]+\\|skip\\)\\>" . font-lock-keyword-face)
                              ;; Date formats, accounts, etc.
                              ("\\<\\(%Y-%m-%d\\|\\b[a-zA-Z0-9:_-]+\\b\\)\\>" . font-lock-variable-name-face)
                              ;; Operators
                              ("\\(=~\\|==\\|!=\\|<=\\|>=\\)" . font-lock-builtin-face)
                              ;; Variables like %description, %amount
                              ("\\(%[a-zA-Z_]+\\)" . font-lock-variable-name-face)
                              ;; General words
                              ("\\b\\w+\\b" . font-lock-string-face))))

  (define-minor-mode hledger-rules-mode
    "Minor mode for editing .rules files in hledger."
    :lighter " hledger-rules"
    (hledger-rules-mode-setup)))

;; tools for display during presentations
(use-package master-of-ceremonies
  :after dslide
  :ensure (master-of-ceremonies 
           :host github
           :repo "positron-solutions/master-of-ceremonies"))

(use-package hide-mode-line :disabled)

(use-package apptainer-mode
  :ensure (apptainer-mode :type git :host github :repo "jrgant/apptainer-mode")
  :mode ("\\.def\\'" . apptainer-mode))

(use-package kkp :disabled              ; having problems with org-export not been able to export subtree on kitty
  :defer 1
  :unless (display-graphic-p)
  :ensure t
  :demand
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package rng-nxml
  :ensure nil
  :config
  (setq rng-nxml-auto-validate-flag nil))

(use-package eat
  :ensure (eat :type git
               :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el"
                                 ;; for the info manual node
                                 ;; https://github.com/progfolio/elpaca/issues/241
                                 "fdl.texi" "gpl.texi")))
  :bind
  ("<f9>" . eat))

;; Mouse support on terminal
(use-package xt-mouse
  :ensure nil
  :config
  (xterm-mouse-mode))

(use-package org-drill
  :after org
  :init
  ;; fix problem with latex fragment on tty
  (defun org-drill-present-default-answer (session reschedule-fn)
  "Present a default answer.

SESSION is the current session.
RESCHEDULE-FN is the function to reschedule."
  (prog1 (cond
          ((oref session drill-answer)
           (org-drill-with-replaced-entry-text
            (format "\nAnswer:\n\n  %s\n" (oref session drill-answer))
            (funcall reschedule-fn session)
            ))
          (t
           (org-drill-hide-subheadings-if 'org-drill-entry-p)
           (org-drill-unhide-clozed-text)
           (org-drill--show-latex-fragments)
           (ignore-errors
             (org-display-inline-images t))
           (org-cycle-hide-drawers 'all)
           (org-remove-latex-fragment-image-overlays)
           (org-drill-with-hidden-cloze-hints
            (funcall reschedule-fn session)))))))

(use-package flymake-ruff
  :ensure (flymake-ruff
           :type git
           :host github
           :repo "erickgnavar/flymake-ruff")
  :hook
  (eglot-managed-mode . (lambda ()
                          (flymake-ruff-load)
                          ;; for some reason I need to "start" 'flymake' again
                          (flymake-start))))


(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
