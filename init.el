(defvar my-start-time (current-time)
  "Time when Emacs was started")

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
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
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; 'always-defer' means that for a package to load we need a ':hook' or using a ':general' keybinding
;; if there is none, we need to explicitly add ':demand' to load the package
;; can also load with ':defer time'
(setq use-package-verbose nil		; don't print anything
      use-package-compute-statistics nil ; compute statistics about package initialization
      use-package-minimum-reported-time 0.0001
      use-package-expand-minimally t	; minimal expanded macro
      use-package-always-defer t)	; always defer, don't "require", except when :demand

;; Block until current queue processed.
(elpaca-process-queues)

;; general for keybinding
(use-package general
  :demand)

;; control minor-mode indication in the mode-line
(use-package diminish
  :demand)

;; ':general' and ':diminish' add keywords to 'use-package'
;; need to process before continue
(elpaca-process-queues)

;; minimizes GC interference with user activity
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 0.5
        gcmh-high-cons-threshold (* 16 1024 1024)) 
  (gcmh-mode 1))

;; basics and better default
(use-package emacs
  :elpaca nil
  :defer 1
  :general
  ('normal "gy" 'revert-buffer-quick)
  ('insert "C-v" 'yank)                 ; for helping in minibuffer.
  ("C-<tab>" 'next-window-any-frame)
  ("<backtab>" 'previous-window-any-frame)
  ("C-x C-M-e" 'pp-macroexpand-last-sexp)
  ("C-x C-e" 'eval-defun)
  ("C-x e" 'eval-last-sexp)
  ("C-h j" 'describe-keymap)
  ;; some sexp moving commads (treesit changes some)
  ('normal :prefix "z"
           "u" 'backward-up-list
           "b" 'backward-sexp
           "f" 'forward-sexp)
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
        jit-lock-defer-time 0
        mouse-wheel-follow-mouse 't
        ;; scroll-conservatively 0
        ;; scroll-step 0
       )

  ;; other basiscs
  (setq ring-bell-function 'ignore)
  ;; (setq inhibit-startup-screen t) ;; now I respect the startup screen.

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

  (setq-default
   completion-cycle-threshold nil    ; show all candidates
   completions-detailed t	    ; add details in completions as prefix/sufix
   idle-update-delay 1.1  ; Slow down the UI being updated to improve performance
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
  (setq-default truncate-lines t))

(use-package pixel-scroll
  :elpaca nil
  :if (string-greaterp emacs-version "29") ; need emacs > 29
  :defer 1 
  :config
  ;; text scroll pixel by pixel
  (pixel-scroll-precision-mode t))

;; My custom emacs theme
(use-package seralk-theme :disabled
  :elpaca nil
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
  :elpaca nil
  :defer 1
  :config
  (winner-mode))

;; Visit file from where you left
(use-package saveplace
  :elpaca nil
  :defer 1
  :config
  (save-place-mode))

;; typeface
(use-package custom-typefaces
  :elpaca nil
  :preface
  (setq default-monospace '("JetBrains Mono NF")) ;; "Recursive Mono Linear Static"
  (setq default-unicode '("JetBrains Mono NF"))
  (setq default-proportional '("Recursive Sans Line Static"))
  (setq default-comments '("Recursive Mono Casual Static"))
  :custom-face 
  ;; "Victor Mono" sometimes is nice for comments or "Recursive Mono Casual Static".
  ;; Monospace favorites are "JetBrains Mono NF" and "Iosevka NF", or "Recursive Mono Linear Static".
  ;; Variable pitch favorites "Iosevka Etoile", "Recursive Sans Linear Static"
  ;; 'constant'
  (default  ((t (:family ,(car default-monospace) :height 90))))
  (variable-pitch ((t (:family ,(car default-proportional)))))
  ;; comment
  (font-lock-comment-face ((t (:family ,(car default-comments) :slant italic))))
  (font-lock-constant-face ((t (:family ,(car default-monospace)))))
  ;; outline 4 inherits from comment face... make it oblique instead of italic
  (outline-4 ((t (:inherit font-lock-doc-face))))
  ;; so summary line aligned
  (gnus-summary-normal-unread  ((t (:family ,(car default-monospace)))))
  ;; when using variable pitch in org mode, use monospace for code blocks
  (org-block ((t (:family ,(car default-monospace)))))
  (org-table ((t (:family ,(car default-monospace)))))
  (org-meta-line ((t (:family ,(car default-monospace)))))
  (org-verbatim ((t (:family ,(car default-monospace)))))
  (org-code ((t (:slant italic :inherit org-verbatim :box nil))))
  (tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face))))
  :init
  (set-fontset-font  t 'unicode (car default-unicode) nil 'prepend))

;; change typeface size font
(use-package emacs-frame-zoom
  :elpaca nil
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
  :elpaca nil
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
  :elpaca nil
  :config
  ;; abbrev for speed and less strain
  (setq-default abbrev-mode t)
  (diminish 'abbrev-mode)
  (setq save-abbrevs 'silently))

(use-package color-identifiers-mode)

;; save recent visited files
(use-package recentf
  :elpaca nil
  :defer 5
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 25
        recentf-auto-cleanup 'mode))

;; Enable autorevert on specific modes
(use-package autorevert
  :elpaca nil
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
  :general
  ("C-h f" 'helpful-callable)
  ("C-h d" 'helpful-at-point)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  :init
  (defvar read-symbol-positions-list nil) ; fix bug in upstream emacs
  :config
  ;; don't create multiple buffers, just switch the current
  (defun pop-or-switch-to-buffer (buffer-name)
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-name)
      (pop-to-buffer buffer-name)))
  (setq helpful-switch-buffer-function 'pop-or-switch-to-buffer))

;; completion UI (vertical list in minibuffer)
(use-package vertico
  :elpaca (vertico :type git :host github :repo "minad/vertico"
                     :files (:defaults "extensions/*"))
  :general
  ('insert vertico-map "C-k" 'vertico-exit-input)
  ('normal vertico-map
           "C-n" 'vertico-next ; same as in insert mode
           "C-p" 'vertico-previous) ; same as in insert mode
  :init
  (vertico-mode)
  (setq vertico-resize t))

;; improves behavior when dealing with directories in the minibuffer
(use-package vertico-directory
  :elpaca nil
  :after vertico
  :general
  (vertico-map "RET" 'vertico-directory-enter
               "DEL" 'vertico-directory-delete-char
               "M-DEL" 'vertico-directory-delete-word)
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; repeat last vertico session
(use-package vertico-repeat
  :elpaca nil
  :after vertico
  :general
  ("M-r" 'vertico-repeat))

;; use vertico to complete in region with orderless in terminal
(use-package vertico-terminal :disabled
  :elpaca nil
  :unless (display-graphic-p)
  :init
  ;; Use `consult-completion-in-regionegion' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; allows different completion UI configuration
(use-package vertico-multiform
  :elpaca nil
  :after vertico
  :general
  ('insert vertico-map "C-<tab>" 'vertico-multiform-reverse)
  :init
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-outline unobtrusive)
          (consult-line unobtrusive)
          (consult-buffer unobtrusive)))

  ;; for spell checker
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 25))))

;; `completion STYLE` with flexible candidate filtering
;; filter with space-separated components and match components in any order
;; filter means how a input string is matched against candidates
(use-package orderless
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
  :elpaca nil
  ;; :general
  ;; (minibuffer-mode-map "C-n" 'minibuffer-next-completion)
  ;; (minibuffer-mode-map "C-p" 'minibuffer-previous-completion)
  :config
  ;; first TAB shows candidates
  ;; second TAB switches to the candidates buffer
  (setq completion-auto-select 'second-tab
        ;; Just one column is better.
        completions-format 'one-column
        completions-max-height 20
        completions-header-format nil))

;; save the search history
(use-package savehist
  :elpaca nil
  :defer 1
  :config
  (savehist-mode))

;; minibuffer annotations details
(use-package marginalia
  :if (eq system-type 'gnu/linux)
  :general
  (minibuffer-local-map "M-A" 'marginalia-cycle)
  :defer 1
  :config
  (marginalia-mode))

;; enhances multiple commands based on completion
;; practical navigation and search commands 
(use-package consult
  :general
  ;; m/f/b <SPC> for bookmarks/files/buffers narrowing
  ("C-x b" 'consult-buffer)		; enhanced switch to buffer
  ("M-s" 'consult-outline)		; navigation by headings
  ("C-c o" 'consult-imenu)		; navigation by "imenu" items
  ("M-y" 'consult-yank-pop)		; editing cycle through kill-ring
  ('normal "C-M-s" 'consult-line)	; search lines with preview
  ("C-c C-f" 'consult-focus-lines)	; show only matching results
  ("C-c m" 'consult-mark)
  ;; two parts: search  and filter
  ;; #<search string>#<filter terms> filtering with orderless! amazing!
  ;; command line can be specified after "--", example: #<search string> -- -C 10 for context!! WHAT!
  ("C-c r" 'consult-ripgrep)		; search file contents
  ("C-c f" 'consult-find-fd)		; search files in directories
  ;; (minibuffer-local-completion-map "<tab>" 'minibuffer-force-complete)
  ("M-e" 'consult-isearch-history)
  :hook
  ;; hook for using default completion mode
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; configure preview behavior to `any` key and specific time delay
  (consult-customize consult-buffer consult-bookmark
                     :preview-key '(:debounce 5 any))
  ;; use a key for preview from ripgrep and find
  (consult-customize consult-ripgrep consult-find-fd
                     :preview-key '("M-."))
  (consult-customize consult-line :preview-key 'any)

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
(use-package consult-dir
  :elpaca (consult-dir :type git :host github :repo "karthink/consult-dir") 
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
  :elpaca (embark :files (:defaults "embark-org.el"))
  ;; :demand                               ; load it independently of bind and hook
  :general
  ('(insert visual normal) "C-z" 'embark-act)  ; use "\" for "evil-execute-in-emacs-state"
  ('(insert motion) minibuffer-local-map "C-z" 'embark-act)  ; use "\" for "evil-execute-in-emacs-state"
  ("C-S-z" 'embark-dwim)
  ("C-h B" 'embark-bindings)
  (embark-function-map "h" 'helpful-symbol)
  (embark-variable-map "h" 'helpful-symbol)
  ('normal grep-mode-map "g r" 'embark-rerun-collect-or-export) ; back to completion after 'embark-export' to grep buffer
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
  :elpaca nil
  :after embark
  :demand)

;; utility for using icons fonts
(use-package all-the-icons :disabled
  :custom
  (all-the-icons-scale-factor 1))

(use-package nerd-icons
  :elpaca (nerd-icons :host github :repo "rainstormstudio/nerd-icons.el"
                      :files (:defaults "data"))
  :defer 1
  :demand ;require
  :custom
  ;; need to install the nerd-font
  ;; For kitty terminal need to add family to kitty config (C-S-<f2>)
  (nerd-icons-font-family "JetBrains Mono NF")
  :config
  ;; to use with corfu and kind-icon
  (setq kind-icon-use-icons nil)
  (setq kind-icon-mapping
        `(
          (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
          (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
          (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
          (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
          (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
          (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
          (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
          (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
          (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
          (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
          (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
          (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
          (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
          (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
          (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
          (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
          (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
          (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
          (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
          (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
          (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
          (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
          (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
          (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
          (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
          (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
          (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
          (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
          (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
          (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

(use-package nerd-icons-dired
  :elpaca (nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :elpaca (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  ;; need to load after marginalia 
  ;; https://github.com/rainstormstudio/nerd-icons-completion/issues/4
  :after (nerd-icons marginalia)
  :init
  (nerd-icons-completion-mode))

;; automatic insert matching pairs and navigation
;; highlight matching parens
;; for wrap/unwrap I use evil-surround
;; expand/contract (slurp) is good for elisp
(use-package smartparens
  :diminish smartparens-mode
  ;; :elpaca (:includes smartparens-config)
  :general
  ('normal smartparens-mode-map "M-l" 'sp-next-sexp)
  ('normal smartparens-mode-map "M-h" 'sp-previous-sexp)
  ('normal smartparens-mode-map "M-k" 'sp-up-sexp)
  ('normal smartparens-mode-map "M-j" 'sp-down-sexp)
  ('(normal visual) smartparens-mode-map "] ]" 'sp-forward-sexp) ; go to balancing closing pair
  ('(normal visual) smartparens-mode-map "[ [" 'sp-beginning-of-sexp) ; go back to matching opening pair
  ;; binding all modes for Latex
  ('insert '(prog-mode-map LaTeX-mode-map org-mode-map) "C-<tab>" 'sp-forward-sexp)
  :custom-face
  (sp-show-pair-match-content-face ((t (:inherit region))))
  :hook
  (prog-mode . smartparens-mode)
  (LaTeX-mode . smartparens-mode)
  (nxml-mode . smartparens-mode)
  ;; (org-mode . smartparens-mode) ; messing with org-mode
  ;; (smartparens-mode . smartparens-strict-mode) ; enforce pairs to be balanced
  (smartparens-mode . show-smartparens-mode) ; instead of default show-paren-mode
  :config
  (setq sp-show-pair-delay 0.125
        sp-max-prefix-length 25         ; reduces work
        sp-max-pair-length 4            ; reduces work
        )
  ;; show context (echo area) when closing delimiter is off screen
  (setq show-paren-context-when-offscreen 'overlay))

(use-package smartparens-config
  :elpaca nil
  :demand
  :after smartparens
  :config
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p)))

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :general
  (flymake-mode-map "M-n" 'flymake-goto-next-error) 
  (flymake-mode-map "M-N" 'flymake-goto-prev-error)
  :config
  ;; delay check, check only on save
  (setq flymake-no-changes-timeout 1
        flymake-show-diagnostics-at-end-of-line nil ; just use "M-n"
        flymake-mode-line-lighter "Fly")
  ;; avoid warning in the 'flymake' log
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; 'flymake' just for C++ in org edit special
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/Example_002d_002d_002dConfiguring-a-tool-called-directly.html
(use-package flymake :disabled                             ;not working with org edit special
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

(use-package evil-multiedit
  :after evil
  :custom-face
  ;; works in gui only
  (iedit-occurrence ((t (:box (:line-width (-1 . -1)) :inherit nil :style nil))))
  :general
  ('visual "R" 'evil-multiedit-match-all)
  ("M-d" 'evil-multiedit-match-and-next)
  ("M-C-d" 'evil-multiedit-match-and-prev)
  ('(normal visual) evil-multiedit-mode-map "M-t" 'evil-multiedit-toggle-or-restrict-region) 
  ('normal evil-multiedit-mode-map "<escape>" 'evil-multiedit-abort)
  ('visual "C-S-d" 'evil-multiedit-restore)
  ('insert evil-multiedit-mode-map "<RET>" nil) ; avoid toggling when completing with corfu
  :config
  (setq evil-multiedit-follow-matches t)
  (defun make-evil-multiedit-case-sensitive (fn &rest args)
    (let ((case-fold-search (not iedit-case-sensitive)))
      (apply fn args)))

  (advice-add #'evil-multiedit-match-and-next :around #'make-evil-multiedit-case-sensitive)

  ;; Change the face for terminal
  (when (not (display-graphic-p))
    (set-face-attribute 'iedit-occurrence nil :inherit 'isearch)))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode
  :general
  ;; autoload keymap, will trigger the loading of `evil-mc` library
  ;; use prefix for `cursors-map` from evil collection
  ('(normal visual) "g ." '(:keymap evil-mc-cursors-map))
  (evil-mc-key-map "g r" nil)
  :config
  (global-evil-mc-mode 1)
  ;; extra commands for multiple cursts
  (push '(eval-last-sexp . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(evil-org-delete . ((:default . evil-mc-execute-default-evil-delete))) evil-mc-known-commands)
  (push '(org-yank . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(evil-paste-before . ((:default . evil-mc-execute-default-evil-paste))) evil-mc-known-commands)
  (push '(evil-org-beginning-of-line . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(evil-digit-argument-or-evil-org-beginning-of-line . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(sp-forward-sexp . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(evil-surround-change . ((:default . evil-mc-execute-default-evil-surround-region))) evil-mc-known-commands))

(use-package evil
  :defer 0.5
  :diminish evil-mode
  :init
  ;; Set before loading evil or evil collection
  (setq evil-want-keybinding nil ; preference for evil-collection
        evil-want-minibuffer t); evil in minibuffer
  :general
  (evil-motion-state-map "C-i" nil)     ; avoid conflicting with tab in terminal
  ("C-c \\" 'evil-emacs-state)
  ('normal ";" 'evil-search-forward)
  ('normal "M-p" 'evil-paste-from-register)
  ('(normal visual) :prefix "SPC" "l" 'evil-last-non-blank)
  ('(normal visual) :prefix "SPC" "h" 'evil-first-non-blank)
  ('normal :prefix "SPC" "a" 'evil-append-line)
  ('(normal visual) "[ ]" 'evil-next-close-paren)
  ('(normal visual) "] [" 'evil-previous-open-paren)
  ('normal "j" 'evil-next-visual-line)
  ('normal "k" 'evil-previous-visual-line)
  ('normal "C-c r" nil)
  ('normal "C-S-o" 'evil-jump-forward)
  :config
  (evil-mode 1)
  (general-def '(normal motion) "TAB" nil)
  (setq
   lazy-highlight-cleanup nil           ; persist highlight
   lazy-highlight-max-at-a-time nil
   evil-kill-on-visual-paste nil        ; don't add replaced test onto kill ring
   lazy-highlight-initial-delay 0)

  (evil-set-undo-system 'undo-redo)	; use native redo function

  (add-to-list 'evil-insert-state-modes 'log-edit-mode)
  (add-to-list 'evil-insert-state-modes 'message-mode)

  ;; initialize locals buffer in normal state instead of emacs state
  (add-hook 'gdb-locals-mode-hook 'evil-normal-state)
  
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
(use-package evil-easymotion :disabled
  :defer 1
  :after evil
  :config
  (evilem-default-keybindings "SPC"))

;; move aronud text
(use-package evil-snipe
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode)
  :general
  ('normal evil-snipe-override-mode-map "f" 'evil-snipe-f)
  :after evil
  :defer 1
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible
        evil-snipe-smart-case t)
  ;; "f [" goes to parenthesis or bracket
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  (push '(?\] "[]})]") evil-snipe-aliases)
  ;; define t to be like s 
  (evil-snipe-def 2 inclusive "t" "T"))

;; visualize evil commands
(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :defer 1
  :config
  (evil-goggles-mode)
  (setq
   evil-goggles-duration 0.8        ; show what I copied
        evil-goggles-async-duration 0.8  ; affects indenting
        evil-goggles-blocking-duration 0) ; don't want to wait when deleting
  (evil-goggles-use-diff-faces))

;; unimpaired is a collection of commands with '[' or ']'
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t) ; makes company works betters I think
  (evil-collection-init))

;; navigation: gh, gj, gk, gl
;; promoting/demoting headings: M-hjkl
;; headings: M-ret
(use-package evil-org
  :diminish evil-org-mode
  :after org
  :general
  ('normal org-mode-map "x" 'evil-delete-char)
  :hook
  (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects insert  additional shift todo)))

;; 'org-agenda' evil keybindings
(use-package evil-org-agenda
  :elpaca nil
  :after org-agenda
  :demand
  :config
  (evil-org-agenda-set-keys)
  ;; change to my preferred after set keys
  (general-def 'motion org-agenda-mode-map
           "gt" 'org-todo-list
           "gd" 'org-agenda-view-mode-dispatch))

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

;; jump to matched tags
(use-package evil-matchit :disabled
  :after python evil
  :config
  (global-evil-matchit-mode 4))

(use-package undo-propose :disabled     ; using regular undo in region
  :after evil
  :general
  ('normal 'global "C-c u" 'undo-propose)
  ('normal 'global "u" 'undo-only)
  :config
  (setq undo-propose-pop-to-buffer t))

;; visual undo
(use-package vundo
  :commands vundo)

(use-package magit
  :general
  ("C-x g" 'magit-status)
  ("C-x C-g" 'magit-file-dispatch)
  (magit-diff-section-map "M-RET" 'magit-diff-visit-worktree-file)
  :config
  ;; after evil collection
  (general-def magit-section-mode-map "C-<tab>" nil)
  (general-def 'normal magit-section-mode-map "C-<tab>" nil)
  (general-def '(normal visual) magit-status-mode-map "g t" nil) ; using for switching tabs
  (setq magit-diff-hide-trailing-cr-characters t
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  ;; open commit in insert mode
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  ;; auto refresh magit
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; show colors
(use-package rainbow-mode
  :commands rainbow-mode 
  :diminish rainbow-mode)

;; Add-ons to Org https://git.sr.ht/~bzg/org-contrib
;; some file produces a bug with inline images (can not toggle) so it is better
;; to use just what is needed
(use-package org-contrib
  ;; build only selected (symlink to `build/' and make an pre-compiled '.elc')
  :elpaca (org-contrib :files ("lisp/org-eldoc.el" ; show src block arguments
                               "lisp/ox-bibtex.el" ; export latex properly
                               "lisp/ox-extra.el" ; ignore headlines (need to config)
                               ))
  :after org
  :init
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))) 

(use-package org
  ;; :elpaca nil
  :diminish org-indent-mode
  :mode (("\\.org$" . org-mode))
  :custom-face
  (org-block ((t (:inherit org-agenda-restriction-lock))))
  :general
  ("C-c s i" 'org-insert-link-global)   ; e.g. to insert link to pdf in a latex buffer
  (org-mode-map "C-c C-l" 'org-insert-link)
  (org-mode-map "C-," nil)              ; using that for flyspell
  (org-mode-map "C-c ," 'org-insert-structure-template)
  ;; ('normal org-mode-map "TAB" 'org-cycle) ; avoid binding tab
  ('normal org-mode-map :prefix "z"
           "s j" 'org-babel-next-src-block
           "s k" 'org-babel-previous-src-block
           "n" 'org-toggle-narrow-to-subtree
           "k" 'org-previous-visible-heading
           "j" 'org-next-visible-heading)
  ('normal org-mode-map :prefix "SPC"
           "ves" 'org-babel-execute-subtree
           "vg" 'org-babel-goto-named-src-block) 
  ('normal org-mode-map :prefix "g"
           "k" 'org-backward-heading-same-level
           "j" 'org-forward-heading-same-level
           "n" 'org-babel-next-src-block
           "p" 'org-babel-previous-src-block)
  ;; global map
  ('(normal visual) "M-o" 'org-open-at-point-global)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  ;; (org-mode . org-indent-mode)          ; align with heading, sometimes slow
  :config
  (setq org-hide-emphasis-markers nil        ; avoid noisy //,__, **(makes annoying to edit) 
        org-startup-indented nil		; start collapsed
        org-startup-folded t               ; folded in "overview" state
        org-hide-leading-stars nil           ; don't show a  bunch of '*' (maybe is more performant if shows)
        org-edit-src-content-indentation 0
        org-pretty-entities nil           ; use latex instead...
        org-ellipsis "…"                ;use single character for elipses
        org-outline-path-complete-in-steps nil
        org-special-ctrl-a/e t       ; when jump to beginning of line be aware of *
        org-cycle-separator-lines 0  ; no empty lines between headings
        org-fontify-quote-and-verse-blocks nil ; no special fortification for those blocks 
        org-insert-heading-respect-content nil ; nil: heading after current line/ t: after current sub-tree
        org-catch-invisible-edits 'show-and-error ; make visible then abort
        org-tags-column 0                        ; tag right after text
        org-html-htmlize-output-type 'inline-css   ; nil to export as plain text
        org-startup-with-inline-images t           ; show images
        org-indent-indentation-per-level 1         ; indent just 1 space
        org-image-actual-width nil)     ; if width is specified use that, otherwise keep original size
  (transient-mark-mode -1)

  ;; remove org-cycle-hide-drawers from cycle hook
  ;; so it shows the plots inside a "results drawer" when the heading is opened
  (setq org-cycle-hook
        '(org-cycle-hide-archived-subtrees
          org-cycle-hide-drawers
          org-cycle-show-empty-lines
          org-optimize-window-after-visibility-change))

  ;; add extra todo keywords
  ;; (setq org-todo-keywords '((sequence "TODO" "PROG" "DONE"))
  ;;       org-todo-keyword-faces '(("PROG" . (:foregroud "blue"))))
  )

;; bug when display image using :dir
;; https://lists.gnu.org/archive/html/emacs-orgmode/2021-04/msg00246.html
(use-package org-display-inline-image-hack
  :elpaca nil
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
         (org-redisplay-inline-images)))))

  (add-hook 'org-babel-after-execute-hook 
            #'ded:org-babel-inline-display-subtree))

;; To refresh the image after it changed in disk
;; problem with cache image when included as link
;; need to clear cache
;; https://list.orgmode.org/alpine.DEB.2.22.394.2212112203210.121316@shell3.miskatonic.org/T/#t
(use-package org-refresh-inline-image-linked-hack :disabled
  :elpaca nil
  :after org
  :hook (org-babel-after-execute . org-refresh-inline-images)
  :init
  (defun org-refresh-inline-images ()
    (interactive)
    (clear-image-cache))
  ;; clear cache before redisplay, maybe slow with a lot of images
  (advice-add 'org-redisplay-inline-images :before #'clear-image-cache))

;; problem with babel execute subtree and showing images outside the subtree
(use-package org-display-inline-image-after-execute-subtreee
  :elpaca nil
  :after org
  :general
  (org-mode-map "C-c C-v C-s" 'org-redisplay-after-execute-subtree )
  :init
  (defun org-redisplay-after-execute-subtree ()
    "Redisplay images after execute subtree"
    (interactive)
    (org-babel-execute-subtree)
    (org-redisplay-inline-images)))

;; org image remote
(use-package org-display-inline-image-remote
  :elpaca nil
  :after org
  :init
  ;; maybe too slow when there are many images
  (setq org-display-remote-inline-images 'cache))

;; org export
(use-package ox :disabled
  :elpaca nil
  :after org
  :init
  (setq org-export-in-background t))

(use-package ox-html
  :elpaca nil
  :after org
  :init
  ;; don't scale svg images
  (setq org-html-head "<style> .org-svg {width: auto} </style>"))

;; export to markdown
(use-package ox-md :disabled
  :elpaca nil
  :demand                               ; demand after some time after org is loaded
  :defer 1
  :after org)

;; better support for code blocks
(use-package ox-gfm
  :demand                               ; demand after some time after org is loaded
  :defer 1
  :after org)

;; Deals with "<>" delimiters in org mode source blocks
;; https://emacs.stackexchange.com/a/68321
(use-package org-<>-delimiter-hack
  :elpaca nil
  :after org
  :init
  (defun org-syntax-table-modify ()
    "Modify `org-mode-syntax-table' for the current org buffer."
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  (add-hook 'org-mode-hook #'org-syntax-table-modify))

;; load ob-C when executing C++ source block
(use-package ob-C
  :elpaca nil
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
  :elpaca nil
  :after org
  :commands org-babel-execute:python
  :init
  (setq org-babel-python-command "python3") ; python3 please!
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:exports . "results")))) ; export only plots by default

(use-package ob-julia :disabled
  :elpaca nil
  :after org
  :commands org-babel-execute:julia
  :init
  (setq org-babel-default-header-args:julia
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          ;; add tag variable to all python blocks... maybe not ideal, but usefull
          (:exports . "results"))))

(use-package julia-mode :disabled
  :mode ("\\.jl\\'" . julia-mode))

(use-package julia-vterm :disabled)

(use-package ob-julia-vterm :disabled
  :after org
  :commands org-babel-execute:julia-vterm
  :init
  (setq org-babel-default-header-args:julia-vterm
        '((:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:exports . "results")))

  ;; alias
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm))

(use-package ob-core
  :elpaca nil
  :after org
  :general
  ('normal org-mode-map "g s" (general-simulate-key "C-u C-u C-c C-v C-t"))
  :init
  ;; mkdirp allows creating the :dir if it does not exist
  (add-to-list 'org-babel-default-header-args '(:mkdirp . "yes"))
  (add-to-list 'org-babel-default-header-args '(:noweb . "no-export")))

;; custom org function
(use-package org-image-resize-inline
  :elpaca nil
  :after org
  :general
  ('normal org-mode-map :prefix "SPC"
           "xv" 'org-redisplay-inline-images)
  ('normal org-mode-map "C-+" 'org-zoom-inline-images)
  ('normal org-mode-map "C-_" 'org-zoom-out-inline-images)
  :init
  (defun org-zoom-inline-images (&optional amt)
    (interactive "p")
    ;; get size specified or start with 300
    (let* ((size (if org-image-actual-width
                     org-image-actual-width
                   300))
           ;; amount can be specified with prefix argument
           ;; or use default value
           (amt (if (eq current-prefix-arg nil)
                    50
                  current-prefix-arg))
           (new-size (+ size amt)))
      (setq org-image-actual-width new-size)
      ;; don't redisplay if in tramp
      (if (not (file-remote-p default-directory))
          (org-redisplay-inline-images))))

  (defun org-zoom-out-inline-images ()
    (interactive)
    (setq org-image-actual-width nil)
    ;; don't redisplay if in tramp
    (if (not (file-remote-p default-directory))
        (org-redisplay-inline-images))))

;; for windows
(use-package ob-python
  :elpaca nil
  :after org
  :when (eq system-type 'windows-nt)
  :init
  ;; windows uses python for versions > 3, argh... 
  (setq org-babel-python-command "python"))

(use-package org-clock
  :elpaca nil
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
  :elpaca nil
  :general
  ('normal org-mode-map "z e" 'org-edit-special)
  ('normal org-src-mode-map "z e" 'org-edit-src-exit)
  ('normal org-mode-map "z g" 'org-toggle-blocks-visibility)
  :after org
  :init
  ;; babel and source blocks
  (setq org-src-fontify-natively t
        org-src-window-setup 'split-window-below ; don't move my windows around!
        org-src-preserve-indentation t  ; preserve indentation in code
        org-adapt-indentation nil ; no extra space... better use indent mode (virtual)
        org-edit-src-content-indentation 0 ; dont indent source code
        org-src-tab-acts-natively t	; if t, it is slow!
        org-confirm-babel-evaluate nil) ; doesn't ask for confirmation

  ;; hide source blocks
  ;; https://emacs.stackexchange.com/a/7366 
  (defvar org-blocks-hidden nil)
  (defun org-toggle-blocks-visibility ()
    (interactive)
    (if org-blocks-hidden
        (org-show-block-all)
      (org-hide-block-all))
    (setq-local org-blocks-hidden (not org-blocks-hidden))))

;; Use unicode for delimiter of noweb syntax
;; this avoids problems with sntax highlight inside the block
;; https://emacs.stackexchange.com/a/73720
(use-package org-source-noweb-delimiter
  :elpaca nil
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
  :elpaca nil
  :after org
  :init
  (org-babel-lob-ingest "~/.emacs.d/lisp/scripts.org"))

;; Agenda
;; 'org-agenda-view-mode-dispatch' to view month/week 
;; 'org-agenda-todo' change todo state
;; 'org-todo-list' go to todo view (not scheduled), org just use the agenda view that include both ('C-c a n')
;; Some concepts:
;; 1. 'task': some todo item.
;; 2. 'event': something taking place at a time and optionaly at a place.
;; An event has a plain time stamp. If you missed, it just stays in the past. If
;; marked as 'done' it disappear from the agenda (default behavior).
;; A task can be:
;; 1. 'scheduled': start the 'task' on a given date. It is shown until marked as 'done'.
;; 2. 'deadline': finish a 'task' by this date. It can have warnings before the deadline.
(use-package org-agenda
  :elpaca nil
  :general
  ("C-c a" 'org-agenda)
  :config
  (setq org-agenda-files '("~/SeaDrive/My Libraries/notes/log-notes/")
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
  :elpaca nil
  :if (eq system-type 'gnu/linux)
  :after org
  :init
  ;; change scale of latex preview in org-mode
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1)
        ;; org-startup-with-latex-preview t
        org-latex-image-default-width nil ; don't scale my images!
        org-latex-images-centered t
        org-preview-latex-image-directory "~/.cache/ltximg/"
        ;; use svg images for latex preview
        org-preview-latex-default-process 'dvisvgm)

  ;; mint and svg export require '-shell-escape' option
  (setq org-latex-listings 'minted
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; extra latex packages for every header
  (setq org-latex-packages-alist '(("newfloat" "minted" nil)
                                   ("" "svg"  nil)
                                   ("" "mathrsfs" nil) ; for command mathscr (Calligraphic letters)
                                   ("" "bm, upgreek")
                                   ("" "algorithm, algpseudocode")
                                   ("a4paper, margin=20mm" "geometry" nil)))

  (add-to-list 'org-latex-default-packages-alist '("colorlinks=true, linkcolor=blue, citecolor=blue, filecolor=magenta, urlcolor=cyan" "hyperref" nil)))

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
  :elpaca nil
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
          (:prologue . "exec 2>&1")
          (:epilogue . ":"))))

;; for UML diagrams in org-mode
;; need to install `yay plantuml`
(use-package ob-plantuml
  :elpaca nil
  :commands org-babel-execute:plantuml
  :config
  (setq org-plantuml-exec-mode 'plantuml))

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
  (org-mode-map "C-M-S-y" 'org-download-clipboard)
  :init
  (setq
   org-download-image-dir "./images"
   org-download-image-html-width 350
   org-download-image-latex-width 10)
  ;; can resize screen shot
  (setq org-download-screenshot-method "flameshot gui --raw > %s"))

;; wsl specific config
(use-package org-download
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

;; 'husnpell' is alternative to 'aspell' that accepts multiple simultaneous dictionaries
;; download 'hunspell' and the dictionaries 'yay hunspell hunspell-en (de, pt)
;; run 'hunspell -D' to check where dictionaries are
;; https://emacs.stackexchange.com/a/21379
(use-package flyspell :disabled
  :elpaca nil
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

;; Attempt to make flyspell faster by restricting to region, instead of buffer
;; note: makes it slow when saving the buffer
;; if using 'wucuo', should not use 'flyspell-mode'
(use-package wucuo :disabled
  :hook
  (text-mode . wucuo-start)
  (prog-mode . wucuo-start))

;; completion in region manually summoned with <tab> (no auto pop up)
;; allows space (separator M-SPC) between filter words (combined with oderless)
(use-package corfu
  :elpaca (corfu :type git :host github :repo "minad/corfu"
                   :files (:defaults "extensions/*"))
  :general
  (corfu-map "<tab>" 'corfu-next
             "<backtab>" 'corfu-previous
             "C-n" 'corfu-next
             "C-p" 'corfu-previous)
  ('insert "C-n" nil
           "C-p" nil)
  :defer 1
  :hook
  (text-mode . (lambda () (setq-local corfu-auto-prefix 3))) ; increase prefix for text
  (prog-mode . (lambda ()
                 ;; decrease prefix for text
                 (setq-local corfu-auto-prefix 2
                             corfu-auto-delay 0.05)))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)                ; show doc -> using eldoc box, sometimes they overlap
  (setq corfu-auto t                    ; enables timer-based completion
        corfu-auto-delay 0.3
        corfu-auto-prefix 2
        corfu-quit-no-match 'separator))

;; corfu extension
(use-package corfu-history
  :elpaca nil
  :after corfu
  :config
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; `completion at point' extensions for specific candidates in `completion in region'
(use-package cape
  :elpaca (cape :type git :host github :repo "minad/cape")
  :demand
  :after corfu
  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; cape does not support 'case-replace' yet: https://github.com/minad/cape/issues/51
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; TODO: maybe not needed anymore 
;; see [[orgit-log:~/.local/src/emacs/::("master")][~/.local/src/emacs/ (magit-log "master")]]
(use-package cape-eglot
  :elpaca nil
  :after orderless eglot
  :init
  ;; use orderless style for completion (default is flex)
  ;; https://github.com/minad/corfu/wiki
  (setq completion-category-overrides '((eglot (styles orderless)))))

;; use corfu on terminal
(use-package corfu-terminal
  :elpaca (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
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
  :elpaca nil
  :general
  ("M-/" 'dabbrev-completion)           ; this can be completed with corfu
  ("C-M-/" 'dabbrev-expand)
  :config
  ;; preserve the expansion's case pattern
  (setq dabbrev-case-replace nil))

;; Allows selectively display portions of program
(use-package hideshow
  :elpaca nil
  :diminish hs-minor-mode
  :general
  ('normal hs-minor-mode-map "z H" 'hs-hide-all)
  ('normal hs-minor-mode-map "z l" '(lambda ()
                                      (interactive)
                                      (evil-toggle-fold)
                                      (hs-hide-level 1)))
  ('normal hs-minor-mode-map "<tab>" (general-predicate-dispatch nil
                                       (my-header-p)
                                       'my-toggle-fold))
  :init
  (defun my-header-p ()
    "Return non-nil if the cursor is on a header line."
    (save-excursion
      (back-to-indentation)
      (or (hs-looking-at-block-start-p)
          (hs-already-hidden-p)
          ;; outline identifies headers with a regex per mode works better than
          ;; 'hs-looking-at-block-start-p' if the regex is available
          (outline-on-heading-p))))
  (defun my-toggle-fold ()
    "Cycle visibility: show all, then first level, then collapse."
    (interactive)
    (save-excursion
      (back-to-indentation)
      (pcase last-command
        ;; After showing first level, hide all
        ('my-cycle-visibility-show-all
         (hs-hide-block)
         (message "Hide all")
         (setq this-command 'my-cycle-visibility-hide-all))
        ;; 
        ('my-cycle-visibility-show-first
         (hs-hide-level 1)
         (message "Show first level")
         (setq this-command 'my-cycle-visibility-show-all))
        (_
         ;; If it is not hidden, hide. If it is hidden, then show and trigger
         ;; the cycling sequence.
         (if (not (hs-already-hidden-p))
             (hs-hide-block)
           (progn
             (hs-show-block)
             (setq this-command 'my-cycle-visibility-show-first)))
         (message "Toggle")))))
  :hook
  (prog-mode . hs-minor-mode)
  (nxml-mode . hs-minor-mode))

;; folding
;; note: evil collection also sets a bunch of keybindings
(use-package outline
  :diminish outline-minor-mode
  :mode ("\\.inp\\'" . outline-minor-mode)
  :elpaca nil
  :diminish outline-minor-mode
  :hook
  ;;(prog-mode . outline-minor-mode) ; using the hideshow package
  ;; (emacs-lisp-mode . outline-minor-mode)
  ;; (markdown-mode . outline-minor-mode)
  ;; (conf-mode . outline-minor-mode)
  (LaTeX-mode . outline-minor-mode)
  (evil-collection-setup . (lambda (&rest a)
                             ;; need to rebind after loading outline because 'general' uses
                             ;; `after-load-functions' and 'evil-collection' uses `eval-after-load'
                             ;; 'evil-collection' end up binding last...
                             ;; https://github.com/emacs-evil/evil-collection/issues/214#issuecomment-451489870
                             (general-def 'normal outline-mode-map "z k" 'outline-previous-visible-heading)
                             (general-def 'normal outline-mode-map "z l" nil)))
  :general
  ('normal outline-minor-mode-map "<tab>" (general-predicate-dispatch nil
                                            (outline-on-heading-p) 'outline-cycle))
  ('normal outline-mode-map :prefix "z"
           "j" 'outline-next-visible-heading
           "o" 'outline-show-children   ; show first level
           "A" 'outline-show-all)
  ;; ('normal outline-mode-map "C-j" nil)
  ('normal outline-mode-map "M-j" nil)  ; conflicts with c multiline comment
  :config
  (setq outline-minor-mode-cycle nil    ; using general predicate dispatch instead
        ;; outline-minor-mode-highlight 'append  ;;  bug with C++ source block
        ))  

;; trying to make outline work with python docstring
(use-package outline-python-regex :disabled
  :elpaca nil
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
  :elpaca auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :commands TeX-command-sentinel
  :custom-face
  (font-latex-sectioning-1-face ((t (:slant oblique :box t :height 1.0))))
  (font-latex-sectioning-2-face ((t (:underline t :inherit outline-1 :height 1.0))))
  (font-latex-sectioning-3-face ((t (:slant italic :height 1.0))))
  (font-latex-sectioning-4-face ((t (:slant oblique :underline t :height 1.0))))
  (font-latex-sectioning-5-face ((t (:slant normal :height 1.0))))
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
  
  ;; specific config
  ;; -shell-escape for minted (syntax highlight) and svg export
  (setq LaTeX-command "latex -shell-escape")
  
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
  :elpaca nil
  :after latex
  :general
  (LaTeX-mode-map "C-c C-x C-l" 'preview-buffer) ; same as in org
  ('normal LaTeX-mode-map "g p" '(:keymap preview-map))
  (preview-map
   "b" 'preview-buffer
   "c" 'preview-clearout-buffer
   "s" 'preview-section
   "p" 'preview-at-point)
  :init
  ;; preview latex config
  (setq preview-default-option-list '("displaymath" "textmath" "showlabels")
        preview-auto-cache-preamble t))

;; latex function
(use-package latex-clipboard
  :elpaca nil
  :after latex
  :general
  (LaTeX-mode-map "C-M-y" 'my-tex-insert-clipboard)
  :init
  ;; function definition, not the best...
  (defun my-tex-insert-clipboard ()
    (interactive)
    (setq folder-path (concat default-directory "images/"));make the img directory
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
                        "images/img_"
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

;; fake headers for latex
;; https://emacs.stackexchange.com/a/3103
(use-package latex-fake-header
  :elpaca nil
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

;; text objects for latex editing
;; math, commands, delimiters are usefull
(use-package evil-tex
  :general (evil-tex-mode-map "M-n" nil) ; using with flymake
  :after latex
  :hook (LaTeX-mode . evil-tex-mode))

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
  :elpaca nil
  :commands dired
  :hook
  (dired-mode . dired-hide-details-mode)
  ;; (dired-mode . (lambda () (toggle-truncate-lines)))
  :general
  (dired-mode-map "C-c C-d" 'mkdir
                  "C-," 'dired-omit-mode)
  ('normal dired-mode-map "h" 'dired-up-directory)
  (dired-mode-map "M-o" 'dired-omit-mode)
  ('normal dired-mode-map "l" 'dired-find-alternate-file)
  ('normal dired-mode-map "C-<return>" 'dired-find-file-other-window)
  ("C-x C-j" 'dired-jump-other-window)
  ("C-x j" 'dired-jump)
  (dired-jump-map "j" nil)             ; remove repeat with "j"
  (dired-mode-map "SPC" nil)             ; use for easymotion
  ('normal image-dired-thumbnail-mode-map "+" 'image-increase-size)
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

  ;; after 'evil-collection'
  ;; because general uses `after-load-functions' and evil-collection uses `eval-after-load'
  (general-def 'normal dired-mode-map "SPC" nil)

  ;; Open pdf in dired with `!` and the default application 
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "okular")
          ("\\.png\\'" "feh --auto-reload")
          ("\\.svg\\'" "feh --auto-reload"))))

;; open dired as a sidebar
(use-package dired-sidebar
  :general
  ("C-x C-j" 'dired-sidebar-jump)
  ('normal dired-sidebar-mode-map
           "l" 'dired-sidebar-find-file ; use 'C-u' to select specific window
           "h" 'dired-sidebar-up-directory)
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

;; improved dired
;; problem with tramp
(use-package dirvish :disabled
  :elpaca (dirvish :type git :host github :repo "alexluigit/dirvish")
  :general
  ('normal dired-mode-map "Y" 'dirvish-copy-file-path)
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes '(collapse file-size))
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  ;; kill buffer after entry file, avoid accumulate buffers
  (put 'dired-find-alternate-file 'disabled nil))  

;; Load modus in terminal, it is very clever to figure out the colors there
(use-package modus-themes
  :defer 1
  :general
  ("<f5>" 'modus-themes-toggle)
  :config
  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-prompts '(intense italic)
        modus-themes-diffs 'desaturated
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-headings '((t . (rainbow))))
  ;; hook to enforce change when theme is toggled (which loads the theme)
  (defun my-modus-tweaks ()
    (progn 
      ;; adjust org modern if GUI
      (eval-after-load 'org-modern
        '(global-org-modern-mode))
      ;; reset icons cache to match theme
      (eval-after-load 'kind-icon
        '(kind-icon-reset-cache))
      ;; recompute face for indentation guide
      (eval-after-load 'hl-indent-scope
        '(hl-indent-scope--auto-color-calc))
      ;; make inside of parenthesis different background
      (eval-after-load 'smartparens
        (set-face-attribute 'sp-show-pair-match-content-face nil :background (modus-themes-get-color-value 'bg-paren-expression)))))
  (add-hook 'modus-themes-after-load-theme-hook 'my-modus-tweaks)

  ;; load the theme automatically in the terminal and disable others automatically
  ;; (if (not (display-graphic-p))
  ;;     (modus-themes-load-theme 'modus-vivendi-tinted))
  )

;; change backgroud of other windows
;; when with custom theme and GUI
(use-package highlight-current-window :disabled
  :when (display-graphic-p)
  :elpaca nil
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
(use-package face-remap
  :elpaca nil
  :diminish (buffer-face-mode))

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
  :elpaca nil
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
  :elpaca nil
  :general
  (c++-ts-mode-map "C-x c" 'compile)
  :init
  ;; From the documentation, substitute to 'tree-sitter' based modes
  ;; [[help:c-ts-mode][help:c-ts-mode]] 
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode))
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
  :general
  (python-mode-map "<backtab>" nil)
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
  (add-hook 'python-mode-hook #'my-python-mode-hook)

  ;; flake8 combines pyflakes (error checker) with stylistic check against pep8 standards.
  ;; using flake9 for support to 'pyproject.toml'.
  (setq python-flymake-command '("flake8" "-")
        python-check-command "/home/nasser/.local/bin/flake8"))

(use-package python-ts
  :elpaca nil
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :general
  (python-ts-mode-map "<backtab>" nil)
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
  (add-hook 'python-mode-hook #'my-python-mode-hook)

  ;; flake8 combines pyflakes (error checker) with stylistic check against pep8 standards.
  ;; using flake9 for support to 'pyproject.toml'.
  (setq python-flymake-command '("flake8" "-")
        python-check-command "/home/nasser/.local/bin/flake8"))

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
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; key chord hint
(use-package which-key
  :defer 1
  :diminish which-key-mode
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

;; dependency of consult-bibtex
(use-package bibtex-completion
  :after consult-bibtex
  :elpaca (bibtex-completion :host github
                               :repo "tmalsburg/helm-bibtex"
                               :files (:defaults (:exclude "helm-bibtex.el" "ivy-bibtex.el")))
  :init
  (setq bibtex-completion-bibliography "~/.bibliography.bib"
        bibtex-completion-library-path "~/SeaDrive/My Libraries/bibliography/"
        bibtex-completion-pdf-open-function (lambda (fpath)
                                              (call-process "xdg-open" nil 0 nil fpath)))
  ;; dont prompt for anything, just insert the citation please.
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil))

;; using okular to at least view the documents...
(use-package bibtex-completion-wsl
  :elpaca nil
  :when (string-match "-[Mm]icrosoft" operating-system-release)
  :after consult-bibtex
  :init
  (setq bibtex-completion-pdf-open-function (lambda (fpath)
                                              (call-process "okular" nil 0 nil fpath))))

;; search bibtex bibliography with consult
;; depends on helm-bibtex
(use-package consult-bibtex
  :if (eq system-type 'gnu/linux)
  :elpaca (consult-bibtex :host github
                            :repo "mohkale/consult-bibtex")
  :general
  ("C-c b" 'consult-bibtex)
  :config
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map))))

;; bibtex completion add option for pdf view (one for annotation other for viewing)
(use-package consult-bibtex-annotation
  :elpaca nil
  :after consult-bibtex
  :init
  (defun bibtex-completion-open-pdf-annotation (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (call-process "xournalpp" nil 0 nil fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))
  (consult-bibtex-embark-action consult-bibtex-open-pdf-annotation bibtex-completion-open-pdf-annotation)
  (define-key consult-bibtex-embark-map "n" #'consult-bibtex-open-pdf-annotation))

;; option to open with evince for printing
(use-package consult-bibtex-evince
  :elpaca nil
  :after consult-bibtex
  :init
  (defun bibtex-completion-open-pdf-evince (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (call-process "evince" nil 0 nil fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))
  (consult-bibtex-embark-action consult-bibtex-open-pdf-evince bibtex-completion-open-pdf-evince)
  (define-key consult-bibtex-embark-map "p" #'consult-bibtex-open-pdf-evince))

;; option for open with pdf-tools (default with find-file when `openwith-mode' is disabled) 
(use-package consult-bibtex-pdftools
  :elpaca nil
  :after consult-bibtex
  :init
  (defun bibtex-completion-open-pdf-tools (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function 'find-file))
      (bibtex-completion-open-pdf keys fallback-action)))
  (consult-bibtex-embark-action consult-bibtex-open-pdf-tools bibtex-completion-open-pdf-tools)
  (define-key consult-bibtex-embark-map "t" #'consult-bibtex-open-pdf-tools))

(use-package citar :disabled
  :general
  ("C-c b" 'citar-insert-citation)
  :custom
  (citar-bibliography '("~/.bibliography.bib"))
  :config
  (setq citar-library-paths '("~/SeaDrive/My Libraries/bibliography/"))
  ;; icons
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")

  ;; don't prompt files, just open
  (setq citar-file-open-prompt nil)

  ;; refresh cache after bib changes
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook)))

;; function to open with xournal
(use-package citar-annotation
  :elpaca nil
  :after citar :disabled
  :init)

(use-package biblio
  :commands biblio-lookup)

(use-package server
  :elpaca nil
  :defer 1
  :config
  (or (server-running-p)
      (server-start)))

(use-package table
  :elpaca nil
  :after org)

;; needs to be added manually to .emacs.d/lisp folder
(use-package wsl-path :disabled
  :if (not (string-match "-[Mm]icrosoft" operating-system-release))
  :elpaca nil
  :load-path "./lisp"
  :commands (wsl-path-activate
             wsl-path-convert-file-name)
  :init
  (wsl-path-activate))

(use-package yasnippet
  :diminish yas-minor-mode
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
(use-package exec-path-from-shell
  :config
  ;; non interative shell start up faster
  ;; (setq exec-path-from-shell-arguments nil)
  :commands (exec-path-from-shell-initialize))

;; browser the web inside emacs
(use-package eww
  :elpaca nil
  :general
  ("<f12>" 'eww)                        ; with C-u prefix, open new buffer
  ('normal eww-mode-map "C-c y" 'eww-copy-page-url)
  ('normal eww-mode-map "<SPC>" nil)               ; use for other things
  :hook
  ;; (eww-after-render . (lambda () (eww-readable)))  ; does not work for all
  (eww-mode . visual-line-mode)
  :config
  (setq shr-use-fonts t                 ; change heading size
        shr-use-colors t
        shr-max-image-proportion .5
        shr-bullet "• "
        browse-url-browser-function 'browse-url-default-browser ; open in eww by default
        eww-auto-rename-buffer t                    ; each page on its own buffer
        eww-search-prefix "https://www.google.com/search?hl=en&lr=lang_en&q="))

;; jump to link
(use-package ace-link
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
(use-package vterm
  :general
  ;; ("<f9>" 'vterm-other-window)
  (vterm-mode-map "<f9>" nil
                  "C-w" nil
                 "<backtab>" nil)
  :config
  (general-def '(normal insert) vterm-mode-map "C-s" nil)
  (setq vterm-max-scrollback 20000
        vterm-timer-delay 0)
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("scp" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("docker" "/bin/bash")))

;; Quickly switch to 'vterm' buffer.
(use-package vterm-toggle
  :general
  ("<f9>" 'vterm-toggle-cd) 	; opens term in current cd including remote
  ("C-<f9>" 'vterm-toggle-insert-cd)
  (vterm-mode-map "s-n" 'vterm-toggle-forward
                  "s-p" 'vterm-toggle-backward)
  :config
  ;; toggle terminal bellow the selected window (avoid messing with other windows)
  ;; https://github.com/jixiuf/vterm-toggle/issues/33#issuecomment-1100027238
  (add-to-list 'display-buffer-alist `(,vterm-buffer-name display-buffer-below-selected)))

;; multiple terminals
(use-package multi-vterm
  :general
  ("S-<f9>" 'multi-vterm)
  :commands multi-vterm
  :config
  (add-to-list 'display-buffer-alist `("^\\*vterminal.*" display-buffer-below-selected)))

(use-package terminal-here
  :general
  ('normal "C-<f7>" 'terminal-here-launch)
  :config
  ;; change terminal command
  (when (string= system-name "ryzen-ms7c37")
    (setq terminal-here-terminal-command 'gnome-terminal)))

(use-package keycast
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
  :elpaca nil
  :after org :demand				; load after org
  :hook (org-mode . (lambda ()
                      (add-hook 'before-save-hook 'time-stamp)))
  :config
  (setq time-stamp-active t
        time-stamp-format "%Y-%m-%d %H:%M:%S"
        time-stamp-end "$"		; regex for end of line
        time-stamp-start "#\\+lastmod:[ \t]*"))

;; terminal emacs with evil cursor indication
;; does not work with mosh: https://github.com/mobile-shell/mosh/issues/352 
(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :init
  (evil-terminal-cursor-changer-activate))

;; don't change background when in terminal
(use-package terminal-disable-bg :disabled
  :elpaca nil
  :unless (display-graphic-p)
  :init
  (set-face-background 'default "undefined"))

(use-package repeat
  ;; :if (string-greaterp emacs-version "28") ; need emacs > 28
  :elpaca nil
  :defer 1
  :demand
  :config
  ;; useful to resize windows
  (setq repeat-keep-prefix t)
  ;; built-in command repeater (like hydra)
  (repeat-mode t))

;; built in windows resize functions
(use-package window
  :elpaca nil
  :general
  ("C-c w" '(:keymap resize-window-repeat-map))
  ("C-x C-o" 'other-window)
  ('normal "C-w TAB" 'other-window)
  (other-window-repeat-map "TAB" 'other-window)
  (resize-window-repeat-map "j" 'enlarge-window)
  (resize-window-repeat-map "k" 'shrink-window)
  (resize-window-repeat-map "h" 'shrink-window-horizontally)
  (resize-window-repeat-map "l" 'enlarge-window-horizontally))

;; create backlinks when linking org-mode headings
(use-package org-super-links
  :elpaca (org-super-links :type git :host github :repo "toshism/org-super-links")
  ;; :after org  ; can use outside of org-mode, so use the keybindings to load
  :general
  (:prefix "C-c s"
           "l" 'org-super-links-store-link
           "p" 'org-super-links-insert-link)
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
  :elpaca nil
  :after org
  :demand                               ; explicitly require 'org-id'
  :config
  ;; automatic generate id for headings
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; citations support in org-mode
(use-package oc
  :elpaca nil
  :after org
  :general
  (org-mode-map "C-c C-b" 'org-cite-insert)
  :config
  (setq org-cite-global-bibliography '("~/.bibliography.bib")))

;; highlight indentation
(use-package highlight-indent-guides :disabled
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; suppress error on tui
(use-package highlight-indent-guides :disabled
  :after highlight-indent-guides
  :unless (display-graphic-p)
  :init
  (setq highlight-indent-guides-suppress-auto-error t))

;; highlight based on scope
(use-package hl-indent-scope :disabled
  :when (display-graphic-p)
  :elpaca (hl-indent-scope :type git :host nil :repo "http://codeberg.org/ideasman42/emacs-hl-indent-scope")
  :hook
  (prog-mode . hl-indent-scope-mode))

;; emacs built in version control
(use-package vc-git
  :elpaca nil
  :general
  ("C-x v p" 'vc-push)
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
  :elpaca nil
  :config
  ;; scp is faster than ssh for copying files, but scp is apparently deprecated
  ;; https://www.reddit.com/r/emacs/comments/xul3qm/how_to_make_tramp_faster/
  (setq tramp-default-method "scp"
        tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
        tramp-histfile-override nil
        tramp-verbose 3)                ; increase to 6 to debug

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
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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
  :elpaca nil
  :general
  ("C-x C-b" 'ibuffer)
  :config
  ;; Grouping
  ;; https://www.emacswiki.org/emacs/IbufferMode#h5o-6
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 ("outputs" (or
                             (name . "\\.out$")))
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
(use-package ibuffer-tramp
  :general
  ('normal ibuffer-mode-map "s t" 'ibuffer-tramp-set-filter-groups-by-tramp-connection)
  ('normal ibuffer-mode-map "s r" 'ibuffer-switch-to-saved-filter-groups))

;; need to install language specific modules:
;; https://github.com/casouri/tree-sitter-module
;; and put in ./emacs.d/tree-sitter
(use-package treesit
  :elpaca nil
  :general
  ('normal c++-ts-mode-map "zj" 'treesit-end-of-defun)
  ('normal c++-ts-mode-map "zk" 'treesit-beginning-of-defun)
  :custom-face
  (font-lock-function-call-face ((t :inherit outline-7)))
  (font-lock-function-name-face ((t :inherit t :weight semi-bold)))
  :config
  ;; download and build grammar dynamic library 
  ;; need to call: 'treesit-install-language-grammar' to install
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python.git")
          (c "https://github.com/tree-sitter/tree-sitter-c.git")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp.git")
          (cmake "https://github.com/uyha/tree-sitter-cmake.git")))
  ;; maximum fontification
  (setq treesit-font-lock-level 4))

;; better code highlight and fold
(use-package tree-sitter :disabled
  :diminish tree-sitter-mode
  :hook
  (c-mode-common . tree-sitter-mode)
  (python-mode . tree-sitter-mode)
  ;; replace regex-based highlighting
  (tree-sitter-after-on . tree-sitter-hl-mode))

;; language bundle for `tree-sitter`
(use-package tree-sitter-langs :disabled
  :demand                               ; require it after loading tree-sitter
  :after tree-sitter)

;; fold based on tree sitter syntax tree
(use-package ts-fold :disabled
  :elpaca (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :general
  ('normal :predicate '(outline-on-heading-p) "<tab>" 'evil-toggle-fold)
  :hook
  (c-mode-common . ts-fold-mode)
  (python-mode . ts-fold-mode)
  :config
  (setq ts-fold-replacement "…"
        ts-fold-summary-exceeded-string "…"
        ts-fold-summary-format " %s"))

;; use tree sitter as evil text objects
(use-package evil-textobj-tree-sitter :disabled
  :elpaca (evil-textobj-tree-sitter :type git
                                      :host github
                                      :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries"))
  :after tree-sitter
  :config
  (define-key evil-outer-text-objects-map "s" (evil-textobj-tree-sitter-get-textobj "statement.outer"))
  (define-key evil-inner-text-objects-map "n" (evil-textobj-tree-sitter-get-textobj "scopename.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

(use-package hide-comnt :disabled
  :elpaca nil
  :general ('normal "g h c" 'hide/show-comments-toggle)
  :commands hide/show-comments-toggle)

(use-package wgrep)

;; config for windows
(use-package emacs-windows-config
  :elpaca nil
  :if (eq system-type 'windows-nt)
  :init
  (setq w32-get-true-file-attributes nil
        recentf-auto-cleanup 'never))

;; use emacs to edit text within chrome
(use-package atomic-chrome
  :commands atomic-chrome-start-server
  :config
  (setq atomic-chrome-default-major-mode 'LaTeX-mode))

;; easily change windows
(use-package ace-window
  :commands aw-select aw-window-list                   ; for dired with C-u
  :general
  ('normal "C-w C-w" 'ace-window)
  ('normal dired-mode-map "l" 'dired-find-alternate-file-ace)
  ('normal dired-mode-map "C-c l" 'dired-find-alternate-file-ace-rem)
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
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :defer 1 
  :config
  (setq highlight-parentheses-colors nil
        highlight-parentheses-attributes '((:box  (:line-width (-1 . -1) :style nil))))
  (global-highlight-parentheses-mode))

;; moving cursor around fast and efficiently
(use-package avy
  :general
  ('normal "/" 'avy-goto-char-timer)
  (isearch-mode-map "C-'" 'avy-isearch)
  (isearch-mode-map "M-'" 'avy-isearch) ; to work in tty as well
  ('(normal visual) :prefix "SPC"
   "j" 'avy-goto-line-below
   "k" 'avy-goto-line-above)
  :demand
  :config
  (setq avy-timeout-seconds 0.2         ; quicker
        avy-all-windows-alt t           ; allow all windows when `C-u`
        avy-all-windows nil))           ; restrict to one window

(use-package all-the-icons-completion
  :elpaca (all-the-icons-completion :type git :host github :repo "MintSoup/all-the-icons-completion")
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
  :elpaca nil
  :custom-face
  (eglot-highlight-symbol-face ((t (:underline t :weight bold))))
  :hook
  (eglot-managed-mode . eglot-inlay-hints-mode)
  (python-mode . eglot-ensure) ; works if there is only one server available
  (python-ts-mode . eglot-ensure)
  ;; python 'flymake' tweak
  (eglot-managed-mode . (lambda ()
                          ;; https://old.reddit.com/r/emacs/comments/xq6rpa/weekly_tips_tricks_c_thread/
                          ;; re-enable 'flymake' checkers because 'eglot' clobbers
                          ;; them when starting
                          (when (or (derived-mode-p 'python-mode)
                                    (derived-mode-p 'python-ts-mode))
                            (add-hook 'flymake-diagnostic-functions 
                                      'python-flymake nil t)
                            ;; for some reason I need to "start" 'flymake' again
                            (flymake-start))))
  ;; (LaTeX-mode . eglot-ensure) ; works if there is only one server available
  (c++-mode . eglot-ensure) ; works if there is only one server available
  (c++-ts-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (js-mode . eglot-ensure) ; works if there is only one server available
  :config
  ;; using a hook to 
  ;; (add-to-list 'eglot-stay-out-of 'flymake)     ; using own flymake command (flake8)

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
  (setq eglot-events-buffer-size 0
        eglot-sync-connect 0            ; don't block LSP connection attempts
        eglot-autoshutdown t))

;; Use 'mypy' check for type in python
;; need to install 'pip install mypy'
;; for examples: https://mypy-lang.org/examples.html
(use-package flymake-mypy :disabled     ; for some reason not working, I'm
                                        ; relying on the language server for
                                        ; type check now
  :elpaca (flymake-mypy :type git :host github :repo "com4/flymake-mypy")
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
;; cd ~/.opt && https://github.com/valentjn/ltex-ls/releases/download/15.2.0/ltex-ls-15.2.0-linux-x64.tar.gz
;; tar -xvf ltex-ls-15.2.0-linux-x64.tar.gz
;; also need java (open JDK 11 development kit)
;; 
;; ltex-ls DOES NOT SUPPORT external files...
;; https://github.com/valentjn/ltex-ls/issues/157#:~:text=External%20files%20are%20not%20supported%20by%20ltex%2Dls
;; just disable spelling with a .dir-locals.el
;; ((nil
;;   (eglot-workspace-configuration
;;    . ((ltex . (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"])))))))
;; 
(use-package eglot-ltex
  :unless (string-match "-[Mm]icrosoft" operating-system-release) ; only in linux
  :elpaca (eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex")
  :commands start-eglot-ltex
  :init
  (setq eglot-languagetool-server-path "~/.opt/ltex-ls-15.2.0/")

  (defun start-eglot-ltex ()
    (interactive)
    (require 'eglot-ltex)
    (setq-local eglot-stay-out-of '(eldoc))  ; in org-mode, standard eldoc behavior (for src blocks heading)
    ;; automatic set a dir locals to org-mode
    (call-interactively #'eglot)
    ;; set custom CAPE functions
    (setq-local completion-at-point-functions
                `(cape-file
                  cape-dabbrev
                  eglot-completion-at-point
                  pcomplete-completions-at-point
                  cape-ispell)))

  ;; example: https://www.emacswiki.org/emacs/DirectoryVariables#:~:text=non%2Ddotted%20notation.-,Without%20a%20.dir%2Dlocals.el%20file,-You%20can%20also
  (dir-locals-set-class-variables
   'ltex-config
   '((nil                               ; all modes
      . ((eglot-workspace-configuration
          . (:ltex . (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"])))))))))

;; need to install grammarly-languageserver
(use-package eglot-grammarly
  :elpaca (:host github :repo "emacs-grammarly/eglot-grammarly")
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
(use-package flymake-languagetool :disabled
  :commands flymake-languagetool-maybe-load
  ;; better to do manually, sometimes slow to start
  ;; :hook (text-mode . flymake-languagetool-maybe-load)
  :config
  (setq flymake-languagetool-server-jar "~/.opt/LanguageTool-5.9/languagetool-server.jar")
  ;; not working
  ;; Remote server config with LanguageTool's free API
  ;; (setq flymake-languagetool-url "https://api.languagetool.org")
  ;; (setq flymake-languagetool-server-port nil)
  ;; (setq flymake-languagetool-server-jar nil)

  ;; activate spell checker as well (instead of flyspell)
  ;; (setq flymake-languagetool-check-spelling t)
  )

(use-package svg-lib
  :elpaca (svg-lib :type git :host github :repo "rougier/svg-lib"))

;; Icons for completion in region.
(use-package kind-icon
  :elpaca (kind-icon :type git :host github :repo "jdtsmith/kind-icon")
  :after corfu 
  :custom
  (kind-icon-default-face 'corfu-default)
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package image-mode
  :elpaca nil
  :config
  (setq image-animate-loop t))

;; Shows which function in the mode line.
(use-package which-func :disabled ; using breadbrum
  :elpaca nil
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
  :general
  ('normal dired-mode-map :prefix "C-c"
           "C-c" 'dired-ranger-copy
           "C-v" 'dired-ranger-paste
           "C-x" 'dired-ranger-move))

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
  :elpaca nil
  :after org
  :init
  (setq org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "okular %s")
          ("\\.pdf:::\\([0-9]+\\)?\\'" . "okular %s -p %1")
          ;; if file has ::<page> opens at this page
          ("\\.pdf::\\([0-9]+\\)?\\'" . "xournalpp %s -n %1"))))

;; Specific for terminal emacs to open images
(use-package org-file-apps-terminal
  :elpaca nil
  :if (not (display-graphic-p))
  :after org
  :init
  (add-to-list 'org-file-apps '("\\.svg\\'" . "inkview %s"))
  (add-to-list 'org-file-apps '("\\.png\\'" . "feh %s")))

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
  :elpaca nil
  :diminish eldoc-mode
  :hook (org-mode . eldoc-mode)
  :config
  ;; never resize echo area display, use always 1 truncated line
  ;; use `eldoc-doc-buffer' for multiple lines (with popper is good)
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-idle-delay 1))

;; save windows configurations and use regular bookmarks file
(use-package burly
  :elpaca (burly :type git :host github :repo "alphapapa/burly.el")
  :general
  ('normal "<f6>" 'burly-bookmark-windows))

;; view large files
(use-package vlf
  :defer 1
  :config
  ;; to have 'vlf' as an option
  (require 'vlf-setup))

;; requires dtach `yay dtach'
;; run shell commands detached from emacs
(use-package detached :disabled
  :elpaca (detached :type git :host nil :repo "https://git.sr.ht/~niklaseklund/detached.el")
  :general
  ;; ('normal "<f7>" 'detached-open-session)
  ([remap async-shell-command] 'detached-shell-mode)
  :custom ((detached-show-output-on-attach t))
  :init
  (detached-init))

;; helps with windows popups
(use-package popper
  :defer 1
  :general
  (popper-mode-map "C-`" 'popper-toggle-latest)
  ('(normal insert) popper-mode-map "C-@" 'popper-toggle-latest) ; for term
  (popper-mode-map "C-M-`" 'popper-cycle)
  ('normal popper-mode-map "q" (general-predicate-dispatch nil
                                 (popper-popup-p (current-buffer)) 'popper-kill-latest-popup))
  :init
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
          "Translate\\*$"               ; gts translate
          ;; "\\*Python\\*"
          "CAPTURE-.*"
          "^\\*Dicti.*"
          "\\*Pueue Log\\*"
          ("\\*BBDB\\*" . hide)         ; when the database add an etry
          "\\*compilation\\*"
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package rainbow-delimiters
  :hook (smartparens-mode . rainbow-delimiters-mode))

;; highlight scope defined by delimiters
;; useful sometimes in tex
(use-package rainbow-blocks :disabled
  :diminish rainbow-blocks-mode
  :hook (LaTeX-mode . rainbow-blocks-mode)
  :config
  (setq rainbow-blocks-outermost-only-face-count 1))

;; alternative to flyspell
(use-package spell-fu :disabled
  :hook (LaTeX-mode . spell-fu-mode))

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

;; usefull for email composing
(use-package flymake-grammarly
  :config
  (setq flymake-grammarly-check-time 0.2)
  :commands load-flymake-with-grammarly)

(use-package virtual-comment :disabled
  :diminish virtual-comment-mode
  :general
  ('normal "C-<return>" 'virtual-comment-make)
  :hook 
  (python-mode . virtual-comment-mode)
  (c++-mode . virtual-comment-mode)
  :config
  (setq virtual-comment-face 'lazy-highlight
        virtual-comment-default-file "~/.emacs.d/.evc"))

;; Improve org latex support to use Auctex (faster and async)
;; I don't know why it fails on the first preview call
(use-package org-auctex ; :disabled; does not play nicely when there is $ in shell src blocks
  :after org
  :elpaca (org-auctex :type git :host github :repo "karthink/org-auctex")
  :hook (org-mode . org-auctex-mode))

;; query for org
(use-package org-ql :disabled
  :after org
  :elpaca (org-ql :host github :repo "alphapapa/org-ql"
                    :files (:defaults (:exclude "helm-org-ql.el"))))

;; allows inline animations in org
(use-package org-inline-anim
  :after org
  :commands org-inline-anim-mode)

;; eldoc in childframe
;; sometimes it gets in the way
(use-package eldoc-box
  :if (display-graphic-p)
  :diminish eldoc-box-hover-mode
  :general
  ('normal :keymaps 'override "K" 'eldoc-box-help-at-point)
  :hook
  (prog-mode . eldoc-box-hover-mode)
  :config
  (setq eldoc-box-only-multi-line t
        eldoc-box-lighter t))

;; async support for dired
(use-package dired-async
  :elpaca nil
  :hook (dired-mode . dired-async-mode))

;; try later as alternative to dired-async
(use-package dired-rsync
  :after dired
  :general
  (dired-mode-map "C-c C-r" 'dired-rsync))

;; function to run local command on remote file
;; https://emacs.stackexchange.com/questions/42252/run-local-command-on-remote-file-with-tramp
(use-package dired-local-command-on-remote
  :elpaca nil
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

;; custom writing rules
;; download vale and put on path (~/.local/bin/)
;; use vale with flymake
;; need ~/.vale.ini with minimum:
;;
;; MinAlertLevel = suggestion
;; [*]
;; BasedOnStyles = Vale
;;
;; or more elaboratate https://vale.sh/generator/
;; for write good, download wget https://github.com/errata-ai/write-good/releases/download/v0.4.0/write-good.zip 
;; and unzip write-good.zip -d ~/.config/vale/styles/
;; add add to the .vale.ini the "StylesPath = /home/nasser/.config/vale/styles"
(use-package flymake-vale
  :elpaca (flymake-vale :type git :host github :repo "tpeacock19/flymake-vale")
  :commands flymake-vale-load)

;; Query synonyms
(use-package le-thesaurus :disabled
  :general
  ("C-c u" 'le-thesaurus-get-synonyms)
  :commands le-thesaurus-get-synonyms)

;; Anther package to find synonyms
(use-package powerthesaurus
  :general
  ("C-c u" 'powerthesaurus-lookup-dwim))

;; org-mode toc heading
(use-package org-make-toc
  :after org
  :commands org-make-toc-insert org-make-toc)

;; more color in dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; show org-babel error or warning when execute block
;; just using prologue command is sufficient
(use-package org-babel-eval-verbose
  :elpaca nil
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

;; dim text color from surroundings
(use-package focus
  :commands focus-mode
  :config
  (add-to-list 'focus-mode-to-thing '(prog-mode . sexp)))

;; list 'imenu' entries in a buffer
;; better faces than 'consult-imenu'
;; 'imenu' gives the namespace, functions, classes and methods in a tree
(use-package imenu-list
  :general
  ('normal "g o" 'imenu-list)
  :config
  (setq imenu-list-auto-resize t
        imenu-list-auto-update nil      ; I want to keep the list from a file
        imenu-list-position 'left
        imenu-list-size 0.1
        imenu-list-focus-after-activation t))

;; deal with ANSI escape sequences for coloring
(use-package ansi-color
  :elpaca nil
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
(use-package org-babel-detangle
  :elpaca nil
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

;; format code when saving based on `.clang-format' file
;; use eglot
(use-package clang-format+ :disabled
  :hook (c-mode-common . clang-format+-mode))

(use-package org-footnote
  :elpaca nil
  :after org
  :init
  ;; put footnotes at the current section
  (setq org-footnote-section nil))

(use-package org-tree-slide
  :after org
  :commands org-tree-slide-mode
  :config
  ;; add keybindings after loading
  ;; (with-eval-after-load "org-tree-slide"
  ;;   (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  ;;   (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))
  )

;; Emcas 'gdb' interface
;; 'gdb-mi.el' is the file where the variables are defined so statements in
;; ':config' would be evaluated after this file is loaded.
(use-package gdb-mi
  :elpaca nil
  :general
  (gud-global-map "C-a" 'gud-run)
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

;; irony mode for 'org-edit-special' c++ 
;; uses libclang
(use-package irony
  :after org
  :hook
  (org-src-mode . (lambda ()
                    (when (string-equal major-mode "c++-mode")
                      (irony-mode)))))

;; eldoc support for irony
(use-package irony-eldoc
  :hook
  (irony-mode . irony-eldoc))

;; flymake for C++ blocks with 'org-edit-special'
;; https://stackoverflow.com/a/14866268 
;; does not work, maybe because of the temp file
(use-package flymake-org-edit-special-c++ :disabled
  :elpaca nil
  :after org
  :hook
  (org-src-mode . (lambda ()
                    (when (string-equal major-mode "c++-mode")
                      (setq-local flymake-cc-command 'flymake-cc-init)
                      (flymake-mode))))
  :init
  (defun flymake-cc-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file)))))


;; tags with different colors in org
(use-package org-rainbow-tags
  :elpaca (:host github :repo "KaratasFurkan/org-rainbow-tags")
  :hook (org-mode . org-rainbow-tags-mode))

;; python support for org-edit-special
(use-package elpy :disabled
  :hook
  (org-src-mode . (lambda ()
                    (when (string-equal major-mode "python-mode")
                      ;; remove company mode
                      (setq elpy-modules '(elpy-module-flymake elpy-module-sane-defaults))
                      (elpy-enable)
                      ;; add company backend as CAPF for cape
                      ;; need some company functions...
                      (require 'company)
                      (setq-local completion-at-point-functions
                                  (cape-company-to-capf 'elpy-company-backend))))))

;; auto complete with company backend adapter for corfu
;; there was a problem with numpy, this fixes: https://github.com/davidhalter/jedi/issues/1864#issuecomment-1306543244
(use-package company-jedi
  :hook
  (org-src-mode . (lambda ()
                    (when (string-equal major-mode "python-mode")
                      (require 'company-jedi)
                      (setq-local completion-at-point-functions
                                  (cape-company-to-capf 'company-jedi))))))

;; authentication file  
(use-package auth-source
  :elpaca nil
  :commands auth-source-search
  :init
  (setq auth-sources '((:source "/home/nasser/SeaDrive/My Libraries/secrets/.authinfo.gpg"))))

;; hide everything except current heading
;; https://stackoverflow.com/a/28031539/20449842
(use-package org-show-current-tidyly
  :elpaca nil
  :after org
  :general
  ('normal org-mode-map "z =" 'org-show-current-heading-tidily)
  :init
  (defun org-show-current-heading-tidily ()
    (interactive)  ;Inteactive
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
      (show-children))))

(use-package puni :disabled
  :hook
  (prog-mode . puni-global-mode)
  (text-mode . puni-global-mode))

(use-package elec-pair :disabled
  :hook
  (prog-mode . electric-pair-mode)
  (text-mode . electric-pair-mode))

(use-package markdown-mode
  :hook
  (markdown-mode . variable-pitch-mode) ; use variable pitch fonts
  (markdown-mode . visual-line-mode))

;; for reading email lists
;; summary marks: [[info:gnus#Read Articles][gnus#Read Articles]]
(use-package gnus
  :elpaca nil
  :general
  ("C-x C-m" 'gnus) 
  :hook
  (gnus-mode . turn-on-gnus-dired-mode )
  ;; (gnus-summary-prepared . variable-pitch-mode)
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
        gnus-summary-line-format (concat
                                  "%U"  ; read status
                                  "%R"  ; reply status
                                  "%z "  ; score
                                  "%d %*"  ; date
                                  "%B"     ; thread tree string 
                                  "%(%[%-20,20n%]%) " ; name
                                  "%I%s\n") 
        gnus-article-sort-functions '((not gnus-article-sort-by-number)) ; newer on top...
        gnus-use-full-window nil       ; don't use entire window!
        gnus-home-directory "~/.emacs.d" ; easier to sync different machines with git
        ;; Attempts to make it faster
        gnus-fetch-old-headers nil       ; build from already read mail, nil is faster, use '^' to get parent
        gnus-check-new-newsgroups nil  ; make start up faster
        gnus-show-threads nil            ; if nil can make faster, threads again with T t
        gnus-use-cross-reference nil
        gnus-always-read-dribble-file t  ; don't ask, just use auto saved data 
        ;; (info "(gnus)Startup Files")
        gnus-read-active-file nil        ; only read '.newsrc', speeds up
        gnus-save-newsrc-file nil        ; I will not use anything other than gnus
        gnus-read-newsrc-file nil        ; speed up start
        ;; Maybe improve https://gluer.org/blog/2023/trying-gnus-as-an-email-client/
        gnus-asynchronous t
        gnus-use-cache t
        gnus-use-header-prefetch t
        ;; search with generalized query syntax:
        ;; from:fulano body:"multi word" attachment:pdf
        gnus-search-use-parsed-queries t
        ;; search all groups in a server when constructing thread
        ;; Sent mails are in another group, this allows to find my sent messages when creating threads.
        ;; And, no need to GCC my sent messages to the inbox.
        gnus-refer-thread-use-search t)

  ;; Setting keybindings after evil-collection
  ;; because general uses `after-load-functions' and evil-collection uses `eval-after-load'
  (general-def 'normal gnus-article-mode-map "SPC" nil)
  (general-def 'normal gnus-article-mode-map "s" nil) ; use for isearch
  (general-def 'normal gnus-group-mode-map "s" nil) ; use for isearch
  (general-def 'normal gnus-summary-mode-map "C-q" 'gnus-summary-expand-window) ; close current article been viewed
  (general-def 'normal gnus-summary-mode-map "TA" 'gnus-summary-refer-thread))

;; Set parameter for each group
;; use email (and smpt server) according to group
(use-package gnus-group-parameters
  :elpaca nil
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
                            (signature "Nasser Alkmim \n +43 677 6408 9171")
                            ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 nasser.alkmim@gmail.com")))
                          ("work"
                           ;; Messages (emails) are GCC to the this group so it collects my sent mails on the server
                           ;; Gmail does not need this, for some reason.
                           (gcc-self "nnimap+work:Sent Items")
                           (posting-style
                            (address "Nasser Alkmim <nasser.alkmim@uibk.ac.at>")
                            (signature-file "/home/nasser/SeaDrive/My Libraries/documents/signature")
                            ("X-Message-SMTP-Method" "smtp smtp.uibk.ac.at 587 c8441205")))))

  ;; So my own messages are not considered new
  (setq gnus-gcc-mark-as-read t))

;; Database for email completion
;; sometimes does not add automatically: use `bbdb-mua-display-sender'
(use-package bbdb
  :after (:any gnus message)
  :config
  (bbdb-initialize 'gnus 'message)
  ;; auto update database based on messages I read
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq bbdb-mua-auto-update-p 'create   ; create contact if it does not exist
        ;; suppress pop up contact list when new contact is created
        bbdb-mua-pop-up nil
        bbdb-message-try-all-headers t  ; try all headers from a message to extract an email
        )

  ;; use capf to provide completion
  (setq bbdb-complete-mail nil
        bbdb-completion-list nil))

(use-package dianyou
  :after gnus
  :general
  ('normal gnus-summary-mode-map "&" 'dianyou-email-view-in-web-ui)
  ('normal gnus-article-mode-map "&" 'dianyou-email-view-in-web-ui))

;; Implements a complete at point function for BBDB database
;; https://github.com/minad/cape/pull/50/commits
(use-package cape-bbdb
  :after cape
  :elpaca nil
  :hook
  (message-mode . bbdb-setup-cape)
  :init
  (declare-function bbdb-records "bbdb")
  (declare-function bbdb-record-field "bbdb")

  (defvar cape--bbdb-properties
    (list :annotation-function (lambda (_) " BBDB")
          :company-kind (lambda (_) 'text)
          :exclusive 'no)
    "Completion extra properties for `cape-bbdb'.")

  (defvar cape--bbdb-records nil)
  (defun cape--bbdb-records ()
    "BBDB records formated like FIRSTNAME LASTNAME <email@example.com>."
    (or cape--bbdb-records
        (setq cape--bbdb-records
              (mapcar #'cape--bbdb-record-format (bbdb-records)))))

  (defun cape--bbdb-record-format (record)
    "Formats a BBDB record into a string like FIRSTNAME LASTNAME <email@example.com>."
    (format "%s %s"
            (bbdb-record-field record 'name)
            (apply #'concat
                   (mapcar (lambda (e) (concat "<" e ">"))
                           (bbdb-record-field record 'mail)))))

  (defun cape-bbdb (&optional interactive)
    "Complete name from BBDB and insert with email.
If INTERACTIVE is nil the function acts like a Capf."
    (interactive (list t))
    (if interactive
        (cape-interactive #'cape-bbdb)
      (let ((bounds (cape--bounds 'word)))
        `(,(car bounds) ,(cdr bounds)
          ,(cape--table-with-properties (cape--bbdb-records) :category 'cape-bbdb)
          ,@cape--bbdb-properties))))

  ;; add to top of `capf` list only in mail buffer with a hook with this function
  (defun bbdb-setup-cape ()
    (setq-local completion-at-point-functions
              (cons #'cape-bbdb
                    completion-at-point-functions))))

;; there is no language server for it yet
(use-package chapel-mode :disabled
  :mode ("\\.chpl\\'" . 'chapel-mode))

;; interact with zotero with 'zotra-cli'
;; add bibtex entry from doi
;; works, but does not download/rename pdf automatic
(use-package zotra :disabled
  :elpaca (zotra :type git :host github :repo "mpedramfar/zotra")
  :commands (zotra-add-entry-from-search)
  :config
  (setq zotra-cli-command "node ~/.local/src/zotra-cli/bin/index.js"))

;; org backend export to reveal.js
;; need to install external 'reveal.js'
(use-package ox-reveal
  :after org
  :demand  ; require after org
  :config
  (setq org-reveal-root "~/.local/src/reveal.js/"))

;; Highlight current line
(use-package hl-line :disabled          ; makes a bit slow
  :elpaca nil
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  :config
  ;; only active window
  (setq hl-line-sticky-flag nil))

(use-package isearch
  :elpaca nil
  :general
  (isearch-mode-map "C-n" 'isearch-repeat-forward)
  (isearch-mode-map "C-p" 'isearch-repeat-backward)
  (isearch-mode-map "C-s" 'isearch-toggle-invisible)
  :config
  ;; after evil collection
  (with-eval-after-load 'evil
   (general-def 'normal "s" 'isearch-forward))
  (general-def 'normal Info-mode-map "s" nil)               ; use isearch in info-mode
  :init
  ;; show the matching count
  (setq isearch-lazy-count t
        ;; make a whitespace work as regex ".*" which represents "anyting in between"
        ;; behavior similar to orderless
        search-whitespace-regexp ".*"))

;; highlight code in eww
(use-package shr-tag-pre-highlight
  :after shr
  :demand
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

;; https://panadestein.github.io/emacsd/#org602eb51
;; add indentation, background color https://github.com/chenyanming/shrface#hacking-the-shr-tag-pre-highlightel
(use-package shr-tag-pre-highlight-hack :disabled
  :elpaca nil
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
  :elpaca nil)

;; translation package
(use-package go-translate
  :general
  ("C-h t" 'gts-do-translate)     ; overrides the tutorial, but ok...
  ('(normal visual) "SPC t" 'gts-do-translate)     ; overrides the tutorial, but ok...
  :config
  (setq gts-translate-list '(("de" "en") ("de" "pt") ("pt" "en"))
        gts-default-translator (gts-translator
                                ;; pick directly
                                :picker (gts-noprompt-picker)
                                :engines (list
                                          ;; (gts-google-engine :parser (gts-google-summary-parser))
                                          (gts-deepl-engine :auth-key
                                                            (funcall
                                                             (plist-get (car (auth-source-search :host "api-free.deepl.com"))
                                                                        :secret)))
                                          (gts-google-rpc-engine))
                                :render (gts-buffer-render)
                                :splitter (gts-paragraph-splitter))))

;; custom function to connect to vpn
(use-package connect-vpn
  :elpaca nil
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
  :elpaca nil
  :general
  ("<f8>" (lambda ()
            (interactive)
            (find-file "/home/nasser/SeaDrive/My Libraries/notes"))))

;; Sync between google calendar and org mode.
;; It is a bit tricky to set up
;; create a project here https://console.developers.google.com/project
;; setup consent in the API & Services -> Credentials, then get the id/secrect
;; in API & Services -> Library: enable Calendar API
(use-package org-gcal
  :after org
  :commands org-gcal-sync
  :hook
  (org-agenda-finalize . (lambda () (org-gcal-sync)))
  :demand
  :init 
  (let ((id (plist-get (nth 0 (auth-source-search :host "gcal")) :user))
        (secret (funcall (plist-get (nth 0 (auth-source-search :host "gcal")) :secret))))
    (setq org-gcal-client-id id
          org-gcal-client-secret secret
          org-gcal-fetch-file-alist '(("nasser.alkmim@gmail.com" .  "~/SeaDrive/My Libraries/notes/log-notes/gcal.org"))))
  ;; stores OAuth token
  (setq-default
   oauth2-auto-plstore "/home/nasser/SeaDrive/My Libraries/secrets/oauth2-auto.plist"
   ;; It is asking for password every time.
   ;; https://github.com/kidd/org-gcal.el/issues/217 related: password has not
   ;; been stored in 'plstore-passphrase-alist'
   plstore-cache-passphrase-for-symmetric-encryption t
   ;; This solves the issue apparently, but you need to type the password and
   ;; not rely on the system saving in password manager.
   ;; After a while you have to allow on the browser again, so it is not a final solution.
   ;; https://github.com/kidd/org-gcal.el/issues/227 describes better the problem
   epg-pinentry-mode 'loopback))

;; Setup template for capture gcal 
(use-package org-capture-template
  :elpaca nil
  :general
  ("C-c c" 'org-capture)
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
  (setq org-default-notes-file "~/SeaDrive/My Libraries/notes/log-notes/tasks.org")

  (setq org-capture-templates '(;; 'Event' is something that happens in a time, it takes a timestamp.
                                ("e" "Event" entry ; type entry creates a headline
                                 (file+datetree "~/SeaDrive/My Libraries/notes/log-notes/gcal.org")
                                 "* %?\n%a")
                                ;; 'Task' is a 'TODO' entry and is scheduled,
                                ;; therefore it is shown continuously until
                                ;; marked as done.
                                ;; The '%^{SCHEDULED}p' prompts for (1) a
                                ;; scheduled value, useless, (2) the date, with
                                ;; the calendar, which is nice, and (3) another
                                ;; value, useless as well.
                                ;; Seen here: https://emacs.stackexchange.com/a/53586
                                ;; Would be nice to just have the date with calendar prompt.
                                ;; This is a solution: https://emacs.stackexchange.com/a/72326
                                ("t" "Task" entry
                                 (file+datetree "~/SeaDrive/My Libraries/notes/log-notes/gcal.org")
                                 "* TODO %?\n%^{SCHEDULED}p\n%a"))))

;; Templates that can be used as 'capf'
(use-package tempel
  :after corfu
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf))

(use-package tempel-collection
  :after tempel)

;; Link to org commits
(use-package orgit
  :after org)

(use-package orgit-forge
  :after org)

;; Stackexchange mode for emacs
(use-package sx
  :general
  ("M-<f12>" 'sx-search)
  (sx-question-list-mode-map "j" 'sx-question-list-next)
  (sx-question-list-mode-map "k" 'sx-question-list-previous)
  :custom-face
  (sx-question-mode-content-face ((t (:background unspecified))))
  :hook
  (sx-question-mode . variable-pitch-mode)
  :config
  (evil-set-initial-state 'sx-question-list-mode 'emacs)
  ;; use the same 'question list' buffer to show the question
  (setq sx-question-mode-display-buffer-function 'switch-to-buffer))

;; Drawing link support in 'org-mode'
;; It is not working to export to latex
(use-package el-easydraw
  :elpaca (el-easydraw :type git :host github :repo "misohena/el-easydraw")
  :after org
  :init
  ;; load the org setup after 'ox', which works when exporting in the bg
  (require 'edraw-org)
  (edraw-org-setup-default)
  (setq edraw-editor-default-grid-visible nil
        edraw-editor-default-tool 'freehand
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
                                                  (stroke . "#000")
                                                  (stroke-width . 1)
                                                  (fill . "none"))))

;; Git annotations
(use-package blamer :disabled           ; problem with it showing in 'org-mode', when I don't enabled 'blamer-mode' in it.
  :hook
  (prog-mode . blamer-mode)
  :config
  (setq blamer-commit-formatter ": %s"
        blamer-force-truncate-long-line t ; don't wrap lines
        blamer-idle-time 2))

(use-package cmake
  :elpaca nil
  :mode ("\\CMakeLists.txt\\'" . cmake-ts-mode))

;; Chat gpt client
(use-package gptel :disabled
  :elpaca (gptel :type git :host github :repo "karthink/gptel")
  :commands gptel
  :general
  ('visual "C-c g" 'gptel-send-menu)
  :config
  (setq gptel-api-key (funcall
                       (plist-get (car (auth-source-search :host "api.openai.com"))
                                  :secret))))

;; Alternative to 'mail-mode' and preferred mode for 'gnus'
(use-package message
  :elpaca nil
  :hook
  ;; disable autofill mode on emails
  (message-mode . (lambda () (auto-fill-mode -1))))

;; fold for '.xml' with 'hide-show'
;; https://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
(use-package xml-hs-fold
  :elpaca nil
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
  :elpaca (jinx :host github :repo "minad/jinx"
                :files (:defaults "*.c"))
  :hook
  (prog-mode . jinx-mode)
  (text-mode . jinx-mode)
  :general
  ("C-," 'jinx-correct)
  ("M-," 'jinx-correct)
  :config
  (setq jinx-languages "en de pt_BR"
        jinx-delay 1))

;; Change default compile command
(use-package compile
  :elpaca nil
  :general
  ([remap compile] '(lambda ()
                       (interactive)
                       (let ((current-prefix-arg '(4)))
                         (call-interactively 'compile))))
  :init
  (setq compile-command "make -k -C ../build"))

(use-package cape-yasnippet
  :elpaca (cape-yasnippet :host github :repo "elken/cape-yasnippet")
  :after yasnippet corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

(use-package ediff
  :elpaca nil
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
(use-package dictionary
  :elpaca nil
  :general
  ("C-h l" 'dictionary-lookup-definition) ; search for word at a point
  :config
  (setq
   dictionary-server "dict.org"         ; instead of a server, use this as default 
  ;; use just one buffer, please.
   dictionary-use-single-buffer t))

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

;; Display information on side of the buffer
(use-package sideline :disabled         ; too much noise, not useful information
  :hook
  (prog-mode . sideline-mode)
  :config
  (setq sideline-backends-skip-current-line t  ; don't display on current line
        sideline-order-left 'down              ; or 'up
        sideline-order-right 'up               ; or 'down
        sideline-format-left "%s   "           ; format for left aligment
        sideline-format-right "   %s"          ; format for right aligment
        sideline-priority 100                  ; overlays' priority
        sideline-display-backend-name t))      ; display the backend name

(use-package sideline-eldoc :disabled
  :elpaca (sideline-eldoc :host github :repo "ginqi7/sideline-eldoc")
  :after sideline
  :diminish sideline-mode
  :demand
  :config
  (add-to-list 'sideline-backends-right '(sideline-eldoc . up)))

(use-package sideline-blame :disabled
  :elpaca (sideline-blame :host github :repo "emacs-sideline/sideline-blame")
  :after sideline
  :demand
  :config
  (add-to-list 'sideline-backends-right '(sideline-blame . down)))

;; ChatGTP client that integrates with 'org-mode'
(use-package chatgpt-shell :disabled
  :elpaca (chatgpt-shell :host github :repo "xenodium/chatgpt-shell")
  :config
  ;; Maybe use a lambda to prevent password prompt
  (setq chatgpt-shell-openai-key (auth-source-pick-first-password :host "api.openai.com"))
  :after org
  :init
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)
  ;; change header arguments, remove "raw"
  (setq org-babel-default-header-args:chatgpt-shell '((:version . nil)
                                                        (:preface . nil)))
  (require 'ob-dall-e-shell)
  (ob-dall-e-shell-setup))

;; Experimental breadcrumb mode based on imenu
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58431#28
(use-package breadcrumb
  :elpaca (breadcrumn :host github :repo "joaotavora/breadcrumb")
  :hook
  (prog-mode . breadcrumb-mode)
  (org-mode . breadcrumb-mode))

(use-package circadian :disabled
  :defer 1
  :config
  (setq circadian-themes '(("7:00" . modus-operandi-tinted)
                           ("20:00" . modus-vivendi-tinted)))
  (circadian-setup))

(use-package pueue
  :elpaca (pueue :host github :repo "xFA25E/pueue")
  :hook (pueue-mode . auto-revert-mode)
  :commands pueue
  :general 
  ('normal "<f7>" 'pueue)
  ('normal pueue-mode-map "?" 'pueue-help)
  ('normal pueue-mode-map "m" 'pueue-mark))

;; Matchs the cursor color when running emacs in terminal
;; makes it much more visible, but it does not change the foreground when over the text as in the GUI
(use-package term-cursos-color
  :elpaca (term-cursos-color :host github :repo "CyberShadow/term-cursor-color")
  :if (not (display-graphic-p))
  :init
  (term-cursor-color-mode))

;; AI code completion based on "huggin face" api
(use-package starhugger :disabled
  :elpaca (starhugger :url "https://gitlab.com/daanturo/starhugger.el")
  :general
  ("M-/" 'starhugger-trigger-suggestion)
  :config
  ;; Use https://huggingface.co/bigcode/starcoderplus instead
  (setq starhugger-model-api-endpoint-url
        "https://api-inference.huggingface.co/models/bigcode/starcoderplus")

  (setq starhugger-api-token (funcall
                              (plist-get (car (auth-source-search :host "api.huggingface.com"))
                                         :secret)))

  ;; `starhugger-inline-menu-item' makes a conditional binding that is only active at the inline suggestion start
  (keymap-set starhugger-inlining-mode-map "TAB" (starhugger-inline-menu-item #'starhugger-accept-suggestion))
  (keymap-set starhugger-inlining-mode-map "M-[" (starhugger-inline-menu-item #'starhugger-show-prev-suggestion))
  (keymap-set starhugger-inlining-mode-map "M-]" (starhugger-inline-menu-item #'starhugger-show-next-suggestion))
  (keymap-set starhugger-inlining-mode-map "M-f" (starhugger-inline-menu-item #'starhugger-accept-suggestion-by-word))

   ;; for evil users, dismiss after pressing ESC twice
  (defvar my-evil-force-normal-state-hook '())
  (defun my-evil-run-force-normal-state-hook-after-a (&rest _)
    (run-hooks 'my-evil-force-normal-state-hook))

  (defun my-starhugger-inline-mode-h ()
    (add-hook 'my-evil-force-normal-state-hook
              (lambda () (starhugger-dismiss-suggestion t))
              nil t))
  (add-hook 'starhugger-inlining-mode-hook #'my-starhugger-inline-mode-h))

(use-package combobulate
  :elpaca (combobulate :url "https://github.com/mickeynp/combobulate")
  :general
  ('normal combobulate-key-map :prefix "z"
           "k" 'combobulate-navigate-beginning-of-defun
           "j" 'combobulate-navigate-end-of-defun)
  ('normal combobulate-key-map
           "M-k" 'combobulate-navigate-logical-previous
           "M-j" 'combobulate-navigate-logical-next)
  :hook
  (python-ts-mode . combobulate-mode)
  (c++-ts-mode . combobulate-mode)
  (yaml-ts-mode . combobulate-mode))

;; use same frame for speedbar
(use-package sr-speedbar
  :elpaca (sr-speedbar :url "https://www.emacswiki.org/emacs/sr-speedbar.el")
  :demand
  :general
  ('normal "g s" 'sr-speedbar-toggle))

;; copy from emacs terminal
(use-package clipetty
  :if (not (display-graphic-p))
  :general
  ('(normal visual) "Y" 'clipetty-kill-ring-save))

(use-package vertical-divider-term :disabled ; not reliable
  :elpaca nil
  :if (not (display-graphic-p))
  :init
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│)))

;; Add syntax highlight to Magit diffs
;; need to install 'yay git-delta'  
;; maybe too slow
(use-package magit-delta :disabled
  :hook (magit-mode . magit-delta-mode))

;; Add text-mode to files without format extension
(use-package custom-files-auto-mode
  :elpaca nil
  :init
  ;; files that end with ".out"
  ;; "\'" matches end of a string
  ;; "$" matches emptry string before a newline
  ;; "\." matches a period (escaped)
  (add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode)))

(use-package tab-bar
  :elpaca nil
  :defer 1
  :config
  (tab-bar-mode))

(use-package xref
  :elpaca nil
  :config
  ;; Need to set both
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function 'xref-show-definitions-completing-read))

;; Show guides on indentation level
(use-package indent-bars
  :if (display-graphic-p)
  :elpaca (indent-bars :url "https://github.com/jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-pattern "."
        indent-bars-width-frac 0.1
        indent-bars-highlight-current-depth '(:width 0.25)))

(use-package docview
  :elpaca nil
  :general
  ('normal doc-view-mode-map "j" 'doc-view-scroll-up-or-next-page)
  ('normal doc-view-mode-map "k" 'doc-view-scroll-down-or-previous-page))

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
