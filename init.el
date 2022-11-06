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
(setq use-package-verbose t		; don't print anything
      use-package-compute-statistics nil; compute statistics about package initialization
      use-package-minimum-reported-time 0.001
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
  ('normal "gy" 'revert-buffer-quick)
  ('insert "C-v" 'yank)                 ; for helping in minibuffer..
  ("C-<tab>" 'next-window-any-frame)
  ("<backtab>" 'previous-window-any-frame)
  ("C-x C-M-e" 'pp-macroexpand-last-sexp)
  ("C-x C-e" 'eval-defun)
  ("C-x e" 'eval-last-sexp)
  ("C-h j" 'describe-keymap)
  :init
  (global-hl-line-mode t) ; highlight current line
  (winner-mode t)	  ; move between windows configuration
  (setq-default fill-column 80)	  ; column length
  (column-number-mode t)  ; show column number in the mode line
  (setq-default indicate-empty-lines nil) ; cleaner

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
  )

(use-package emacs
  :disabled
  :if (string-greaterp emacs-version "29") ; need emacs > 29
  :init
   ; text scroll pixel by pixel
  (pixel-scroll-precision-mode t))

;; custom emacs theme
(use-package seralk-theme
  :straight nil
  :when (display-graphic-p)
  :init
  (load-theme 'seralk t))

;; typeface
(use-package custom-typefaces
  :straight nil
  :init
  ;; to check typefaces: fc-list | grep <typeface>
  ;; victor mono: thin, condensed, italics is informal, oblique (is slanted)
  ;; fira code: ligatures
  ;; (set-face-attribute 'default nil :family "Victor Mono")
  ;; (set-face-attribute 'italic nil :family "Victor Mono" :slant 'oblique :weight 'medium)
  ;; (set-face-attribute 'fixed-pitch nil :family "Victor Mono")
  ;; (set-face-attribute 'variable-pitch nil :family "Input Sans")
  :custom-face 
  ;; outline 4 inherits from comment face... make it oblique instead of italic
  (outline-4 ((t (:inherit font-lock-comment-face :slant oblique))))
  (tree-sitter-hl-face:property ((t (:inherit font-lock-comment-face :slant oblique)))))

;; change typeface size font
(use-package emacs-frame-zoom
  :straight nil
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
           (scale (plist-get org-format-latex-options :scale))
           (new-scale (* scale 1.1)))
      ;; change size of images on org buffers
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (string-equal major-mode "org-mode")
            (org-zoom-inline-images))))
      (set-frame-font (font-spec :size new-size) t t)
      ;; latex preview
      (setq org-format-latex-options (plist-put org-format-latex-options :scale new-scale))))

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
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1))))

;; controls the behavior of windows
(use-package emacs-display-windows
  :disabled
  :straight nil
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
  (setq recentf-max-saved-items 25
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
  :straight (vertico :type git :host github :repo "minad/vertico"
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-reverse
                                vertico-flat
                                vertico-repeat
                                vertico-unobtrusive
                                vertico-grid
                                vertico-multiform)
                     :files (:defaults "extensions/*"))
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
  :straight nil
  :after vertico
  :general
  (vertico-map "C-<tab>" 'vertico-multiform-reverse)
  :init
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-outline unobtrusive)
          (consult-line unobtrusive)
          (consult-buffer unobtrusive)
          ;; (dtache-open-session buffer   ; open dtache sessions in a buffer
          ;;                      (vertico-buffer-display-action . (display-buffer-at-bottom
          ;;                                                        (window-height . 13))))
          )))

;; `completion STYLE` with flexible candidate filtering
;; filter with space-separated components and match components in any order
;; filter means how a input string is matched against candidates
(use-package orderless
  :demand
  :config
  ;; partial completion for files to allows path expansion
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        read-file-name-completion-ignore-case t
        completion-category-overrides '((file (styles . (partial-completion)))
                                        ;; navigate files with initials
                                        (minibuffer (initials))))

  ;; components ending in "~"  match in flex style
  ;; flex style: keep order, but not necessarily consecutive
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun literal-if-equal (pattern _index _total)
    (when (string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1))))

  ;; component start with "!" filter by string not in the candidate
  ;; useful to:
  ;; 1. exclude pattern from `consult-focus`
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  ;; match components based on regex
  ;; components separated by space
  (setq orderless-matching-styles '(orderless-regexp)
        orderless-style-dispatchers '(flex-if-twiddle
                                      literal-if-equal
                                      without-if-bang)))

;; save the search history
(use-package savehist
  :init
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
  ("C-s" 'consult-line)			; search lines with preview
  ("C-S-s" 'consult-focus-lines)	; show only matching results
  ("C-c m" 'consult-mark)
  ;; two parts: search  and filter
  ;; #<search string>#<filter terms> filtering with orderless! amazing!
  ;; command line can be specified after "--", example: #<search string> -- -C 10 for context!! WHAT!
  ("C-c r" 'consult-ripgrep)		; search file contents
  ("C-c f" 'consult-find-fd)		; search files in directories
  ;; (minibuffer-local-completion-map "<tab>" 'minibuffer-force-complete)
  :hook
  ;; hook for using default completion mode
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; configure preview behavior to `any` key and specific time delay
  (consult-customize consult-buffer consult-bookmark
                     :preview-key '(:debounce 5 any))
  ;; use a key for preview from ripgrep and find
  (consult-customize consult-ripgrep consult-find-fd
                     :preview-key (kbd "M-."))
  (consult-customize consult-line :preview-key '(:debounce 0 any))

  ;; use 'fd' instead of 'find'
  (defun consult-find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))

  ;; set project root from vc.el
  ;; available when a project file is visited (project-switch-project)
  ;; #'vc-root-dir ; using current folder as default
  ;; added --no-ignore-vcs to avoid skipping files in gitignore
  (setq consult-project-root-function nil
        consult-ripgrep-args 
        "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --no-ignore-vcs .")

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
  (consult-customize consult-outline :keymap my-consult-outline-map))

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
  :straight (:includes embark-org :files (:defaults "embark-org.el"))
  ;; :demand                               ; load it independently of bind and hook
  :general
  ("M-a" 'embark-act)
  ("C-S-z" 'embark-dwim)
  ("C-h B" 'embark-bindings)
  (embark-function-map "h" 'helpful-symbol)
  (embark-variable-map "h" 'helpful-symbol)
  :commands embark-prefix-help-command
  :hook
  ;; use embark to help find command after prefix (C-h after a prefix key)
  ;; if this is used before `embark-act`, load the package (because of `:commands`)
  ;; `which-key` changes this variable as well, so we need to change it after `which-key`
  (which-key-mode . (lambda ()
                      (setq prefix-help-command #'embark-prefix-help-command)))
  :config
  ;; hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; allows consult previews as you move around an auto-updating embark collect
;; buffer
;; `exbark-collects` grep results to a grep buffer
(use-package embark-consult
  :demand				;necessary for consult preview
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :after (embark consult))

;; targets for org mode
(use-package embark-org
  :after embark
  :demand)

;; utility for using icons fonts
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 0.8))

;; automatic insert matching pairs and navigation
;; highlight matching parens
;; for wrap/unwrap I use evil-surround
;; expand/contract (slurp) is good for elisp
(use-package smartparens
  :diminish smartparens-mode
  :straight (:includes smartparens-config)
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
  ;; (org-mode . smartparens-mode) ; messing with org-mode
  ;; (smartparens-mode . smartparens-strict-mode) ; enforce pairs to be balanced
  (smartparens-mode . show-smartparens-mode) ; instead of default show-paren-mode
  :config
  (setq sp-show-pair-delay 0.125
        sp-max-prefix-length 25         ; reduces work
        sp-max-pair-length 4            ; reduces work
	sp-show-pair-from-inside t))

(use-package smartparens-config
  :straight nil
  :demand
  :after smartparens
  :config
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p)))

(use-package flymake
  :hook (python-mode . flymake-mode)
  :general
  (flymake-mode-map "M-n" 'flymake-goto-next-error) 
  (flymake-mode-map "M-N" 'flymake-goto-prev-error)
  :config
  ;; delay check, check only on save
  (setq flymake-no-changes-timeout 1)
  ;; avoid warning in the flymake log
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; flymake just for C++ in org edit special
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/Example_002d_002d_002dConfiguring-a-tool-called-directly.html
(use-package flymake
  :disabled                             ;not working with org edit special
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

  (advice-add #'evil-multiedit-match-and-next :around #'make-evil-multiedit-case-sensitive))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode
  :general
  ;; autoload keymap, will trigger the loading of `evil-mc` library
  ;; use prefix for `cursors-map` from evil collection
  ('(normal visual) "g ." '(:keymap evil-mc-cursors-map))
  :config
  (global-evil-mc-mode 1)
  ;; extra commands for multiple cursts
  (push '(evil-org-delete . ((:default . evil-mc-execute-default-evil-delete))) evil-mc-known-commands)
  (push '(org-yank . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(evil-paste-before . ((:default . evil-mc-execute-default-evil-paste))) evil-mc-known-commands)
  (push '(evil-org-beginning-of-line . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(evil-digit-argument-or-evil-org-beginning-of-line . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(sp-forward-sexp . ((:default . evil-mc-execute-default-call))) evil-mc-known-commands)
  (push '(evil-surround-change . ((:default . evil-mc-execute-default-evil-surround-region))) evil-mc-known-commands))

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
  ('normal "C-S-o" 'evil-jump-forward)
  :config
  (setq
   lazy-highlight-cleanup nil
   lazy-highlight-max-at-a-time nil
   evil-kill-on-visual-paste nil        ; don't add replaced test onto kill ring
   lazy-highlight-initial-delay 0)

  (evil-set-undo-system 'undo-redo)	; use native redo function

  (add-to-list 'evil-insert-state-modes 'log-edit-mode)
  
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
  :disabled
  :defer 1
  :after evil
  :config
  (evilem-default-keybindings "SPC"))

;; move aroud text
(use-package evil-snipe
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode)
  :general
  ('normal "f" 'evil-snipe-f
           "t" 'evil-snipe-s)
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible
        evil-snipe-smart-case t)
  ;; "f [" goes to parenthesis or bracket
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  (push '(?\] "[]})]") evil-snipe-aliases)
  ;; (evil-snipe-def 2 inclusive "t" "T")  ;define t to be like s
  )

;; visualize evil commands
(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :defer 1
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.8        ; show what I copied
        evil-goggles-blocking-duration 0) ; don't want to wait when deleting
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
;; promoting/demoting headings: M-hjkl
;; headings: M-ret
(use-package evil-org
  :straight (:includes evil-org-agenda)
  :diminish evil-org-mode
  :after evil org
  :general
  ('normal org-mode-map "x" 'evil-delete-char)
  ('normal org-mode-map "z b" 'evil-scroll-line-to-bottom)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
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

;; jump to matched tags
(use-package evil-matchit
  :disabled
  :after python evil
  :config
  (global-evil-matchit-mode 4))

(use-package beacon
  :defer 1
  :diminish beacon-mode
  :config
  (setq beacon-blink-delay 0)
  (setq beacon-size 40)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-duration .8)
  (setq beacon-blink-when-window-scrolls nil)
  (beacon-mode 1))

(use-package undo-propose
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
  :config
  (general-def magit-section-mode-map "C-<tab>" nil)
  (general-def 'normal magit-section-mode-map "C-<tab>" nil)
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

(use-package org-contrib)

(use-package org
  :diminish org-indent-mode
  :mode (("\\.org$" . org-mode))
  :general
  ("C-c s i" 'org-insert-link-global)   ; e.g. to insert link to pdf in a latex buffer
  (org-mode-map "C-c C-l" 'org-insert-link)
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
  (org-mode . org-indent-mode)          ; align with heading
  :config
  (setq org-hide-emphasis-markers nil        ; avoid noisy //,__, **(makes annoying to edit) 
        org-startup-indented nil		; start collapsed
        org-startup-folded t               ; folded in "overview" state
        org-hide-leading-stars t           ; don't show a  bunch of '*'
        org-edit-src-content-indentation 0
        org-pretty-entities nil           ; use latex instead...
        org-ellipsis "…"                ;use single character for elipses
        org-outline-path-complete-in-steps nil
        org-special-ctrl-a/e t       ; when jump to beginning of line be aware of *
        org-cycle-separator-lines 0  ; no empty lines between headings
        org-fontify-quote-and-verse-blocks t ; yes syntax highlighting
        org-insert-heading-respect-content nil ; nil: heading after current line/ t: after current subtree
        org-catch-invisible-edits 'show-and-error ;make visible then abort
        org-tags-column 0                        ; tag right after text
        org-html-htmlize-output-type 'inline-css   ; nil to export as plain text
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
  (setq org-todo-keywords '((sequence "TODO" "PROG" "DONE"))
        org-todo-keyword-faces '(("PROG" . (:foregroud "blue")))))

;; bug when display image using :dir
;; https://lists.gnu.org/archive/html/emacs-orgmode/2021-04/msg00246.html
(use-package org-display-inline-image-hack
  :straight nil
  :after org
  :init
  ;; (remove-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  ;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'append)

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

;; problem with babel execute subtree and showing images outside the subtree
(use-package org-display-inline-image-after-execute-subtreee
  :straight nil
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
  :straight nil
  :after org
  :init
  (setq org-display-remote-inline-images 'download))

;; org export
(use-package ox
  :straight nil
  :after org
  :init
  (setq org-export-in-background t))

(use-package ox-extra
    :after org
    :demand
    :config
    (ox-extras-activate '(ignore-headlines)))

(use-package ox-html
  :straight nil
  :after org
  :config
  ;; don't scale svg images
  (setq org-html-head "<style> .org-svg {width: auto} </style>"))

;; load ob-C when executing C++ source block
(use-package ob-C
  :straight nil
  :after org
  :commands
  (org-babel-execute:C++
   org-babel-execute:C)
  :config
  (setq org-babel-default-header-args:C++
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:exports . "results")))
  ;; same for C
  (setq org-babel-default-header-args:C
        '((:results . "output")
          (:noweb . "no-export")
          (:eval . "never-export")
          (:exports . "results"))))

;; load ob-python only when executing python block
(use-package ob-python
  :straight nil
  :after org
  :commands org-babel-execute:python
  :init
  (setq org-babel-python-command "python3") ; python3 please!
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          (:exports . "results")))) ; export only plots by default

(use-package ob-julia
  :disabled
  :straight nil
  :after org
  :commands org-babel-execute:julia
  :init
  (setq org-babel-default-header-args:julia
        '((:results . "output")
          (:noweb . "no-export") ; referencing other blocks with <<>> syntax, don't expand during export
          (:eval . "never-export") ; don't eval blocks when exporting, except when `:eval yes`
          ;; add tag variable to all python blocks... maybe not ideal, but usefull
          (:exports . "results"))))

(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode))

(use-package julia-vterm)

(use-package ob-julia-vterm
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
  :straight nil
  :after org
  :general
  ('normal org-mode-map "g s" (general-simulate-key "C-u C-u C-c C-v C-t"))
  :init
  ;; mkdirp allows creating the :dir if it does not exist
  (add-to-list 'org-babel-default-header-args '(:mkdirp . "yes"))
  (add-to-list 'org-babel-default-header-args '(:noweb . "no-export")))

;; custom org function
(use-package org-image-resize-inline
  :straight nil
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
      (org-redisplay-inline-images)))

  (defun org-zoom-out-inline-images ()
    (interactive)
      (setq org-image-actual-width nil)
      (org-redisplay-inline-images)))

;; for windows
(use-package ob-python
  :straight nil
  :after org
  :when (eq system-type 'windows-nt)
  :init
  ;; windows uses python for versions > 3, argh... 
  (setq org-babel-python-command "python"))

(use-package org-clock
  :straight nil
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
  :straight nil
  :general
  ('normal org-mode-map "z e" 'org-edit-special)
  ('normal org-src-mode-map "z e" 'org-edit-src-exit)
  :after org
  :init
  ;; babel and source blocks
  (setq org-src-fontify-natively t
	org-src-window-setup 'current-window ; don't move my windows around!
	org-src-preserve-indentation t  ; preserve indentation in code
	org-adapt-indentation nil ; no extra space... better use indent mode (virtual)
        org-edit-src-content-indentation 0 ; dont indent source code
	org-src-tab-acts-natively t	; if t, it is slow!
	org-confirm-babel-evaluate nil)) ; doesn't ask for confirmation

(use-package org-agenda
  :straight nil
  :after org
  :init
  (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
				 "~/OneDrive/Org/notes.org"
				 "~/OneDrive/Org/journal.org"
				 "~/OneDrive/Org/gcal.org"))))

(use-package ox-latex
  :straight nil
  :if (eq system-type 'gnu/linux)
  :after org
  :init
  ;; change scale of latex preview in org-mode
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1)
	;; org-startup-with-latex-preview t
        org-latex-image-default-width nil ; don't scale my images!
        org-latex-images-centered t
        org-preview-latex-image-directory "~/.cache/ltximg/")

  ;; minted code pdf export org
  (setq org-latex-listings 'minted
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; extra latex packages for every header
  (setq org-latex-packages-alist '(("newfloat" "minted" nil)
                                   ("a4paper, margin=20mm" "geometry" nil)))

  (add-to-list 'org-latex-default-packages-alist '("colorlinks=true, linkcolor=blue, citecolor=blue, filecolor=magenta, urlcolor=cyan" "hyperref" nil)))

;; adds keyword `async' to source blocks
(use-package ob-async
  :after org
  :demand
  :init (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  :config   ; first load with demand, require `ob-async', then configure the variable
  ;; ob-python defines its own `async' keyword (which needs a session)
  (setq ob-async-no-async-languages-alist '("python")))

(use-package ob-shell
  :straight nil
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
  :straight nil
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

  ;; use different on wayland
  (when (string= system-name "nasser-t14s")
    (setq org-download-screenshot-method "flameshot gui --raw > %s")))

;; wsl specific config
(use-package org-download
  :after org-download
  :when (string-match "-[Mm]icrosoft" operating-system-release)
  :init
  ;; add .exe to work within wsl2
  (setq org-download-screenshot-method "convert.exe clipboard: %s"))

;; languages spell checker
(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :hook
  (text-mode . flyspell-mode)
  ;; (prog-mode . flyspell-mode) not in programming modes
  :config
  ;; husnpell is alternative to aspell
  (setq ispell-program-name "hunspell")	; dictionary /usr/share/hunspell
  ;; (ispell-set-spellchecker-params) ; makes initial load slow...

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

;; convenient functions for correcting
(use-package flyspell-correct
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-correct-wrapper)
  ('normal flyspell-mode-map "[ ," 'flyspell-correct-wrapper)
  :after flyspell)

;; attempt to make flyspell faster by restricting to region, instead of buffer
;; note: makes it slow when saving the buffer
(use-package wucuo
  :disabled ;; using vale
  :hook
  (text-mode . wucuo-start)
  (prog-mode . wucuo-start))

;; completion in region manually summoned with <tab> (no auto pop up)
;; allows space (separator M-SPC) between filter words (combined with oderless)
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu"
                   :files (:defaults "extensions/*"))
  :general
  (corfu-map "<tab>" 'corfu-next
	     "<backtab>" 'corfu-previous
	     "C-n" 'corfu-next
	     "C-p" 'corfu-previous)
  ('insert "C-n" nil
	   "C-p" nil)
  :defer 1
  :config
  (global-corfu-mode)
  (setq corfu-auto t                    ; enables timer-based completion
        corfu-auto-delay 0.2
	corfu-auto-prefix 1
	corfu-quit-no-match 'separator))

;; corfu extension
(use-package corfu-history
  :straight nil
  :after corfu
  :demand
  :config
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; `completion at point' extensions for specific candidates in `completion in region'
(use-package cape
  :straight (cape :type git :host github :repo "minad/cape")
  :after corfu
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-ispell))

(use-package corfu-doc
  :when (display-graphic-p)
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  :custom
  (corfu-doc-delay 0.5)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)
  (corfu-echo-documentation nil)
  :hook
  (prog-mode . corfu-doc-mode)) 

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

;; allows selectively display portions of program
;; bicycle uses it together with outline
(use-package hideshow
  :straight nil
  :diminish hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode))

;; folding
;; note: evil collection also sets a bunch of keybindings
(use-package outline
  :diminish outline-minor-mode
  :mode ("\\.inp\\'" . outline-minor-mode)
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
  ;; ('normal outline-mode-map "S-<tab>" 'outline-cycle)
  :config
  ;; need to rebind after loading outline
  ;; because general uses `after-load-functions' and evil-collection uses `eval-after-load'
  ;; evil-collection end up binding last...
  (general-def 'normal outline-mode-map "z k" 'outline-previous-visible-heading)
  (setq outline-minor-mode-cycle t
	;; outline-minor-mode-highlight 'append  ;; bug with C++ source block
        ))  

;; trying to make outline work with python docstring
(use-package outline-python
  :disabled
  :straight nil
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
        TeX-auto-save t   ; enable parse on save (to force parse use C-c C-n (TeX-normal-mode))
        TeX-parse-self t ; enable document parsing to get labels for completion
        TeX-PDF-mode t			; output pdf 
        TeX-electric-escape t
        TeX-insert-macro-default-style 'mandatory-args-only ; don't ask for optional argument afte "C-c m"
        TeX-master nil) ; make auctex aware of multi-file documents

  ;; start latex buffer folded
  ;; (add-hook 'find-file-hook 'TeX-fold-buffer t)
  
  ;; specific config
  (setq LaTeX-command "latex -shell-escape") ;; -shell-escape for minted (syntax highlight)
  
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
(use-package latex-fake-header
  :straight nil
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
(use-package reftex
  :disabled
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
  :straight (:type built-in)
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
  ('normal dired-mode-map "SPC" nil)
  ("C-x C-j" 'dired-jump-other-window)
  ("C-x j" 'dired-jump)
  (dired-jump-map "j" nil)             ; remove repeat with "j"
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$"
	dired-auto-revert-buffer t
	dired-listing-switches "-AGhlv --group-directories-first --time-style=long-iso"	; human readable format when in detail
	dired-kill-when-opening-new-dired-buffer t ; kill when changing dir
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
	delete-by-moving-to-trash nil)	; move to trash (problem with naming)

  ;; kill the dired buffer and enters the current line file or directory
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
  (setq dired-sidebar-one-instance-p t      ; just sidebar per frame
        dired-sidebar-use-magit-integration nil) ; open dwim, not parent git
  (defun dired-sidebar-jump ()
    (interactive)
    (dired-sidebar-show-sidebar)        ;show the side bar
    (dired-sidebar-toggle-with-current-directory) ; hide it and re opening with current dir
    (dired-sidebar-toggle-with-current-directory)
    ;; call dired again so it uses dirvish
    ;; not using dirvish now (problems with tram)
    ;; (dired "./")
    ))

;; improved dired
;; problem with tramp
(use-package dirvish
  :disabled
  :straight (dirvish :type git :host github :repo "alexluigit/dirvish")
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

;; load modus in terminal
(use-package modus-themes
  ;; :when (display-graphic-p)
  :custom-face
  ;; (org-meta-line ((t (:height 0.9))))
  ;; (org-drawer ((t (:height 0.9))))
  ;; (org-macro ((t (:height 0.9))))
  (org-verbatim ((t (:box t))))
  (font-latex-sectioning-1-face ((t (:weight bold :slant italic :box t))))
  (font-latex-sectioning-2-face ((t (:weight bold :box t))))
  (font-latex-sectioning-3-face ((t (:weight bold :underline t))))
  (font-latex-sectioning-4-face ((t (:weight bold :slant normal))))
  (font-latex-sectioning-5-face ((t (:weight normal :slant italic :underline t))))
  :config
  (setq modus-themes-org-blocks 'gray-background
	modus-themes-prompts '(intense italic)
	modus-themes-hl-line '(accented intense)
	modus-themes-diffs 'desaturated
	modus-themes-paren-match '(bold underline)
	modus-themes-no-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-syntax '(alt-syntax)
	modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-fringes 'subtle
        modus-themes-headings '((t . (rainbow)))
	modus-themes-mode-line '(borderless accented moody))

  ;; hook to enforce change when theme is toggled (which loads the theme)
  (add-hook 'modus-themes-after-load-theme-hook
            (lambda ()
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
                ;; use oblique version of Victor for italic
                (set-face-attribute 'italic nil :family "Victor Mono" :slant 'oblique :weight 'medium)
                ;; change for specific modes
                ;; and use the italic (informal) for comments
                ;; tree sitter does not work in terminal, apparently
                (when (display-graphic-p)
                  (eval-after-load 'tree-sitter-hl
                    '(set-face-attribute 'tree-sitter-hl-face:comment nil
                                         :family "Victor Mono" :slant 'italic :weight 'semibold)))
                (eval-after-load 'auto-dim-other-buffers
                  '(set-face-attribute 'auto-dim-other-buffers-face nil
                                       :foreground (modus-themes-color 'fg-dim)))
                (eval-after-load 'smartparens
                  '(set-face-attribute 'sp-show-pair-match-content-face nil
                                       :background (modus-themes-color 'bg-paren-expression))))))
  :general
  ("<f5>"  'modus-themes-toggle))

;; modus in terminal
(use-package modus-themes
  :unless (display-graphic-p)
  :init
  (modus-themes-load-operandi))

;; change backgroud of other windows
;; when with custom theme and GUI
(use-package highlight-current-window 
  :disabled
  :when (display-graphic-p)
  :straight nil
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
  :straight nil
  :diminish (buffer-face-mode))

;; syntax highlight in html export of org-mode source blocks
;; does not work well with modus-themes and tree-sitter
(use-package htmlize)

;; `:includes` so straight can recognize dap-python.el and dap-cpptools
(use-package dap-mode
  :disabled
  :after lsp-mode
  :straight (dap-mode :includes (dap-python dap-cpptools)
		      :type git
		      :host github
		      :repo "emacs-lsp/dap-mode") 
  :general
  (lsp-mode-map "<f6>" 'dap-hydra))

(use-package dap-cpptools
  :disabled
  :after dap-mode
  :demand)

(use-package dap-python
  :disabled
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

;; mode for C++
(use-package c++-mode
  :straight (:type built-in)
  :mode ("\\.cpp\\'" . c++-mode)
  :hook
  ;; guess the style based on guess
  (c++-mode . (lambda () (c-guess-buffer)))
  :general
  (c++-mode-map "C-x c" 'compile)
  :config
  ;; change style curly braces on their on line without offset
  ;; https://en.wikipedia.org/wiki/Indentation_style#Variant:_Linux_kernel
  ;; 
  ;; alternatively: to get style from `.clang-format' file
  ;; one first use `eglot-format' (which uses `clangd' server) that can read `.clang-format'
  ;; then `c-guess-buffer-no-install' to set it
  (setq c-default-style "linux"
        c-basic-offset 2)) 

(use-package cc-mode
  :straight (:type built-in)
  :mode ("\\.c\\'" . c-mode)
  :general
  (c-mode-map "C-x c" 'compile)
  :hook
  ;; use `//' for comments
  (c-mode . (lambda () (c-toggle-comment-style -1))))

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

  ;; flake8 combines pyflakes (error checker) with stylistic check against pep8 standards
  (setq python-flymake-command '("flake8" "-")
        python-check-command "/home/nasser/.local/bin/flake8"))

;; formatting python code
(use-package python-black
  :after python
  :commands python-black-buffer)

(use-package numpydoc
  :disabled
  :after python
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :general
  (python-mode-map "C-c C-n" 'numpydoc-generate))

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
  (prog-mode . hl-todo-mode)
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
        bibtex-completion-library-path "~/SeaDrive/My Libraries/bibliography/"
        bibtex-completion-pdf-open-function (lambda (fpath)
                                              (call-process "xdg-open" nil 0 nil fpath)))
  ;; dont prompt for anything, just insert the citation please.
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil))

;; using okular to at leat view the documents...
(use-package bibtex-completion-wsl
  :straight nil
  :when (string-match "-[Mm]icrosoft" operating-system-release)
  :after consult-bibtex
  :init
  (setq bibtex-completion-pdf-open-function (lambda (fpath)
                                              (call-process "okular" nil 0 nil fpath))))

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

;; bibtex completion add option for pdf view (one for annotation other for viewing)
(use-package consult-bibtex-annotation
  :straight nil
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
  :straight nil
  :after consult-bibtex
  :init
  (defun bibtex-completion-open-pdf-evince (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (call-process "evince" nil 0 nil fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))
  (consult-bibtex-embark-action consult-bibtex-open-pdf-evince bibtex-completion-open-pdf-evince)
  (define-key consult-bibtex-embark-map "p" #'consult-bibtex-open-pdf-evince))

(use-package citar
  :disabled
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
  :straight nil
  :after citar
  :disabled
  :init)

(use-package biblio
  :commands biblio-lookup)

(use-package server
  :straight (:type built-in)
  :demand
  :config
  (or (server-running-p)
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
  :diminish yas-minor-mode
  :hook
  (LaTeX-mode . yas-global-mode)
  (org-mode . yas-global-mode)
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
  ;; ("<f9>" 'vterm-other-window)
  (vterm-mode-map "<f9>" nil
                  "C-w" nil
                  "<backtab>" nil)
  :config
  (setq vterm-max-scrollback 20000
        vterm-timer-delay 0))

;; manage multiple vterm's buffers
(use-package vterm-toggle
  :general
  ("<f9>" 'vterm-toggle-cd) 	; opens term in current cd including remote
  (vterm-mode-map "s-n" 'vterm-toggle-forward
		  "s-p" 'vterm-toggle-backward))

;; multiple terminals
(use-package multi-vterm
  :general
  ("S-<f9>" 'multi-vterm)
  :commands multi-vterm)

(use-package terminal-here
  :general
  ('normal "C-<f7>" 'terminal-here-launch)
  :config
  ;; change terminal command
  (when (string= system-name "ryzen-ms7c37")
    (setq terminal-here-terminal-command 'gnome-terminal)))

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
  :defer 1
  :config
  ;; built-in command repeater (like hydra)
  (repeat-mode t))

;; built in windows resize functions
(use-package window
  :straight (:type built-in)
  :general
  ("C-c w" 'enlarge-window-horizontally)
  ("C-x C-o" 'other-window)
  (resize-window-repeat-map "j" 'shrink-window)
  (resize-window-repeat-map "w" 'shrink-window)
  (resize-window-repeat-map "k" 'enlarge-window)
  (resize-window-repeat-map "h" 'shrink-window-horizontally)
  (resize-window-repeat-map "l" 'enlarge-window-horizontally))

;; create backlinks when linking org-mode headings
(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links")
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
  :straight nil
  :after org
  :demand                               ; explicitly require org-id
  :init
  ;; automatic generate id for headings
  (setq org-id-link-to-org-use-id t)
  )

;; citations support in org-mode
(use-package oc
  :straight nil
  :after org
  :general
  (org-mode-map "C-c C-b" 'org-cite-insert)
  :config
  (setq org-cite-global-bibliography '("~/.bibliography.bib")))

;; highligh indentation
(use-package highlight-indent-guides
  :disabled
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; highlight based on scope
(use-package hl-indent-scope
  :straight (hl-indent-scope :type git :host nil :repo "http://codeberg.org/ideasman42/emacs-hl-indent-scope")
  :hook
  (prog-mode . hl-indent-scope-mode))

;; suppress error on tui
(use-package highlight-indent-guides
  :after highlight-indent-guides
  :unless (display-graphic-p)
  :init
  (setq highlight-indent-guides-suppress-auto-error t))

;; emacs built in version control
(use-package vc-git
  :straight (:type built-in)
  :general
  ("C-x v p" 'vc-push)
  :hook
  (diff-mode . outline-minor-mode)
  (vc-git-region-history-mode . outline-minor-mode))

;; manage remote files access and manipulations
;; to use without password, first create and copy the key to the remote
;; `ssh-keygen -b 4096'
;; `ssh-copy-id <remote-address>'
;; then add to `~/.ssh/config'
;;  Host <name>  
;;    HostName ip  
;;    IdentityFile path-to-ssh-key  
;;    User root
(use-package tramp
  :straight (:type built-in)
  :config
  ;; scp is faster than ssh for copying files
  (setq tramp-default-method "scp"
	tramp-verbose 4)
  ;; apparently makes it faster
  ;; https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh 
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

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
  (c++-mode . tree-sitter-mode)
  (c-mode . tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode))

;; langage bundle for `tree-sitter`
(use-package tree-sitter-langs
  :demand                               ; require it after loading tree-sitter
  :after tree-sitter)

;; use tree sitter as evil text objects
(use-package evil-textobj-tree-sitter
  :disabled
  :straight (evil-textobj-tree-sitter :type git
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
  :commands atomic-chrome-start-server
  :config
  (setq atomic-chrome-default-major-mode 'LaTeX-mode))

;; easily change windows
(use-package ace-window
  :general
  ('normal "C-w C-w" 'ace-window))

;; highlight parensthesis
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :hook (prog-mode . highlight-parentheses-mode))

;; moving cursor around fast and efficiently
(use-package avy
  :general
  ('normal "s" 'avy-goto-char-timer)
  ('(normal visual) :prefix "SPC"
           "j" 'avy-goto-line-below
           "k" 'avy-goto-line-above)
  :demand
  :config
  (setq avy-timeout-seconds 0.2         ; quicker
        avy-all-windows-alt t           ; allow all windows when `C-u`
        avy-all-windows nil))           ; restrict to one window

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

(use-package lsp-ltex
  :disabled
  :hook (LaTeX-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred))))

;; icons for completion candidates
;; fork adds support for M-x, imenu
(use-package all-the-icons-completion
  :straight (all-the-icons-completion :type git :host github :repo "MintSoup/all-the-icons-completion")
  :when (display-graphic-p)
  :after marginalia
  :defer 1
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; icons for dired
(use-package all-the-icons-dired
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

;; simple LSP client
;; alternative to lsp, too many dependencies
(use-package eglot
  :hook
  (python-mode . eglot-ensure) ; works if there is only one server available
  (eglot-managed-mode . (lambda ()
                          ;; https://old.reddit.com/r/emacs/comments/xq6rpa/weekly_tips_tricks_c_thread/
                          ;; re-enable flymake checkers because eglot clobbers
                          ;; them when starting
                          (when (derived-mode-p 'python-mode)
                            (add-hook 'flymake-diagnostic-functions 
                                      'python-flymake nil t))))
  ;; (LaTeX-mode . eglot-ensure) ; works if there is only one server available
  (c++-mode . eglot-ensure) ; works if there is only one server available
  (c-mode . eglot-ensure)
  :config
  ;; using a hook to 
  ;; (add-to-list 'eglot-stay-out-of 'flymake)     ; using own flymake command (flake8)

  ;; add watch mode "-w" for performance
  ;; pyright only reanalyze the files that have been modified
  (add-to-list 'eglot-server-programs
               `(python-mode "pyright-langserver" "--watch" "--stdio"))
  :general
  ('normal eglot-mode-map :prefix "gl"
           "l" 'eglot-code-actions
           "h" 'eldoc
           "f" 'eglot-format-buffer
           "r" 'eglot-rename))

;; trying
;; use mypy check for type in python
(use-package flymake-mypy
  :straight (flymake-mypy :type git :host github :repo "com4/flymake-mypy")
  :demand
  :hook ((eglot-managed-mode . (lambda ()
                 (when (derived-mode-p 'python-mode)
                   (flymake-mypy-enable))))))

;; add ltex language server to eglot
;; need to manually download the language server:
;; cd ~/.opt && https://github.com/valentjn/ltex-ls/releases/download/15.2.0/ltex-ls-15.2.0-linux-x64.tar.gz
;; tar -xvf ltex-ls-15.2.0-linux-x64.tar.gz
;; also need java (open JDK 11 development kit)
(use-package eglot-ltex
  :unless (string-match "-[Mm]icrosoft" operating-system-release) ; only in linux
  :straight (eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex")
  :commands start-eglot-ltex
  :init
  (setq eglot-languagetool-server-path "~/.opt/ltex-ls-15.2.0/")
  (defun start-eglot-ltex ()
    (interactive)
    (require 'eglot-ltex)
    (call-interactively #'eglot))
  :hook
  ;; when in latex don't use multiline minibuffer
  ;; avoid sily documentation in the minibuffer
  (eglot-managed-mode . (lambda ()
                          (setq eldoc-echo-area-use-multiline-p nil))))

;; use language tool with flymake
;; download latest version https://languagetool.org/download/
;; wget https://languagetool.org/download/LanguageTool-stable.zip -P ~/Downloads
;; unzip <download> -d ~/.opt/
(use-package flymake-languagetool
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
  :straight (svg-lib :type git :host github :repo "rougier/svg-lib"))

;; icons for completion in region
;; still in development
(use-package kind-icon
  :straight (kind-icon :type git :host github :repo "jdtsmith/kind-icon")
  :after corfu 
  :custom
  (kind-icon-default-face 'corfu-default)
  :defer 1
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; change font on windows wsl gui
(use-package emacs
  :when (and (string-match "-[Mm]icrosoft" operating-system-release)
             (display-graphic-p))
  :config
  (set-face-attribute 'default nil :height 98))

(use-package image-mode
  :straight (:type built-in)
  :config
  (setq image-animate-loop t))

;; show the breadcrumb on top of org buffer
(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full
        org-sticky-header-outline-path-separator " > "))

;; sticky header for progamming modes
(use-package topsy
  :straight (topsy :type git :host github :repo "alphapapa/topsy.el")
  :hook (prog-mode . topsy-mode))

;; work git servers (forges)
(use-package forge
  :after magit)

;; mass copy-paste or copy-move (analogous to cut-paste) for dired
(use-package dired-ranger
  :after dired
  :general
  ('normal dired-mode-map :prefix "C-c"
           "c" 'dired-ranger-copy
           "v" 'dired-ranger-paste
           "x" 'dired-ranger-move))

;; open with external program
(use-package openwith
  :defer 1
  :config
  (openwith-mode)
  (setq openwith-associations '(("\\.pdf\\'" "okular" (file))
                                ("\\.xopp\\'" "xournalpp" (file)))))

(use-package org-file-apps
  :straight nil
  :after org
  :init
  (setq org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . default)
          ("\\.pdf:::\\([0-9]+\\)?\\'" . "okular %s -p %1")
          ("\\.pdf::\\([0-9]+\\)?\\'" . "xournalpp %s -n %1")))) ; if file has ::<page> opens at this page

;; convert pdf to svg to display inline org image
;; only when in the workstation, otherwise too slow
;; requires pdf-tools
(use-package org-inline-pdf
  :when (eq system-type 'gnu/linux)
  :hook (org-mode . org-inline-pdf-mode))

;; insert org link to specified page in pdf
;; uses regular org functions `org-insert-link-global'
(use-package org-pdftools
  :disabled t                             ;org file apps with regex is enough!
  :hook (org-mode . org-pdftools-setup-link))

(use-package doom-themes
  :disabled
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
  :straight nil
  :diminish eldoc-mode
  :hook (org-mode . eldoc-mode)
  :config
  ;; never resize echo area display, use always 1 truncated line
  ;; use `eldoc-doc-buffer' for multiple lines (with popper is good)
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-idle-delay 0.1))

;; save windows configurations and use regular bookmarks file
(use-package burly
  :straight (burly :type git :host github :repo "alphapapa/burly.el"))

;; view large files
(use-package vlf
  :defer 1)

;; requires dtach `yay dtach'
;; run shell commands detached from emacs
(use-package dtache
  :disabled
  :straight (dtache :type git :host gitlab :repo "niklaseklund/dtache")
  :general
  ('normal "<f7>" 'dtache-open-session)
  ('normal dired-mode-map "M-&" 'dtache-shell-command)
  :config
  (dtache-setup)
  (with-eval-after-load 'embark
    ;; add embar actions for 'dtache-open-session'
    (defvar embark-dtache-map (make-composed-keymap dtache-action-map embark-general-map))
    (add-to-list 'embark-keymap-alist '(dtache . embark-dtache-map))))

;; add `:dtache t' option to sh source block
;; dtache org setup requires calls to ssh to recover the sessions
(use-package dtache-org
  :disabled
  :straight nil
  :commands org-babel-execute:sh        ;load when executing code
  :after org dtache
  :init
  (dtache-org-setup))

;; requires dtach `yay dtach'
;; run shell commands detached from emacs
(use-package detached
  :straight (detached :type git :host nil :repo "https://git.sr.ht/~niklaseklund/detached.el")
  :general
  ('normal "<f7>" 'detached-consult-session)
  ('normal dired-mode-map "M-&" 'detached-shell-command)
  :custom ((detached-show-output-on-attach t))
  :init
  (detached-init))

;; helps with windows popups
(use-package popper
  :defer 1
  :general
  (popper-mode-map "C-`" 'popper-toggle-latest)
  (popper-mode-map "C-@" 'popper-toggle-latest) ; for term
  (popper-mode-map "C-M-`" 'popper-cycle)
  :init
  ;; treat those as popups
  (setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$" 
        "output\\*$"           ;for preview latex error
        ".log$"              ;for dtache log
        "^\\*Detached Shell.*"
        help-mode
        "Command\\*$"                   ;for shell command
        ("^\\*Async.*" . hide)                   ; async commands
        "\\*xref\\*"
        "\\*eldoc\\*"                   ; eldoc buffer
        "Help\\*$"
        "\\*Python\\*"
        compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package rainbow-delimiters
  :hook (smartparens-mode . rainbow-delimiters-mode))

;; highlight scope defined by delimiters
;; useful sometimes in tex
(use-package rainbow-blocks
  :disabled
  :diminish rainbow-blocks-mode
  :hook (LaTeX-mode . rainbow-blocks-mode)
  :config
  (setq rainbow-blocks-outermost-only-face-count 1))

;; alternative to flyspell
(use-package spell-fu
  :disabled
  :hook (LaTeX-mode . spell-fu-mode))

;; eye candy for org
(use-package org-modern
  ;; :when (display-graphic-p)             ;only when gui
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star nil)
  ;; don't add fringe, does not play nicely with org indent
  (org-modern-block-fringe nil))

(use-package flymake-grammarly
  :config
  (setq flymake-grammarly-check-time 0.2)
  :commands load-flymake-with-grammarly)

(use-package virtual-comment
  :diminish virtual-comment-mode
  :general
  ('normal "C-<return>" 'virtual-comment-make)
  :hook 
  (python-mode . virtual-comment-mode)
  (c++-mode . virtual-comment-mode)
  :config
  (setq virtual-comment-face 'lazy-highlight
        virtual-comment-default-file "~/.emacs.d/.evc"))

;; improve org latex support
(use-package org-auctex
  :disabled; does not play nicely when there is $ in shell src blocks
  :after org
  :straight (org-auctex :type git :host github :repo "karthink/org-auctex")
  :hook (org-mode . org-auctex-mode)
  :general
  ('normal org-mode-map "g p b" 'org-auctex-preview-buffer)
  ('normal org-mode-map "g p c" 'org-auctex-preview-clearout-buffer))

;; query for org
(use-package org-ql
  :disabled
  :after org
  :straight (org-ql :host github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))

;; allows inline animations in org
(use-package org-inline-anim
  :after org
  :commands org-inline-anim-mode)

;; eldoc in childframe
;; sometimes it gets in the way
(use-package eldoc-box
  :disabled
  :hook (c++-mode . eldoc-box-hover-mode))

;; async support for dired
(use-package dired-async
  :straight nil
  :hook (dired-mode . dired-async-mode))

;; try later as alternative to dired-async
(use-package dired-rsync
  :after dired
  :general
  (dired-mode-map "C-c C-r" 'dired-rsync))

;; function to run local command on remote file
;; https://emacs.stackexchange.com/questions/42252/run-local-command-on-remote-file-with-tramp
(use-package dired-local-command-on-remote
  :straight nil
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
  :straight (flymake-vale :type git :host github :repo "tpeacock19/flymake-vale")
  :commands flymake-vale-load)

;; enhances outline-cycle command
;; binds tab anywhere, default is smarter (indent-for-tab-command)
(use-package bicycle
  :after outline
  :config
  ;; replace outline-cycle
  (advice-add 'outline-cycle :override #'bicycle-cycle))

;; query synonyms
(use-package le-thesaurus
  :commands le-thesaurus-get-synonyms)

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
  :straight nil
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
  :commands focus-mode)

;; trying instead of outline-minor-mode for programming
(use-package origami
  :disabled                             ; bicycle is better
  :straight (origami :type git :host github :repo "elp-revive/origami.el")
  :hook (prog-mode . origami-mode)
  :general
  ('normal origami-mode-map :prefix "Z"
           "o" 'origami-show-only-node
           "a" 'origami-recursively-toggle-node
           "h" 'origami-toggle-all-nodes
           "j" 'origami-next-fold
           "k" 'origami-previous-fold))

;; list imenu entries in a buffer
;; better faces than consult-imenu
(use-package imenu-list
  :general ("C-c o" 'imenu-list-smart-toggle)
  :config
  (setq imenu-list-auto-resize t
        imenu-list-auto-update nil      ; I want to keep the list from a file
        imenu-list-position 'left
        imenu-list-focus-after-activation t))

;; deal with ANSI escape sequences for coloring
(use-package ansi-color
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
  :straight nil
  :after org
  :commands org-babel-detangle-bg
  :init
  (defun org-babel-detangle-bg ()
    "Use `org-babel-detangle' but maintain focus on source code"
    (interactive)
    (org-babel-detangle)
    (find-file buffer-file-name)))

;; format code when saving based on `.clang-format' file
(use-package clang-format+
  :hook (c++-mode . clang-format+-mode))

;; debugge
(use-package gud
  :config
  ;; display source code
  (setq gdb-many-windows t))

(use-package org-footnote
  :straight nil
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

(use-package gdb
  :straight nil
  :config
  (setq gdb-locals-value-limit 1000))

;; irony mode for org-edit-special c++ 
(use-package irony
  :hook
  (org-src-mode . (lambda ()
                          (when (string-equal major-mode "c++-mode")
                            (irony-mode)))))

;; eldoc support for irony
(use-package irony-eldoc
  :hook
  (irony-mode . irony-eldoc))

;; tags with different colors in org
(use-package org-rainbow-tags
  :straight (:host github :repo "KaratasFurkan/org-rainbow-tags")
  :hook (org-mode . org-rainbow-tags-mode))

;; python support for org-edit-special
(use-package elpy
  :hook
  (org-src-mode . (lambda ()
                          (when (string-equal major-mode "python-mode")
                            (setq elpy-modules '(elpy-module-flymake elpy-module-sane-defaults))
                            (elpy-enable)))))

;; authentication file  
(use-package auth-sources
  :straight nil
  :init
  (setq auth-sources '((:source "~/.emacs.d/secrets/.authinfo.gpg"))))

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
