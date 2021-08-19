(defvar my-start-time (current-time)
  "Time when Emacs was started")

(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; development branch of straight
(setq straight-repository-branch "develop")

;; Bootstrap straight.el.
;; straight automatically checks if it needs to be rebuilt.
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

;; Install use-package macros.
(straight-use-package 'use-package)
(setq use-package-verbose nil		; don't print anything
      use-package-compute-statistics nil; compute statistics about package initialization
      use-package-expand-minimally t	; minimal expanded macro
      use-package-always-defer t)	; always defer, don't "require", except when :demand

;; Install package with same name expect specified otherwise
;; use-package expands to straight-use-package (excepts when :straight nil)
(setq straight-use-package-by-default t)

;;; Prevent Emacs-provided Org from being loaded
(straight-register-package 'org)
(straight-register-package 'org-contrib)

;; General for kybinding
(use-package general
  :demand)
(use-package diminish
  :demand)

;; Minimizes GC interferecen with user activity
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 0.5
	gcmh-high-cons-threshold (* 16 1024 1024)) 
  (gcmh-mode 1))

;; Basics
(use-package emacs
  :straight nil
  :general
  ("C-<tab>" 'next-window-any-frame)
  ("<backtab>" 'previous-window-any-frame)
  ("C-c w" 'shrink-window)
  ("C-x C-M-e" 'pp-macroexpand-last-sexp)
  ("C-h j" 'describe-keymap)
  ("C-M-=" 'zoom-frame)
  ("C-M--" 'zoom-frame-out)
  :init
  (global-hl-line-mode t) ; highlight current line
  (winner-mode t)	  ; move between windows configuration
  (setq-default fill-column 80)	  ; column length
  (column-number-mode t)  ; show column number in the mode line
  
  (fringe-mode '(8 . 0))  ; remove fringes

  ;; Setting typefaces
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
    (zoom-frame (- (or amt 1)) frame))

  ;; (set-face-attribute 'default nil :font "Hack" :height 105)

  ;; name on top of window
  (setq-default frame-title-format '("%b [%m]"))

  (setq warning-minimum-level :error)		 ;avoid warning buffer

  ;; scroll
  (setq auto-window-vscroll nil) 		;avoid next-line to trigger line-move-partial
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-follow-mouse 't)
  (setq scroll-step 1)
  ;; (setq mouse-wheel-progressive-speed nil)

  ;; other basiscs
  (setq ring-bell-function 'ignore)
  (setq inhibit-startup-screen t)
  (setq user-full-name "Nasser Alkmim"
	user-mail-address "nasser.alkmim@gmail.com")

  ;; UTF-8 encoding
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; create backups in separate folder
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq create-lockfiles nil)		; files with # problem with onedrive...

  ;; Answering just 'y' or 'n' will do
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; don't ask if it is ok to kill a process when killing a buffer
  (setq kill-buffer-query-functions nil)

  (if (display-graphic-p)
      (blink-cursor-mode 1)
    (progn
      (blink-cursor-mode -1)
      (setq visible-cursor nil)))

  ;; Don't beep at me
  (setq visible-bell t)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;;Slow down the UI being updated to improve performance
  (setq idle-update-delay 1.1)

  ;; add details in completions as prefix/sufix
  (setq completions-detailed t)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Avoid grow and shrink minibuffer
  (setq resize-mini-windows nil)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; 
  (setq indent-tabs-mode nil)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package abbrev
  :straight nil
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
  (setq recentf-max-saved-items 20
	recentf-auto-cleanup 'mode))

(use-package autorevert
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

;; completion UI (vertical minibuffer)
(use-package vertico
  :straight (vertico :type git :host github :repo "minad/vertico"
		     :includes (vertico-buffer
				vertico-directory
				vertico-repeat)
		     :files (:defaults "extensions/vertico-buffer.el"
				       "extensions/vertico-directory.el"
				       "extensions/vertico-repeat.el"))
  :init
  (vertico-mode))

;; improves behavior when dealing with directories in the minibuffer
(use-package vertico-directory
  :after vertico
  :general
  (vertico-map "RET" 'vertico-directory-enter
	       "DEL" 'vertico-directory-delete-char
	       "M-DEL" 'vertico-directory-delete-word)
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; repeat last vertico session
(use-package vertico-repeat
  :after vertico
  :general
  ("M-r" 'vertico-repeat))

;; completion style with flexible candidate filtering
;; filter with space separated components and match components in any order
(use-package orderless
  :after vertico
  :demand
  :config
  ;; partial completion for files to allows path expansion
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; save the search history
(use-package savehist
  :after vertico
  :init
  (savehist-mode))

;; minibuffer annotations details
(use-package marginalia
  :after vertico
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

  ;; two parts: search  and filter
  ;; #<search string>#<filter terms> filtering with orderless! amazing!
  ("C-c r" 'consult-ripgrep)		; search file contents
  ("C-c f" 'consult-find-fd)		; search files in directories
  (minibuffer-local-completion-map "<tab>" 'minibuffer-force-complete)
  :config
  ;; configure preview behavior to `any` key and specific time delay
  (consult-customize consult-buffer consult-bookmark consult-ripgrep consult-find-fd
		     :preview-key '(:debounce 3 any))
  (consult-customize consult-line :preview-key '(:debounce 0 any))

  ;; use 'fd' instead of 'find'
  (defun consult-find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))

  ;; set project root from vc.el
  ;; available when a project file is visited (project-switch-project)
  (setq consult-project-root-function #'vc-root-dir))

;; context menu/action at point or inside the minibuffer (on the top candidate)
;; or in a region
;; `embark-collect` allows acting on a `set of targets` (snapshot or live)
;; `live` works like a completion narrowing that Vertico does 
;; `export` the set of targets are shown in an appropriate major-mode
(use-package embark
  :general
  ("C-S-a" 'embark-act)
  ("C-S-z" 'embark-dwim)
  ("C-h B" 'embark-bindings)
  :commands embark-prefix-help-command
  :hook
  ;; use embark to help find command after prefix (C-h after a prefix key)
  ;; if this is used before `embark-act`, load the package (because of `:commands`)
  ;; `which-key` changes this variable as well, so we need to change it after `which-key`
  (which-key-mode . (lambda ()
		      (setq prefix-help-command #'embark-prefix-help-command)))
  :config
  ;; reminder of available actions
  ;; to select an action, use "@" prefix when in the prompter
  (setq embark-action-indicator
	(lambda (map _target)
	  (which-key--show-keymap "Embark" map nil nil 'no-paging)
	  #'which-key--hide-popup-ignore-command)
	embark-become-indicator embark-action-indicator))

;; allows consult previews as you move around an auto-updating embark collect
;; buffer
;; `exbark-collects` grep results to a grep buffer
(use-package embark-consult
  :demand				;necessary for consult preview
  :hook (embark-collect-mode . embark-consult-preview-minor-mode)
  :after (embark consult))

(use-package all-the-icons)

;; automatic insert matching pairs and navigation
;; for wrap/unwrap I use evil-surround
;; expand/contract (slurp) is good for elisp
(use-package smartparens
  :straight (:includes (smartparens-c smartparens-python))
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
  (smartparens-mode . smartparens-strict-mode)
  (smartparens-mode . show-smartparens-mode) ; instead of default show-paren-mode
  :config
  (sp-local-pair 'latex-mode "$" "$" :trigger "$")
  (sp-local-pair 'python-mode "'" "'" :trigger "'")
  (sp-pair "<" ">" :actions :rem)	      ; remove
  (sp-local-pair 'html-mode "<" ">" :trigger "<")
  (setq sp-show-pair-delay 0
	sp-show-pair-from-inside t))

(use-package smartparens-c
  :after smartparens)

(use-package smartparens-python
  :after smartparens)

(use-package flycheck
  :after lsp
  :hook (python-mode . flycheck-mode))

(use-package evil-multiedit
  :after evil
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
  :config (global-evil-mc-mode 1))

(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :general
  ('normal override "s" 'avy-goto-char-timer)
  ('normal ";" 'evil-search-forward)
  ('normal "M-p" 'evil-paste-from-register)
  ('(normal visual) :prefix "SPC" "l" 'evil-last-non-blank)
  ('(normal visual) :prefix "SPC" "h" 'evil-first-non-blank)
  ('normal :prefix "SPC" "a" 'evil-append-line)
  ('normal :prefix "SPC" "x s" 'save-buffer)
  ('normal "[ ]" 'evil-next-close-paren)
  ('normal "j" 'evil-next-visual-line)
  ('normal "k" 'evil-previous-visual-line)
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
;; headings: M-ret
(use-package evil-org
  :diminish evil-org-mode
  :after evil org
  :straight (:includes evil-org-agenda)
  :hook (org-mode . evil-org-mode))

;; included in evil-org
;; load it when using agenda
(use-package evil-org-agenda
  :after org-agenda evil-org
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
  (setq beacon-blink-delay .5)
  (setq beacon-size 8)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-duration .5)
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
  :straight (:includes (org-id oc ob))
  :diminish org-indent-mode
  :mode (("\\.org$" . org-mode))
  :general
  (org-mode-map "C-c C-l" 'org-insert-link)
  ('normal org-mode-map "z n" 'org-toggle-narrow-to-subtree)
  ('normal org-mode-map "z k" 'org-previous-visible-heading)
  ('normal org-mode-map :prefix "z"
	   "s j" 'org-babel-next-src-block
	   "s k" 'org-babel-previous-src-block)
  :hook ((org-mode . visual-line-mode))
  :custom
   (org-hide-emphasis-markers t) ; avoid noisy //,__, **, 
   (org-startup-indented nil)		; start collapsed
   (org-startup-folded t)	; folded
   (org-hide-leading-stars t)	; don't show a  bunch of '*'
   (org-edit-src-content-indentation 0)
   (org-outline-path-complete-in-steps nil)
   (org-startup-with-inline-images t)
   (org-special-ctrl-a/e t)	     ; when jump to beginning of line be aware of *
   (org-cycle-separator-lines 0)	; no empty lines between headings
   (org-fontify-quote-and-verse-blocks t) ; yes syntax highlighting
   (org-insert-heading-respect-content t) ; insert heading after current tree
   (org-src-tab-acts-natively t)
   (org-catch-invisible-edits 'smart)
  :config
  (transient-mark-mode -1)

  ;; New link type for Org-Hugo internal links
  ;; https://lucidmanager.org/productivity/create-websites-with-org-mode-and-hugo/
  (org-link-set-parameters "hugo"
			   :complete (lambda ()
				       (concat "{{% ref \""
					       (file-relative-name (read-file-name "File: "))
					       "\" %}}"))))

(use-package ox-extra
  :after org
  :demand
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package ob
  :after org
  :init
  (push '"~/.local/bin" exec-path)	; so it can find jupyter binaries
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (shell . t)
     (C . t)
     (python . t)
     (jupyter . t)))

  ;; use just python instead of jupyter-python
  (org-babel-jupyter-override-src-block "python")
  (setq org-babel-default-header-args:C++ '((:results . "output")))

  (setq org-babel-default-header-args:python '((:async . "yes")
					       (:session . "default")
					       (:kernel . "python3")
					       (:results . "output")
					       (:exports . "both")))
  (setq jupyter-org-resource-directory "./jupyter/")
  (setq org-image-actual-width '(350))

  ;; display/update images in the buffer after I evaluate 
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(use-package jupyter
  :after org
  :general
  (org-mode-map "C-c =" 'jupyter-org-hydra/body))

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
  :config
  ;; babel and source blocks
  (setq org-src-fontify-natively t
	;; org-highlight-latex-and-related '(latex)
	;; org-src-window-setup 'current-window
	;; org-src-strip-leading-and-trailing-blank-lines t
	org-src-preserve-indentation t  ; preserve indentation in code
	org-adapt-indentation nil ; Non-nil means adapt indentation to outline node level.
	org-src-tab-acts-natively t	; if t, it is slow!
	org-export-babel-evaluate nil
	org-confirm-babel-evaluate nil)) ; doesn't ask for confirmation

(use-package org-agenda
  :after org
  :straight nil
  :config
  (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
				 "~/OneDrive/Org/notes.org"
				 "~/OneDrive/Org/journal.org"
				 "~/OneDrive/Org/gcal.org"))))

(use-package ox-latex
  :after org
  :straight nil
  :config
  ;; change scale of latex preview in org-mode
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  ;; minted code pdf export org
  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("newfloat" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-classes
	       '("koma-article"
		 "\\documentclass{scrartcl}
		\\usepackage{microtype}
		\\usepackage{tgpagella}
		\\usepackage[scale=.9]{tgheros}
		\\usepackage{tgcursor}
		\\usepackage{paralist} "
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package ob-async
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python")))

(use-package ob-shell
  :straight nil
  :after org
  :config
  (setq org-babel-default-header-args:shell
	'((:results . "output")
	  (:shebang . "#!/bin/bash -i") ; always get my .bashrc aliases
	  )))

(use-package org-roam-server
  :hook (org-roam-mode . org-roam-server-mode)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

(use-package org-roam
  :custom
  ;; (org-roam-directory "~/OneDrive/nasser-website/content/roam") ; on windows
  (org-roam-directory "/mnt/c/Users/nasse/OneDrive/nasser-website/content/roam") ; on linux with wsl
  :general
  ('normal :prefix "SPC" "n f" 'org-roam-find-file) ; so it can autoload org-roam
  ('normal org-roam-mode-map :prefix "SPC" "n l" 'org-roam)
  (:states '(normal visual) :keymaps 'org-mode-map :prefix "SPC" "n i" 'org-roam-insert)
  ('insert org-mode-map "C-c n i" 'org-roam-insert)
  :config
  (org-roam-mode))

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
   ;; choco install imagemagick.app -PackageParameters LegacySupport=true
   org-download-screenshot-method "imagemagick/convert"
   org-download-image-dir "./images"
   org-download-image-html-width 350)
  (setq org-download-screenshot-method "convert.exe clipboard: %s") ; add .exe to work within wsl2
  (setq org-download-screenshot-file "./screenshot.png")) ; where temporary screenshot will be saved so convert can work

;; languages spell checker
(use-package flyspell
  :hook ((LaTeX-mode . flyspell-mode)
	 (org-mode . flyspell-mode)
	 (prorg-mode . flyspell-prog-mode))
  :config
  ;; husnpell is alternative to aspell
  (setq ispell-program-name "hunspell")	; dictionary /usr/share/hunspell
  (when (eq system-type 'windows-nt)
    (setq ispell-dictionary "en_US,pt_BR")
    (ispell-hunspell-add-multi-dic "en_US,pt_BR"))
  (ispell-set-spellchecker-params)

  (setq flyspell-issue-message-flag nil ; don't emit messages
	ispell-personal-dictionary "~/.dotfiles/hunspell/.personal"))

;; flyspell uses `hooks` and `sit-for` to delay
;; this uses `idle-timers`
(use-package flyspell-lazy
  :hook
  (flyspell-mode . flyspell-lazy-mode))

(use-package flyspell-correct
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-correct-wrapper)
  :after flyspell)

;; completion in region manually summoned with <tab> (no auto pop up)
;; allows space between filter words (combined with oderless)
(use-package corfu
  :disabled
  :hook ((prog-mode . corfu-mode)
	 (org-mode . corfu-mode))
  :general
  (corfu-map "<tab>" 'corfu-next
	     "<backtab>" 'corfu-previous
	     "C-n" 'corfu-next
	     "C-p" 'corfu-previous)
  ('normal corfu-map "<escape>" 'corfu-quit)
  ('insert "C-n" nil
	   "C-p" nil)
  :config
  (setq corfu-auto t
	corfu-auto-prefix 1
	corfu-quit-no-match t))

;; completion in region
(use-package company
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
  :after company
  :init
  (company-prescient-mode))

;; folding
(use-package outline
  :diminish outline-minor-mode
  :demand				; don't autoload, just load it.
  :straight nil
  :hook
  (prog-mode . outline-minor-mode)
  (markdown-mode . outline-minor-mode)
  (conf-mode . outline-minor-mode)
  :general
  ('normal outline-mode-map "C-j" nil)
  ('normal outline-mode-map "z j" 'outline-next-visible-heading)
  ('normal outline-mode-map "z o" 'outline-show-children)
  ('normal outline-mode-map "z h" 'outline-hide-body)
  ('normal outline-mode-map "<tab>" 'outline-cycle)
  ('normal outline-mode-map "z k" 'outline-previous-visible-heading)
  :config
  (setq outline-minor-mode-cycle t
	outline-minor-mode-highlight 'append))  

(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom-face 
  (font-latex-sectioning-2-face ((t (:underline t :weight bold))))
  (font-latex-sectioning-3-face ((t (:weight bold))))
  (font-latex-sectioning-4-face ((t (:weight normal :slant normal))))
  (font-latex-sectioning-5-face ((t (:weight normal :slant italic))))
  :general
  (LaTeX-mode-map "C-M-y" 'my-tex-insert-clipboard)
  ('normal outline-mode-map
    "g j" nil
    "g k" nil
    "C-j" nil)
  ('normal LaTeX-mode-map "g p" '(:keymap preview-map))
  (preview-map
	   "b" 'preview-buffer
	   "c" 'preview-clearout-buffer
	   "s" 'preview-section
	   "p" 'preview-at-point)
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (prettify-symbols-mode)
              (LaTeX-math-mode)		; ` to easy type greek
              (turn-on-reftex)
              (reftex-isearch-minor-mode)
	      (visual-line-mode)
	      (outline-minor-mode) ; latex like org
	      (outline-hide-sublevels 1) ; start folded
              (turn-off-auto-fill)))

  ;; preview latex
  (setq preview-default-option-list '("displaymath" "floats" "graphics"
				      "textmath" "showlabels")
	preview-scale-function 1.2
	preview-auto-cache-preamble t)

  (setq TeX-save-query nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)       ;enable document parsing
  (setq-default TeX-master nil) ;make auctex aware of multi-file documents
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)			; output pdf 
  (setq TeX-electric-escape t)
  (setq global-font-lock-mode t)

  (setq LaTeX-command "latex -shell-escape") ;; -shell-escape for minted (syntax highlight)
  
  (setq TeX-source-correlate-method 'synctex) ;; Method for enabling forward and inverse search 
  (setq TeX-source-correlate-start-server t) ;; inhibit the question to start a server process
  (setq TeX-source-correlate-mode t) ;; jump to source

  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook  
	    'TeX-revert-document-buffer) ;; Update PDF buffers after successful LaTeX runs

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
	       '(output-pdf "Okular"))

  (defun my-tex-insert-clipboard ()
    (interactive)
    (setq folder-path (concat default-directory "img/"));make the img directory
					;create the directory if it doesn't exist
    (if (not (file-exists-p folder-path))
	(mkdir folder-path))
    ;; correct path to use convert.exe inside wsl and create file
    (setq wsl-folder-path (replace-regexp-in-string "\n\\'" ""
						(shell-command-to-string (concat "wslpath -w "
										 folder-path))))

    (setq image-path (concat wsl-folder-path
			     "\\img_"
			     (format-time-string "%Y%m%d_%H%M%S_.png")))
    (let* ((image-file (concat 
			"img/img_"
			(format-time-string "%Y%m%d_%H%M%S_.png")))
	   (exit-status
	    (call-process "convert.exe" nil nil nil
			  "clipboard:" image-path)))
      (insert (format "
\\begin{figure}[ht!]
  \\centering
  \\includegraphics[width=.5\\linewidth]{%s}
\\end{figure}" image-file))))
  )

(use-package evil-tex
  :after latex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package reftex
  :after latex
  :commands reftex-toc
  :general
  ('normal reftex-mode-map :prefix "g r"
	   "t" 'reftex-toc
	   "v" 'reftex-view-crossref
	   "g" 'reftex-goto-label
	   "r" 'reftex-reference
	   "c" 'reftex-citation)
  :config
  (setq reftex-cite-prompt-optional-args t) ; Prompt for empty optional arguments in cite
  ;; https://www.gnu.org/software/emacs/manual/html_mono/reftex.html
  (setq reftex-enable-partial-scans t)
  (setq reftex-keep-temporary-buffers nil)
  (setq reftex-save-parse-info t)
  (setq reftex-trust-label-prefix '("fig:" "eq:")))

(use-package dired
  :straight nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :general
  (dired-mode-map "C-c C-d" 'mkdir)
  ('normal dired-mode-map "h" 'dired-up-directory)
  ("M-o" 'dired-omit-mode)
  ('normal dired-mode-map "l" 'dired-find-alternate-file)
  ('normal dired-mode-map "SPC" nil)
  ("C-x C-j" 'dired-jump-other-window)
  ("C-x j" 'dired-jump)
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$"
	dired-auto-revert-buffer t
	dired-listing-switches "-alh"	; human readable format when in detail
	dired-kill-when-opening-new-dired-buffer t
	delete-by-moving-to-trash t)	; move to trash

  ;; kill the dired buffer eand enters the current line file or directory
  (put 'dired-find-alternate-file 'disabled nil))

;; subtree folder expansion
(use-package dired-subtree
  :after dired treemacs-icons-dired
  :general
  ('normal dired-mode-map "<tab>" 'dired-subtree-toggle))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode)
  :config 
  (treemacs-resize-icons 12))

(use-package treemacs
  :disabled 				; using dired and bookmarks
  :straight (:includes treemacs-icons-dired)
  :config
  (setq
   treemacs-git-mode nil
   treemacs-sorting 'mod-time-desc ; modified early
   treemacs-is-never-other-window t
   treemacs-width 25)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  :general
  (treemacs-mode-map "C-<return>" 'treemacs-display-current-project-exclusively)
  (treemacs-mode-map "<f8>" 'treemacs-quit)
  ("<f8>" 'treemacs-select-window))

(use-package treemacs-evil
  :after treemacs evil
  :general
  (treemacs-mode-map :prefix "<treemacs-state>"
		     "h" 'treemacs-root-up
		     "l" 'treemacs-root-down)
  :demand)

(use-package doom-themes
  :disabled
  :commands ap/load-doom-theme
  :general
  ("<f7>" 'ap/load-doom-theme)
  :config
  ;; (load-theme 'doom-one t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config)
  
  (defun ap/load-doom-theme (theme)
    "Disable active themes and load a Doom theme."
    (interactive (list (intern (completing-read "Theme: "
						(->> (custom-available-themes)
						  (-map #'symbol-name)
						  (--select (string-prefix-p "doom-" it)))))))
    (ap/switch-theme theme)
    (set-face-foreground 'org-hide (face-background 'default)))
  (defun ap/switch-theme (theme)
    "Disable active themes and load THEME."
    (interactive (list (intern (completing-read "Theme: "
						(->> (custom-available-themes)
						  (-map #'symbol-name))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme 'no-confirm)))

(use-package modus-themes
  :config
  (setq modus-themes-org-blocks 'tinted-background
	modus-themes-prompts '(intense italic)
	modus-themes-hl-line '(accented)
	modus-themes-diffs 'desaturated
	modus-themes-completions 'opinionated
	modus-themes-paren-match '(bold underline)
	modus-themes-no-mixed-fonts nil
	modus-themes-variable-pitch-ui nil
	modus-themes-syntax '(faint alt-syntax yellow-comments)
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-fringes 'subtle
	modus-themes-mode-line '(borderless accented moody))

  ;; hook to enforce change when theme is toggled
  (add-hook 'modus-themes-after-load-theme-hook
	    (lambda ()
	      (progn 
		(set-face-attribute 'auto-dim-other-buffers-face nil
				    :foreground (modus-themes-color 'fg-inactive))
		(set-face-attribute 'sp-show-pair-match-content-face nil
				    :background (modus-themes-color 'bg-paren-expression)))))

  ;; runs the hook
  (if (display-graphic-p)
      (modus-themes-load-operandi)
    (modus-themes-load-vivendi))
  :general
  ("<f5>"  'modus-themes-toggle))

(use-package auto-dim-other-buffers
  :defer 1
  :init
  (auto-dim-other-buffers-mode t))

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

; change python virtual envirnment variables
(use-package pyvenv
  :commands pyvenv-activate
  :config
  (setq pyvenv-default-virtual-env-name "~/miniconda3/envs/"))

(use-package c++-mode
  :straight nil
  :mode ("\\.cpp\\'" . c++-mode)
  :general
  (c++-mode-map "C-x c" 'compile))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :general
  (org-mode-map :prefix "C-l" "o" 'lsp-org)
  (org-mode-map :prefix "C-l" "d" 'lsp-virtual-buffer-disconnect)
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq read-process-output-max (* 1024 1024))
  :hook ((python-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-idle-delay 1
	lsp-enable-folding nil		;potential to be slow
	lsp-enable-text-document-color nil ;potential to be slow
	lsp-keep-workspace-alive nil; terminate server if last workspace buffer is closed
	lsp-enable-on-type-formatting nil  ;don't format automatically
	lsp-headerline-breadcrumb-enable nil)  ;disable breadcrumb

  ;; using corfu, not company
  (setq lsp-completion-provider :none))

;; alternative to lsp-python-ms
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred)))
  :config
  ;; https://github.com/microsoft/pyright/discussions/1466
  ;; don't infer types from library source
  ;; treat all as unknowns when type is nonpresent
  (setq lsp-pyright-use-library-code-for-types nil))

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

(use-package adaptive-wrap
  :diminish visual-line-mode
  :straight adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

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

;; embark front-end to helm-bibtex
(use-package bibtex-actions
  :straight (:includes oc-bibtex-actions)
  :general
  (:prefix "C-c b" "b" 'bibtex-actions-insert-citation
	   "r" 'bibtex-actions-refresh)
  :config
  (setq bibtex-completion-bibliography "~/.bibliography.bib"
	bibtex-completion-library-path "~/SeaDrive/My Libraries/PhD/bibliography/pdf/"
	bibtex-completion-pdf-open-function (lambda (fpath)
					      (call-process "xdg-open" nil 0 nil fpath)))

  ;; windows wsl config
  (defvar host (substring (shell-command-to-string "hostname") 0 -1))
  (when (string= host "lt135-c842")
    (setq bibtex-completion-library-path
	  "/mnt/c/Users/c8441205/seadrive/Nasser A/My Libraries/PhD/bibliography/pdf/")
    (setq bibtex-completion-pdf-open-function
	  (lambda (fpath) (shell-command (concat
			    ;; "cmd.exe /C start \"\" " ; this is to use with windows zotero file path (slower)
			    "wslview "	; version 3.2.1 works with spaces in path
			    (shell-quote-argument fpath))))))
  
  ;; make the 'bibtex-actions' bindings available from `embark-act'.
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
    (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
    (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map)))

  ;; update cache when bib change
  (bibtex-actions-filenotify-setup '(LaTeX-mode-hook org-mode-hook)))

;; bibtex action just for org-mode (from package bibtex-actions)
(use-package oc-bibtex-actions
  :after (org oc bibtex-actions)
  :general
  ("C-c b" 'org-cite-insert)
  ("M-o" 'org-open-at-point))

(use-package server
  :straight nil
  :demand
  :config (or (server-running-p)
	    (server-start)))

(use-package table
  :after org)

;; needs to be added manually to .emacs.d/lisp folder
(use-package wsl-path
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
  :straight nil
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
  :if (not (display-graphic-p))
  :init
  (evil-terminal-cursor-changer-activate))

(use-package repeat
  :defer 1
  :config
  ;; built-in command repeater (like hydra)
  (repeat-mode t))

;; built in windows resize functions
(use-package window
  :straight nil
  :general
  (resize-window-repeat-map "j" 'shrink-window)
  (resize-window-repeat-map "k" 'enlarge-window)
  (resize-window-repeat-map "h" 'shrink-window-horizontally)
  (resize-window-repeat-map "l" 'enlarge-window-horizontally))

;; create backlinks when linking org-mode headings
(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links")
  :after org
  :general
  ('normal org-mode-map :prefix "C-c s"
	   "s" 'org-super-links-link
	   "l" 'org-super-links-store-link
	   "p" 'org-super-links-insert-link
	   "d" 'org-super-links-quick-insert-drawer-link
	   "i" 'org-super-links-quick-insert-inline-link
	   "C-d" 'org-super-links-delete-link))

;; loads the org-id library from org repository
;; for creating org-ids for more robust linking, avoid referencing issues
(use-package org-id
  :after org-super-links
  :demand
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package oc
  :after org
  :config
  (setq org-cite-global-bibliography '("~/.bibliography.bib")))

;; compile and run code
(use-package quickrun
  :commands quickrun quickrun-shell)

;; highligh indentation
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; emacs built in version control
(use-package vc-git
  :straight nil
  :hook
  (diff-mode . outline-minor-mode)
  (vc-git-region-history-mode . outline-minor-mode))

;; manage remote files access and manipulations
(use-package tramp
  :straight nil
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

(use-package tree-sitter-langs
  :after tree-sitter
  :demand)

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

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
