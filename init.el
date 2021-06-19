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
(use-package general :demand)
(use-package diminish :demand)

;; Minimizes GC interferecen with user activity
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024)) 
  (gcmh-mode 1))

;; Basics
(use-package emacs
  :straight nil
  :general
  ("C-<tab>" 'other-window)
  ("C-<iso-lefttab>" 'other-frame)
  ("C-c w" 'shrink-window)
  ("C-x C-M-e" 'pp-macroexpand-last-sexp)
  ("C-h j" 'describe-keymap)
  :init
  (global-hl-line-mode t)
  (winner-mode t)
  (repeat-mode t)
  (setq-default frame-title-format '("%b [%m]")) ;name on top of window

  ;; font height
  ;; (set-face-attribute 'default nil :height 100)

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

  ;; UTF-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; create backups in separate folder
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq create-lockfiles nil)		; files with # problem with onedrive...

  ;; Answering just 'y' or 'n' will do
  (defalias 'yes-or-no-p 'y-or-n-p)

  (blink-cursor-mode t)

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

  ;; Grow and shrink minibuffer
  (setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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

;; show matching parenthesis
(use-package paren
  :defer 1
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; save recent visited files
(use-package recentf
  :defer 1
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 25
	recentf-auto-cleanup 'mode))

(use-package autorevert
  :defer 1
  :config
  (setq auto-revert-interval 2)
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
  :init
  (vertico-mode))

:; completion style with flexible, fuzzy candidate filtering
;; alternative to default completion by tab
(use-package orderless
  :after vertico
  :demand
  :config
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
  :init (marginalia-mode))

;; practical commands to select from lists
(use-package consult	
  :general
  ("M-s" 'consult-outline)
  ("C-c r" 'consult-ripgrep)
  ("C-c f" 'consult-find)
  ("C-c o" 'consult-imenu)
  ("C-x b" 'consult-buffer)
  ("M-y" 'consult-yank-pop)
  ("C-s" 'consult-line)
  (minibuffer-local-completion-map "<tab>" 'minibuffer-force-complete)
  :config
  (setq consult-preview-key nil; key to trigger preview
	consult-narrow-key "<")		; go back to full list command
  ;; C-s C-s to search with previous search
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (setf (alist-get #'consult-line consult-config) (list :keymap my-consult-line-map))

  (consult-customize consult-line :preview-key 'any))

;; context menu/action at point or minibuffer
(use-package embark
  :general
  ("C-S-a" 'embark-act)
  :config
  ;; actions with "@" when in the prompter
  ;; prefer the default
  ;; (setq embark-prompter 'embark-completing-read-prompter)
  (setq embark-action-indicator
	(lambda (map _target)
	  (which-key--show-keymap "Embark" map nil nil 'no-paging)
	  #'which-key--hide-popup-ignore-command)
	embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :demand				;necessary for consult preview
  :hook (embark-collect-mode . embark-consult-preview-minor-mode)
  :after (embark consult))

(use-package all-the-icons)

(use-package smartparens
  :diminish smartparens-mode  
  :commands smartparens-mode
  :general
  ('normal smartparens-mode-map "M-l" 'sp-next-sexp)
  ('normal smartparens-mode-map "M-h" 'sp-previous-sexp)
  ('normal smartparens-mode-map "M-k" 'sp-up-sexp)
  ('normal smartparens-mode-map "M-j" 'sp-down-sexp)
  ('normal smartparens-mode-map "C-M-l" 'sp-forward-sexp)
  ;; binding for Latex
  ('insert LaTeX-mode-map "<tab>" 'sp-forward-sexp)
  ('insert LaTeX-mode-map "C-<tab>" 'sp-backward-up-sexp)
  :hook
  (python-mode . smartparens-mode)
  (c++-mode . smartparens-mode)
  (lisp-interaction-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (LaTeX-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  :config
  (sp-local-pair 'latex-mode "\\left(" "\\right)" :trigger "\\l(")
  (sp-local-pair 'latex-mode "$" "$" :trigger "$")
  ;; highligh matching brackets
  (show-smartparens-global-mode 0)
  ;; so that paren highlights do not override region marking (aka selecting)
  (setq show-paren-priority -1) 
  (setq show-paren-when-point-inside-paren t)
  (setq sp-show-pair-from-inside t)
  (setq show-paren-style 'mixed)) 

(use-package flycheck
  :hook ((python-mode . flycheck-mode)))

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
  :general
  ('(normal visual) "g s" evil-mc-cursors-map)
  :init (global-evil-mc-mode 1))

(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :general
  ('normal global "s" 'avy-goto-char-timer)
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
  :after evil
  :init
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
  :after evil
  :init
  (evil-goggles-mode)
  :config
  (setq evil-goggles-pulse t)
  (setq evil-goggles-duration 0.2)
  (evil-goggles-use-diff-faces))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer nil ; does not play nice with vertico
	evil-collection-company-use-tng nil) ; makes company works betters I think
  (evil-collection-init))

;; navigation: gh, gj, gk, gl
;; headings: M-ret
(use-package evil-org
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
  :config
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
  ("C-x g" 'magit-status))

(use-package rainbow-mode
  :defer 1
  :diminish rainbow-mode
  :config (rainbow-mode))

(use-package org-contrib)

(use-package org
  :diminish org-indent-mode
  :mode (("\\.org$" . org-mode))
  :general
  (org-mode-map "C-c C-l" 'org-insert-link)
  ('normal org-mode-map :prefix "z"
	   "s j" 'org-babel-next-src-block
	   "s k" 'org-babel-previous-src-block)
  :hook (org-mode . visual-line-mode)
  :custom
   (org-hide-emphasis-markers t) 
   (org-startup-indented nil)
   (org-startup-folded t)	; folded
   (org-hide-leading-stars t) 
   (org-edit-src-content-indentation 0)
   (org-outline-path-complete-in-steps nil)
   (org-startup-with-inline-images t)
   (org-cycle-separator-lines 0)
   (org-fontify-quote-and-verse-blocks t)
   (org-insert-heading-respect-content t) ; insert heading after current tree
   (org-catch-invisible-edits 'smart)
  :config
  (transient-mark-mode -1)
  (setq org-todo-keywords '(
			    (sequence "TODO(t)" "NEXT(n)" "REVW(r)" "|" "DONE(d)")
			    (sequence "R1(1)" "R2(2)" "R3(3)" "R4(4)" "R5(5)" "R6(6)"))))

(use-package ox-extra
  :after org
  :demand
  :config
(ox-extras-activate '(ignore-headlines)))

(use-package ob
  :straight nil
  :after (:any org jupyter)
  :config
  (add-to-list 'exec-path "~/.local/bin") ;tell emacs where jupyter was installed
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
	org-src-tab-acts-natively nil	; if t, it is slow!
	org-export-babel-evaluate nil
	org-confirm-babel-evaluate nil)) ; doesn't ask for confirmation

(use-package org-agenda
  :after org
  :straight nil
  :config
  (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
				 "~/OneDrive/Org/notes.org"
				 "~/OneDrive/Org/journal.org"
				 "~/OneDrive/Org/gcal.org"
				 "~/OneDrive/Concurso/Notas/notas_concurso.org"))))

(use-package ox-latex
  :after org
  :straight nil
  :config
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

(use-package jupyter
  :after org
  :config
  (general-def org-mode-map "C-c =" 'jupyter-org-hydra/body))

(use-package ob-shell
  :straight nil
  :after org
  :config
  (setq org-babel-default-header-args:shell '((:results . "output"))))

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
   org-download-image-dir "."
   org-download-image-html-width 350)
  (setq org-download-screenshot-method "convert.exe clipboard: %s") ; add .exe to work within wsl2
  (setq org-download-screenshot-file "./screenshot.png")) ; where temporary screenshot will be saved so convert can work

;; languages spell checker
(use-package flyspell
  :hook ((LaTeX-mode . flyspell-mode)
	 (org-mode . flyspell-mode))
  :config
  (setq ispell-program-name "hunspell")	; dictionary /usr/share/hunspell
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (when (eq system-type 'windows-nt)
    (setq ispell-dictionary "en_US,pt_BR")
    (ispell-hunspell-add-multi-dic "en_US,pt_BR"))
  (ispell-set-spellchecker-params)

  (setq flyspell-delay 5		; 5 seconds
	ispell-personal-dictionary "~/.dotfiles/hunspell/.personal"))

(use-package flyspell-correct
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-correct-wrapper)
  :after flyspell)

(use-package flyspell-lazy
  :if (memq system-type '(windows-nt))
  :after flyspell
  :commands flyspell-lazy-mode
  :hook ((LaTeX-mode . flyspell-lazy-mode)
	 (org-mode . flyspell-lazy-mode)))

;; completion pop up
(use-package company
  :diminish company-mode
  :hook ((python-mode . company-mode)
	 (LaTeX-mode . company-mode)
	 (c++-mode . company-mode)
	 (emacs-lisp-mode . company-mode)
	 (org-mode . company-mode))
  :config
  (add-to-list 'company-backends 'company-capf)
  (setq company-idle-delay .2
	company-format-margin-function #'company-vscode-dark-icons-margin
	company-minimum-prefix-length 1))

;; completion suggestions with machine-learning
(use-package company-tabnine
  :disabled 				; slow
  :after ((:any latex org) company)
  :init
  (add-to-list 'company-backends #'company-tabnine)
  :config
  (setq company-tabnine-always-trigger nil))

(use-package company-prescient
  :after company
  :init
  (company-prescient-mode))

(use-package outline
  :straight nil
  :demand
  :general
  ('normal outline-mode-map "C-j" nil)
  ('normal outline-mode-map "z j" 'outline-next-visible-heading)
  ('normal outline-mode-map "z k" 'outline-previous-visible-heading))  

(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom-face 
  (font-latex-sectioning-2-face ((t (:underline t :weight bold))))
  (font-latex-sectioning-3-face ((t (:weight bold))))
  (font-latex-sectioning-4-face ((t (:weight light :slant normal))))
  (font-latex-sectioning-5-face ((t (:weight light :slant italic))))
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
  (setq preview-default-option-list '("displaymath" "floats" "graphics" "textmath")
	preview-scale-function 1.1
	preview-auto-cache-preamble t)

  (setq TeX-save-query nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)       ;enable document parsing
  (setq-default TeX-master nil) ;make auctex aware of multi-file documents
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq TeX-electric-escape t)
  (setq global-font-lock-mode t)

  (setq LaTeX-command "latex -shell-escape") ;; shell escape for minted (syntax highlight)
  (setq TeX-source-correlate-method 'synctex) ;; Method for enabling forward and inverse search 
  (setq TeX-source-correlate-start-server t) ;; inhibit the question to start a server process
  (setq TeX-source-correlate-mode t) ;; jump to source

  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook  
	    'TeX-revert-document-buffer) ;; Update PDF buffers after successful LaTeX runs

  ;; nomenclature for latex
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list 
		  '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
		    (lambda (name command file)
		      (TeX-run-compile name command file)
		      (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
		    nil t :help "Create nomenclature file")))

  ;; using "Zathura" on WSL
  ;; sometime "PDF Tools" good because of "pdf-view-set-slice-from-bounding-box"
  (add-to-list 'TeX-view-program-selection
	       '(output-pdf "PDF Tools"))

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
  \\includegraphics[width=.5\\textwidth]{%s}
\\end{figure}" image-file))))
  )

(use-package evil-tex
  :after latex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package reftex
  :after latex
  :commands reftex-toc
  :general ('normal "<SPC> =" 'reftex-toc)
  :config
  (setq reftex-cite-prompt-optional-args t) ; Prompt for empty optional arguments in cite
  ;; https://www.gnu.org/software/emacs/manual/html_mono/reftex.html
  (setq reftex-enable-partial-scans t)
  (setq reftex-keep-temporary-buffers nil)
  (setq reftex-save-parse-info t)
  (setq reftex-trust-label-prefix '("fig:" "eq:")))

;; cycle visibility in outline-minor-mode
(use-package outline-magic
  :after latex
  :general
  ('normal LaTeX-mode-map "<tab>" 'outline-cycle)
  ('normal outline-mode-map "C-j" nil))

(use-package dired
  :straight nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :general
  ("<f7>" 'open-my-notes)
  ("C-x j" 'dired-jump)
  (dired-mode-map "C-c C-d" 'mkdir)
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (setq dired-auto-revert-buffer t)
  (defun open-my-notes ()
    (interactive)
    (dired "/mnt/c/Users/c8441205/OneDrive/nasser-website/content/notes/")))

;; enhances dired
(use-package dired+
  :demand
  :after dired
  :custom-face
  (diredp-omit-file-name ((t (:strike-through nil)))) ; don't strike my text
  :config
  ;; keep just one dired buffer please
  (diredp-toggle-find-file-reuse-dir t))

;; subtree folder expansion
(use-package dired-subtree
  :after dired
  :general ('normal dired-mode-map "<tab>" 'dired-subtree-toggle-and-revert)
  :config
  (defun dired-subtree-toggle-and-revert ()
    (interactive)
    (dired-subtree-toggle)
    (revert-buffer)))

;; icons for dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs
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
  (treemacs-resize-icons 12)
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

(use-package solaire-mode
  :disabled
  :hook ((change-major-mode . turn-on-solaire-mode)
         (after-revert . turn-on-solaire-mode)
         (ediff-prepare-buffer . solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (setq solaire-mode-auto-swap-bg t)
  (solaire-global-mode +1))

(use-package doom-themes
  :disabled
  :commands ap/load-doom-theme
  :after solaire-mode
  :general
  ("<f6>" 'ap/load-doom-theme)
  :config
  ;; (load-theme 'doom-one t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t
	doom-themes-treemacs-theme "doom-colors") ; if nil, italics is universally disabled
  (doom-themes-treemacs-config)
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
  :init
  (setq modus-themes-org-blocks 'tinted-background
	modus-themes-hl-line 'intense-background
	modus-themes-diffs 'desaturated
	modus-themes-completions 'opinionated
	modus-themes-mode-line 'borderless-accented-moody)
  (load-theme 'modus-operandi t)
  :general
  ("<f5>"  'modus-themes-toggle))

(use-package htmlize)

;; :includes so straight can recognize dap-python.el
(use-package dap-mode
  :after lsp-mode
  :straight (dap-mode :includes dap-python
		      :type git
		      :host github
		      :repo "emacs-lsp/dap-mode") 
  :general
  (lsp-mode-map "<f6>" 'dap-hydra))

(use-package dap-python
  :after dap-mode
  :demand ; so it loads, "requires", dap-python
  :init
  (setq dap-python-debugger 'debugpy))

; Change python virtual envirnment variables
(use-package pyvenv
  :commands pyvenv-activate
  :config
  (setq pyvenv-default-virtual-env-name "/home/nasser/miniconda3/envs/"))

(use-package dap-cpptools
  :straight nil
  :after dap-mode)

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
	lsp-enable-on-type-formatting nil  ;don't format automatically
	lsp-headerline-breadcrumb-enable nil))  ;disable breadcrumb

(use-package lsp-python-ms
  :after lsp
  :config (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))  ; or lsp-deferred

(use-package lsp-ui
  :if (memq system-type '(gnu/linux))
  :commands lsp-ui-doc
  :after lsp
  :config
  (setq
   lsp-ui-doc-delay 3			; show doc only after this time
   lsp-ui-sideline-enable nil))

(use-package python		   ; for syntax highlight
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . toggle-truncate-lines)
  :config
  ;; dont guess the indent offset
  (setq python-indent-guess-indent-offset nil))

(use-package python-black
  :after python
  :commands python-black-buffer)

(use-package adaptive-wrap
  :straight adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package goto-last-change
  :general ('normal "g b" 'goto-last-change)
  :bind ("C-x C-j" . goto-last-change))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :init
  (which-key-mode t))

(use-package hl-todo
  :demand
  :hook ((python-mode . hl-todo-mode)
	 (LaTeX-mode . hl-todo-mode))
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

(use-package bibtex-actions
  ;; :straight (bibtex-actions :host github :repo "bdarcus/bibtex-actions")
  :general
  (:prefix "C-c b" "b" 'bibtex-actions-insert-citation)
  (:prefix "C-c b" "r" 'bibtex-actions-refresh)
  :config
  (setq bibtex-completion-bibliography
	'("/mnt/c/Users/c8441205/OneDrive/Academy/PhD/bibliography/numerical.bib"))
  (setq bibtex-completion-pdf-field "File")

  ;; set progam to open pdf with default windows application
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  ;; (call-process "cmd.exe" nil 0 nil (concat "/C start " fpath))
	  (shell-command (concat
			  "cmd.exe /C start \"\" "
			  (shell-quote-argument fpath)))))
  
  ;; Make the 'bibtex-actions' bindings available from `embark-act'.
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map)))

  ;; configue icons
  (setq bibtex-actions-symbols
	`((pdf . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
		  ,(all-the-icons-icon-for-file "foo.pdf" :face 'bibtex-actions-icon-dim)))
	  (note . (,(all-the-icons-icon-for-file "foo.txt") .
		   ,(all-the-icons-icon-for-file "foo.txt" :face 'bibtex-actions-icon-dim)))        
	  (link . 
		(,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
		 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface bibtex-actions-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces))

(use-package server
  :defer 1
  :init (server-start))

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
  (yas-minor-mode))

;; ensures environment variables inside Emacs is the same in the user's shell
(use-package exec-path-from-shell
  :defer 3
  :config
  ;; shell will inherits Emacs's environmental variables
  ;; remove -l -i flags (login and interactive) so it loads faster
  ;; set environment variables in ~/.bash_profile instead of ~/.bashrc
  (setq exec-path-from-shell-arguments nil)
  ;; emacs GUI inherits minimal environment variables
  ;; I'm using to run jupyter with the conda environment as default
  ;; (getenv "SHELL") "/bin/bash"
  (exec-path-from-shell-initialize))

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

(use-package hyperbole
  :general
  ('normal "z h" 'hyperbole)
  ("M-o" 'hkey-operate))

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

(use-package pdf-continuous-scroll-mode
  :straight (pdf-continuous-scroll-mode
	     :type git
	     :host github
	     :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :general
  ('normal pdf-continuous-scroll-mode-map
	   "j" 'pdf-continuous-scroll-forward
	   "k" 'pdf-continuous-scroll-backward)
  :hook (pdf-view-mode . pdf-continuous-scroll-mode))

(use-package hydra)

;; Terminal emulator based on libvterm (in C)
(use-package vterm
  :general
  (vterm-mode-map "<f8>" nil))

(use-package multi-vterm
  :general
  ("<f9>" 'multi-vterm))

(use-package keycast
  :commands keycast-mode keycast-log-mode)

(use-package gif-screencast
  :commands gif-screencast
  :general
  ("<f6>" 'gif-screencast)
  (gif-screencast-mode-map "<f6>" 'gif-screencast-stop)
  :config
  ;; change the function gif-screencast--generate-gif to generate file without ":"
  ;; (format-time-string "output-%F-%H-%M-%S.gif" (current-time))
  (setq gif-screencast-output-directory "./gif/"))


(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
(put 'list-threads 'disabled nil)
