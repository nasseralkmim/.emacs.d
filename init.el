;; (set-frame-size (selected-frame) 140 50)
(defvar my-start-time (current-time)
  "Time when Emacs was started")

(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; development branch of straight
(setq straight-repository-branch "develop")

;; Use Straight el
;; straight automatically checks if it need to be rebuilt
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

;; Install use-package
(straight-use-package 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)
;; makes :straight t by default
(setq straight-use-package-by-default t)

(use-package general); (general-def 'normal org-mode-map "key" 'def ) example with 2 positional arguments
(use-package diminish :defer t)

;; Speed up bootstrapping
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 100000000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)
(use-package emacs
  :straight nil
  :general
  ("C-<tab>" 'other-window)
  ("C-c w" 'shrink-window)
  :init
  (when (eq system-type 'windows-nt)
      (setq user-emacs-directory "c:/Users/nasse/.emacs.d/"))

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (winner-mode t)
  (repeat-mode t)

  (setq-default frame-title-format '("%b [%m]")) ;name on top of window
  (setq warning-minimum-level :error)		 ;avoid warning buffer

  (setq auto-window-vscroll nil) 		;avoid next-line to trigger line-move-partial
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  (setq ring-bell-function 'ignore)
  (setq mouse-wheel-progressive-speed nil)
  (setq inhibit-startup-screen t)
  (setq user-full-name "Nasser Alkmim"
	user-mail-address "nasser.alkmim@gmail.com")

  ;; UTF-8 please
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Don't create backups
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq create-lockfiles nil)		; files with # (problem with onedrive...)

  ;; Answering just 'y' or 'n' will do
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; (blink-cursor-mode 1)

  ;; Don't beep at me
  (setq visible-bell t)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
(use-package abbrev
  :straight nil
  :config
  ;; abbrev for speed and less strain
  (setq-default abbrev-mode t)
  (diminish 'abbrev-mode)
  (setq save-abbrevs 'silently))
(use-package emacs
  :straight nil
  :config
  ;; (set-face-attribute 'default nil :font "Iosevka etoile-10")
  ;; (set-face-attribute 'variable-pitch nil :font "Iosevka aile-10")
  (set-face-attribute 'default nil :height 95)
  )
(use-package benchmark-init
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(use-package color-identifiers-mode
  :defer t)
(use-package paren
  :defer 1
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))
(use-package recentf
  :defer 1
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 500
          recentf-max-menu-items 15
          recentf-auto-cleanup 60))
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
  ('normal "C-h f" 'helpful-callable)
  ('normal "C-h v" 'helpful-variable)
  ('normal "C-h k" 'helpful-key))
(use-package org-cliplink
  :commands org-cliplink
  :after org
  :general
  (org-mode-map "C-c C-l" 'org-cliplink))
(use-package vertico
  :init
  (vertico-mode))
(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
(use-package savehist
  :after vertico
  :config
  (savehist-mode))
(use-package selectrum			; like a vertical minibuffer
  :disabled
  :init (selectrum-mode t))
(use-package selectrum-prescient	;prescient provides matching of space-separated components
  :after selectrum
  :config
  (prescient-persist-mode t)
  (selectrum-prescient-mode t))
(use-package marginalia
  :disabled
  :after vertico
  :config (marginalia-mode))
(use-package consult
  :general
  ("M-s" 'consult-outline)
  ("C-c o" 'consult-imenu)
  ("C-x b" 'consult-buffer)
  ("M-y" 'consult-yank-pop)
  ("C-s" 'consult-line)    ;; C-r reverse
  (minibuffer-local-completion-map "<tab>" 'minibuffer-force-complete)
  :config
  (setq consult-preview-key nil
	consult-narrow-key "<")
  (consult-customize consult-line :preview-key 'any)
  ;; C-s C-s to search with previous search
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  (setf (alist-get #'consult-line consult-config) (list :keymap my-consult-line-map)))
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
	embark-become-indicator embark-action-indicator)
  )
(use-package embark-consult
  :demand t				;necessary for consult preview
  :hook (embark-collect-mode . embark-consult-preview-minor-mode)
  :after (embark consult))
(use-package all-the-icons :defer t)
(use-package smartparens
  :diminish smartparens-mode  
  :commands smartparens-mode
  :general
  ('normal smartparens-mode-map "M-l" 'sp-next-sexp)
  ('normal smartparens-mode-map "M-h" 'sp-previous-sexp)
  ('normal smartparens-mode-map "M-k" 'sp-up-sexp)
  ('normal smartparens-mode-map "M-j" 'sp-down-sexp)
  ('normal smartparens-mode-map "C-M-l" 'sp-forward-sexp)
  :init
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'lisp-interaction-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
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
  ('normal evil-mc-key-map "g c" evil-mc-cursors-map)
  ('normal evil-mc-key-map "g r" nil)
  :config (global-evil-mc-mode 1))
(use-package evil
  :defer 1
  :diminish evil-mode
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :general
  ('normal global "s" 'avy-goto-char-timer)
  ('normal ";" 'evil-search-forward)
  ('normal "M-p" 'evil-paste-from-register)
  ('normal :prefix "SPC" "l" 'evil-last-non-blank)
  ('normal :prefix "SPC" "h" 'evil-first-non-blank)
  ('normal :prefix "SPC" "a" 'evil-append-line)
  ('normal :prefix "SPC" "x s" 'save-buffer)
  ('normal "[ ]" 'evil-next-close-paren)
  ('normal "j" 'evil-next-visual-line)
  ('normal "k" 'evil-previous-visual-line)
  :config
  (setq
   lazy-highlight-cleanup nil
   lazy-highlight-max-at-a-time nil
   lazy-highlight-initial-delay 0)
  
  ;; fix tab behavior in org-source-block
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
	      :around #'evil-org-insert-state-in-edit-buffer)
  )
(use-package evil-smartparens
  :disabled
  :hook (smartparens-mode . evil-smartparens-mode)
  :after evil)
(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC"))
(use-package evil-snipe
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode)
  :general ('normal "f" 'evil-snipe-f)
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible
	evil-snipe-smart-case t))
(use-package evil-goggles
  :after evil
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse t)
  (setq evil-goggles-duration 0.2)
  (evil-goggles-use-diff-faces))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t
	evil-collection-company-use-tng nil) ; makes company works betters I think
  (evil-collection-init))
(use-package rg
  :general ('normal "C-c r" 'rg-menu))
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
  :general
  ('normal "g c" 'evil-surround-change)
  :init					; avoid kybind autoload
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
  (general-unbind 'normal magit-mode-map "C-<tab>")
  :bind ("C-c g" . magit-status))
(use-package rainbow-mode
  :defer 1
  :diminish rainbow-mode
  :config (rainbow-mode))
(use-package org
  ;; :straight org-plus-contrib
  :diminish org-indent-mode
  :mode (("\\.org$" . org-mode))
  :bind(("C-c c" . org-capture)
        ("C-c a" . org-agenda)
	:map org-mode-map
	("C-c l" . org-store-link)
	("M-p" . org-previous-item)
	("M-n" . org-next-item))
  :general
  ('normal org-mode-map :prefix "SPC" "x i" 'org-clock-in)
  ('normal org-mode-map :prefix "SPC" "x o" 'org-clock-out)
  ('normal org-mode-map :prefix "SPC" "x x" 'org-clock-in-last)
  ('normal org-mode-map "[ ]" 'outline-up-heading)
  ('normal org-mode-map :prefix "SPC" "u" 'outline-up-heading)
  ('normal org-mode-map :prefix "g" "s j" 'org-babel-next-src-block)
  ('normal org-mode-map :prefix "g" "s k" 'org-babel-previous-src-block)
  :hook (org-mode . visual-line-mode)
  :custom
   (org-hide-emphasis-markers t) 
   (org-startup-indented nil)
   (org-startup-folded t)
   (org-hide-leading-stars t) 
   (org-edit-src-content-indentation 0)
   (org-outline-path-complete-in-steps nil)
   (org-startup-with-inline-images t)
   (org-cycle-separator-lines 0)
   (org-fontify-quote-and-verse-blocks t)
  :config
  (transient-mark-mode -1)
  (setq org-todo-keywords '(
			    (sequence "TODO(t)" "NEXT(n)" "REVW(r)" "|" "DONE(d)")
			    (sequence "R1(1)" "R2(2)" "R3(3)" "R4(4)" "R5(5)" "R6(6)"))))
(use-package ob
  :straight nil
  :after org
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
  ;;   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))
  ;; (use-package org-clock
  ;;   :straight nil
  ;;   :after org
  ;;   :config
  ;;   ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  ;;   (setq org-clock-persistence-insinuate t
  ;; 	org-clock-persist t
  ;; 	org-clock-in-resume t
  ;; 	org-clock-out-remove-zero-time-clocks t
  ;; 	org-clock-mode-line-total 'current
  ;; 	org-duration-format (quote h:mm))
)
(use-package org-src
  :straight nil
  :after org
  :config
  ;; Org babel and source blocks
  (setq org-src-fontify-natively t
	;; org-highlight-latex-and-related '(latex)
	;; org-src-window-setup 'current-window
	;; org-src-strip-leading-and-trailing-blank-lines t
	org-src-preserve-indentation t  ; preserve indentation in code
	org-adapt-indentation nil ; Non-nil means adapt indentation to outline node level.
	org-src-tab-acts-natively nil ; evil handles that? default is t...
	org-export-babel-evaluate nil
	org-confirm-babel-evaluate nil) ; doesn't ask for confirmation
  )
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
  (setq org-babel-default-header-args:sh '((:results . "output")))
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
  (setq org-download-screenshot-file "./screenshot.png") ; where temporary screenshot will be saved so convert can work
  )
(use-package flyspell
  :diminish flyspell-mode
  :commands flyspell-mode
  :hook ((LaTeX-mode . flyspell-mode)
	 (org-mode . flyspell-mode))
  :config
  (setq ispell-program-name "hunspell")	; dictionary /usr/share/hunspell
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (when (eq system-type 'windows-nt)
    (setq ispell-dictionary "en_US,pt_BR")
    (ispell-hunspell-add-multi-dic "en_US,pt_BR"))
  (ispell-set-spellchecker-params)
  (setq flyspell-delay 5)		; 5 seconds
  )
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
(use-package company
  :diminish company-mode
  :commands company-mode
  :hook ((python-mode . company-mode)
	 (LaTeX-mode . company-mode)
	 (c++-mode . company-mode)
	 (emacs-lisp-mode . company-mode)
	 (org-mode . company-mode))
  :config
  (add-to-list 'company-backends 'company-capf)
  (when (eq system-type 'gnu/linux)
      (setq company-idle-delay 0
	    company-format-margin-function #'company-vscode-dark-icons-margin
	    company-minimum-prefix-length 1)))
(use-package company-posframe
  :disabled
  :diminish company-posframe-mode
  :after company
  :config
  (company-posframe-mode 1)
  (setq company-posframe-show-metadata nil
	company-posframe-show-indicator nil))
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))
(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom-face 
  (font-latex-sectioning-2-face ((t (:weight bold))))
  (font-latex-sectioning-3-face ((t (:weight bold :slant italic))))
  (font-latex-sectioning-4-face ((t (:slant italic))))
  (font-latex-sectioning-5-face ((t (:weight light))))
  :general
  (LaTeX-mode-map "C-M-y" 'my-tex-insert-clipboard)
  ('normal outline-mode-map
    "g j" nil
    "g k" nil)
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
	      (outline-hide-sublevels 1) ; start folded
              (turn-off-auto-fill)))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode) ; latex like org

  ;; preview latex
  (setq preview-default-option-list '("displaymath" "floats" "graphics" "textmath")
	preview-scale-function 1.25
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

  ;; using zathura on WSL
  (add-to-list 'TeX-view-program-selection
	       '(output-pdf "Zathura"))

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
(use-package outline-magic
  :after latex
  :general
  ('normal outline-mode-map "z j" 'outline-next-visible-heading)
  ('normal outline-mode-map "z k" 'outline-previous-visible-heading)
  ('normal LaTeX-mode-map "<tab>" 'outline-cycle)
  ('normal outline-mode-map "C-j" nil))
(use-package dired
  :straight nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (setq dired-auto-revert-buffer t))
(use-package dired-subtree
  :after dired
  :general ('normal dired-mode-map "<tab>" 'dired-subtree-toggle))
(use-package treemacs
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay      0.9
	  treemacs-directory-name-transformer    #'identity
	  treemacs-display-in-side-window        t
	  treemacs-eldoc-display                 t
	  treemacs-file-event-delay              3000
	  treemacs-file-extension-regex          treemacs-last-period-regex-value
	  treemacs-file-follow-delay             0.5
	  treemacs-file-name-transformer         #'identity
	  treemacs-follow-after-init             t
	  treemacs-git-command-pipe              ""
	  treemacs-goto-tag-strategy             'refetch-index
	  treemacs-indentation                   1
	  treemacs-indentation-string            " "
	  treemacs-is-never-other-window         t
	  treemacs-max-git-entries               5000
	  treemacs-missing-project-action        'ask
	  treemacs-move-forward-on-expand        nil
	  treemacs-no-png-images                 nil
	  treemacs-no-delete-other-windows       t
	  treemacs-project-follow-cleanup        nil
	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                      'left
	  treemacs-read-string-input             'from-child-frame
	  treemacs-recenter-distance             0.1
	  treemacs-recenter-after-file-follow    nil
	  treemacs-recenter-after-tag-follow     nil
	  treemacs-recenter-after-project-jump   'always
	  treemacs-recenter-after-project-expand 'on-distance
	  treemacs-show-cursor                   nil
	  treemacs-show-hidden-files             t
	  treemacs-silent-filewatch              nil
	  treemacs-silent-refresh                nil
	  treemacs-sorting                       'mod-time-desc ; modified early
	  treemacs-space-between-root-nodes      t
	  treemacs-tag-follow-cleanup            t
	  treemacs-tag-follow-delay              1.5
	  treemacs-user-mode-line-format         nil
	  treemacs-user-header-line-format       nil
	  treemacs-width                         35
	  treemacs-workspace-switch-cleanup      nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-toggle-fixed-width)
    )
  (setq treemacs-git-mode nil)
  (treemacs-resize-icons 12)
  :general
  (treemacs-mode-map "<f8>" 'treemacs-quit)
  ("<f8>" 'treemacs-select-window)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("<f8>"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
(use-package treemacs-evil
  :after treemacs evil)
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
(use-package doom-modeline
  :disabled
  :config
  (doom-modeline-mode)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-icon t))
(use-package modus-themes
  :init
  (setq modus-themes-org-blocks 'rainbow
	modus-themes-hl-line 'intense-background
	modus-themes-completions 'opinionated
	modus-themes-mode-line nil)
  (load-theme 'modus-operandi t)
  :general
  ("<f5>"  'modus-themes-toggle))
(use-package htmlize
  :defer t)
(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))
(use-package dap-mode
  :after lsp-mode)
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
	lsp-headerline-breadcrumb-enable nil ;disable breadcrumb
	))
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
  :defer 3
  :diminish which-key-mode
  :config
  (which-key-mode t))
(use-package hl-todo
  :after python
  :hook (python-mode . hl-todo-mode))
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
(use-package epresent
  :commands epresent-run)
(use-package emacs
  :defer 1
  :if (memq system-type '(windows-nt))
  :config
  ;; open cmd
  (defun my-open-cmd ()
    "open cmd at file location"
    (interactive)
    (start-process-shell-command (format "pwsh (%s)" default-directory) nil "start pwsh"))
  (when (eq system-type 'windows-nt)
    (bind-key "C-x m" 'my-open-cmd)))
(use-package server
  :defer 3
  :config (server-start))
(use-package table
  :after org)
(use-package wsl-path
  :straight nil
  :load-path "./lisp"
  :commands (wsl-path-activate
	     wsl-path-convert-file-name)
  :init
  (wsl-path-activate))
(use-package yasnippet
  :config
  (yas-global-mode))


(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
