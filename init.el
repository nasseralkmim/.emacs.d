(defvar my-start-time (current-time)
  "Time when Emacs was started")

(if (eq system-type 'windows-nt)
    (setq user-emacs-directory "c:/Users/nasse/.emacs.d/"))

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
;; makes :straight t by default
(setq straight-use-package-by-default t)
(use-package general) 			; (general-def 'normal org-mode-map "key" 'def ) example with 2 positional arguments
(use-package diminish :defer t)

;; Speed up bootstrapping
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 100000000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)
(use-package emacs
  :general
  ("C-<tab>" 'other-window)
  :init
  (setq w32-pipe-read-delay 0)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (winner-mode t)

  (set-frame-size (selected-frame) 140 50)

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

  (blink-cursor-mode 0)

  ;; Don't beep at me
  (setq visible-bell t)
  
;; set a default font Iosevka, Hack, PragmataPro
  (set-face-attribute 'default nil :font "Iosevka etoile-10")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka-10")
  (set-face-attribute 'variable-pitch nil :font "Iosevka aile-10")
  ;; ;; specify font for all unicode characters
  ;;(set-fontset-font t 'unicode (font-spec :family "Iosevka") nil 'prepend)
  ;; For testing purposes: →„Σ"←

  ;; abbrev for speed and less strain
  (setq-default abbrev-mode t)
  (diminish 'abbrev-mode)
  (setq save-abbrevs 'silently)
  )
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(use-package color-identifiers-mode
  :defer t)
(use-package paren
  :config (show-paren-mode 1))
(use-package recentf
  :defer 10
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 500
          recentf-max-menu-items 15
          recentf-auto-cleanup 60))
(use-package autorevert
  :defer 10
  :config (global-auto-revert-mode 1))
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
(use-package org-appear
  :straight (org-appear :type git :host github :repo "/awth13/org-appear")
  :hook (org-mode . org-appear-mode))
(use-package selectrum			; like a vertical minibuffer
  :init
  (selectrum-mode +1))
(use-package selectrum-prescient	;prescient provides matching of space-separated components
  :after selectrum
  :config
  (prescient-persist-mode t)
  (selectrum-prescient-mode t))
(use-package marginalia
  :config (marginalia-mode))
(use-package consult
  :bind (("C-c o" . consult-imenu)
	 ("C-x b" . consult-buffer)
	 ("C-s" . consult-line)    ;; C-r reverse
	 ("M-y" . consult-yank-pop)
	 ("<help> a" . consult-apropos)
	 ("C-S-s" . consult-isearch)
	 :map minibuffer-local-completion-map
	 ("<tab>" . minibuffer-force-complete))
  :general
  ('normal :prefix "SPC" "x b" 'consult-buffer)
  ('normal :prefix "SPC" "x o" 'consult-imenu))
(use-package embark
  :general
  ("C-S-a" 'embark-act)
  :config
  (setq embark-prompter 'embark-completing-read-prompter)
  )
(use-package embark-consult
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
  ('normal :prefix "g z" "m" 'evil-mc-make-all-cursors)
  ('normal :prefix "g z" "u" 'evil-mc-undo-all-cursors)
  ('normal :prefix "g z" "c" 'evil-mc-make-cursor-here)
  ('normal :prefix "g z" "n" 'evil-mc-make-and-goto-next-match)
  ('normal :prefix "g z" "p" 'evil-mc-make-and-goto-prev-match)
  ('normal :prefix "g z" "N" 'evil-mc-skip-and-goto-next-match)
  ('normal :prefix "g z" "P" 'evil-mc-skip-and-goto-prev-match)
  ('normal evil-mc-key-map "C-n" 'evil-mc-make-and-goto-next-cursor)
  :config (evil-mc-mode 1))
(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-keybinding nil)
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
  :config
  (setq
   lazy-highlight-cleanup nil
   lazy-highlight-max-at-a-time nil
   lazy-highlight-initial-delay 0)
  )
(use-package evil-smartparens
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
  (setq evil-goggles-pulse nil)
  (setq evil-goggles-duration 0.2)
  (evil-goggles-use-diff-faces))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
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
  :bind ("C-c g" . magit-status))
(use-package evil-magit
  :after magit evil)
(use-package rainbow-mode
  :defer 5
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
  (org-mode-map "<C-tab>" nil)
  ('normal org-mode-map :prefix "SPC" "x i" 'org-clock-in)
  ('normal org-mode-map :prefix "SPC" "x o" 'org-clock-out)
  ('normal org-mode-map :prefix "SPC" "x x" 'org-clock-in-last)
  ('normal org-mode-map "[ ]" 'outline-up-heading)
  :hook (org-mode . visual-line-mode)
  :custom
   (org-hide-emphasis-markers t) 
   (org-startup-indented nil)
   (org-startup-folded t)
   (org-hide-leading-stars t) 
   (org-hide-leading-stars-before-indent-mode nil)
   (org-odd-levels-only t)
   (org-edit-src-content-indentation 0)
   (org-goto-interface 'outline-path-completion) ;; org goto play nice with ivy
   (org-goto-max-level 4)
   (org-outline-path-complete-in-steps nil)
   (org-startup-with-inline-images t)
   (org-cycle-separator-lines 0)
   (org-fontify-quote-and-verse-blocks t)
  :config
  (transient-mark-mode -1)
  
  
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persistence-insinuate t
	org-clock-persist t
	org-clock-in-resume t
	org-clock-out-remove-zero-time-clocks t
	org-clock-mode-line-total 'current
	org-duration-format (quote h:mm))

  (setq org-todo-keywords '(
			    (sequence "TODO(t)" "NEXT(n)" "REVW(r)" "|" "DONE(d)")
			    (sequence "R1(1)" "R2(2)" "R3(3)" "R4(4)" "R5(5)" "R6(6)")))

  (use-package jupyter
    :general
    (org-mode-map "C-c =" 'jupyter-org-hydra/body))

  ;; The workaround is to just download and extract the module file
  ;; manually into the root directory of the zmq library.  download
  ;; emacs-zmq.dll from:
  ;; https://github.com/dzop/emacs-zmq/releases/download/v0.10.10/emacs-zmq-x86_64-w64-mingw32.tar.gz
  ;; and put on .emacs.d/straight/build/zmq

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (ditaa . t)
     (C . t)
     (python . t)
     (jupyter . t)))

  (setq org-ditaa-jar-path "C:/Program Files/ditaa/ditaa.jar")
  (setq org-image-actual-width '(350))
  (setq python-shell-interpreter "/usr/bin/python")
  (setq org-babel-python-command "python3")
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						      (:session . "python")
						      (:kernel . "python")
						      (:results . "output")
						      (:exports . "both")))

  (setq jupyter-org-resource-directory "./jupyter/")
  ;; minted code pdf export org
  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("newfloat" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; 
  ;; Org babel and source blocks
  (setq org-src-fontify-natively t
	;; org-highlight-latex-and-related '(latex)
	;; org-src-window-setup 'current-window
	;; org-src-strip-leading-and-trailing-blank-lines t
	org-src-preserve-indentation t  ; preserve indentation in code
	org-adapt-indentation nil ; Non-nil means adapt indentation to outline node level.
	org-src-tab-acts-natively t
	org-export-babel-evaluate nil
	org-confirm-babel-evaluate nil) ; doesn't ask for confirmation

 ;; ;;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
				 "~/OneDrive/Org/notes.org"
				 "~/OneDrive/Org/journal.org"
				 "~/OneDrive/Org/gcal.org"
				 "~/OneDrive/Concurso/Notas/notas_concurso.org")))
  (setq org-imenu-depth 2)

  (require 'ox-latex)
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
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  )
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
  ("C-M-y" 'org-download-screenshot)
  :config
  (setq
   ;; choco install imagemagick.app -PackageParameters LegacySupport=true
   org-download-screenshot-method "imagemagick/convert"
   org-download-image-dir "."
   org-download-image-html-width 350)
  (if (eq system-type 'gnu/linux)
      (setq org-download-screenshot-method "convert.exe clipboard: %s") ; add .exe to work within wsl2
    (setq org-download-screenshot-file "./tmp/screenshot.png")) ; where temporary screenshot will be saved so convert can work
    )
(use-package flyspell
  :diminish flyspell-mode
  :commands flyspell-mode
  :hook ((LaTeX-mode . flyspell-mode)
	 (org-mode . flyspell-mode))
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-goto-next-error)
  :config
  (setq ispell-program-name "hunspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (if (eq system-type 'windows-nt)
      '((setq ispell-dictionary "en_US,pt_BR")
       (ispell-hunspell-add-multi-dic "en_US,pt_BR")))
  (ispell-set-spellchecker-params))
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
  (if (eq system-type 'gnu/linux)
      (setq company-idle-delay 0.25
	    company-minimum-prefix-length 2
	    lsp-idle-delay 0.25)))
(use-package company-box
  :diminish company-box-mode
  :if (memq system-type '(gnu/linux))
  :hook (company-mode . company-box-mode))
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))
(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :general
  ('normal "<SPC> v" 'TeX-view)
  :init
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (prettify-symbols-mode)
              (LaTeX-math-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)
              (turn-off-auto-fill)))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode) ; latex like org
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

  ;; shell escape for minted (syntax highlight)
  (setq LaTeX-command "latex -shell-escape")

  ;; Method for enabling forward and inverse search 
  (setq TeX-source-correlate-method 'synctex)
  ;; inhibit the question to start a server process
  (setq TeX-source-correlate-start-server t)

  ;; jump to source
  (setq TeX-source-correlate-mode t)

  ;; Update PDF buffers after successful LaTeX runs  
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook  
	    'TeX-revert-document-buffer)

    ;; use sumatra to view pdf
    ;; http://stackoverflow.com/questions/14448606/sync-emacs-auctex-with-sumatra-pdf
    ;; -set-color-range #fdf4c1 #282828
  (when (eq system-type 'windows-nt)
    (setq TeX-view-program-list
	  '(("Sumatra PDF" ("\"C:/Users/nasse/AppData/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
			    (mode-io-correlate " -forward-search %b %n ") " %o"))))
    (assq-delete-all 'output-pdf TeX-view-program-selection)
    (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))
  (when (eq system-type 'gnu/linux)
    (setq TeX-view-program-list
	  '(("Sumatra PDF" ("\"/mnt/c/Users/nasse/AppData/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
			    (mode-io-correlate " -forward-search %b %n ") " %o"))))
    (assq-delete-all 'output-pdf TeX-view-program-selection)
    (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))

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
  (LaTeX-mode-map "<tab>" 'outline-cycle))
(use-package dired
  :straight nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (setq dired-auto-revert-buffer t))
(use-package dired-subtree
  :after dired
  :general ('normal dired-mode-map "C-c i" 'dired-subtree-insert))
(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
	  treemacs-deferred-git-apply-delay   0.5
	  treemacs-display-in-side-window     t
	  treemacs-file-event-delay           100
	  treemacs-file-follow-delay          0.2
	  treemacs-follow-after-init          t
	  treemacs-follow-recenter-distance   0
	  treemacs-git-command-pipe           ""
	  treemacs-goto-tag-strategy'refetch-index
	  treemacs-indentation                1
	  treemacs-indentation-string         " "
	  treemacs-is-never-other-window      t ; avoid jumping to treemacs when changing window
	  treemacs-max-git-entries            5000
	  treemacs-no-png-images              nil
	  treemacs-no-delete-other-windows    t
	  treemacs-project-follow-cleanup     nil
	  treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-recenter-after-file-follow nil
	  treemacs-recenter-after-tag-follow  nil
	  treemacs-show-cursor                nil
	  treemacs-show-hidden-files          t
	  treemacs-silent-filewatch           nil
	  treemacs-silent-refresh             nil
	  treemacs-sorting 'mod-time-desc
	  treemacs-space-between-root-nodes   t
	  treemacs-tag-follow-cleanup         t
	  treemacs-tag-follow-delay           1.5
	  treemacs-width                      25)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 12)
    ;; (treemacs-follow-mode nil)
    ;; (treemacs-filewatch-mode nil)
    (treemacs-fringe-indicator-mode t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :general
  (treemacs-mode-map "<f8>" 'treemacs-quit)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("<f8>"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
(use-package treemacs-all-the-icons
  :disabled
  :after treemacs)
(use-package treemacs-evil
  :after treemacs evil)
(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))
(use-package solaire-mode
  :hook (after-init . solaire-global-mode)
  :config
  (setq solaire-mode-auto-swap-bg nil))
(use-package doom-themes
  :disabled
  :after solaire-mode
  :config
  (load-theme 'doom-dracula t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t
	doom-themes-treemacs-theme "doom-colors") ; if nil, italics is universally disabled
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
(use-package doom-modeline
  :after solaire-mode
  :config
  (doom-modeline-mode)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-icon t))
(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode))
(use-package modus-themes
  :init
  (setq modus-themes-org-blocks 'grayscale)
  (load-theme 'modus-vivendi t)
  :bind ("<f5>" . modus-themes-toggle))
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
  :mode ("\\.cpp\\'" . cc-mode)
  :general
  (c++-mode-map "C-x c" 'compile)
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :general
  ('normal "K" 'lsp-describe-thing-at-point)
  ('normal "g d" 'lsp-find-definition)
  ('normal "g e" 'lsp-find-references)
  ('normal lsp-mode-map :prefix "SPC" "c f" 'lsp-format-buffer)
  :hook ((python-mode . lsp-deferred)
	 (c++-mode . lsp))
  :custom
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 1500)
  (if (eq system-type 'windows-nt)		;slow on windows...
      ((lsp-signature-render-documentation nil) ;don't show docstirng in modeline
       (lsp-signature-auto-activate nil) ; don't show help in the modeline
       (read-processpre-output-max (* 1024 1024))
       (lsp-completion-show-detail nil)	;don't show detail on company completion
       (lsp-completion-show-kind nil)	; don't show kind in company completion
       (lsp-auto-execute-action nil) 	; don't execute single action
       (lsp-before-save-edits nil)		; avoid apply edits before saving
       (lsp-keymap-prefix "C-,")
       (lsp-eldoc-enable-hover nil)		;don't show eldoc on modeline
       (lsp-modeline-diagnostics-enable nil)
       (lsp-modeline-code-actions-enable nil)
       (lsp-diagnostic-package :none)
       (lsp-completion-provider :none)
       (lsp-enabale-links nil)))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))
(use-package lsp-python-ms
  :config (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))  ; or lsp-deferred
(use-package lsp-ui
  :if (memq system-type '(gnu/linux))
  :after lsp)
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
(use-package hydra
  :defer 5
  :bind (("C-c C-w" . hydra-window-resize/body)
         ("C-c C-u" . hydra-outline/body)
         ("C-x C-'" . hydra-fold/body))
  :config
  (defhydra hydra-expand-region ()
    "region: "
    ("k" er/expand-region "expand")
    ("j" er/contract-region "contract"))
  (general-def 'visual 'global "v" 'hydra-expand-region/body)

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
  :general ('normal "g b" 'goto-last-change)
  :bind ("C-x C-j" . goto-last-change))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package which-key
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
  :straight (bibtex-actions :host github :repo "bdarcus/bibtex-actions")
  :general
  (:prefix "C-c b" "b" 'bibtex-actions-insert-citation)
  :config
  (setq bibtex-completion-bibliography "C:/Users/nasse/OneDrive/Academy/PhD/bibliography/references.bib")
  (setq bibtex-completion-pdf-field "File")

  (defun helm-w32-prepare-filename (file)
    "Convert filename FILE to something usable by external w32 executables."
    (replace-regexp-in-string ; For UNC paths
     "/" "\\"
     (replace-regexp-in-string ; Strip cygdrive paths
      "/cygdrive/\\(.\\)" "\\1:"
      file nil nil) nil t))
  (defun helm-w32-shell-execute-open-file (file)
    (with-no-warnings
      (w32-shell-execute "open" (helm-w32-prepare-filename file))))
  (defun helm-open-file-with-default-tool (file)
    "Open FILE with the default tool on this platform."
    (let (process-connection-type)
      (if (eq system-type 'windows-nt)
	  (helm-w32-shell-execute-open-file file)
	)))
  (setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)
 
  (with-eval-after-load 'embark
    (setf (alist-get 'bibtex embark-keymap-alist) 'bibtex-actions-map))

  (setq bibtex-actions-icon
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
    :group 'all-the-icons-faces)
  )

(use-package emacs
  :if (memq system-type '(windows-nt))
  :init
  ;; open cmd
  (defun my-open-cmd ()
    "open cmd at file location"
    (interactive)
    (start-process-shell-command (format "pwsh (%s)" default-directory) nil "start pwsh"))
  (if (eq system-type 'windows-nt)
      (bind-key "C-x m" 'my-open-cmd)))
(use-package server
  :defer 10
  :config (server-start))
(use-package table
  :after org)


(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))


