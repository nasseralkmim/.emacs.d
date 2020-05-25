(defvar my-start-time (current-time)
  "Time when Emacs was started")


;;; Bootstrap
;; Speed up bootstrapping
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 800000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)

;; maybe improve performance on windows
(setq w32-pipe-read-delay 0)

(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(global-eldoc-mode -1)
(global-hl-line-mode 1)
(column-number-mode 1)
(winner-mode t)


(setq auto-window-vscroll nil) 		;avoid next-line to trigger line-move-partial
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq ring-bell-function 'ignore)
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(setq user-full-name "Nasser Alkmim"
      user-mail-address "nasser.alkmim@gmail.com")

(lambda () (progn
  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (set-window-buffer nil (current-buffer))))

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

(setq use-package-verbose t		; report details
      use-package-expand-minimally t	; expanded coded as minimal as possible
      use-package-enable-imenu-support t ; imenu can find definitions
      use-package-always-defer nil      ;I'm not used to that
      use-package-minimum-reported-time 0.01)

;; Manually set loat-path
(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file)

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


;; (set-face-attribute 'default nil :height 100)

;; set a default font Iosevka, Hack, PragmataPro
(set-face-attribute 'default nil
                    :family "Iosevka sparkle"
		    ;; :family "IBM Plex Mono Medium"
                    :height 90
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

(blink-cursor-mode 1)

;; Don't beep at me
(setq visible-bell t)

(use-package benchmark-init
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(use-package general)
(use-package diminish :defer t)
(use-package color-identifiers-mode
  :defer t)
(use-package recentf
  :defer 10
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 500
          recentf-max-menu-items 15
          recentf-auto-cleanup 60))
(use-package deft
  :general ('normal "C-c d" 'deft)
  :config (setq deft-directory "~/OneDrive/Knowledge/ROAM"))
(use-package swiper
  :bind (("C-S-s" . swiper-isearch)
	 ("C-s" . swiper)))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file))
  :general
  ('normal org-mode-map "C-c C-j" 'counsel-org-goto)
  :config
  (use-package smex :ensure t))
(use-package ivy
  :diminish ivy-mode
  :general ('normal :prefix "SPC" "x b" 'ivy-switch-buffer)
  :bind (:map ivy-minibuffer-map
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
(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))
(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode))
(use-package all-the-icons-ivy
    :after (all-the-icons ivy)
    :custom (all-the-icons-ivy-file-commands '(counsel-dired-jump
                                               counsel-find-file
                                               counsel-file-jump
                                               counsel-find-library
                                               counsel-git
                                               counsel-projectile-find-dir
                                               counsel-projectile-find-file
                                               counsel-recentf))
    :config (all-the-icons-ivy-setup))
(use-package smartparens
  :diminish smartparens-mode  
  :commands smartparens-mode
  :general
  ;; ('normal smartparens-mode-map "M-l" 'sp-next-sexp)
  ;; ('normal smartparens-mode-map "M-h" 'sp-previous-sexp)
  ;; ('normal smartparens-mode-map "M-k" 'sp-up-sexp)
  ;; ('normal smartparens-mode-map "M-j" 'sp-down-sexp)
  ('normal smartparens-mode-map "C-M-l" 'sp-forward-sexp)
  :init
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'lisp-interaction-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  :config
  (sp-local-pair 'org-mode "_" "_" )
  (sp-local-pair 'org-mode "*" "*" )
  (sp-local-pair 'latex-mode "$" "$")
  (sp-local-pair 'latex-mode "\\left(" "\\right)" :trigger "\\l(")
  ;; highligh matching brackets
  (show-smartparens-global-mode 0)
  ;; so that paren highlights do not override region marking (aka selecting)
  (setq show-paren-priority -1) 
  (setq show-paren-when-point-inside-paren t)
  (setq sp-show-pair-from-inside t)
  (setq show-paren-style 'mixed)) 
(use-package flycheck
  :disabled
  :hook (python-mode . flycheck-mode))
(use-package evil
  :defer 1
  :diminish evil-mode
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :general
  ("<C-tab>" 'evil-window-next)
  ("<C-S-iso-left-tab>" 'evil-window-prev)
  ('normal global "s" 'avy-goto-char-timer)
  ('normal ";" 'evil-search-forward)
  ('normal "M-p" 'evil-paste-from-register)
  ('normal :prefix "SPC" "l" 'evil-last-non-blank)
  ('normal :prefix "SPC" "h" 'evil-first-non-blank)
  :config
  (evil-mode 1)
  (setq
   lazy-highlight-cleanup nil
   lazy-highlight-max-at-a-time nil
   lazy-highlight-initial-delay 0))
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
      "d" 'downcase-word
      "u" 'upcase-word
      "t" 'org-todo
      "m" 'cfw:open-org-calendar
      "p" 'evil-jump-item
      "g c" 'my-magit-stage-all-and-commit
      "SPC" 'org-agenda-show-and-scroll-up
      "p" 'counsel-evil-registers
      "z s" 'org-narrow-to-subtree
      "z w" 'widen
      "x v" 'org-toggle-inline-images
      "x l" 'org-toggle-latex-fragment
      "x j" 'org-clock-goto
      "x u" 'outline-up-heading
      "x x" 'org-clock-in-last
      "<tab>" 'next-multiframe-window
      "," 'flyspell-correct-at-point
      "x s" 'save-buffer)

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
  :general ('normal "f" 'evil-snipe-f)
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible
	evil-snipe-smart-case t))
(use-package evil-numbers
  :after evil
  :general
  ('normal "C-c =" 'evil-numbers/inc-at-pt)
  ('normal "C-c -" 'evil-numbers/dec-at-pt)
  :diminish evil-numbers-modes)
(use-package evil-goggles
  :defer 30
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
  (setq evil-collection-outline-bind-tab-p nil)
  ;; (delete 'paren evil-collection-mode-list)
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
  :defer 25
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
  :init
  (global-undo-tree-mode -1)
  :config
  (setq undo-propose-pop-to-buffer t))
(use-package magit
  :bind ("C-c g" . magit-status)
  :commands my-magit-stage-all-and-commit
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
(use-package evil-magit
  :after magit evil)
(use-package rainbow-mode
  :defer 5
  :diminish rainbow-mode
  :config (rainbow-mode))
(use-package org
  :ensure org-plus-contrib
  :defer 2
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
  :init
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
	;; org-ellipsis "…"
	org-modules '(org-habit)
	org-cycle-include-plain-lists t
	org-image-actual-width nil
	org-goto-interface 'outline-path-completion ;; org goto play nice with ivy
	org-goto-max-level 4
	org-outline-path-complete-in-steps nil
	org-startup-with-inline-images t
	org-cycle-separator-lines 0
	org-fontify-whole-heading-line t
	org-fontify-done-headline nil
	org-fontify-quote-and-verse-blocks t)
	
  (set-face-attribute 'org-ellipsis nil :underline nil)
  ;; (eval-after-load 'org
  ;;   '(org-load-modules-maybe t)) ;; I think this makes org load slowly
  
  (setq org-file-apps '((auto-mode . emacs)
			("\\.mm\\'" . default)
			("\\.x?html?\\'" . default)
			("\\.pdf::\\([0-9]+\\)\\'" . "sumatrapdf \"%s\" -page %1")
			("\\.pdf\\'" . "\"c:/Program Files (x86)/Foxit Software/Foxit Reader/Foxit Reader/FoxitReader.exe\" \"%s\" ")
			;; ("\\.pdf\\'" . default)
			))
  
  ;; (set-face-attribute 'org-block-begin-line nil :foreground "#005f87")
  ;; (set-face-attribute 'org-block-end-line nil :foreground "#3a3a3a")
  ;; org markups meta line --> change to grey100 when presenting
  ;; (set-face-attribute 'org-meta-line nil :height 0.6)
  ;; (set-face-attribute 'org-drawer nil :height 0.6)
  ;; (set-face-attribute 'org-date nil :height 0.6)
  ;; (set-face-attribute 'org-special-keyword nil :height  0.6)

  (custom-set-variables
   ;; here there is customization for other variables 
   '(org-priority-faces (quote ((65 . "LightPink") (66 . "OrangeRed") (67 . "IndianRed")))))

  (setq org-todo-keywords '(
			    (sequence "TODO(t)" "NEXT(n)" "REVW(r)" "|" "DONE(d)")
			    (sequence "R1(!)" "R2(!)" "R3(!)" "R4(!)" "R5(!)" "R6(!)")))
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
     (C . t)
     (latex . t)
     (shell . t)))

  (setq org-babel-C++-compiler "clang")

  ;; plantuml jar file path
  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/plantuml.jar"))
  (setq org-babel-default-header-args:python
        '((:exports . "both")
          (:results . "output")))
  (setq org-babel-default-header-args:C++
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
	org-agenda-use-time-grid nil
	org-scheduled-past-days 1000)	; don't show delayed task on other days

  (setq org-imenu-depth 2)
  (setq org-default-notes-file "~/OneDrive/Org/notes.org")

  ;; Which days are weekend?
  ;; (setq org-agenda-weekend-days nil)
  ;; start agenda on current day
  (setq org-agenda-start-on-weekday 1)
  ;; min and max percentages of agenda window height
  ;; (setq org-agenda-window-frame-fractions '(0 . 1))

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
           "* %T \n\n%?")))
  (require 'org-depend)
  )
(use-package org-cliplink
  :after org
  :bind (:map org-mode-map
	      ("C-x p i" . 'org-cliplink)))
(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ("C-c o" . crux-open-with))
  :general ('normal :prefix "SPC" "c" 'crux-capitalize-region))
(use-package org-roam-server
  :hook (org-roam-mode . org-roam-server-mode))
(use-package org-roam
  :custom (org-roam-directory "~/OneDrive/Knowledge/ROAM")
  :general
  ('normal :keymap 'org-roam-mode-map :prefix "SPC" "n f" 'org-roam-find-file)
  ('normal :keymap 'org-roam-mode-map :prefix "SPC" "n l" 'org-roam)
  (:states '(normal visual) :keymaps 'org-mode-map :prefix "SPC" "n i" 'org-roam-insert)
  ('insert :keymap 'org-mode-map "C-c n i" 'org-roam-insert)
  :config
  (org-roam-mode +1)
  (setq org-roam-completion-system 'ivy))
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
    (let* ((filename (concat 
                        (buffer-name)
                        "_"
                        (format-time-string "%Y%m%d_%H%M%S_.png")))
	   (filepath (concat myvar/folder-path
			     filename))

           ;; (exit-status
           ;;  (call-process "convert" nil nil nil
           ;;                "clipboard:" image-file))
	   )
      ;; http://www.sastibe.de/2018/11/take-screenshots-straight-into-org-files-in-emacs-on-win10/
      (shell-command "snippingtool /clip")
      (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filepath "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
      (insert "#+attr_org: :width 400 \n")
      (insert (concat "[[file:img/" filename "]]"))
  ;; (org-insert-link nil (concat "file:" filename ) nil)

      (org-display-inline-images))))
(use-package org-clock
  :after org
  :ensure nil
  :general
  ('normal 'org-mode-map :prefix "SPC" "x i" 'org-clock-in)
  ('normal 'org-mode-map :prefix "SPC" "x o" 'org-clock-out)
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
  (setq org-agenda-scheduled-leaders '("" "")
	org-agenda-block-separator "")

  ;; Save clock data and notes in the LOGBOOK drawer
  ;; (setq org-clock-into-drawer t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-clock-into-drawer 1)
  
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t))
(use-package org-sticky-header
  :disabled
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'reversed))
(use-package flyspell
  :diminish flyspell-mode
  :commands flyspell-mode
  :hook (('LaTeX-mode . flyspell-mode)
	 ('org-mode . flyspell-mode))
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-goto-next-error)
  :config
  (setq ispell-program-name "hunspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (setq ispell-dictionary "en_US,pt_BR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,pt_BR"))
(use-package flyspell-lazy
  :commands flyspell-lazy-mode
  :hook ((LaTeX-mode . flyspell-lazy-mode)
	 (org-mode . flyspell-lazy-mode)))
(use-package flyspell-correct-ivy
  :demand t
  :after flyspell-lazy
  :bind (:map flyspell-mode-map
	      ("C-c C-SPC" . flyspell-correct-wrapper)
	      ("C-c C-;" . flyspell-correct-at-point))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))
(use-package company
  :diminish company-mode
  :commands company-mode
  :hook (
	 ;; (python-mode . company-mode)
	 (LaTeX-mode . company-mode)
	 (c++-mode . company-mode)
	 (emacs-lisp-mode . company-mode)
	 (org-mode . company-mode))
  :config
  ;; (setq company-idle-delay 0.2
  ;;       company-echo-delay 0 ; remove annoying blinking
  ;;       company-minimum-prefix-length 2
  ;;       company-show-numbers t
  ;; 	company-tooltip-limit 5
  ;; 	company-transformers nil
  ;;       company-require-match 'never  ; 'company-explicit-action-p
  ;; 	company-selection-wrap-around t
  ;;       company-tooltip-flip-when-above t
  ;;       company-dabbrev-ignore-case 'keep-prefix 
  ;;       company-dabbrev-downcase 'case-replace)
  ;; ;; display inline
  ;; (setq company-frontends
  ;;     '(company-pseudo-tooltip-unless-just-one-frontend
  ;;       company-preview-frontend
  ;;       company-echo-metadata-frontend))
  )
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))
(use-package company-posframe
  :diminish company-posframe-mode
  :after company
  :config
  (company-posframe-mode 1))
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :bind ("C-S-f" . forward-whitespace)
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
	 '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
			   (mode-io-correlate " -forward-search %b %n ") " %o"))))
   (assq-delete-all 'output-pdf TeX-view-program-selection)
   (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))
   )
  (when (eq system-type 'gnu/linux)
    ;; (setq TeX-view-program-list '(("evince" "evince --page-index=%(outpage) %o")))
    (assq-delete-all 'output-pdf TeX-view-program-selection)
    (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
    )
  
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
  (setq reftex-trust-label-prefix '("fig:" "eq:"))
  (setq reftex-default-bibliography "C:/Users/nasse/OneDrive/Bibliography/references-zot.bib"))
(use-package company-bibtex
  :after latex company
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography
	'("~/OneDrive/Bibliography/references-zot.bib")))
(use-package ivy-bibtex
  :bind ("C-c b b" . ivy-bibtex)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq bibtex-completion-bibliography 
        '("~/OneDrive/Bibliography/references-zot.bib"))
  ;; (setq bibtex-completion-library-path 
  ;;       '("c:/Users/nasse/OneDrive/Bibliography/references-pdf"
  ;;         "c:/Users/nasse/OneDrive/Bibliography/references-etc"))

  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "File")
  
  (if (eq system-type 'windows-nt)
   ;; ;;open pdf with external viwer foxit
   (setq bibtex-completion-pdf-open-function
         (lambda (fpath)
           (call-process "C:/Program Files (x86)/Foxit Software/Foxit Reader/Foxit Reader/FoxitReader.exe" nil 0 nil
			 fpath)))
   )
  
  ;; (setq bibtex-completion-pdf-open-function
  ;;       (lambda (fpath)
  ;;         (call-process "SumatraPDF" nil 0 nil fpath)))

  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))
(use-package dired
  :ensure nil
  :commands dired
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (setq dired-auto-revert-buffer t)
  (setq dired-hide-details-mode t)
  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup))

(use-package treemacs
  :ensure t
  :defer t
  ;; :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
	  treemacs-deferred-git-apply-delay   0.5
	  treemacs-display-in-side-window     t
	  treemacs-file-event-delay           5000
	  treemacs-file-follow-delay          0.2
	  treemacs-follow-after-init          nil
	  treemacs-follow-recenter-distance   0
	  treemacs-git-command-pipe           ""
	  treemacs-goto-tag-strategy'refetch-index
	  treemacs-indentation                1
	  treemacs-indentation-string         " "
	  treemacs-is-never-other-window      nil
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
	  treemacs-sorting 'alphabetic-desc
	  treemacs-space-between-root-nodes   t
	  treemacs-tag-follow-cleanup         t
	  treemacs-tag-follow-delay           1.5
	  treemacs-width                      25)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 12)
    ;; (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
(use-package treemacs-evil
  :after treemacs evil
  :ensure t)
(use-package solaire-mode
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg))
(use-package base16-theme
  :disabled
  :config
  (load-theme 'base16-rebecca t))
(use-package kaolin-themes
  :disabled
  :config
  (load-theme 'kaolin-ocean t)
  (kaolin-treemacs-theme))
(use-package doom-themes
  :disabled
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
	doom-themes-enable-italic t
	doom-treemacs-use-generic-icons nil) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)

  (custom-theme-set-faces
   'user
   `(show-paren-match ((t (:underline t :weight ultra-bold))))
   ;; `(show-paren-match-expression ((t (:foreground "#e45649" :background "#f0f0f0" :inherit nil))))
   `(show-paren-match-expression ((t (:inherit 'default :underline nil))))
   )
  )
(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))
(use-package doom-modeline
  :defer 3
  :config
  (doom-modeline-mode)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-icon t))
(use-package lsp-mode
  :commands lsp
  :general
  ('normal "K" 'lsp-describe-thing-at-point)
  ('normal "g d" 'lsp-find-definition)
  ('normal "g e" 'lsp-find-references)
  :hook ((python-mode . lsp)
	 (c++-mode . lsp))
  :config
  (setq lsp-highlight-symbol-at-point nil
	lsp-document-highlight-delay 1
	lsp-enable-snippet nil))
(use-package lsp-ui
  :disabled
  :after lsp-mode
  :commands lsp-ui-mode
  :commands
  (setq lsp-ui-doc-enable nil))
(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet nil
	lsp-enable-snippet nil))
(use-package flymake
  :commands flymake-mode
  :general
  ('normal "C-," 'flymake-goto-next-error)
  :config
  (setq flymake-max-parallel-syntax-checks 2
	flymake-no-changes-timeout 10
	flymake-number-of-errors-to-display 2))
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  (setq flymake-diagnostic-at-point-timer-delay 2))
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
	 (python-mode . dap-mode))
  :config
  (eval-when-compile
    (require 'cl))
  (require 'dap-python)
  (require 'dap-lldb))
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . toggle-truncate-lines)
  :config
  ;; dont guess the indent offset
  (setq python-indent-guess-indent-offset nil))
(use-package highlight-symbol
  :after python anaconda-mode
  :hook (python-mode . highlight-symbol-mode))
(use-package highlight-indent-guides
  :after python
  :diminish highlight-indent-guides-mode
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'lisp-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))
(use-package adaptive-wrap
  :ensure adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))
(use-package calfw
  :commands cfw:open-org-calendar
  :config
  (setq cfw:details-window-size 30))
(use-package calfw-org
  :after org calfw)
(use-package toc-org
  :after org
  :commands org-export-dispatch
  :config  (add-hook 'org-mode-hook 'toc-org-enable))
(use-package hydra
  :defer 5
  :bind (("C-c C-w" . hydra-window-resize/body)
         ("C-c C-u" . hydra-outline/body)
         ("C-x C-m " . multiple-cursors-hydra/body)
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
  :bind ("C-x C-j" . goto-last-change))
(use-package outline-mode
  :ensure nil
  :hook ((python-mode . outline-minor-mode)
	 (LaTeX-mode . outline-minor-mode)
	 (emacs-lisp-mode . outline-minor-mode))
  :commands outline-minor-mode)
(use-package realgud 
  :disabled
  :commands realgud:ipdb)
(use-package avy
  :diminish avy-mode
  :bind (("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line))
  :general
  ('evil-treemacs-state-map "g a" 'avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.4))
(use-package counsel-projectile
  :general
  ("C-c p f" 'counsel-projectile-find-file)
  :config
  (counsel-projectile-mode +1))
(use-package projectile
  :diminish projectile-mode
  :after counsel-projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy) ;So projectile works with ivy
  (setq projectile-git-submodule-command nil)
  (setq projectile-indexing-method 'alien))
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-tools-install))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package org-page
  :ensure t
  :bind (("C-x C-a p" . op/do-publication-and-preview-site)
         ("C-x C-a C-p" . op/do-publication)
         ("C-x C-a C-n" . op/new-post))
  :config
  (setq op/repository-directory "c:/Users/nasse/OneDrive/nasseralkmim.github.io/")
  (setq op/site-domain "http://nasseralkmim.github.io/")
  (setq op/personal-disqus-shortname "nasseralkmim")
  (setq op/site-main-title "Nasser Alkmim")
  (setq op/site-sub-title " ")
  (setq op/personal-github-link "https://github.com/nasseralkmim")
  (setq op/personal-google-analytics-id "UA-74704246-1")

  (setq op/tag-rss t)                   ;rss for each tag

  (setq op/theme-root-directory "c:/Users/nasse/OneDrive/nasseralkmim.github.io/themes/")
  (setq op/theme 'mdo)

  (setq op/category-ignore-list '("themes" "assets" "blog"))

  (setq op/category-config-alist
        '(("notes" ;; this is the default configuration
           :label "Notes"
           :show-meta t
           :show-comment t
           :uri-generator op/generate-uri
           :uri-template "/notes/%y/%m/%d/%t/"
           :sort-by :date     ;; how to sort the posts
           :category-index t) ;; generate category index or not
          ("index"
           :show-meta nil
           :show-comment nil
           :uri-generator op/generate-uri
           :uri-template "/"
           :sort-by :date
           :category-index nil)
          ("about"
           :show-meta nil
           :show-comment nil
           :uri-generator op/generate-uri
           :uri-template "/about/"
           :sort-by :date
           :category-index nil)))
  ;; remove blog heading
  (defun op/get-file-category (org-file)
    "Get org file category presented by ORG-FILE, return all categories if
ORG-FILE is nil. This is the default function used to get a file's category,
see `op/retrieve-category-function'. How to judge a file's category is based on
its name and its root folder name under `op/repository-directory'."
    (cond ((not org-file)
           (let ((cat-list '("index" "about"))) ;; 3 default categories
             (dolist (f (directory-files op/repository-directory))
               (when (and (not (equal f "."))
                          (not (equal f ".."))
                          (not (equal f ".git"))
                          (not (member f op/category-ignore-list))
                          (file-directory-p
                           (expand-file-name f op/repository-directory)))
                 (setq cat-list (cons f cat-list))))
             cat-list))
          ((string= (expand-file-name "index.org" op/repository-directory)
                    (expand-file-name org-file)) "index")
          ((string= (expand-file-name "about.org" op/repository-directory)
                    (expand-file-name org-file)) "about")
          (t (car (split-string (file-relative-name (expand-file-name org-file)
                                                    op/repository-directory)
                                "[/\\\\]+")))))
  (defun op/new-post (&optional category filename)
    "Setup a new post.

CATEGORY: this post belongs to
FILENAME: the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
    (interactive
     (let* ((c (read-string "Category: " "notes"))
	    (f (read-string "filename: " "new-post.org")))
       (list c f)))
    (if (string= category "")
	(setq category "notes"))
    (if (string= filename "")
	(setq filename "new-post.org"))
    (unless (string-suffix-p ".org" filename)
      (setq filename (concat filename ".org")))
    (let* ((dir (concat (file-name-as-directory op/repository-directory)
			(file-name-as-directory category)))
	   (path (concat dir filename)))
      (op/git-change-branch op/repository-directory op/repository-org-branch)
      (if (file-exists-p path)
	  (error "Post `%s' already exists." path))
      (unless (file-directory-p dir)
	(mkdir dir t))
      (switch-to-buffer (find-file path))
      (if (called-interactively-p 'any)
	  (call-interactively 'op/insert-options-template)
	(op/insert-options-template "<Insert Your Title Here>"
				    "/%y/%m/%d/%t/"
				    "add, keywords, here"
				    "add, tags, here"
				    "add description here"))
      (save-buffer)))

  (defun op/insert-options-template (&optional title uri
					       keywords tags description)
    "Insert a template into current buffer with information for exporting.

TITLE: the title of this post
URI: the uri of this post, usually looks like: /2013/12/27/the-post-title,
the following parameters could be used:
    %y: to represent the year of creation date
    %m: to represent the month of creation date
    %d: to represent the day of creation date
KEYWORDS: the keywords of this post, used by search engine
TAGS: the tags of this post, should be separated by comma and space
DESCRIPTION: the description of this post, it will be displayed in RSS feed

Note that this function does not verify the input parameters, it is users'
responsibility to guarantee these parameters are valid."
  (interactive
   (let* ((i (read-string "Title: "))
          (u (read-string "URI(%y, %m and %d can be used to represent year, \
month and day): " (unless (string= i "")
                    (format-spec "/notes/%y/%m/%d/%t"
                                 `((?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(encode-string-to-url i)))))))
          (k (read-string "Keywords(separated by comma and space [, ]): "))
          (a (read-string "Tags(separated by comma and space [, ]): "))
          (d (read-string "Description: ")))
     (list i u k a d)))
  (if (not (bolp)) (newline))
  (insert (format
	   "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+URI:         %s
#+KEYWORDS:    %s
#+TAGS:        %s
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+DESCRIPTION: %s
"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           (if (string= uri "") "<TODO: insert your uri here>" uri)
           (if (string= keywords "")
               "<TODO: insert your keywords here>"
             keywords)
           (if (string= tags "") "<TODO: insert your tags here>" tags)
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           nil ;; org-export-with-toc
           org-export-preserve-breaks
           ;; org-export-html-expand
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps
           (if (string= description "")
               "<TODO: insert your description here>"
             description)))) 

)
(use-package which-key
  :defer 20
  :config
  (which-key-mode t))
(use-package hl-todo
  :after python
  :hook (python-mode . hl-todo-mode))
;; abbrev for speed and less strain
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(setq save-abbrevs 'silently)

;; open cmd
(defun my-open-cmd ()
  "open cmd at file location"
  (interactive)
  (start-process-shell-command (format "cmd(%s)" default-directory) nil "start cmd"))
(if (eq system-type 'windows-nt)
    (bind-key "C-x m" 'my-open-cmd))
(if (eq system-type 'gnu/linux)
    (global-set-key (kbd "C-x m") (kbd "M-! urxvt RET")))

;; set 80 width columns
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))
(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 94))
(general-def 'normal "C-w 8" 'set-80-columns)

(server-start)
(global-auto-revert-mode)


(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))


