(defvar my-start-time (current-time)
  "Time when Emacs was started")
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; UTF-8 please
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; (setq inhibit-compacting-font-caches t)
;; (setq w32-pipe-read-delay 0)

;; Added by Package.el.  This must come before configurations of
;; installed packages.
(package-initialize)
;; (load-theme 'manoj-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1) 
(setq-default bidi-display-reordering t)
(setq timer-max-repeats 1)
(setq initial-scratch-message "")
(setq initial-major-mode 'lisp-mode)

;; Don't load old .elc files when the .el file is newer
(setq load-prefer-newer t)
(setq inhibit-startup-screen t)
(setq user-full-name "Nasser Alkmim"
      user-mail-address "nasser.alkmim@gmail.com")

 ;;; Set up package
;; initalize all ELPA packages
(require 'package)
(setq package-enable-at-startup nil
      package-archives
      '(("melpa"           . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/"))
      package-user-dir "~/.emacs.d/elpa/")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish :ensure t)
(require 'bind-key)
(setq use-package-verbose nil
      use-package-minimum-reported-time 0.01)
(setq use-package-enable-imenu-support t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(use-package challenger-deep-theme
  :disabled
  :ensure t
  :config
  (load-theme 'challenger-deep t)
  (let* ((headline `(:background nil :box nil)))
    (custom-theme-set-faces
     'user
     `(org-level-4 ((t (:height 1.0))))
     `(org-level-3 ((t (:height 1.0))))
     `(org-level-2 ((t (,@headline :height 1.1))))
     `(org-level-1 ((t (,@headline :height 1.2))))
     `(org-document-title ((t (,@headline :height 1.25 :underline nil))))
     `(org-block-begin-line ((t (:box nil))))
     `(org-block-end-line ((t (:box nil)))))))
(use-package habamax-theme
  :disabled
  :ensure t
  :config
  (setq habamax-theme-variable-heading-heights nil)
  (load-theme 'habamax t))
(use-package flatui-dark-theme
  :ensure t
  :config
(custom-theme-set-faces
     'user
     `(org-level-4 ((t (:height 1.0))))
     `(org-level-5 ((t (:foreground "#c0392b"))))
     `(org-level-3 ((t (:height 1.0))))
     `(org-level-2 ((t (:height 1.0))))
     `(org-level-1 ((t (:height 1.0))))
     `(org-document-title ((t (:height 1.0 :underline nil))))
     `(org-block-begin-line ((t (:box nil))))
     `(org-block-end-line ((t (:box nil)))))
  (load-theme 'flatui-dark t))
(use-package gruvbox-theme
  :disabled
  :ensure t
  :config (load-theme 'gruvbox-dark-hard t))
(use-package zerodark-theme
  :ensure t
  :disabled
  :config
  (load-theme 'zerodark t)
  (setq inhibit-compacting-font-caches t)
  (zerodark-setup-modeline-format))
(use-package color-identifiers-mode
  :ensure t
  :defer t)
(use-package darkokai-theme
  :ensure t
  :disabled
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))
(use-package moe-theme
  :disabled
  :ensure t
  :config
  (setq moe-theme-highlight-buffer-id nil)
  (moe-dark))

;; ;; set a default font Iosevka, Hack, PragmataPro
;; (set-face-attribute 'default nil
;;                     :family "Iosevka"
;;                     :height 100
;;                     :weight 'normal
;;                     :width 'normal)
;; ;; ;; specify font for all unicode characters
;; (set-fontset-font t
;;                   'unicode
;;                   (font-spec :family "Dejavu Sans mono"
;;                              :width 'normal
;;                              :height 100
;;                              :weight 'normal) nil 'prepend)
;; ;; For testing purposes: →„Σ"←

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; from Sacha page
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq-default indent-tabs-mode nil)

;; use shift-arrows to move between windows
(windmove-default-keybindings)

;; highlight current line
(setq global-hl-line-mode 0)
                                        ; wrap lines
;; (global-visual-line-mode)
;; (diminish 'visual-line-mode)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

(column-number-mode t)
;; unprettify symbol when at right edge
(setq prettify-symbols-unprettify-at-point 'right-edge) 
(setq uniquify-buffer-name-style 'forward)
;; Don't beep at me
(setq visible-bell t)

;; Don't create backups
(setq make-backup-files nil)
(set-fringe-mode '(6 . 0))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(use-package recentf
  :defer 10
  :config
  (progn
    (recentf-mode t)
    (setq recentf-max-saved-items 500
          recentf-max-menu-items 15)))
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
  (setq org-special-ctrl-a/e t)
  (transient-mark-mode -1)
  (setq org-log-done 'time)         ;Log the time a task is completed.
  (setq org-habit-graph-column 53) ;position the habit graph on the agenda to the right of he defaul 
  (setq org-habit-following-days 4)
  (setq org-hide-emphasis-markers t) 
  (setq inhibit-splash-screen t)
  (setq org-indent-mode nil)         ;indent the headings for clean view
  (setq org-startup-indented nil)
  (setq org-hide-leading-stars t) 
  (setq org-startup-align-all-tables nil)
  (setq org-hide-leading-stars-before-indent-mode nil)
  (setq org-odd-levels-only t)
  ;; (setq org-tags-column -66) ;where the tags are places
  (setq org-use-speed-commands t)
  (setq org-edit-src-content-indentation 0)
  (setq org-support-shift-select t)
  (setq line-spacing '0.1 )
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :underline nil)
  (setq org-modules '(org-habit))
  (eval-after-load 'org
    '(org-load-modules-maybe t))
  
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf::\\([0-9]+\\)\\'" . "sumatrapdf \"%s\" -page %1")
                        ("\\.pdf\\'" . default)))

  (setq org-cycle-include-plain-lists t)
  (setq org-image-actual-width nil)
  (setq org-startup-with-inline-images t)
  ;; (set-face-attribute 'org-block-begin-line nil :foreground "#005f87")
  ;; (set-face-attribute 'org-block-end-line nil :foreground "#3a3a3a")
  ;; org markups meta line --> change to grey100 when presenting
  (set-face-attribute 'org-meta-line nil :height 0.8)
  (set-face-attribute 'org-special-keyword nil :height  0.8)

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
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))
(use-package ox-extra
  :after org
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
(use-package ox-reveal
  :disabled
  :ensure t
  :after org
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t))
(use-package ox-beamer
  :after org
  :config
  (progn
    ;; allow for export=>beamer by placing
    ;; #+LaTeX_CLASS: beamer in org files
    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass[presentation]{beamer}"
                   ("\\section{%s}"        . "\\section*{%s}")
                   ("\\subsection{%s}"     . "\\subsection*{%s}")
                   ("\\subsubsection{%s}"  . "\\subsubsection*{%s}")))))
(use-package ox-latex
  :after org
  :config
  ;; code highlight for latex org export
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-minted-options
        '(("frame" "lines") ("linenos" "true") ("fontsize" "\\scriptsize")
          ("breakbytoken" "true") ("baselinestretch" ".8")))
  ;; add a nice font to org latex export
  (add-to-list 'org-latex-packages-alist '("" "libertine"))
  (add-to-list 'org-latex-packages-alist '("" "inconsolata"))
  ;; For code fragments typesetting
  ;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
  (setq org-latex-listings 'minted)

  (add-to-list 'org-latex-minted-langs '(ipython "python"))

                                        ; change foreground color of latex macros inside org
  (setq org-highlight-latex-and-related '(latex script entities))

  ;; use imagemagick to preview latex
  (setq org-latex-create-formula-image-program 'imagemagick)
  ;; add shell escape to pdflatex command
  (setq org-preview-latex-process-alist 
        '((dvipng :programs
                  ("latex" "dvipng")
                  :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("latex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
          (dvisvgm :programs
                   ("latex" "dvisvgm")
                   :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
                   (1.7 . 1.5)
                   :latex-compiler
                   ("latex -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("latex" "convert")
                       :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O")))))
(use-package ox-md
  :after org)
(use-package org-download
  :disabled
  :ensure t
  :after org
  :config
  (setq-default org-download-image-dir "./img/")
  (setq-default org-download-heading-lvl nil))
(use-package ob-async
  :disabled
  :ensure t
  :after org
  :config
  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block))
(use-package org-ref
  :disabled
  :after org
  :ensure t
  :config
  (setq org-ref-default-bibliography 
        '("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib"))
  (setq org-ref-pdf-directory 
        '("C:/Users/Nasser/OneDrive/Bibliography/references-pdf/"
          "C:/Users/Nasser/OneDrive/Bibliography/references-etc/"))
  (setq org-ref-completion-library 'org-ref-ivy-bibtex)
  (org-ref-ivy-cite-completion))
(use-package org-sticky-header
  :ensure t
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'reversed))
(use-package org-super-agenda
  :disabled
  :ensure t
  :after org-agenda
  :init (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))
(use-package org
  :defer t
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "REVW" "|" "DONE(d)")))
  ;; 
  (setq org-todo-keyword-faces 
        '(("TODO" :background "tomato" :foreground "#000000"  :weight bold :height 0.8)
          ("NEXT" :background "#edd400" :foreground "#000000"  :weight bold :height 0.8)
          ("DONE" :background "#6ac214" :foreground "#000000"  :weight bold :height 0.8)
          ("REVW" :background "deep sky blue" :foreground "#000000"  :weight bold :height 0.8)))
  
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-cycle-separator-lines 0)) 
(use-package org-clock
  :after org
  :config
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persistence-insinuate t)
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)
  (setq org-clock-mode-line-total 'current)
  (setq org-duration-format (quote h:mm))
  (setq org-clocktable-defaults
        '(:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize t :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns 3 :formatter nil))
  ;; remove schedule tag on agenda
  (setq org-agenda-scheduled-leaders '("" ""))

  ;; Save clock data and notes in the LOGBOOK drawer
  ;; (setq org-clock-into-drawer t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-clock-into-drawer 1)
  
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t))
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
(use-package org-tree-slide
  :ensure t
  :bind (("<f9>" . org-tree-slide-mode)
         ("<f12>" . org-tree-slide-move-next-tree)
         ("<f11>" . org-tree-slide-move-previous-tree)
         ("C-<f12>" . org-babel-next-src-block)
         ("C-<f11>" . org-babel-previous-src-block))
  :config
  (global-set-key (kbd "S-<f9>") 'org-tree-slide-skip-done-toggle)
  (org-tree-slide-simple-profile)
  (setq org-tree-slide-modeline-display 'outside)
  (setq org-tree-slide-cursor-init nil))
(use-package org-page
  :ensure t
  :bind (("C-x C-a p" . op/do-publication-and-preview-site)
         ("C-x C-a C-p" . op/do-publication)
         ("C-x C-a C-n" . op/new-post))
  :config
  (setq op/repository-directory "c:/Users/Nasser/OneDrive/nasseralkmim.github.io/")
  (setq op/site-domain "http://nasseralkmim.github.io/")
  (setq op/personal-disqus-shortname "nasseralkmim")
  (setq op/site-main-title "Nasser Alkmim")
  (setq op/site-sub-title " ")
  (setq op/personal-github-link "https://github.com/nasseralkmim")
  (setq op/personal-google-analytics-id "UA-74704246-1")

  (setq op/tag-rss t)                   ;rss for each tag

  (setq op/theme-root-directory "c:/Users/Nasser/OneDrive/nasseralkmim.github.io/themes/")
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
                                "[/\\\\]+"))))))
(use-package org
  :defer t
  :config
  (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
                                 "~/OneDrive/Org/notes.org"
                                 "~/OneDrive/Org/journal.org"
                                 "~/OneDrive/Org/gcal.org"
                                 "~/OneDrive/Concurso/Notas/notas_concurso.org")))

  (setq 
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-timestamp-if-done nil)

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
  :disabled
  :defer t
  :config
  (defun ded/org-shonw-next-heading-tidily ()
    "Show next entry, keeping other entries closed."
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (show-children))
      (outline-next-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)))

  (defun ded/org-show-previous-heading-tidily ()
    "Show previous entry, keeping other entries closed."
    (let ((pos (point)))
      (outline-previous-heading)
      (unless (and (< (point) pos) (bolp) (org-on-heading-p))
        (goto-char pos)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)))

  (add-to-list 'org-speed-commands-user
               '("n" ded/org-show-next-heading-tidily))
  (add-to-list 'org-speed-commands-user
               '("p" ded/org-show-previous-heading-tidily)))
(use-package toc-org
  :ensure t
  :after org
  :init  (add-hook 'org-mode-hook 'toc-org-enable))
(use-package multiple-cursors
  :ensure t
  :defer t)
(use-package avy
  :ensure t 
  :diminish avy-mode
  :bind (("C-x C-SPC" . avy-goto-char)
         ("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line))
  :config
  (setq avy-timeout-seconds 0.4))
(use-package smartscan
  :ensure t
  :bind (("M-p" . smartscan-symbol-go-back)
         ("M-n" . smartscan-symbol-go-forward))
  :config
  (smartscan-mode 1))
(use-package dumb-jump
  :ensure t
  :bind ("C-M-g". dumb-jump-go)
  :config
  (dumb-jump-mode))
(use-package ace-window
  :ensure t 
  :config
  (setq aw-scope 'global)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (ace-window-display-mode)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0)))))
  :bind (("C-o " . other-window)
         ("C-x o " . ace-window)))
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-s" . counsel-grep-or-swiper)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file))
  :config
  (use-package smex :ensure t))
(use-package ivy-posframe
  :ensure t
  :disabled
  :after ivy
  :config
  (setq ivy-display-function #'ivy-posframe-display-at-point))
(use-package ivy
  :ensure t
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
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-abbreviate-paths t)
  (setq ivy-rich-path-style 'abbrev))
(use-package ivy-bibtex
  :ensure t
  :bind ("C-c b b" . ivy-bibtex)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq bibtex-completion-bibliography 
        '("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib"))
  (setq bibtex-completion-library-path 
        '("C:/Users/Nasser/OneDrive/Bibliography/references-pdf"
          "C:/Users/Nasser/OneDrive/Bibliography/references-etc"))

  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "File")

  ;; ;;open pdf with external viwer foxit
  ;; (setq bibtex-completion-pdf-open-function
  ;;       (lambda (fpath)
  ;;         (call-process "C:/Program Files (x86)/Foxit Software/Foxit Reader/FoxitReader.exe" nil 0 nil fpath)))
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "SumatraPDF" nil 0 nil fpath)))

  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  )
(use-package powerthesaurus
  :ensure t
  :disabled
  :commands powerthesaurus-lookup-word)
(use-package swiper
  :ensure t
  :bind (("C-c u" . swiper-all))
  :config
  (setq swiper-include-line-number-in-search t))
(use-package hydra
  :defer 5
  :ensure t
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
(use-package ivy-hydra
  :ensure t
  :defer t
  :after hydra)
(use-package magit
  :ensure t 
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

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))
(use-package counsel-projectile
  :ensure t
  :bind ("C-c p p " . counsel-projectile-switch-project)
  :config
  (counsel-projectile-mode))
(use-package projectile
  :ensure t
  :after counsel-projectile
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy) ;So projectile works with ivy
  (setq projectile-indexing-method 'alien))
(use-package go-mode
  :disabled
  :ensure t
  :mode ("\\.go\\'" . go-mode))
(use-package hide-comnt
  :load-path "c:/Users/Nasser/.emacs.d/elpa"
  :commands hide/show-comments-toggle)
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
  :ensure t
  :after python
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'lisp-interaction-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))
(use-package python-docstring
  :ensure t
  :disabled
  :after python
  :init
  (add-hook 'python-mode-hook #'python-dosctring-mode))
(use-package lpy
  :disabled
  :load-path "C:/Users/Nasser/.emacs.d/elpa/lpy-master"
  :after python
  :init (add-hook 'python-mode-hook 'lpy-mode))
(use-package worf
  :disabled
  :ensure t
  :after lpy)
(use-package elpy 
  :disabled
  :ensure t
  :after python
  :bind (:map elpy-mode-map 
              ("C-c C-k" . elpy-shell-kill))
  :config
  (electric-indent-local-mode -1)
  (elpy-use-ipython)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)
  (delete 'elpy-module-company elpy-modules)
  (delete 'elpy-module-yasnippet elpy-modules)
  (delete 'elpy-module-django elpy-modules)
  (elpy-enable)
  ;; use py.test
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (defun elpy-goto-definition-or-rgrep ()
    "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
      (error (elpy-rgrep-symbol
              (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
  (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep))
(use-package cl
  :after elpy)
(use-package hl-todo
  :ensure t
  :after python
  :init (add-hook 'python-mode-hook 'hl-todo-mode))
(use-package anaconda-mode
  :ensure t
  :after python
  :init
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'anaconda-mode))
(use-package company-anaconda
  :ensure t
  :after anaconda-mode
  :config
  (setq jedi:complete-on-dot t)
  (setq company-minimum-prefix-length 1)
  (remove-hook 'anaconda-mode-response-read-fail-hook
               'anaconda-mode-show-unreadable-response)
  (add-to-list 'company-backends 'company-anaconda))
(use-package company-childframe
  :diminish company-childframe-mode
  :ensure t
  :after company
  :config
  (company-childframe-mode 1)
  ;; let desktop.el not record the company-childframe-mode
  (require 'desktop) ;this line is needed.
  (push '(company-childframe-mode . nil)
        desktop-minor-mode-table))
(use-package company-quickhelp
  :disabled
  :ensure t
  :after python
  :config (company-quickhelp-mode 1))
(use-package prescient
  :ensure t) 
(use-package ivy-prescient
  :ensure t
  :after ivy
  :config
  (ivy-prescient-mode))
(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode))
(use-package realgud
  :ensure t
  :commands (realgud:ipdb)
  :config
  (setq gud-tooltip-mode 1)
  (setq tooltip-mode 1))
(use-package smartparens
  :diminish smartparens-mode  
  :ensure t
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
  (setq show-paren-style 'mixed))       ;expression
(use-package latex-extra
  :ensure t
  :after latex
  :init
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  :config
  (setq latex/override-font-map nil)
  (auto-fill-mode -1))
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
      ))
  )
(use-package reftex
  :after latex
  :ensure t
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
  :ensure t
  :after latex
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography
	'("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib")))
(use-package flycheck
  :ensure t
  :init
  (add-hook 'python-mode-hook 'flycheck-mode)
  :commands flycheck-mode
  :config
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode nil)
  (setq flycheck-display-errors-delay 1.5)
  (setq flycheck-idle-change-delay 3))
(use-package flyspell-lazy
  :ensure t
  :init
  (add-hook 'LaTeX-mode-hook 'flyspell-lazy-mode)
  (add-hook 'org-mode-hook 'flyspell-lazy-mode)
  :config
  (flyspell-lazy-mode 1))
(use-package langtool
  :ensure t
  :commands langtool-check
  :config
  (setq langtool-language-tool-jar "c:/Users/Nasser/.emacs.d/LanguageTool-4.0/languagetool-commandline.jar")
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  (setq langtool-autoshow-message-function
        'langtool-autoshow-detail-popup))
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
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c C-SPC" . flyspell-correct-word-generic)))
(use-package company
  :diminish company-mode
  :ensure t
  :commands company-mode
  :init
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'lisp-interaction-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.1
        company-echo-delay 0 ; remove annoying blinking)
        company-show-numbers t 
        company-require-match nil  ; 'company-explicit-action-p
        company-tooltip-flip-when-above t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  ;; display inline
  (setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend)))
(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))
(use-package company-flx
  :ensure t
  :disabled
  :after company
  :config
  (company-flx-mode +1)
  (setq company-flx-limit 400))
(use-package undo-tree
  :ensure t 
  :bind ("C-z" . undo-tree-undo)
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)))
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (emacs-lisp-mode . rainbow-mode))
(use-package rainbow-delimiters
  :ensure t 
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTex-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode))
(use-package which-key
  :ensure t
  :defer 5
  :diminish (which-key-mode)
  :config
  (which-key-mode)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))
(use-package key-chord
  :ensure t
  :after evil
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "]]" "\\")
  (key-chord-define evil-insert-state-map ";;" "/")
  (key-chord-define evil-insert-state-map "::" "?")
  (key-chord-define evil-insert-state-map "}}" "|")
  (key-chord-define evil-insert-state-map "==" "+")
  (key-chord-define evil-insert-state-map "99" "(")
  (key-chord-define evil-insert-state-map "--" "_")
  (key-chord-define evil-insert-state-map "''" "^")
  (key-chord-define evil-insert-state-map "[[" "{"))
(use-package neotree
  :disabled
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-smart-open t)
  (setq neo-vc-integration nil)
  ;; Do not allow neotree to be the only open window
  (setq-default neo-dont-be-alone t)
  (setq neo-fit-to-contents nil)
  (setq neo-theme 'arrow)
  (setq neo-window-fixed-size nil))
(use-package treemacs
  :disabled
  :ensure t
  :config
  (setq treemacs-collapse-dirs
        (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
  :bind (("<f8>". treemacs)
        ("M-0" . treemacs-find-file)))
(use-package treemacs-evil
  :disabled
  :after treemacs evil
  :ensure t)
(use-package pfuture
  :ensure t
  :after treemacs)
(use-package all-the-icons
  :ensure t
  :disabled 
  :after treemacs)
(use-package beacon
  :diminish beacon-mode
  :ensure t
  :defer 10
  :config
  (setq beacon-blink-delay .5)
  (setq beacon-size 4)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-duration .5)
  (setq beacon-blink-when-window-scrolls t)
  (beacon-mode 1))
(use-package dired+
  :disabled
  :ensure t
  :defer t)
(use-package dired
  :commands dired
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (setq dired-hide-details-mode t)
  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup))
(use-package direx
  :disabled
  :ensure t
  :after dired
  :bind ("C-x C-j" . direx:jump-to-directory)
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-listing-switches "-alhv"))
(use-package feebleline
  :disabled
  :ensure t
  :defer 1
  :config
  (window-divider-mode t)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places (quote bottom-only))
  (feebleline-mode t))
(use-package smart-mode-line
  :ensure t
  :disabled
  :config
  (setq sml/name-width 20)
  (setq sml/shorten-directory t)
  (setq sml/theme 'dark)
  (sml/setup))
(use-package imenu-anywhere
  :disabled
  :ensure t
  :bind* ("C-." . imenu-anywhere))
(use-package imenu-list
  :disabled
  :ensure t
  :bind ("C-." . imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t))
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package evil-multiedit
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("C-;" . evil-multiedit-match-all))
  :config
  (evil-multiedit-default-keybinds))
(use-package iedit
  :disabled
  :ensure t
  :bind ("C-;" . iedit-mode))
(use-package goto-last-change
  :ensure t
  :bind ("C-x C-j" . goto-last-change))
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'smartparens-mode)
  (use-package smartparens-html))
(use-package highlight-numbers
  :ensure t
  :diminish highlight-numbers-mode
  :commands highlight-numbers-mode
  :init (add-hook 'python-mode-hook 'highlight-numbers-mode))
(use-package highlight-operators
  :ensure t
  :diminish highlight-operators-mode
  :commands highlight-operators-mode
  :init (add-hook 'python-mode-hook 'highlight-operators-mode))
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :disabled
  :ensure t
  :commands highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'matlab-mode-hook #'highlight-symbol-mode))
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :ensure t
  :commands highlight-parentheses-mode
  :init
  (add-hook 'LaTeX-mode-hook 'highlight-parentheses-mode)
  (add-hook 'python-mode-hook 'highlight-parentheses-mode))
(use-package ibuffer
  :ensure t
  :bind ("C-c C-b" . ibuffer))
(use-package matlab-mode
  :disabled
  :ensure t
  :mode ("\\.m\\'" . matlab-mode))
(use-package eldoc
  :config
  (global-eldoc-mode -1))
(use-package git-gutter+
  :diminish git-gutter+-mode
  :ensure t
  :bind (("C-M-z C-M-s" . git-gutter+-stage-hunks)
         ("C-M-z C-M-c" . git-gutter+-stage-and-commit))
  :init
  (add-hook 'python-mode-hook 'git-gutter+-mode)
  (add-hook 'lisp-interaction-mode-hook 'git-gutter+-mode))
(use-package git-gutter-fringe+
  :ensure t
  :after git-gutter+
  :config
  (git-gutter-fr+-minimal))
(use-package column-marker
  :disabled
  :ensure t
  :commands column-marker-1
  :init
  (add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80))))
(use-package epa-file
  :after org-crypt
  :config
  (epa-file-enable))
(use-package org-crypt
  :after org
  :commands (org-encrypt-entry org-decrypt-entry)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "CB17DA00")
  (setq org-crypt-disable-auto-save nil))
(use-package vimish-fold
  :disabled
  :ensure t
  :bind (("C-' C-'" . vimish-fold-toggle)
         ("C-' C-c" . vimish-fold)
         ("C-' C-d" . vimish-fold-delete)
         ("C-' C-a" . vimish-fold-toggle-all))
  :init
  (add-hook 'lisp-interaction-mode-hook 'vimish-fold-mode)
  (add-hook 'python-mode-hook 'vimish-fold-mode))
(use-package origami
  :ensure t
  :disabled
  :bind (("C-<tab>" . origami-recursively-toggle-node)
         ("C-M-<tab>". origami-toggle-all-nodes))
  :init
  (add-hook 'lisp-interaction-mode-hook 'origami-mode)
  (add-hook 'python-mode-hook 'origami-mode))
(use-package helpful
  :ensure t
  :disabled)
(use-package pdf-tools
  :after latex
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :ensure t
  :config
  (pdf-tools-install))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
(use-package focus
  :ensure t
  :commands 'focus-mode
  :disabled)
(use-package use-package-chords
  :ensure t
  :disabled t
  :config (key-chord-mode 1))
(use-package evil
  :ensure t
  :diminish evil-mode
  :defer 1
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "pk" 'sp-up-sexp)
  (key-chord-define evil-normal-state-map ";." 'evil-search-forward)
  (key-chord-define evil-normal-state-map "xc" 'avy-goto-char)
  (key-chord-define evil-normal-state-map "xs" 'save-buffer)
  (evil-define-key 'normal 'global "s" 'avy-goto-char-timer)
  (evil-define-key 'normal 'global "j" 'evil-next-visual-line)
  (evil-define-key 'normal 'global "k" 'evil-previous-visual-line))
(use-package evil-easymotion
  :ensure t
  :after evil
  :config
  (evilem-default-keybindings "SPC"))
(use-package evil-leader
  :ensure t
  :disabled
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
      "e" 'counsel-find-file
      "b" 'ivy-switch-buffer))
(use-package evil-snipe
  :ensure t
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode)
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible))
(use-package evil-smartparens
  :ensure t
  :diminish smartparens-strict-mode
  :hook ((smartparens-enabled . smartparens-strict-mode)
         (smartparens-enabled . evil-smartparens-mode))
  :after smartparens)
(use-package evil-cleverparens
  :ensure t
  :disabled
  :diminish evil-cleverparens-mode
  :after evil 
  :init (setq evil-cleverparens-use-s-and-S nil)
  :hook (emacs-lisp-mode . evil-cleverparens-mode))
(use-package evil-goggles
  :ensure t
  :after evil
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))
(use-package evil-collection
  :ensure t
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
                                    calendar
                                    eww))
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-outline-bind-tab-p nil)
  (evil-collection-init))
(use-package evil-org
  :ensure t
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
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package fold-dwim
  :ensure t
  :bind ("C-<tab>" . fold-dwim-toggle)
  :commands fold-dwim-toggle) 
(use-package crux
   :ensure t
   :bind (("C-a" . crux-move-beginning-of-line)
          ("S-<ret>" . crux-smart-open-line)))
(use-package academic-phrases
  :ensure t
  :commands (academic-phrases academic-phrases-by-section))
(use-package define-word
  :ensure t
  :commands define-word-at-point)
(use-package dashboard
  :disabled
  :ensure t
  :config
  (setq dashboard-items '((agenda . 10) (recents . 5)))
  (dashboard-setup-startup-hook))
(defun my/open-cmd()
  (interactive)
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
    (set-process-query-on-exit-flag proc nil)))
(bind-key "C-x m" 'my/open-cmd)
(setq ad-redefinition-action 'accept)
(winner-mode 1)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-set-key (kbd "M-]") 'delete-horizontal-space)
(setq resize-mini-windows t) ;; was grow-only
(setq focus-follows-mouse t)
(other-window 1 'visible)
(select-frame-set-input-focus (selected-frame))
(bind-key "C-x C-o" 'next-multiframe-window)
(setq mouse-autoselect-window nil)
;; (setq mouse-wheel-scroll-amount '(5)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mous
(setq auto-window-vscroll nil)
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))
(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))
(defun set-68-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 68))
(global-set-key "\C-x8" 'set-80-columns)
(global-set-key "\C-x7" 'set-68-columns)
(setq-default fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))
(setq-default fringe-indicator-alist (assq-delete-all 'continuation fringe-indicator-alist))
(setq right-fringe-width 0)
(set-display-table-slot standard-display-table 'wrap ?\ )

(put 'set-goal-column 'disabled nil)
;; (setq redisplay-dont-pause t
;;       scroll-margin 1
;;       scroll-step 1
;;       scroll-conservatively 1000
;;       scroll-preserve-screen-position 1)
(defun open-buffer-path ()
  "Run explorer on the directory of the current buffer."
  (interactive)
  (shell-command (concat "explorer "
                         (replace-regexp-in-string "/" "\\\\"
                                                   (file-name-directory
                                                    (buffer-file-name)) nil nil))))
(global-set-key [M-f9] 'open-buffer-path)
;; Then reset it as late as possible; these are the reasonable defaults I use.
(setq gc-cons-threshold 1000000
      gc-cons-percentage 0.1)
(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
