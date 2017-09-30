 ;; Added by Package.el.  This must come before configurations of
 ;; installed packages.  Don't delete this line.  If you don't want it,
 ;; just comment it out by adding a semicolon to the start of the line.
 ;; You may delete these explanatory comments.
 (package-initialize)

 (defvar my-start-time (current-time)
   "Time when Emacs was started")

 (menu-bar-mode 0)
 (tool-bar-mode 0)
 (scroll-bar-mode 0)
 (tooltip-mode 0)

 (setq initial-scratch-message "")

 ;; Don't load old .elc files when the .el file is newer
 (setq load-prefer-newer t)

 (setq inhibit-startup-screen t)

 ;; Don't edit this file, edit ~/.emacs.d/config.org instead ...

 (setq user-full-name "Nasser Alkmim"
       user-mail-address "nasser.alkmim@gmail.com")
 (package-initialize nil)
 (setq package-enable-at-startup nil)
 ;;; Set up package
 ;; initalize all ELPA packages
 (require 'package)
 (setq package-enable-at-startup nil
       package-archives
       '(("melpa"           . "http://melpa.org/packages/")
         ("gnu" . "http://elpa.gnu.org/packages/")
         ("org" . "http://orgmode.org/elpa/")))
 (unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))

 (eval-when-compile
   (require 'use-package))
 (require 'diminish)                ;; if you use :diminish
 (require 'bind-key)
 (setq use-package-verbose t)
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 (load custom-file)
(use-package moe-theme
  :disabled t
  :ensure t
  :config
  (setq moe-theme-highlight-buffer-id nil)
  (moe-dark))
 ;; set a default font Iosevka, Hack, 
 (set-face-attribute 'default nil :family "Iosevka Term" :height 90)

 ;; specify font for all unicode characters
(set-fontset-font t 'unicode "Dejavu Sans Mono" nil 'prepend)

   ;; These functions are useful. Activate them.
   (put 'downcase-region 'disabled nil)
   (put 'upcase-region 'disabled nil)
   (put 'narrow-to-region 'disabled nil)
   (put 'dired-find-alternate-file 'disabled nil)

   ;; Answering just 'y' or 'n' will do
   (defalias 'yes-or-no-p 'y-or-n-p)

   ;; UTF-8 please
   (prefer-coding-system 'utf-8)
   (set-default-coding-systems 'utf-8)
   (set-terminal-coding-system 'utf-8)
   (set-keyboard-coding-system 'utf-8)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

   ;; from Sacha page
   (when (display-graphic-p)
     (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

   (setq-default indent-tabs-mode nil)

   ;; use shift-arrows to move between windows
   (windmove-default-keybindings)

   ;; highlight current line
   ;; (global-hl-line-mode 1)
   ;; (set-face-background 'hl-line "SlateGray1")

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
  :defer 30
  :config
  (progn
    (recentf-mode t)
    (setq recentf-max-saved-items 500
          recentf-max-menu-items 15))
  (run-at-time (current-time) 300 'recentf-save-list))
(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :bind(("C-c a" . org-agenda)
        ("C-c l" . org-store-link)
        ("C-c c" . org-capture)
        ("M-p" . org-previous-item)
        ("M-n" . org-next-item))
  :init
  ;; (add-hook 'org-mode-hook 'visual-line-mode)
  :config
  (setq org-special-ctrl-a/e t)
  (transient-mark-mode nil)
  (setq org-log-done 'time)         ;Log the time a task is completed.
  (setq org-habit-graph-column 50) ;position the habit graph on the agenda to the right of he defaul 
  (setq org-hide-emphasis-markers t) 
  (setq inhibit-splash-screen t)
  (setq org-indent-mode t)         ;indent the headings for clean view
  (setq org-hide-leading-stars t) 
  (setq org-hide-leading-stars-before-indent-mode t)
  (setq org-odd-levels-only t)
  (diminish 'org-indent-mode)
  (setq org-startup-indented t)
  ;; (setq org-tags-column -66) ;where the tags are places
  (setq org-use-speed-commands t)
  (setq org-edit-src-content-indentation 0)
  (setq org-support-shift-select t)
  (setq line-spacing '0.1 )

  ;; This is for remove the annoying background color on the headings, 
  ;; level 1 and level 2, when using the material-theme. 
  (custom-set-faces
   '(org-level-1 ((t (:background nil :bold t :overline nil))))
   '(org-level-2 ((t (:background nil :bold t :overline nil)))))

  (setq org-agenda-weekend-days nil)

  (setq org-modules '(org-habit))
  (eval-after-load 'org
    '(org-load-modules-maybe t))
  
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf::\\([0-9]+\\)\\'" . "sumatrapdf \"%s\" -page %1")
                        ("\\.pdf\\'" . default)))

  (setq org-cycle-include-plain-lists 'integrate)
  (setq org-image-actual-width t)
  (setq org-startup-with-inline-images t)
  (set-face-attribute 'org-block-begin-line nil :height .7)
  (set-face-attribute 'org-block-end-line nil :height .5)
  ;; org markups meta line --> change to grey100 when presenting

  (set-face-attribute 'org-meta-line nil :height 0.8 :slant 'normal :foreground "grey70")
  (set-face-attribute 'org-special-keyword nil :height 0.8 :slant 'normal :foreground "grey70")

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
  :ensure t
  :after org
  :config
  (setq-default org-download-image-dir "./img/")
  (setq-default org-download-heading-lvl nil))
(use-package ob-async
  :disabled t
  :ensure t
  :after org
  :config
  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block))
(use-package org-ref
  :disabled t
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
(use-package org
  :defer t
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))

  (setq org-todo-keyword-faces 
        '(("TODO" :background "tomato" :foreground "#5f5f5f" :weight bold )
          ("STRT" :background "#edd400" :foreground "#5f5f5f" :weight bold )
          ("DONE" :background "#6ac214" :foreground "#5f5f5f" :weight bold )))
  
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
  
  ;; Change task state to STARTED when clocking in
  ;; (setq org-clock-in-switch-to-state "STRT")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
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
         :category-index nil))))
(use-package org
  :defer t
  :config
  (setq org-agenda-files (quote ("~/OneDrive/Org/gtd.org"
                                 "~/OneDrive/Org/notes.org"
                                 "~/OneDrive/Org/journal.org"
                                 "~/OneDrive/Org/gcal.org"
                                 "~/OneDrive/TerraCap/terracap_notes.org")))

  (setq 
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-timestamp-if-done t)

   (setq org-default-notes-file "~/OneDrive/Org/notes.org")

   
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
  :defer t
  :config
  (defun ded/org-show-next-heading-tidily ()
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
  :bind (("C-x C-m C-m" . mc/edit-lines)
         ("C-x C-m C-n" . mc/mark-next-like-this)))
 (defun my/open-cmd()
  (interactive)
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
    (set-process-query-on-exit-flag proc nil)))
 (bind-key "C-x m" 'my/open-cmd)
(use-package avy
  :ensure t 
  :diminish avy-mode
  :bind (("C-x C-SPC" . avy-goto-char)
         ("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line)))
(use-package dumb-jump
  :ensure t
  :bind ("C-M-g". dumb-jump-go)
  :config
  (dumb-jump-mode))
(use-package ace-window
  :ensure t 
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (ace-window-display-mode)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
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
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  ;; Disable ido
  (with-eval-after-load 'ido
    (ido-mode -1)
    ;; Enable ivy
    (ivy-mode 1))
  ;; for recent candidates
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  ;; ;; Show recently killed buffers when calling ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths
  ;; ;; Do not show "./" and "../" in the counsel-find-file completion list
  (setq ivy-extra-directories nil))
(use-package ivy-rich
    :after ivy
    :ensure t
    :config
    (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
    (setq ivy-virtual-abbreviate 'full
      ivy-rich-switch-buffer-align-virtual-buffer t)
    (setq ivy-rich-abbreviate-paths t))
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

  ;;open pdf with external viwer foxit
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "SumatraPDF" nil 0 nil fpath)))

  (setq ivy-bibtex-default-action 'bibtex-completion-insert-citation))
(use-package swiper
  :ensure t
  :bind (("C-c u" . swiper-all))
  :config
  (setq swiper-include-line-number-in-search t))
(use-package hydra
  :ensure t
  :bind (("C-c C-w" . hydra-window-resize/body)
         ("C-x C-o" . hydra-outline/body)
         ("C-x C-m " . multiple-cursors-hydra/body))
  :config
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
    ("q" nil)))
(use-package ivy-hydra
  :ensure t
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
  (counsel-projectile-on)
  (use-package projectile
    :ensure t 
    :diminish projectile-mode
    :init
    (projectile-global-mode)
    (setq projectile-completion-system 'ivy) ;So projectile works with ivy
    (setq projectile-indexing-method 'alien)))
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
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
;; https://github.com/hlissner/doom-emacs/blob/master/modules/lang/python/config.el#L16
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --no-color-info"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil))
(use-package highlight-indent-guides
  :ensure t
  :after python
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'fill))
(use-package elpy 
  :ensure t
  :after python
  :bind (:map elpy-mode-map 
              ("C-c C-k" . elpy-shell-kill))
  :config
  (electric-indent-local-mode -1)
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
(use-package lisp
  :mode ("\\.el\\'" . lisp-interaction-mode))
(use-package lispy
  :ensure t
  :bind ("C-c C-d" . lispy-describe-inline)
  :after lisp
  :init
  (add-hook 'lisp-interaction-mode-hook 'lispy-mode))
(use-package anaconda-mode
  :ensure t
  :after python
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (use-package company-anaconda
    :ensure t
    :after company
    :config
    (setq jedi:complete-on-dot t)
    (setq company-minimum-prefix-length 1)
    (setq company-idle-delay 0.3)
    (remove-hook 'anaconda-mode-response-read-fail-hook
                 'anaconda-mode-show-unreadable-response)
    (add-to-list 'company-backends 'company-anaconda)))
(use-package company-quickhelp
  :ensure t
  :after python
  :config (company-quickhelp-mode 1))
(use-package realgud
  :ensure t
  :commands (realgud:ipdb))
(use-package smartparens
  :ensure t 
  :commands smartparens-mode
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  :config
  (show-smartparens-global-mode t)
  (sp-local-pair 'org-mode "_" "_" )
  (sp-local-pair 'latex-mode "$" "$" )
  (sp-local-pair 'latex-mode "\\left(" "\\right)" :trigger "\\l(")
  ;; highligh matching brackets
  (show-paren-mode 1) 
  (setq show-paren-style 'expression))
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (prettify-symbols-mode)
              (LaTeX-math-mode)
              (smartparens-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)       ;enable document parsing
  (setq-default TeX-master nil) ;make auctex aware of multi-file documents
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
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
  ;; use sumatra to view pdf
  ;; http://stackoverflow.com/questions/14448606/sync-emacs-auctex-with-sumatra-pdf
  ;; -set-color-range #fdf4c1 #282828
  (setq TeX-view-program-list
        '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                          (mode-io-correlate " -forward-search %b %n ") " %o"))))
  ;; jump to source
  (setq TeX-source-correlate-mode t)
  (eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))
  :init
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
  :ensure t
  :bind ("C-c =" . reftex-toc)
  :config
  (setq reftex-cite-prompt-optional-args t) ; Prompt for empty optional arguments in cite
  ;; https://www.gnu.org/software/emacs/manual/html_mono/reftex.html
  (setq reftex-enable-partial-scans t)
  (setq reftex-keep-temporary-buffers nil)
  (setq reftex-save-parse-info t)
  (setq reftex-trust-label-prefix '("fig:" "eq:")))
(use-package company-bibtex
  :ensure t
  :after latex
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography
	'("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib")))
(use-package flycheck
  :ensure t 
  :commands flycheck-mode
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode nil)
  (setq flycheck-display-errors-delay 1.5)
  (setq flycheck-idle-change-delay 3)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (text-mode markdown-mode gfm-mode))

  (add-to-list 'flycheck-checkers 'proselint))
(use-package flyspell
  :ensure t
  :commands flyspell-mode
  :config
  (add-to-list 'exec-path "C:/Program Files (x86)/Hunspell/bin")
  (setq ispell-program-name (locate-file "hunspell"
                                         exec-path exec-suffixes 'file-executable-p))
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,pt_BR") nil utf-8)))
  (flyspell-mode 1))
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c C-SPC" . flyspell-correct-word-generic)))
(use-package company
  :ensure t
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'company-mode)
  :config
  (setq company-idle-delay .1)
  (setq company-show-numbers t)

  (delete 'company-capf company-backends)

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas-minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
  
  ;; Also these lines are useful to trigger the completion 
  ;; pressing the key you want.
  (global-set-key [backtab] 'tab-indent-or-complete))
(use-package company-statistics 
  :ensure t
  :after company
  :config
  (company-statistics-mode))
(use-package company-flx
  :ensure t
  :after company
  :config
  (company-flx-mode +1))
(use-package undo-tree
  :ensure t 
  :bind ("C-z" . undo-tree-undo)
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)))
(use-package rainbow-delimiters
  :ensure t 
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTex-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode))
(use-package which-key
  :ensure t
  :defer 20
  :diminish (which-key-mode)
  :config
  (which-key-mode))
(use-package key-chord
  :ensure t
  :defer 10
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define-global "]]" "\\")
  (key-chord-define-global ";;" "/")
  (key-chord-define-global "::" "?")
  (key-chord-define-global "}}" "|"))
(use-package neotree
  :disabled t
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
  :ensure t
  :bind ("<f8>" . treemacs-toggle)
  :config
  (setq treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-collapse-dirs              3
        treemacs-silent-refresh             nil
        treemacs-change-root-without-asking t
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil
        treemacs-is-never-other-window      nil
        treemacs-goto-tag-strategy          'refetch-index)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))
(use-package treemacs-projectile
  :ensure t
  :after treemacs
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))
(use-package pfuture
  :ensure t
  :after treemacs)
(use-package all-the-icons
  :ensure t
  :disabled 
  :after treemacs)
(use-package smart-mode-line
  :defer 10
  :ensure t
  :config
  (setq sml/theme 'light)
  (sml/setup))
(use-package dired+
  :ensure t
  :defer t)
(use-package imenu-list
  :ensure t
  :bind ("C-." . imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t))
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package iedit
  :ensure t
  :bind* ("C-;" . iedit-mode))
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
(use-package highlight-symbol
  :ensure t
  :commands highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'matlab-mode-hook #'highlight-symbol-mode))
(use-package highlight-parentheses
  :ensure t
  :commands highlight-parentheses-mode
  :init
  (add-hook 'org-mode-hook 'highlight-parentheses-mode)
  (add-hook 'LaTeX-mode-hook 'highlight-parentheses-mode)
  (add-hook 'python-mode-hook 'highlight-parentheses-mode))
(use-package ibuffer
  :ensure t
  :bind ("C-c C-b" . ibuffer))
(use-package matlab-mode
  :ensure t
  :mode ("\\.m\\'" . matlab-mode))
(use-package git-gutter+
  :ensure t
  :bind (("C-M-z C-M-s" . git-gutter+-stage-hunks)
         ("C-M-z C-M-c" . git-gutter+-stage-and-commit))
  :init
  (add-hook 'python-mode-hook 'git-gutter+-mode)
  (add-hook 'lisp-interaction-mode-hook 'git-gutter+-mode)
  (add-hook 'org-mode-hook 'git-gutter+-mode))
(use-package git-gutter-fringe+
  :ensure t
  :after git-gutter+
  :config
  (git-gutter-fr+-minimal))
(use-package column-marker
  :ensure t
  :commands column-marker-1
  :init
  (add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80))))
(use-package epa-file
  :after org
  :config
  (epa-file-enable))
(use-package org-crypt
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "CB17DA00")
  (setq org-crypt-disable-auto-save nil))
 (setq ad-redefinition-action 'accept)
 (winner-mode 1)
 (global-auto-revert-mode t)
 (setq global-auto-revert-non-file-buffers t)
 (setq auto-revert-verbose nil)
 (global-set-key (kbd "M-]") 'delete-horizontal-space)
 (setq resize-mini-windows t) ;; was grow-only
 (setq focus-follows-mouse t)
 (setq mouse-autoselect-window nil)
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

 (message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
 (put 'set-goal-column 'disabled nil)
