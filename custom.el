(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "a433b4f6e0f8a1fe7cc8411419a708b6ca911320f34e07e6c73e37bb98a710d2" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "37def0fac11a4890922af9febc8394e3b6e3c68904a294a2d440b1904e979c7e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(fci-rule-color "#d6d6d6")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/OneDrive/TerraCap/terracap_notes.org" "~/OneDrive/Org/gtd.org" "~/OneDrive/Org/notes.org" "~/OneDrive/Org/journal.org" "~/OneDrive/Org/gcal.org")))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-beamer-frame-level 1)
 '(org-latex-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(org-preview-latex-process-alist
   (quote
    ((dvipng :programs
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
                  ("pdflatex %f")
                  :image-converter
                  ("convert %f  %O")))))
 '(package-selected-packages
   (quote
    (rainbow-identifiers gruvbox-theme rainbow-blocks ivy-hydra ivy comapny-anaconda jedi-core dumb-jump highlight-indent-guides diredp flycheck-pyflakes ob-async auctex pretty-mode dired+ spaceline web-mode org-gcal ropemacs indent-tools auto-dim-other-buffers auto-dim-other-buffer apropospriate-theme goto-last-change org-plus-contrub latex-unicode-math-mode dashboard-startup ox-beamer org-plus-contrib expand-region auto-dictionary beacon company-anaconda anaconda-mode python-docstring imenu-list ox-latex org-latex org hide-lines epresent org-present smex swiper org-tree-slide doom-neotree neotree doom-themes counsel-projectile zenburn-theme writegood-mode workgroups2 which-key websocket use-package unicode-fonts undo-tree theme-looper tao-theme tablist smartparens smart-mode-line request realgud rainbow-delimiters py-autopep8 projectile pos-tip popwin pdf-tools ox-reveal orgtbl-show-header org-ref org-page org-download org-bullets ob-ipython moe-theme magit lispy leuven-theme latex-preview-pane ivy-bibtex hyperbole flyspell-correct-ivy flycheck esup elpy cyberpunk-theme counsel company-statistics company-jedi company-flx company-auctex color-theme-sanityinc-tomorrow cl-generic cdlatex babel auto-complete anti-zenburn-theme alert afternoon-theme)))
 '(safe-local-variable-values
   (quote
    ((TeX-master . t)
     (TeX-command-extra-options . "-shell-escape"))))
 '(vc-annotate-background "#181e26")
 '(vc-annotate-color-map
   (quote
    ((20 . "#7bc275")
     (40 . "#a0c077")
     (60 . "#c6bf79")
     (80 . "#ECBE7B")
     (100 . "#eaae6e")
     (120 . "#e89f61")
     (140 . "#E69055")
     (160 . "#db8981")
     (180 . "#d082ae")
     (200 . "#C57BDB")
     (220 . "#d874b0")
     (240 . "#eb6d86")
     (260 . "#ff665c")
     (280 . "#d6655f")
     (300 . "#ae6563")
     (320 . "#856567")
     (340 . "#5D656B")
     (360 . "#5D656B"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:background nil :bold t :overline nil))))
 '(org-level-2 ((t (:background nil :bold t :overline nil))))
 '(org-level-3 ((t (:inherit outline-3 :bold t :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :bold t :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :bold t :height 1.0)))))
