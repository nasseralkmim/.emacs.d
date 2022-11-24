(deftheme seralk)

(custom-theme-set-faces `seralk
                        ;; latex
                        `(font-latex-sectioning-1-face ((t (:weight bold :slant oblique :box t :inherit outline-1))))
                        `(font-latex-sectioning-2-face ((t (:weight bold :box t :inherit outline-2))))
                        `(font-latex-sectioning-3-face ((t (:weight bold :underline t :inherit outline-3))))
                        `(font-latex-sectioning-4-face ((t (:weight bold :slant normal :inherit outline-4))))
                        `(font-latex-sectioning-5-face ((t (:weight normal :slant oblique :underline t :inherit outline-5))))
                        `(font-latex-string-face ((t (:foreground "saddle brown"))))
                        ;; general
                        `(font-lock-comment-face ((t (:foreground "gray65" :slant italic :weight medium))))
                        `(mode-line-active ((t (:background "gray65" :box (:line-width -1 :style released-button)))))
                        `(mode-line-inactive ((t (:background "gray95" :box (:line-width -1 :style released-button)))))
                        ;; evil
                        `(evil-snipe-matches-face ((t (:inherit 'match :box (:line-width (-1 . -1) :style nil)))))
                        `(evil-snipe-first-match-face ((t (:inherit 'evil-snipe-matches-face))))
                        ;; org
                        `(org-block ((t (:background "gray97"))))
                        `(org-inline-src-block ((t (:background "gray97"))))
                        `(org-ellipsis ((t (:ellipsis nil))))
                        `(org-meta-line ((t (:foreground "gray60"))))
                        `(org-drawer ((t (:inherit 'org-meta-line))))
                        `(org-macro ((t (:inherit 'org-meta-line))))
                        `(org-verbatim ((t (:box t))))
                        ;; rainbow delimiter from modus operandi
                        `(rainbow-delimiters-depth-1-face ((t (:foreground "#000000"))))
                        `(rainbow-delimiters-depth-2-face ((t (:foreground "#a8007f"))))
                        `(rainbow-delimiters-depth-3-face ((t (:foreground "#005f88"))))
                        `(rainbow-delimiters-depth-4-face ((t (:foreground "#904200"))))
                        `(rainbow-delimiters-depth-5-face ((t (:foreground "#7f10d0"))))
                        `(rainbow-delimiters-depth-6-face ((t (:foreground "#006800"))))
                        ;; dired subtree
                        `(dired-subtree-depth-1-face ((t (:foreground "#000000"))))
                        `(dired-subtree-depth-2-face ((t (:foreground "#000000"))))
                        `(dired-subtree-depth-3-face ((t (:foreground "#000000"))))
                        `(dired-subtree-depth-4-face ((t (:foreground "#000000"))))
                        ;; smartparens
                        `(sp-show-pair-match-content-face ((t (:inherit 'highlight))))
                        `(sp-show-pair-match-face ((t (:box (:line-width (-1 . -1) :style nil)))))
                        `(show-paren-match ((t (:syle nil)))) ; smart parens is enough
                        ;; region without style, work with 'evil-multiedit'
                        ;; https://emacs.stackexchange.com/questions/47002/adding-box-around-text-without-changing-the-text-width
                        ;; `(region ((t (:box (:line-width (-1 . -1) :style nil)))))
                        `(iedit-occurrence ((t (:box (:line-width (-1 . -1) :style nil)))))
                        `(ts-fold-replacement-face ((t (:box (:line-width (-1 . -1) :style nil)))))
                        )

(provide-theme 'seralk)
