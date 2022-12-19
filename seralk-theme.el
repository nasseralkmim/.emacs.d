(deftheme seralk)

(custom-theme-set-faces `seralk
                        ;; latex
                        `(font-latex-sectioning-1-face ((t (:weight bold :slant oblique :box t :inherit outline-1))))
                        `(font-latex-sectioning-2-face ((t (:weight bold underline t :inherit outline-2))))
                        `(font-latex-sectioning-3-face ((t (:weight bold :slant italic :inherit outline-3))))
                        `(font-latex-sectioning-4-face ((t (:weight bold :slant oblique :underline t :inherit outline-5))))
                        `(font-latex-sectioning-5-face ((t (:weight normal :slant normal :inherit outline-4))))
                        `(font-latex-string-face ((t (:foreground "saddle brown"))))
                        ;; general
                        `(font-lock-comment-face ((t (:foreground "gray65" :slant italic :weight medium))))
                        `(mode-line-inactive ((t (:box (:line-width (-1 . -1) :style released-button)))))
                        `(mode-line-active ((t (:box (:line-width (-1 . -1))))))
                        ;; evil
                        `(evil-snipe-matches-face ((t (:inherit match :box (:line-width (-1 . -1) :style nil)))))
                        `(evil-snipe-first-match-face ((t (:inherit 'evil-snipe-matches-face))))
                        ;; org
                        `(org-block ((t (:inherit org-agenda-restriction-lock))))
                        `(org-inline-src-block ((t (:inherit org-agenda-restriction-lock))))
                        `(org-ellipsis ((t (:ellipsis nil))))
                        ;; `(org-meta-line ((t (:foreground "gray60"))))
                        `(org-drawer ((t (:inherit 'org-meta-line))))
                        `(org-macro ((t (:inherit 'org-meta-line))))
                        `(org-verbatim ((t (:box (:line-width (-1 . -1))))))
                        ;; smartparens
                        `(sp-show-pair-match-content-face ((t (:style nil))))
                        `(sp-show-pair-match-face ((t (:box (:line-width (-1 . -1) :style nil)))))
                        `(show-paren-match ((t (:syle nil)))) ; smart parens is enough
                        ;; region without style, work with 'evil-multiedit'
                        ;; https://emacs.stackexchange.com/questions/47002/adding-box-around-text-without-changing-the-text-width
                        ;; `(region ((t (:box (:line-width (-1 . -1) :style nil)))))
                        `(iedit-occurrence ((t (:box (:line-width (-1 . -1) :style nil)))))
                        ;; `(ts-fold-replacement-face ((t (:box (:line-width (-1 . -1) :style nil)))))
                        `(ts-fold-replacement-face ((t (:inherit 'font-lock-comment-face))))
                        )

(provide-theme 'seralk)
