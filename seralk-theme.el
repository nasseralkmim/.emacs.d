(deftheme seralk)

;; custom face for comments
;; apparently there is no way to do this directly on the existing face
;; https://emacs.stackexchange.com/a/9604
(defface my-font-lock-comment-face
  '((((background dark)) (:foreground  "#dfaf7a" :weight medium :slant italic))
    (((background light)) (:foreground "#7b5000" :weight medium :slant italic)))
  "Face for comments in light and dark background.")

;; almost black (#222222) and almost white (#f2f2f2)
;; https://anthonyhobday.com/sideprojects/saferules/
(defface my-default-face
  '((((background dark)) (:foreground  "#f2f2f2" :background "#222222" ))
    (((background light)) (:foreground "#222222" :backgrond "#f2f2f2")))
  "Face for default text in light and dark background.")

(custom-theme-set-faces
 `seralk
 `(default ((t (:inherit my-default-face))))
 ;; latex
 `(font-latex-sectioning-1-face ((t (:weight bold :slant oblique :box t :inherit outline-1))))
 `(font-latex-sectioning-2-face ((t (:weight bold underline t :inherit outline-2))))
 `(font-latex-sectioning-3-face ((t (:weight bold :slant italic :inherit outline-3))))
 `(font-latex-sectioning-4-face ((t (:weight bold :slant oblique :underline t :inherit outline-5))))
 `(font-latex-sectioning-5-face ((t (:weight normal :slant normal :inherit outline-7))))
 `(font-latex-string-face ((t (:foreground "saddle brown"))))
 ;; general
 `(mode-line-inactive ((t (:box (:line-width (-1 . -1) :style released-button)))))
 ;; evil
 `(evil-snipe-matches-face ((t (:inherit match :box (:line-width (-1 . -1) :style nil)))))
 `(evil-snipe-first-match-face ((t (:inherit evil-snipe-matches-face))))
 ;; org
 `(org-meta-line ((t (:inherit font-lock-builtin-face :weight light :slant italic))))
 `(org-block ((t (:inherit org-agenda-restriction-lock))))
 `(org-inline-src-block ((t (:inherit org-agenda-restriction-lock))))
 `(org-ellipsis ((t (:ellipsis nil))))
 `(org-drawer ((t (:inherit org-meta-line))))
 `(org-macro ((t (:inherit org-meta-line))))
 `(org-verbatim ((t (:box (:line-width (-1 . -1) :color "gray80")))))
 ;; smartparens
 `(sp-show-pair-match-content-face ((t (:style nil))))
 `(sp-show-pair-match-face ((t (:box (:line-width (-1 . -1) :color "gray60" :style nil)))))
 `(show-paren-match ((t (:syle nil)))) ; smart parens is enough
 ;; region without style, work with 'evil-multiedit'
 ;; https://emacs.stackexchange.com/questions/47002/adding-box-around-text-without-changing-the-text-width
 ;; `(region ((t (:box (:line-width (-1 . -1) :style nil)))))
 `(iedit-occurrence ((t (:box (:line-width (-1 . -1))))))
 ;; `(ts-fold-replacement-face ((t (:box (:line-width (-1 . -1) :style nil)))))
 `(ts-fold-replacement-face ((t (:inherit font-lock-comment-face))))
 `(default ((t (:inherit 'default))))
 `(variable-pitch-text ((t (:inherit variable-pitch))))
 ;; shr for eww
 `(shr-code ((t (:box (:line-width (-1 . -1) :color "gray80")))))
 `(shr-h3 ((t (:weight bold))))
 `(shr-h4 ((t (:slant italic))))
 ;; eglot
 `(eglot-highlight-symbol-face ((t (:underline t :weight bold))))
 `(font-lock-comment-face ((t (:inherit my-font-lock-comment-face))))
 ;; rainbow delimite
 `(rainbow-delimiters-depth-1-face ((t (:inherit default))))
 `(rainbow-delimiters-depth-2-face ((t (:inherit outline-2))))
 `(rainbow-delimiters-depth-3-face ((t (:inherit outline-3))))
 )

(provide-theme 'seralk)
