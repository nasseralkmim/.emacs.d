(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1 -shell-escape")
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-latex-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(safe-local-variable-values
   (quote
    ((TeX-master . t)
     (TeX-command-extra-options . "-shell-escape")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "white" :foreground "black"))))
 '(fringe ((t (:background "white"))))
 '(mode-line ((t (:background "grey75" :foreground "black" :box nil))))
 '(org-level-1 ((t (:background nil :bold t :overline nil))))
 '(org-level-2 ((t (:background nil :bold t :overline nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ae6a6a"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#7aa3de"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#c5ca89"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#7bcb7e"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#9cb9e9"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#9898ca"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#72c572"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#8eb0ed"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#82cfb4")))))
