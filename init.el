;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)
;(setq use-package-verbose t)

;;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/OneDrive/ANAC/Notas ANAC.org" "~/OneDrive/Org/organizer.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:background nil :bold t :overline nil))))
 '(org-level-2 ((t (:background nil :bold t :overline nil)))))
