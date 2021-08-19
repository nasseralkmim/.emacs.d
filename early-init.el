;; prevent emacs package (package.el) initialization
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)
;; inhibit resizing frame
(setq frame-inhibit-implied-resize t)
(setq-default
 load-prefer-newer t
 mode-line-format nil
 package-enable-at-startup nil
 package-native-compile t)

(setq-default
 default-frame-alist
 '((horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (menu-bar-lines . 0)                 ;; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 8)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

