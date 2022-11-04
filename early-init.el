;; prevent emacs package (package.el) initialization
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)
;; inhibit resizing frame
(setq frame-inhibit-implied-resize t)

(setq-default
 default-frame-alist
 '((horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (menu-bar-lines . 0)                 ;; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 8)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;; from: https://github.com/SystemCrafters/rational-emacs/blob/master/early-init.el
;; native compilation settings
(when (featurep 'native-compile)
  ;; silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t))

(set-face-attribute 'default nil :family "Victor Mono")
(set-face-attribute 'italic nil :family "Victor Mono" :slant 'oblique :weight 'medium)
(set-face-attribute 'fixed-pitch nil :family "Victor Mono")
(set-face-attribute 'variable-pitch nil :family "Input Sans")
