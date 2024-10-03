;; prevent emacs package (package.el) initialization
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)
;; inhibit resizing frame
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)       ; start at scratch buffer
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
;; Remove "For information about GNU Emacs..." message at startup
;; https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Avoid delay with corfu
;; https://github.com/minad/corfu/issues/307 
(setq-default pgtk-wait-for-event-timeout 0)

(setq-default
 default-frame-alist
 '((horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (menu-bar-lines . 0)                 ;; No menu bar
   ;; (right-divider-width . 1)            ;; Thin vertical window divider
   ;; (right-fringe . 8)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   (vertical-scroll-bars . nil)
   (font . "Commit Mono")))       ;; No vertical scroll-bars

;; from: https://github.com/SystemCrafters/rational-emacs/blob/master/early-init.el
;; native compilation settings
(when (featurep 'native-compile)
  ;; silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t))

;; from https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el
(setq-default cursor-in-non-selected-windows nil)
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq read-process-output-max (* 128 1024))
(set-language-environment "UTF-8")

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
