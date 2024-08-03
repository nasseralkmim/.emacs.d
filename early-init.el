;; prevent emacs package (package.el) initialization
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)
;; inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Avoid delay with corfu
;; https://github.com/minad/corfu/issues/307 
(setq-default pgtk-wait-for-event-timeout 0)

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

(setq-default cursor-in-non-selected-windows nil)
(setq idle-update-delay 1.0)
(setq auto-mode-case-fold nil)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
(setq initial-buffer-choice nil
      inhibit-startup-buffer-menu t
      inhibit-x-resources t)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)

(advice-add #'display-startup-echo-area-message :override #'ignore)

(advice-add #'display-startup-screen :override #'ignore)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
