;; prevent emacs package initialization
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)
;; inhi resizing frame
(setq frame-inhibit-implied-resize t)
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(window-divider-mode)
