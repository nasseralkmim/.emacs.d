;; prevent emacs package (package.el) initialization
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)
;; inhibit resizing frame
(setq frame-inhibit-implied-resize t)
;; prevent glimpse of UI been disabled
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; Work around to a crippling performance issue I reported affecting Emacs 28
;; after cario was made the default: https://debbugs.gnu.org/db/40/40733.html
(add-to-list 'face-ignored-fonts "Adobe Blank")
