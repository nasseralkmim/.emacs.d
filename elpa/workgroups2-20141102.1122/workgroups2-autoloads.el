;;; workgroups2-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "workgroups2" "workgroups2.el" (22222 11755
;;;;;;  0 0))
;;; Generated autoloads from workgroups2.el

(autoload 'workgroups-mode "workgroups2" "\
Turn `workgroups-mode' on and off.
ARG is nil - toggle
ARG >= 1   - turn on
ARG == 0   - turn off
ARG is anything else, turn on `workgroups-mode'.

\(fn &optional ARG)" t nil)

(autoload 'wg-help "workgroups2" "\
Just call `apropos-command' on \"^wg-\".
There used to be a bunch of help-buffer construction stuff here,
including a `wg-help' variable that basically duplicated every
command's docstring;  But why, when there's `apropos-command'?

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; workgroups2-autoloads.el ends here
