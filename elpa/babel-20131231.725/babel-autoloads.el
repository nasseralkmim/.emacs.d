;;; babel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "babel" "babel.el" (22341 11126 0 0))
;;; Generated autoloads from babel.el

(autoload 'babel "babel" "\
Use a web translation service to translate the message MSG.
Display the result in a buffer *babel* unless the optional argument
NO-DISPLAY is nil.

If the output is short enough to display in the echo area (which is
determined by the variables `resize-mini-windows' and
`max-mini-window-height'), it is shown there, but it is nonetheless
available in buffer `*babel*' even though that buffer is not
automatically displayed.

\(fn MSG &optional NO-DISPLAY ACCEPT-DEFAULT-SETUP)" t nil)

(autoload 'babel-region "babel" "\
Use a web translation service to translate the current region.
With prefix argument, yank the translation to the kill-ring.

\(fn START END &optional ARG)" t nil)

(autoload 'babel-as-string "babel" "\
Use a web translation service to translate MSG, returning a string.

\(fn MSG)" t nil)

(autoload 'babel-buffer "babel" "\
Use a web translation service to translate the current buffer.
Default is to present the translated text in a *babel* buffer.
With a prefix argument, replace the current buffer contents by the
translated text.

\(fn)" t nil)

(autoload 'babel-version "babel" "\
Show the version number of babel in the minibuffer.
If optional argument HERE is non-nil, insert version number at point.

\(fn &optional HERE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; babel-autoloads.el ends here
