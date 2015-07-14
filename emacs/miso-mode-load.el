;;; miso-mode-load.el --- automatically extracted autoloads
;;; Commentary:

;; To install miso-mode, add the following lines to your .emacs file:
;;   (add-to-list 'load-path "PATH CONTAINING miso-mode-load.el" t)
;;   (require 'miso-mode-load)
;;
;; After this, miso-mode will be used for files ending in '.miso'.

;;; Code:



(autoload 'miso-mode "miso-mode" "\
Major mode for editing Miso source text.

This mode provides (not just) basic editing capabilities for
working with Miso code. It offers almost complete syntax
highlighting and indentation.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.miso\\'" 'miso-mode))

;;;***

(provide 'miso-mode-load)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; miso-mode-load.el ends here
