;;;; Prelude personal configuration

(scroll-bar-mode -1)
(menu-bar-mode -1)

(prelude-require-package 'ag)
(prelude-require-package 'moe-theme)

(setq avy-all-windows 't)
(setq aw-keys '(97 115 100 102 103 104 106 107 108)) ;; ace-window "asdfghjkl"

(defun my-term-mode-hook ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-a") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))

(add-hook 'term-mode-hook 'my-term-mode-hook)

(require 'moe-theme)

(defvar cycle-themes-list (vector 'moe-dark 'moe-light))
(defvar cycle-themes--index 0)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defun get-current-theme ()
  (elt cycle-themes-list cycle-themes--index))

(defun switch-themes ()
  (interactive)
  (load-theme (elt cycle-themes-list cycle-themes--index))
  (setq cycle-themes--index (mod (1+ cycle-themes--index)
                                 (length cycle-themes-list))))

(key-chord-unset-global "jl")
(key-chord-define-global "xx" 'smex)
(key-chord-define-global "tj" 'switch-themes)

(define-key key-translation-map (kbd "M-ESC") (kbd "`"))

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api)
                                (start-figwheel!)
                                (cljs-repl))")

(set-face-attribute 'default nil :family "SF Mono" :height 160 :weight 'regular)

(provide 'personal)
;;; personal.el ends here
