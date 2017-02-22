;;;; Prelude personal configuration

(scroll-bar-mode -1)
(menu-bar-mode -1)

(prelude-require-package 'ag)

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

(prelude-require-package 'color-theme-solarized)

(load-theme 'solarized t)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

(defun next-solarized-mode ()
  (if (eq frame-background-mode 'light)
      'dark
    'light))

(defun switch-themes ()
  (interactive)
  (customize-set-variable 'frame-background-mode (next-solarized-mode))
  (load-theme 'solarized t))

(key-chord-unset-global "jl")
(key-chord-define-global "xx" 'smex)
(key-chord-define-global "tj" 'switch-themes)

(define-key key-translation-map (kbd "M-ESC") (kbd "`"))

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api)
                                (start-figwheel!)
                                (cljs-repl))")

(set-face-attribute 'default nil :family "Hack" :height 150)

(provide 'personal)
;;; personal.el ends here