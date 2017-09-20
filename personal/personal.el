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

;; outline-minor-mode hack for solarized
(outline-minor-mode t)
(outline-minor-mode nil)

(prelude-require-package 'color-theme-solarized)

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

;;; GitHub

(prelude-require-package 'github-browse-file)

;;; Fish
(prelude-require-package 'fish-mode)

;;; Prelude FC-Github
(prelude-install-search-engine "fullcontact"
                               "https://github.com/search?q=org:fullcontact%20"
                               "Search FullContact GitHub: ")

(global-set-key (kbd "C-c F") 'prelude-fullcontact)

(setq face-height 120)

(set-face-attribute 'default nil :family "Hack" :height face-height)

(defun set-face-height (height)
  (set-face-attribute 'default nil :height height))

(defun change-face-height (action)
  (setq face-height (funcall action face-height 20))
  (set-face-height face-height))

(global-set-key (kbd "M-s-+") (lambda () (interactive) (change-face-height '+)))
(global-set-key (kbd "M-s-–") (lambda () (interactive) (change-face-height '-)))

;; No text in frame title
(setq frame-title-format "")

(setq clojure-indent-style :align-arguments)

(setq avy-all-windows 'all-frames)

(global-set-key (kbd "C-c T") 'ansi-term)

(setq ido-default-buffer-method 'selected-window)

;; Start in full-screen
(toggle-frame-fullscreen)

;; ivy
(prelude-require-package 'ivy)
(ivy-mode)
(global-set-key (kbd "C-s") 'swiper)
(setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                              (t . ivy--regex-fuzzy)))

;; projectile ivy
(prelude-require-package 'counsel-projectile)
(counsel-projectile-on)

(provide 'personal)
;;; personal.el ends here
