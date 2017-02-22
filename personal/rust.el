;;;; Rust configuration

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(provide 'rust)
;;; rust.el ends here
