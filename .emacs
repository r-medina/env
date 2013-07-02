 ;; defines scrolling to top
(defun scroll-point-to-top ()
  "Defines a function that emulates C-u 0 C-l (C-l = recenter)"
  (interactive)
  (recenter 0))

;; use C-l to scroll to top
(global-unset-key [?\C-l])
(global-set-key [?\C-l] 'scroll-point-to-top)

;; column-number-mode
(column-number-mode 1)

;; save backups elsewhere
(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)

;; add the user-contributed repository
(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

;; how big tabs are
(setq-default standard-indent 8)

;; tabs
(setq-default indent-tabs-mode t)

;; linum mode!
(global-linum-mode t)

;; how to relead file
(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))

;; actually reload it
(global-set-key "\C-c\C-r" 'reload-file)


;; buffer navigation
(global-unset-key (kbd "C-n"))

(global-set-key (kbd "C-n o") 'windmove-up)
(global-set-key (kbd "C-n l") 'windmove-down)
(global-set-key (kbd "C-n j") 'windmove-left)
(global-set-key (kbd "C-n k") 'windmove-right)

(setq windmove-wrap-around t)


;; for multiple web languages
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
