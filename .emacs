;;; Ricky Medina's .emacs file

;; 2013

;; github.com/r-medina
;; ricky.medina91@gmail.com


;;; I.   Essentials

;; common lisp
(require 'cl)

;; adds my stuff to load path
(add-to-list 'load-path "~/.emacs.d/usr")

;; save backups elsewhere
(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)

;; add the user-contributed repository
(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)


;;; II.  Programming/Modes

;; for multiple web languages
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode t)

;; auctex
;; something in tex-site is fucking up some of my keybindings
(require 'tex-site)
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; for python and scheme auto-complete
(require 'auto-complete)
(require 'scheme-complete)

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; matlab mode
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; python auto-complete
(add-hook 'jedi-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)

;; scheme auto complete
(add-hook 'scheme-mode-hook 'auto-complete-mode)

;; racket shit
(autoload 'scheme-mode "quack"
  "Major mode for editing Racket files" t)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))


;;; III. Look

;; column-number-mode
(column-number-mode t)

;; turn on stripe-buffer-mode
(add-hook 'find-file-hook 'stripe-buffer-mode)

;; show matching parenthesis
(show-paren-mode t)

;; prettier mode-line
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

;; highlight line
(global-hl-line-mode t)

;; linum mode!
(global-linum-mode t)

;; for better linum mode formatting
(defvar my-linum-format-string "%4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
			 (count-lines (point-min) (point-max)))))
	 ;; my own formatting string
	 (format (concat "%" (number-to-string width) "d\u2502 ")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face 'linum))

;; turn off linum mode for shells
(require 'linum-off)

;; fixing scrolling behavior to be less jumpy
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; visual switching
(require 'switch-window)


;;; IV.  Key bindings

;; buffer navigation
(global-set-key (kbd "C-x o") 'switch-window)
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n o") 'windmove-up)
(global-set-key (kbd "C-n l") 'windmove-down)
(global-set-key (kbd "C-n j") 'windmove-left)
(global-set-key (kbd "C-n k") 'windmove-right)
(setq windmove-wrap-around t)

 ;; defines scrolling to top
(defun scroll-point-to-top ()
  "Defines a function that emulates C-u 0 C-l (C-l = recenter)"
  (interactive)
  (recenter 0))
;; use C-l to scroll to top
(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l") 'scroll-point-to-top)

;; how to relead file
(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))
;; actually reload it
(global-set-key (kbd "C-c C-r") 'reload-file)

;(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t") 'comment-or-uncomment-region)
(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u") 'magit-status)
;(global-unset-key (kbd "C-f"))
;(global-unset-key (kbd "C-b"))

;; quick minor modes
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m w") 'whitespace-mode)
(global-set-key (kbd "M-m s") 'stripe-buffer-mode)
(global-set-key (kbd "M-m l") 'linum-mode)
(global-set-key (kbd "M-m p") 'paredit-mode)
(global-set-key (kbd "M-m o") 'outline-minor-mode)

;; Outline-minor-mode key map
;(define-prefix-command 'cm-map nil "Outline-")
(define-prefix-command 'outline-mode-map)
(define-key outline-mode-map (kbd "h") 'hide-sublevels)
(define-key outline-mode-map (kbd "b") 'hide-body)
(define-key outline-mode-map (kbd "a") 'show-all)
(define-key outline-mode-map (kbd "c") 'hide-entry)
(define-key outline-mode-map (kbd "e") 'show-entry)
(global-set-key (kbd "C-o") outline-mode-map)

;; jump to word starting with PREFIX
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'ace-jump-mode)

;; open .emacs
(global-set-key (kbd "C-c . e") (lambda() (interactive)(find-file "~/.emacs")))
;; open .bashrc
(global-set-key (kbd "C-c . b") (lambda() (interactive)(find-file "~/.bashrc")))
;; open .profile
(global-set-key (kbd "C-c . p") (lambda() (interactive)(find-file "~/.profile")))
;; open shell
(global-set-key (kbd "C-c s") 'shell)


;;; V.

;; color matching delimiters
(global-rainbow-delimiters-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(quack-programs
   (quote
    ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#707183" :weight extra-bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#7388d6" :weight extra-bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#909183" :weight extra-bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#709870" :weight extra-bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#907373" :weight extra-bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#6276ba" :weight extra-bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#858580" :weight extra-bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#80a880" :weight extra-bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#887070" :weight extra-bold)))))
