;;; Ricky Medina's .emacs file

;; 2013

;; github.com/r-medina
;; ricky.medina91@gmail.com

;; TODO:
;; - move some require statements to autoloads or hooks
;; - edit find-file-hook such that it calls auto-complete-mode
;; - elget
;; - fix that my-clean leaves some trailing whitespace


;;; I.  Essentials

;; common lisp
(require 'cl)

;; adds my stuff to load path
(add-to-list 'load-path "~/.emacs.d/usr")

;; save backups elsewhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; add the user-contributed repository
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; utf-8 for correct behavior in terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq ispell-program-name "aspell")

;; better buffer naming behavior
(require 'uniquify)

;; for kill-ring integration with clipboard
(require 'pbcopy)
(turn-on-pbcopy)

;; so selections work like in everywhere else
(delete-selection-mode 1)

;; for editing the browser
;; (when (and (require 'edit-server nil t) (daemonp))
;;   (edit-server-start))

;; for undo with window operations
(setq winner-mode t)


;;; II.  Programming/Modes

;; cleaning shit up
(defun my-clean ()
  (interactive)
  (setq end (point-max))
  "does some cleaning up of files"
  (untabify 0 end)
  (indent-region 0 end)
  (delete-trailing-whitespace 0 end))

;; no tabs
(setq-default indent-tabs-mode nil)

;; struggling with tabs
(setq default-tab-width 4)

;; for multiple web languages
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
      '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
        (javascript-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
        (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode t)

;; auctex
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

;; java stuffs
(autoload 'malabar-mode "malabar-mode"
  "Better Java major mode" t)
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(add-hook 'malabar-mode-hook 'auto-complete-mode)

;; scala
(require 'scala-mode2)

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

;; paredit auto-loads
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'javascript-mode-hook       #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

;; racket shit
(autoload 'scheme-mode "quack"
  "Major mode for editing Racket files" t)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;; scheme auto complete
(add-hook 'scheme-mode-hook 'auto-complete-mode)

;; scss
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; javascript indent
(setq js-indent-level 2)

;; smarter tabs for buzzfeed js
;; (autoload 'smart-tabs-mode "smart-tabs-mode"
;;    "Intelligently indent with tabs, align with spaces!")
;; (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;; (autoload 'smart-tabs-advice "smart-tabs-mode")
;; (autoload 'smart-tabs-insinuate "smart-tabs-mode")
;; (smart-tabs-insinuate 'javascript)

;; js2-mode for better javascript shiz
(add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js-mode-hook 'smart-tabs-mode)
;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


;; json parsing
;; taken from https://code.google.com/p/js2-mode/issues/detail?id=50#c7
(defadvice js2-reparse (before json)
  (setq js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)

(defadvice js2-parse-statement (around json)
  (if (and (= tt js2-LC)
           js2-buffer-file-name
           (string-equal (substring js2-buffer-file-name -5) ".json")
           (eq (+ (save-excursion
                    (goto-char (point-min))
                    (back-to-indentation)
                    (while (eolp)
                      (next-line)
                      (back-to-indentation))
                    (point)) 1) js2-ts-cursor))
      (setq ad-return-value (js2-parse-assign-expr))
    ad-do-it))
(ad-activate 'js2-parse-statement)

;; pyret
(autoload 'pryret-mode "pyret"
  "Pyret language major mode" t)
(add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
(add-to-list 'file-coding-system-alist '("\\.arr\\'" . utf-8))

;; ocaml complete
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
(defun ac-ocaml-candidates (prefix)
  "Candidates for OCaml auto-completion"
  (let ((candidates)
        (module-name
         (when (string-match "\\([A-Za-z_][A-Za-z0-9_']*\\)[.]" prefix)
           (match-string 1 prefix))))
    (if module-name
        (iter '(lambda (sym) (push (concat module-name "." sym) candidates))
              (ocaml-module-symbols (assoc module-name (ocaml-module-alist))))
      (iter
       '(lambda (mod)
          (iter '(lambda (sym) (push sym candidates))
                (ocaml-module-symbols mod)))
       (ocaml-visible-modules))
      (iter '(lambda (mod) (push (car mod) candidates)) (ocaml-module-alist)))
    candidates))

;; auto-complete for ocaml
(ac-define-source ocaml
  '((available . (require 'caml-help nil t))
    (candidates . (ac-ocaml-candidates ac-prefix))
    (prefix . "\\(?:[^A-Za-z0-9_.']\\|\\`\\)\\(\\(?:[A-Za-z_][A-Za-z0-9_']*[.]\\)?[A-Za-z0-9_']*\\)")
    (symbol . "s")))

;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4)
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

;; perl auto complete?
(add-hook 'cperl-mode-hook
          (lambda()
            (require 'perl-completion)
            (perl-completion-mode t)))
(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t)
               (auto-complete-mode t)
               (make-variable-buffer-local 'ac-sources)
               (setq ac-sources
                     '(ac-source-perl-completion)))))

;; FIX THIS SOON
;; Completion words longer than 4 characters
;; (custom-set-variables
;;  '(ac-ispell-requires 4))
;; (eval-after-load "auto-complete"
;;   '(progn
;;   (ac-ispell-setup)))
;; (defun my/enable-ac-ispell ()
;;   (add-to-list 'ac-sources 'ac-source-ispell))


;;; III. Look

(load-theme 'tango-dark t)

;; column-number-mode
(column-number-mode t)

;; show matching parenthesis
(show-paren-mode t)

;; prettier mode-line
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

;; linum mode!
(global-linum-mode t)

;; outline mode
(add-hook 'find-file-hook 'outline-minor-mode)

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
;; randomly decided to stop working
(require 'switch-window)

;; not very good
;; make it do what ace-jump-mode does instead ie just text
;; (auto-dim-other-buffers-mode t)

;; turn on stripe-buffer-mode
(add-hook 'dired-mode-hook 'stripe-listify-buffer)
(add-hook 'find-file-hook 'hl-line-mode)
;; (add-hook 'stripe-buffer-mode-hook 'hl-line-mode)
;; (add-hook 'find-file-hook 'stripe-buffer-mode)


;;; IV.  Key bindings

;; defines scrolling to top
(defun scroll-point-to-top ()
  "Defines a function that emulates C-u 0 C-l (C-l = recenter)"
  (interactive)
  (recenter 0))

;; how to relead file
(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))

;; ibuffer
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; defining a minor mode for all my keys!!
;; stolen from: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; buffer navigation
(define-prefix-command 'nav-map)
(define-key nav-map (kbd "o") 'windmove-up)
(define-key nav-map (kbd "l") 'windmove-down)
(define-key nav-map (kbd "j") 'windmove-left)
(define-key nav-map (kbd "k") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-n") nav-map)
(setq windmove-wrap-around t)

;; use C-l to scroll to top
(define-key my-keys-minor-mode-map (kbd "C-l") 'scroll-point-to-top)

;; actually reload it
(define-key my-keys-minor-mode-map (kbd "C-c C-r") 'reload-file)

                                        ;(global-unset-key (kbd "C-q"))
(define-key my-keys-minor-mode-map (kbd "C-t") 'comment-or-uncomment-region)
(define-key my-keys-minor-mode-map (kbd "C-u") 'magit-status)
                                        ;(global-unset-key (kbd "C-f"))
                                        ;(global-unset-key (kbd "C-b"))

;; quick minor modes
(define-prefix-command 'quick-modes-map)
(define-key quick-modes-map (kbd "w") 'whitespace-mode)
(define-key quick-modes-map (kbd "s") 'stripe-buffer-mode)
(define-key quick-modes-map (kbd "r") 'rainbow-delimiters-mode)
(define-key quick-modes-map (kbd "l") 'linum-mode)
(define-key quick-modes-map (kbd "p") 'paredit-mode)
(define-key quick-modes-map (kbd "o") 'outline-minor-mode)
(define-key quick-modes-map (kbd "d") 'delete-selection-mode)
(define-key quick-modes-map (kbd "a") 'auto-complete-mode)
(define-key my-keys-minor-mode-map (kbd "M-m") quick-modes-map)

;; Outline-minor-mode key map
(define-prefix-command 'outline-mode-map)
(define-key outline-mode-map (kbd "h") 'hide-sublevels)
(define-key outline-mode-map (kbd "b") 'hide-body)
(define-key outline-mode-map (kbd "a") 'show-all)
(define-key outline-mode-map (kbd "c") 'hide-entry)
(define-key outline-mode-map (kbd "e") 'show-entry)
(define-key my-keys-minor-mode-map (kbd "C-o") outline-mode-map)

;; jump to word starting with PREFIX
(global-unset-key (kbd "C-j"))
(define-key my-keys-minor-mode-map (kbd "C-j") 'ace-jump-mode)

;; listing packages
(define-key my-keys-minor-mode-map (kbd "M-P") 'package-list-packages)

;; open .emacs
(define-key my-keys-minor-mode-map (kbd "C-c . e")
  (lambda() (interactive)(find-file "~/.emacs")))
;; open .bashrc
(define-key my-keys-minor-mode-map (kbd "C-c . b")
  (lambda() (interactive)(find-file "~/.bashrc")))
;; open .profile
(define-key my-keys-minor-mode-map (kbd "C-c . p")
  (lambda() (interactive)(find-file "~/.profile")))
;; open shell
(define-key my-keys-minor-mode-map (kbd "C-c s") 'shell)
;; cleaning function
(define-key my-keys-minor-mode-map (kbd "C-c C") 'my-clean)

;; personal minor mode for key map. GREAT hack
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;; toggle my minor mode
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m m") 'my-keys-minor-mode)


;;; V.

;; color matching delimiters
(global-rainbow-delimiters-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-delay 0.1)
 '(blink-cursor-interval 0.1)
 '(fill-column 85)
 '(quack-programs (quote ("." "bigloo" "csi" "csi -hygienic" "drracket" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#171538"))))
 ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "#707183" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "#7388d6" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "#909183" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "#709870" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "#907373" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "#6276ba" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "#858580" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "#80a880" :weight extra-bold))))
 ;; '(rainbow-delimiters-depth-9-face ((t (:foreground "#887070" :weight extra-bold))))
 '(stripe-highlight ((t (:background "#0b0b0b")))))
