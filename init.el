;; emacs settings

(package-initialize)

;; a function to add load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add some directories to load-path
(add-to-load-path "elisp" "elpa")

;; a destination of backup files
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

;; settings for font
;; need to install Myrica
;; Myrica.TTC -> "Myrica M", MyricaM.TTC -> "Myrica MM"
(add-to-list 'default-frame-alist '(font . "Myrica M"))

;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" .
				   "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; use-package
(require 'use-package)

;; theme settings
;; taken from https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config))

;; modeline settings
;; need: "M-x all-the-icons-install-fonts"
(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

;; show the guide of keybinds
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; use counsel (without using ivy-mode)
(use-package counsel
  :config
  (counsel-mode 1))

;; use rg.el
(use-package rg
  :config
  (rg-enable-default-bindings))

;; use markdown-mode
(use-package markdown
  :mode
  ("\\.md\\'" . gfm-mode))

;; store links for org-mode
(define-key global-map "\C-cl" 'org-store-link)

;; dired settings
(setq dired-recursive-copies 'always)
(setq dired-isearch-filenames t)

;; short cut for wdired (push 'e' in dired-mode)
(require 'wdired)
(setq wdired-allow-to-change-permissions t)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;; show line numbers
(require 'linum)
(global-linum-mode)

;; show corresponding parentheses
(show-paren-mode t)

;; assign C-h to backspace
(global-set-key "\C-h" 'delete-backward-char)

;; window-resizer
;; taken from https://khiker.hatenablog.jp/entry/20100119/window_resize
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
	(current-width (window-width))
	(current-height (window-height))
	(dx (if (= (nth 0 (window-edges)) 0) 1
	      -1))
	(dy (if (= (nth 1 (window-edges)) 0) 1
	      -1))
	action c)
    (catch 'end-flag
      (while t
	(setq action
	      (read-key-sequence-vector (format "size[%dx%d]"
						(window-width)
						(window-height))))
	(setq c (aref action 0))
	(cond ((= c ?l)
	       (enlarge-window-horizontally dx))
	      ((= c ?h)
	       (shrink-window-horizontally dx))
	      ((= c ?j)
	       (enlarge-window dy))
	      ((= c ?k)
	       (shrink-window dy))
	      ;; otherwise
	      (t
	       (let ((last-command-char (aref action 0))
		     (command (key-binding action)))
		 (when command
		   (call-interactively command)))
	       (message "Quit")
	       (throw 'end-flag t)))))))

;; use C-q as a prefix key
(define-key global-map "\C-q" (make-sparse-keymap))

;; assign quoted-insert to C-q C-q
(global-set-key "\C-q\C-q" 'quoted-insert)

;; window-resizer : C-q C-r (resize)
(global-set-key "\C-q\C-r" 'my-window-resizer)

;; instead of C-x o
(global-set-key "\C-ql" 'windmove-right)
(global-set-key "\C-qh" 'windmove-left)
(global-set-key "\C-qj" 'windmove-down)
(global-set-key "\C-qk" 'windmove-up)

;; settings for gtags
(require 'gtags)
(global-set-key "\M-t" 'gtags-find-tag)
(global-set-key "\M-r" 'gtags-find-rtag)
(global-set-key "\M-s" 'gtags-find-symbol)
(global-set-key "\C-t" 'gtags-pop-stack)

;; ### settings for C ###
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     ;; use flyspell-prog-mode
	     (flyspell-prog-mode)
))

;; ### settings for sh-mode ###
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; ### mode settings for other extensions ###
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; ;; ### settings for python ###
;; ;; jedi
;; (add-hook 'python-mode-hook 'jedi:setup)
;; ;; autopep8
;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;; ;; pyflakes
;; (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; ;; yasnippets
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;; ### settings for Rust ###
;; (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
;; (require 'company-racer)
;; (with-eval-after-load 'company
;;    (add-to-list 'company-backends 'company-racer))
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'rust-mode-hook #'flycheck-rust-setup)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ### settings for scheme (using Gauche) ###
;; taken from http://ayato.hateblo.jp/entry/20130710/1373448057
;; (setq process-coding-system-alist
;;       (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
;; (setq scheme-program-name "/usr/bin/env gosh -i")

;; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

;; (defun scheme-other-window ()
;;   "Run Gauche on other window"
;;   (interactive)
;;   (split-window-horizontally (/ (frame-width) 2))
;;   (let ((buf-name (buffer-name (current-buffer))))
;;     (scheme-mode)
;;     (switch-to-buffer-other-window
;;      (get-buffer-create "*scheme*"))
;;     (run-scheme scheme-program-name)
;;     (switch-to-buffer-other-window
;;      (get-buffer-create buf-name))))

;; (define-key global-map
;;   "\C-cG" 'scheme-other-window)

;; ### settings for TeX ###
;; AUCTeX
(require 'tex-site)
(require 'tex-jp)
(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq japanese-LaTeX-default-style "jarticle")
(setq kinsoku-limit 10)
(setq LaTeX-indent-level 2)
(setq LaTeX-item-indent 0)
(setq TeX-output-view-style '(("^dvi$" "." "xdvi '%d'")))
(setq preview-image-type 'dvipng)
(add-hook 'LaTeX-mode-hook (function (lambda ()
   (add-to-list 'TeX-command-list
     '("pTeX" "%(PDF)ptex %`%S%(PDFout)%(mode)%' %t"
      TeX-run-TeX nil (plain-tex-mode) :help "Run ASCII pTeX"))
   (add-to-list 'TeX-command-list
     '("pLaTeX" "platex %`%S%(PDFout)%(mode)%' %t"
      TeX-run-TeX nil (latex-mode) :help "Run ASCII pLaTeX"))
   (add-to-list 'TeX-command-list
     '("acroread" "acroread '%s.pdf' " TeX-run-command t nil))
   (add-to-list 'TeX-command-list
     '("pdf" "dvipdfmx -V 4 '%s' " TeX-run-command t nil))
   (add-to-list 'TeX-command-list
     '("pLaTeX2pdf" "pLaTeX %S %(mode) %t && dvipdfmx -V 4 %d"
     TeX-run-TeX nil (latex-mode) :help "Run pLaTeX and dvipdfmx"))
)))

(setq org-latex-classes '(("jsarticle"
             "\\documentclass{jsarticle}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{url}
\\usepackage{atbegshi}
\\AtBeginShipoutFirst{\\special{pdf:tounicode EUC-UCS2}}
\\usepackage[dvipdfmx,setpagesize=false]{hyperref}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  [EXTRA]"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("jsbook"
             "\\documentclass{jsbook}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{url}
\\usepackage{atbegshi}
\\AtBeginShipoutFirst{\\special{pdf:tounicode EUC-UCS2}}
\\usepackage[dvipdfmx,setpagesize=false]{hyperref}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  [EXTRA]"
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
))

;; Aspell
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(global-set-key (kbd "C-M-$") 'ispell-complete-word)

;; indent in Latex-mode
(defun my-tex-mode-init ()
   (setq tex-indent-arg 2))
(add-hook 'tex-mode-hook 'my-tex-mode-init)

;; settings added automatically by Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (markdown-mode gnu-elpa-keyring-update rg counsel which-key doom eyebrowse doom-modeline use-package doom-themes flycheck company-racer flycheck-rust racer rust-mode ggtags ensime expand-region sml-mode jedi flymake-python-pyflakes py-autopep8 exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4")))))
