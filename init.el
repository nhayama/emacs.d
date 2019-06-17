;; emacs settings

(package-initialize)

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
  ;; (doom-themes-org-config)
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
    ;; '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

;; show the guide of keybinds
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; M-x のコマンドにキーバインドが割り当てられていたら表示する
(use-package amx)

;; find-fileのファイル名補完で大文字小文字を区別しない設定
(setq read-file-name-completion-ignore-case t)

;; org-mode 用のリンク保存
(define-key global-map "\C-cl" 'org-store-link)

;; wdired のショートカット (dired で e を押す)
(require 'wdired)
(setq wdired-allow-to-change-permissions t)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;; 拡張子ごとにモードを設定
(add-to-list 'auto-mode-alist '("\\.lp\\'" . prolog-mode))

;; バックアップファイルの保存先
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

;; フォントの設定
;; Myrica をインストールしておく必要あり
;; Myrica.TTC -> "Myrica M", MyricaM.TTC -> "Myrica MM"
(add-to-list 'default-frame-alist '(font . "Myrica M"))

;; 行番号を表示
(require 'linum)
(global-linum-mode)

;; C-h を バックスペースへ
(global-set-key "\C-h" 'delete-backward-char)

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "elpa")

;; dired settings
;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;; window-resizer
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

;; C-q をプリフィックスキー化
(define-key global-map "\C-q" (make-sparse-keymap))

;; quoted-insert は C-q C-q へ割り当て
(global-set-key "\C-q\C-q" 'quoted-insert)

;; window-resizer は C-q C-r (resize) で
(global-set-key "\C-q\C-r" 'my-window-resizer)

;; C-x o のかわりに
(global-set-key "\C-ql" 'windmove-right)
(global-set-key "\C-qh" 'windmove-left)
(global-set-key "\C-qj" 'windmove-down)
(global-set-key "\C-qk" 'windmove-up)

;; gtags の設定
(require 'gtags)
(global-set-key "\M-t" 'gtags-find-tag)
(global-set-key "\M-r" 'gtags-find-rtag)
(global-set-key "\M-s" 'gtags-find-symbol)
(global-set-key "\C-t" 'gtags-pop-stack)


;; ### C言語の設定 ###
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     ;; flyspell-prog-mode をオンにする
	     (flyspell-prog-mode)
))

;; ### sh-mode の設定 ###
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; ;; ### python の設定 ###
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

;; ;; ### Rust の設定 ###
;; (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
;; (require 'company-racer)
;; (with-eval-after-load 'company
;;    (add-to-list 'company-backends 'company-racer))
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'rust-mode-hook #'flycheck-rust-setup)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ### TeXの設定 ###
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

;; Latex-mode のインデント
(defun my-tex-mode-init ()
   (setq tex-indent-arg 2))
(add-hook 'tex-mode-hook 'my-tex-mode-init)

;; Custom によって自動で追加された設定
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (amx which-key org-pomodoro doom eyebrowse doom-modeline use-package doom-themes flycheck company-racer flycheck-rust racer rust-mode ggtags ensime expand-region haskell-mode sml-mode jedi flymake-python-pyflakes py-autopep8 exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4")))))
