;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; el-get packages
(el-get-bundle auto-complete)
(el-get-bundle use-package)
(el-get-bundle yasnippet)
(el-get-bundle helm)
(el-get-bundle helm-git-grep)
(el-get-bundle bind-key)
(el-get-bundle diminish)
(el-get-bundle lua-mode)
(el-get-bundle php-mode)
(el-get-bundle web-mode)
(el-get-bundle flycheck)
(el-get-bundle neotree)
(el-get-bundle undo-tree)
(el-get-bundle visual-regexp)
(el-get-bundle git-gutter)
(el-get-bundle minimap)
(el-get-bundle birds-of-paradise-plus-theme)
;; 手動で入れる（暫定）
(el-get-bundle magit)


;; ---------------------------------------- use-package
;; use-package がなければ、ロードしない
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))
(setq use-package-verbose t)

;; 不必要なマイナーモードの表示を消す。
(use-package diminish)
;; キーバインドをラップする
(use-package bind-key)

;; ---------------------------------------- L&F
;; 毎回、x-1で分割ウィンドウを消すのが面倒なので、表示されないように
(setq inhibit-splash-screen t)

;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)
;;(global-linum-mode t)

;;ツールバーを削除
(when window-system
  (tool-bar-mode 0)
  )

;;メニューバーを削除
(menu-bar-mode 0)

;; スクロールバーを削除
(when window-system
  (scroll-bar-mode 0)
  )

;; ファイルサイズの表示
(size-indication-mode t)

;;; 釣合う括弧をハイライトする
(show-paren-mode 1)

;; フォントの設定
(when window-system
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil
		      :family "Courier 10 Pitch"
		      :height 90)
    
    (set-fontset-font (frame-parameter  nil 'font)
		      'japanese-jisx0208
		      (font-spec :family "serif"
				 :size 14)))

  
  (when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
		      :family "Verily Serif Mono"
		      :height 90)
    
    (set-fontset-font (frame-parameter  nil 'font)
		      'japanese-jisx0208
		      (font-spec :family "はんなり明朝"
				 :size 14)))
  )

;; 左右２分割したとき、下の行が折り返しなし&行末が揃うように調整する
;; AA  BB  CC  DD  EE  FF  GG  HH  II  JJ  KK  LL  MM  NN  OO  PP  QQ  RR  SS  TT
;; あ　い　う　え　お　か　き　く　け　こ  さ　し　す　せ　そ　た　ち　つ　て　と

;; フランス語のための設定
;; アクサン記号入力モード
(defun fr ()
  (interactive)
  (set-input-method 'latin-1-prefix))

;; F9キーに割当て。切りたければC-/で。
(bind-key "<f9>" 'fr)

;; テーマのロード
(when window-system
  ;;(load-theme 'misterioso t)
  ;;(load-theme 'wombat t)
  (load-theme 'birds-of-paradise-plus t)
  )
;; GUI時、現在行に色をつける
(when window-system
  (global-hl-line-mode t)
  )

;; CUI時、現在行を下線
(unless window-system
  (setq hl-line-face 'underline)
  (global-hl-line-mode)
  )

;; ------------------------------------------- バックアップに関する設定
;; バックアップファイルを一箇所にまとめる
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs-backup"))
	    backup-directory-alist
	    )
      )

;; .#*などのバックアップを作らない
(setq auto-save-default nil)

;; emacs24の右から左へのテキストのためのシステムをオフにする。
(setq-default bidi-display-reordering nil
	      bidi-paragraph-direction (quote left-to-right))

;; ---------------------------------------- キーバインド（Emacs標準）
;; C-u をよく使う操作のプレフィックスにする
(global-unset-key "\C-u")

;; C-m に newline-and-indent を割当 初期値はnewline
(bind-key "C-m" 'newline-and-indent)
;; C-hにバックスペースを割り当て デーモンから起動時およびミニバッファ等に対応
(define-key key-translation-map [?\C-h] [?\C-?])

;; タブの設定
;; C-i でタブ文字入力
;;(global-set-key "\C-i" '(lambda ()
;;			  (interactive)
;;			  (insert "\t")))

;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)

;; 行の折り返しトグル C-c l
(bind-key "C-u C-l" 'toggle-truncate-lines)

;; ノートPCだとC-HOME, C-ENDが押しづらいので
(bind-key "C-u C-e" 'end-of-buffer)
(bind-key "C-u C-a" 'beginning-of-buffer)

;; ウィンドウ切り替え C-t
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(bind-key "C-t" 'other-window-or-split)

;; ウィンドウ分割 C-u C-v, C-u C-h
(bind-key "C-u C-b" 'split-window-below)
(bind-key "C-u C-v" 'split-window-right)

;; バッファ切り替え C-u C-n, C-u C-p
(bind-key "C-u C-n" 'next-buffer)
(bind-key "C-u C-p" 'previous-buffer)

; M-x, M-p で 5行移動できるように設定
(bind-key "M-n" (kbd "M-5 C-n"))
(bind-key "M-p" (kbd "M-5 C-p"))

;; 正規表現置換えをわかりやすく
(bind-key "M-%" 'vr/query-replace)

;; F5キーでミニマップ表示のトグル
(bind-key "<f5>" 'minimap-mode)

;; F6キーで日付挿入
(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
(bind-key "<f6>" 'insert-current-time)

(define-key global-map "\C-cd" `insert-current-time)
;; F7キーでホワイトスペース表示ON/OFF
(bind-key "<f7>" 'whitespace-mode)

;; ------------------------------------------- minimap
(use-package minimap
  :config
  )

;; ------------------------------------------- undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

;; ------------------------------------------- neo-tree
(use-package neotree
  :config
  (bind-key [f8] 'neotree-toggle))

;; ------------------------------------------- auto-complete
(use-package auto-complete
  :config
  (setq ac-auto-start nil)
  (bind-key "C-<tab>" 'auto-complete)
  )

;; ------------------------------------------- helm
(use-package helm)

(use-package helm-config
  :config
  (bind-key "C-u C-u" 'helm-command-prefix)
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)
  (bind-key "C-u C-u C-b" 'helm-mini)
  (bind-key "C-u C-u o" 'helm-occur)
  
  ;; デフォルトキーバインドの置換え
  (bind-key "M-x" 'helm-M-x)
  (setq helm-M-x-fuzzy-match t)

  (bind-key "M-y" 'helm-show-kill-ring)

  (bind-key "C-x C-f" 'helm-find-files)
  
  
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (helm-mode 1)
  )

;; ---------------------------------------- web-mode
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml$"		. web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$"	. web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp$"	. web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]$"	. web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$"	. web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"	. web-mode))

  ;; インデント数
  (setq web-mode-html-offset	2)
  (setq web-mode-css-offset	2)
  (setq web-mode-script-offset	2)
  (setq web-mode-php-offset	2)
  (setq web-mode-java-offset	2)
  (setq web-mode-asp-offset	2)
  )

;; ---------------------------------------- flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)

  (bind-key "C-c n" 'flycheck-next-error)
  (bind-key "C-c p" 'flycheck-previous-error)
  (bind-key "C-c d" 'flycheck-list-error)
  )
;; ---------------------------------------- magit
(use-package magit
  :config
  ;;(when (eq system-type 'windows-nt)
  ;;  (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
  ;;  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  ;;  )
  (bind-key "C-u C-c" 'magit-status)
  (when (eq system-type 'windows-nt)
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
    )
  )
;; ---------------------------------------- git-gutter-mode
(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  )
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
