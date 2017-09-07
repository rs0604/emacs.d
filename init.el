
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
(el-get-bundle anything)
;;(el-get-bundle magit)
(el-get-bundle bind-key)
(el-get-bundle diminish)
(el-get-bundle lua-mode)
(el-get-bundle neotree)
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
(global-linum-mode t)

;;ツールバーを削除
(tool-bar-mode 0)

;;メニューバーを削除
(menu-bar-mode 0)

;; スクロールバーを削除
(scroll-bar-mode 0)

;; ファイルサイズの表示
(size-indication-mode t)

;;; 釣合う括弧をハイライトする
(show-paren-mode 1)

;; フォントの設定
(set-face-attribute 'default nil
                     :family "Courier 10 Pitch"
                    :height 90)

;; 日本語フォントの設定
(set-fontset-font (frame-parameter  nil 'font)
	'japanese-jisx0208
	(font-spec :family "serif"
		   :size 14))



;; 左右２分割したとき、下の行が折り返されないようにフォントサイズを調整
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;; テーマのロード
(when window-system
  (load-theme 'misterioso t)
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

;; C-i でタブ文字入力
(global-set-key "\C-i" '(lambda ()
			  (interactive)
			  (insert "\t")))

;; 行の折り返しトグル C-c l
(bind-key "C-u C-l" 'toggle-truncate-lines)

;; ノートPCだとC-HOME, C-ENDが押しづらいので
(bind-key "C-u C-e" 'end-of-buffer)
(bind-key "C-u C-a" 'beginning-of-buffer)

;; ウィンドウ切り替え C-u C-w
(bind-key "C-u C-w" 'other-window)

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

;; F7キーでホワイトスペース表示ON/OFF
(bind-key "<f7>" 'whitespace-mode)

;; ------------------------------------------- neo-tree
(use-package neotree
  :config
  (bind-key [f8] 'neotree-toggle))

;; ------------------------------------------- anything
;; C-j をanything関連のプレフィックスにする
(global-unset-key "\C-j")
(use-package anything-startup)

(use-package anything-config
  :config
  (setq anything-enable-shortcuts 'prefix)
  (bind-key "@" 'anything-select-with-prefix-shortcut anything-map)
  (bind-key "C-j C-b" 'anything-mini)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
