;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  )
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
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
(el-get-bundle company-mode/company-mode)
(el-get-bundle xcwen/ac-php)
(el-get-bundle emacs-phpcbf)
(el-get-bundle use-package)
(el-get-bundle yasnippet)
(el-get-bundle helm)
(el-get-bundle helm-cmd-t)
(el-get-bundle helm-gtags)
(el-get-bundle bind-key)
(el-get-bundle diminish)
(el-get-bundle lua-mode)
(el-get-bundle php-mode)
(el-get-bundle web-mode)
(el-get-bundle markdown-mode)
(el-get-bundle flycheck)
(el-get-bundle flycheck-phpstan)
(el-get-bundle neotree)
(el-get-bundle undo-tree)
(el-get-bundle visual-regexp)
(el-get-bundle git-gutter)
(el-get-bundle git-gutter-fringe)
(el-get-bundle minimap)
(el-get-bundle highlight-symbol)
(el-get-bundle powerline)
(el-get-bundle magit)

(el-get-bundle clues-theme)

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

;; テーマのロード
(when window-system
  ;;(load-theme 'misterioso t)
  ;;(load-theme 'wombat t)
  ;;(load-theme 'sourcerer t)
  (load-theme 'clues t)
  ;;(load-theme 'darktooth t)
  (set-frame-parameter nil 'alpha 90)
  )

;; フォントの設定
(set-face-attribute 'default nil
                    :family "monospace"
                    :height 90)

;; 日本語フォントの設定
(when window-system
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil
                        :family "Verily Serif Mono"
                        :height 90)

    (set-face-attribute 'mode-line nil
                        :family "Verily Serif Mono"
                        :height 90)

    (set-fontset-font (frame-parameter  nil 'font)
                      'japanese-jisx0208
                      (font-spec :family "VL ゴシック"
                                 :size 14)))

  (when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
                        :family "Verily Serif Mono"
                        :height 90)

    (set-face-attribute 'mode-line nil
                        :family "Garamond"
                        :height 100)

    (set-fontset-font (frame-parameter  nil 'font)
                      'japanese-jisx0208
                      (font-spec :family "Meiryo"
                                 :size 14)))
  )

;; 左右２分割したとき、下の行が折り返しなし&行末が揃うように調整する
;; AA  BB  CC  DD  EE  FF  GG  HH  II  JJ  KK  LL  MM  NN  OO  PP  QQ  RR  SS  TT
;; あ　い　う　え　お　か　き　く　け　こ  さ　し　す　せ　そ　た　ち　つ　て　と

;; OSのクリップボードと共有する
(setq select-enable-clipboard t)
;; フランス語のための設定
;; アクサン記号入力モード
(defun fr ()
  (interactive)
  (set-input-method 'latin-1-prefix))

;; F9キーに割当て。切りたければC-/で。
(bind-key "<f9>" 'fr)

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

;; GUIだとほとんど使わないサスペンド機能を無効化
(global-unset-key "\C-z")

;; C-m に newline-and-indent を割当 初期値はnewline
(bind-key "C-m" 'newline-and-indent)
;; C-hにバックスペースを割り当て デーモンから起動時およびミニバッファ等に対応
(define-key key-translation-map [?\C-h] [?\C-?])

;; タブの設定
;; C-i でタブ文字入力
;;(global-set-key "\C-i" '(lambda ()
;;			  (interactive)
;;			  (insert "\t")))

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

;; F5キーでPHPデバッグログ挿入（yasnippet設定までの仮
(defun insert-php-debuglog()
  (interactive)
  (insert "error_log(print_r($data,true),'3','/vagrant/public_html/application/logs/debug.log');"))
(bind-key "<f5>" 'insert-php-debuglog)

;; F6キーで日付挿入
(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
(bind-key "<f6>" 'insert-current-time)

(define-key global-map "\C-cd" `insert-current-time)
;; F7キーでホワイトスペース表示ON/OFF
(bind-key "<f7>" 'whitespace-mode)

;; git関連のプレフィックスキー
;; リポジトリ内で grep
(bind-key "C-u C-g C-g" 'helm-cmd-t-grep)
;; リポジトリ内で find
(bind-key "C-u C-g C-f" 'helm-cmd-t-repos)

;; ------------------------------------------- フォーマット関連

;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)

;; タブ幅の設定 web-mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; タブ幅の設定 lua-mode
(defun lua-mode-indent-hook ()
  "Hooks for Lua mode."
  (setq lua-indent-level 2)
  )
(add-hook 'lua-mode-hook 'lua-mode-indent-hook)

;; 保存時、自動で行末のスペースを削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; 保存時、自動でタブをスペースに変換

;; ------------------------------------------- org-mode
;; org-directoryはDropBox内のファイルを指定
(when (eq system-type 'windows-nt)
  (setq org-directory "~/../../Dropbox/agenda/")
  )
(when (eq system-type 'gnu/linux)
  (setq org-directory "~/Dropbox/agenda/")
  )
(setq org-agenda-files (list org-directory))
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)
(setq calendar-holidays nil) ;; 不要なら削除

;; ------------------------------------------- markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/bin/pandoc"))
;;  :init (setq markdown-command "multimarkdown"))

;; ------------------------------------------- company
(use-package company
  :config
  (global-company-mode +1)
  (setq company-auto-expand t) ;; 1個目を自動的に補完
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
      (setq company-idle-delay 0) ; 遅延なしにすぐ表示
      (setq company-minimum-prefix-length 2) ; デフォルトは4
      (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
      (setq completion-ignore-case t)
      (setq company-dabbrev-downcase nil)
      (bind-key "C-<tab>" 'company-complete)
      ;; C-n, C-pで補完候補を次/前の候補を選択
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
      (define-key company-active-map (kbd "C-h") nil) ;; C-hはバックスペース割当のため無効化
      (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h

      ;; 未選択項目
      (set-face-attribute 'company-tooltip nil
                  :foreground "#36c6b0" :background "#244f36")
      ;; 未選択項目&一致文字
      (set-face-attribute 'company-tooltip-common nil
                    :foreground "white" :background "#244f36")
      ;; 選択項目
      (set-face-attribute 'company-tooltip-selection nil
                  :foreground "#a1ffcd" :background "#007771")
      ;; 選択項目&一致文字
      (set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "#007771")
      ;; スクロールバー
      (set-face-attribute 'company-scrollbar-fg nil
                  :background "#4cd0c1")
      ;; スクロールバー背景
      (set-face-attribute 'company-scrollbar-bg nil
                          :background "#002b37")
      )

;; ------------------------------------------- minimap
(use-package minimap
  :config
  )

;; ------------------------------------------- undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  )

;; ------------------------------------------- neo-tree
(use-package neotree
  :config
  (bind-key [f8] 'neotree-toggle))


;; ------------------------------------------- helm
(use-package helm)

(use-package helm-config
  :diminish helm-mode
  :config
  (bind-key "C-;" 'helm-for-files)
  (bind-key "C-u C-u" 'helm-command-prefix)
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)
  (bind-key "C-u C-u C-b" 'helm-mini)
  (bind-key "C-u C-u o" 'helm-occur)
  (bind-key "C-x C-b" 'helm-buffers-list)

  ;; デフォルトキーバインドの置換え
  (bind-key "M-x" 'helm-M-x)
  (setq helm-M-x-fuzzy-match t)

  (bind-key "M-y" 'helm-show-kill-ring)

  (bind-key "C-x C-f" 'helm-find-files)

  ;; 自動補完を無効化
  ;; (custom-set-variables '(helm-ff-auto-update-initial-value nil))
  (setq helm-ff-auto-update-initial-value nil)

  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(helm-c-yas-complete . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory . nil))

  ;; . と .. を候補から除外
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :around (lambda (fcn file)
                        (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
                                              (funcall fcn file))))
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (helm-mode 1)
  )

;; ---------------------------------------- helm-gtags
;; C-jをタグジャンプのためのプレフィックスにする
(global-unset-key "\C-j")
(use-package helm-gtags
  :config
  (add-hook 'helm-gtags-mode-hook
            '(lambda ()
               ;; 入力されたタグの定義元へジャンプ
               (local-set-key (kbd "C-j C-t") 'helm-gtags-find-tag)

               ;; 入力タグを参照する場所へジャンプ
               (local-set-key (kbd "C-j C-r") 'helm-gtags-find-rtag)

               ;; 入力シンボルを参照する場所へジャンプ
               (local-set-key (kbd "C-j C-s") 'helm-gtags-find-symbol)

               ;; タグ一覧からタグを選択し、その定義元へジャンプする
               (local-set-key (kbd "C-j C-l") 'helm-gtags-select)

               ;; ジャンプ前の場所に戻る
               (local-set-key (kbd "C-j C-j") 'helm-gtags-pop-stack)))

  (add-hook 'php-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
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

  ;; 色の設定
  ;;(custom-set-faces
  ;; '(web-mode-doctype-face
  ;;   ((t (:foreground "#82AE46"))))
  ;; '(web-mode-html-tag-face
  ;;   ((t (:foreground "#E6B422" :weight bold))))
  ;; '(web-mode-html-attr-name-face
  ;;   ((t (:foreground "#C97586"))))
  ;; '(web-mode-html-attr-value-face
  ;;   ((t (:foreground "#82AE46"))))
  ;; '(web-mode-comment-face
  ;;   ((t (:foreground "#D9333F"))))
  ;; '(web-mode-server-comment-face
  ;;   ((t (:foreground "#D9333F"))))
  ;; '(web-mode-css-rule-face
  ;;   ((t (:foreground "#A0D8EF"))))
  ;; '(web-mode-css-pseudo-class-face
  ;;   ((t (:foreground "#FF7F00"))))
  ;; '(web-mode-css-at-rule-face
  ;;   ((t (:foreground "#FF7F00"))))
  ;; )
  ;; インデント数
  ;; (setq web-mode-html-offset	2)
  ;; (setq web-mode-css-offset	2)
  ;; (setq web-mode-script-offset	2)
  ;; (setq web-mode-php-offset	2)
  ;; (setq web-mode-java-offset	2)
  ;; (setq web-mode-asp-offset	2)
  )

;; ---------------------------------------- flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)

  (bind-key "C-c n" 'flycheck-next-error)
  (bind-key "C-c p" 'flycheck-previous-error)
  (bind-key "C-c d" 'flycheck-list-error)
  )

;; ---------------------------------------- flycheck-phpstan
(defun my-php-mode-hook ()
  "My PHP-mode hook."
  (require 'flycheck-phpstan)
  (flycheck-mode t)
  (flycheck-select-checker 'phpstan)
  (setq php-mode-coding-style 'psr2
        c-basic-offset 4))
(add-hook 'php-mode-hook 'my-php-mode-hook)

;; ---------------------------------------- phpcbf
(require 'phpcbf)

(custom-set-variables
 '(phpcbf-executable "/usr/bin/phpcbf")
 '(phpcbf-standard "PSR2"))

;; Auto format on save.
(add-hook 'php-mode-hook 'phpcbf-enable-on-save)
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
(unless window-system
  (use-package git-gutter
    :diminish git-gutter-mode
    :config
    (global-git-gutter-mode t)))

;; ---------------------------------------- git-gutter-fringe
(when window-system
  (use-package git-gutter-fringe
    :diminish git-gutter-mode
    :config
    (global-git-gutter-mode t)
    (global-linum-mode t)
    ))

;; ---------------------------------------- highlight-symbol
;; カーソル位置のハイライト
;; highlight-symbol
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 1.0) ;; 1秒後のハイライト
  (add-hook 'prog-mode-hook 'highlight-symbol-mode) ;; プログラミング言語の時自動で on
  ;;(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode) ;; M-p/M-nでシンボル間を移動
  )

;; ---------------------------------------- powerline
(use-package powerline
  :config
  (defconst fgcolor1-active "#382048")
  (defconst bgcolor1-active "#F0E0D0")
  (defconst fgcolor2-active "#D0C0A0")
  (defconst bgcolor2-active "#382848")
  (defconst fgcolor3-active "#D0C0A0")
  (defconst bgcolor3-active "#201038")
  (defconst fgcolor1-inactive "#382048")
  (defconst bgcolor1-inactive "#A0A080")
  (defconst fgcolor2-inactive "#A0A080")
  (defconst bgcolor2-inactive "#201830")
  (defconst fgcolor3-inactive "#A0A080")
  (defconst bgcolor3-inactive "#100818")

  (set-face-attribute 'mode-line nil
                      :foreground fgcolor1-active
                      :background bgcolor1-active
                      :bold t
                      :box nil)

  (set-face-attribute 'powerline-active1 nil
                      :foreground fgcolor2-active
                      :background bgcolor2-active
                      :bold t
                      :box nil
                      :inherit 'mode-line)

  (set-face-attribute 'powerline-active2 nil
                      :foreground fgcolor3-active
                      :background bgcolor3-active
                      :bold t
                      :box nil
                      :inherit 'mode-line)

  (set-face-attribute 'mode-line-inactive nil
                      :foreground fgcolor1-inactive
                      :background bgcolor1-inactive
                      :bold t
                      :box nil)

  (set-face-attribute 'powerline-inactive1 nil
                      :foreground fgcolor2-inactive
                      :background bgcolor2-inactive
                      :bold t
                      :box nil
                      :inherit 'mode-line)

  (set-face-attribute 'powerline-inactive2 nil
                      :foreground fgcolor3-inactive
                      :background bgcolor3-inactive
                      :bold t
                      :box nil
                      :inherit 'mode-line)
  (powerline-default-theme)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (darktooth-theme magit bind-key)))
 '(safe-local-variable-values (quote ((php-project-root . auto)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
