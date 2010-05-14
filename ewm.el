;;; ewm.el --- simple window manager for emacs

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Version: 1.0
;; Keywords: tools, window manager

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; EmacsにWindow管理を入れたコンセプト実装。
;; ・バッファの表示履歴の管理
;; ・ポップアップ場所の固定
;; ・EclipseのPerspectiveみたいなイメージ
;; ・プラグインによる拡張
;;
;; パースペクティブ
;; ・メインコーディング画面
;; ・２画面比較画面
;; ・ドキュメント参照画面
;; 　・followモード
;; ・バッファ一覧画面
;; 　・機能バッファも含めるかどうか
;; 　・バッファ選択したらパースペクティブを選択

;; ○インストール
;; 1) ewm.el, window-layout.el をロードパスに置く。
;; 2) 以下を .emacs に書く
;;    (require 'ewm)
;; 3) M-x ewm:start-management で開始。
;;    ※止めるには (prefix) C-q か、 M-x ewm:stop-management で終了。

;; ○開発方針
;; とりあえずやりたいこと（Window管理、パースペクティブ、プラグイン）を
;; 実装してみて、整理・汎用化を後で考えてみる。

;; このプログラムを実行して入ってしまうアドバイスやフック
;; * advice 
;;  - buffer系
;;     switch-to-buffer, pop-to-buffer
;;  - window-configuration系
;;     current-window-configuration
;;     window-configuration-frame
;;     compare-window-configurations
;;     set-window-configuration
;;     window-configuration-p
;; * hook
;;     kill-buffer-hook
;;     window-configuration-change-hook
;;     completion-setup-hook
;; * override variable
;;     special-display-function

;; ○略語、表記など
;; pst          : perspective
;; ewm:c-       : カスタマイズ変数
;; ewm:$        : 構造体定義

;; ○ソース構成
;; 全体カスタマイズ / ewm:c-
;; 基本関数
;; 履歴管理 / ewm:history-
;; アドバイス（switch-to-buffer, pop-to-bufferなど）
;; パースペクティブフレームワーク / ewm:pst-
;; プラグインフレームワーク / ewm:plugin-
;; メニュー / ewm:menu-
;; プラグイン定義 / ewm:def-plugin-
;; パースペクティブ定義 / ewm:dp-
;;   code  / ewm:dp-code-
;;   doc   / ewm:dp-doc-
;;   two   / ewm:dp-two-
;;   array / ewm:dp-array-
;; 全体制御

;;; Code:

(eval-when-compile (require 'cl))

(require 'imenu)
(require 'easymenu)
(require 'windmove)

(require 'window-layout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Customize

(defvar ewm:c-max-history-num 20 "Number of buffer history.")   ; 履歴の保存数
(defvar ewm:c-recordable-buffer-p  ; 履歴として記録したいBuffer
      (lambda (buf)
        (buffer-local-value 'buffer-file-name buf))) ; ファイル名に関連ついてるもの
(defvar ewm:c-blank-buffer         ; 白紙バッファ
      (let ((buf (get-buffer-create " *ewm:blank*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (buffer-disable-undo buf)
          (erase-buffer)
          (setq buffer-read-only t)) buf)
      "Blank buffer.")

(defvar ewm:prefix-key "C-c ; ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Macro / Utilities

(defmacro ewm:aif (test-form then-form &rest else-forms)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'ewm:aif 'lisp-indent-function 2)

(defun ewm:define-keymap (keymap-list &optional prefix)
  (let ((map (make-sparse-keymap)))
    (mapc 
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro 
              (if prefix 
                  (replace-regexp-in-string "prefix" prefix (car i))
                (car i)))
           (car i))
         (cdr i)))
     keymap-list)
    map))

(defun ewm:add-keymap (keymap keymap-list &optional prefix)
  (mapc 
   (lambda (i)
     (define-key keymap
       (if (stringp (car i))
           (read-kbd-macro 
            (if prefix 
                (replace-regexp-in-string "prefix" prefix (car i))
              (car i)))
         (car i))
       (cdr i)))
   keymap-list)
  keymap)

(eval-and-compile
  (defvar ewm:debug nil "Debug output switch.")) ; debug
(defvar ewm:debug-count 0 "[internal] Debug output counter.") ; debug

(defmacro ewm:message (&rest args)
  (when ewm:debug
    `(progn 
       (with-current-buffer (get-buffer-create "*ewm:debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" ewm:debug-count (format ,@args)))))
       (incf ewm:debug-count))))

(defun ewm:message-mark ()
  (interactive)
  (ewm:message "==================== mark ==== %s" 
               (format-time-string "%H:%M:%S" (current-time))))

(defun ewm:string-trim (txt)
  "Remove white space characters at head and tail
from the given string."
  (let ((ret txt))
    (setq ret (if (string-match "^\\s-*" ret)
                  (substring ret (match-end 0))
                ret))
    (or
     (loop for i downfrom (1- (length ret)) downto 0 do 
           (if (/= 32 (char-syntax (aref ret i)))
               (return (substring ret 0 (1+ i)))))
     "")))

(defun ewm:strtime (time)
  (if (equal (cdddr (decode-time time))
             (cdddr (decode-time (current-time))))
      (format-time-string "Today  %H:%M:%S" time)
    (format-time-string   "%Y/%m/%d %H:%M:%S" time)))

(defface ewm:face-title 
  '((((type tty pc) (class color) (background light))
     :foreground "green" :weight bold)
    (((type tty pc) (class color) (background dark))
     :foreground "yellow" :weight bold)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for ewm titles at level 1."
  :group 'ewm)

(defface ewm:face-subtitle
  '((((type tty pc) (class color)) :foreground "lightblue" :weight bold)
    (t :height 1.2 :inherit variable-pitch))
  "Face for ewm titles at level 2."
  :group 'ewm)

(defface ewm:face-item
  '((t :inherit variable-pitch :foreground "DarkSlateBlue"))
  "Face for ewm items."
  :group 'ewm)

(defun ewm:rt (text face)
  (put-text-property 0 (length text) 'face face text) text)

(defun ewm:rt-format (text &rest args)
  (apply 'format (ewm:rt text 'ewm:face-item)
         (loop for i in args
               if (consp i)
               collect (ewm:rt (car i) (cdr i))
               else
               collect (ewm:rt i 'ewm:face-subtitle))))

(defun ewm:tp (text prop value)
  (if (< 0 (length text))
    (put-text-property 0 1 prop value text))
  text)

(defun ewm:find (name name-func seq)
  (loop for i in seq
        if (eq name (funcall name-func i))
        return i))

(defmacro ewm:delete! (name name-func seq)
  `(setq ,seq 
         (delete-if 
          (lambda (i) (eq ,name (funcall ',name-func i)))
             ,seq)))

(defmacro ewm:safe-call (method object &rest args)
  (let ((sym (gensym)))
    `(let ((,sym (,method ,object)))
       (if ,sym
           (funcall ,sym ,@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Base API / History Management

(defun ewm:frame-param-get (name)
  (frame-parameter (selected-frame) name))  

(defun ewm:frame-param-set (name val)
  (set-frame-parameter (selected-frame) name val))  

(defun ewm:history-get ()
  (ewm:frame-param-get 'ewm:buffer-history))

(defun ewm:history-save (buffer-history)
  (ewm:frame-param-set
   'ewm:buffer-history buffer-history)
  buffer-history)

(defun ewm:history-get-backup ()
  ;; history back undoキュー
  (ewm:frame-param-get
   'ewm:buffer-history-backup))

(defun ewm:history-save-backup (buffer-history-backup)
  (ewm:frame-param-set 
   'ewm:buffer-history-backup
   buffer-history-backup)
  buffer-history-backup)

(defun ewm:history-recordable-p (buffer)
  (if (and buffer (buffer-live-p buffer))
      (funcall ewm:c-recordable-buffer-p buffer)))

(defun ewm:history-add (buffer)
  ;; 死んでるバッファのクリア
  ;; undoキューのクリア（LRU）
  ;; 履歴に追加、後ろを削除
  ;; フレームパラメーターに保存
  (ewm:message "#HISTORY-ADD : %s" buffer)
  (ewm:aif (get-buffer buffer)
      (let* ((prev-history (ewm:history-get))
             (last-buffer (car prev-history))
             (history
             (mapcar 
               'car
               (sort 
                (loop for h in (append
                                (cdr prev-history)
                                (ewm:history-get-backup))
                     for b = (get-buffer h)
                     if (and b (buffer-live-p b))
                     collect (cons b (float-time 
                                      (buffer-local-value 
                                       'buffer-display-time b))))
               (lambda (i j) 
                 (> (cdr i) (cdr j)))))))
        (when last-buffer
          (setq history (cons last-buffer history)))
        (when (ewm:history-recordable-p it)
          (ewm:history-save-backup nil)
          (setq history
                (cons it 
                      (if (member it history)
                          (remove it history)
                        (if (< ewm:c-max-history-num (length history))
                            (nbutlast history) history))))
          (ewm:history-save history)))))

(defun ewm:history-back ()
  ;;undoキューに突っ込んでhistoryから一つ戻す
  ;;表示の更新は自前で。
  (let ((history (ewm:history-get))
        (history-backup (ewm:history-get-backup)))
    (when (and history (cdr history))
      (push (pop history) history-backup))
    (ewm:history-save history)
    (ewm:history-save-backup history-backup)))

(defun ewm:history-forward ()
  ;;undoキューから一つ戻してhistoryに入れる
  ;;表示の更新は自前で
  (let ((history (ewm:history-get))
        (history-backup (ewm:history-get-backup)))
    (when history-backup
      (push (pop history-backup) history))
    (ewm:history-save history)
    (ewm:history-save-backup history-backup)))

(defun ewm:history-delete (buffer)
  ;;kill-buffer-hook等から履歴から削除するために呼ぶ
  ;;表示の更新は自前で
  (let ((history (ewm:history-get))
        (history-backup (ewm:history-get-backup)))
    (setq history (remove buffer history))
    (setq history-backup (remove buffer history-backup))
    (when (and (null history) history-backup)
      (push (pop history-backup) history))
    (ewm:history-save history)
    (ewm:history-save-backup history-backup)))

(defun ewm:history-get-main-buffer ()
  ;;現在編集中のバッファ（＝履歴の最新という前提）
  (ewm:aif (ewm:history-get)
      (car it) ewm:c-blank-buffer))

(defun ewm:managed-p ()
  ;;このフレームがWMで管理対象かどうか
  (ewm:pst-get-instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Advices / Overriding Functions

(defvar ewm:ad-now-overriding nil "[internal] Recursive execution flag.") ; 乗っ取り中なら t → 元の関数を実行

(defmacro ewm:with-advice (&rest body)
  ;;switch-to-buffer, pop-to-bufferが無限ループにならないようにするマクロ。
  ;;ユーザーのアクションではなくて、内部の動作なのでこれらの関数を
  ;;本来の動きにしたい場合はこのマクロで囲む。
  `(if ewm:ad-now-overriding
       (progn ,@body) ; 再帰している場合
     (setq ewm:ad-now-overriding t) ; 初回
     (unwind-protect 
         (progn ,@body) 
       (setq ewm:ad-now-overriding nil))))

(defadvice switch-to-buffer (around
                             ewm:ad-override
                             (buf &optional norecord))
  (ewm:message "#SWITCH-TO-BUFFER %s" buf)
  (let (overrided)
    (when (and buf 
               (not ewm:ad-now-overriding) ; 再入してなくて
               (ewm:managed-p)) ; 管理対象フレームの場合は乗っ取る
      (ewm:with-advice
       (ewm:message "#AD-SWITCH-TO-BUFFER %s" buf)
       (ewm:history-add buf)
       (setq overrided (ewm:pst-switch-to-buffer (get-buffer-create buf)))))
    (if overrided
        (progn
          (set-buffer buf))
      ad-do-it) ; それ以外はもとの関数へ（画面更新はしないので必要な場合は自分でする）
    buf))

(defadvice pop-to-buffer (around 
                          ewm:ad-override
                          (buf &optional other-window norecord))
  (ewm:message "#POP-TO-BUFFER %s" buf)
  (let (overrided)
    (when (and buf 
               (not ewm:ad-now-overriding) ; 再入してなくて
               (ewm:managed-p)) ; 管理対象フレームの場合は乗っ取る
      (ewm:with-advice
       (ewm:message "#AD-POP-TO-BUFFER %s" buf)
       (ewm:history-add buf)
       (setq overrided (ewm:pst-pop-to-buffer (get-buffer-create buf)))))
    (if overrided
        (progn (set-buffer buf) buf)
      ad-do-it))) ; それ以外はもとの関数へ（画面更新はしないので必要な場合は自分でする）

(defun ewm:override-special-display-function (buf &optional args)
  (ewm:message "#SPECIAL-DISPLAY-FUNC %s" buf)
  (let (overrided)
    (when (and 
           buf 
           (not ewm:ad-now-overriding) ; 再入してなくて
           (ewm:managed-p)) ; 管理対象フレームの場合は乗っ取る
      (ewm:with-advice
       (ewm:message "#AD-SPECIAL-DISPLAY-FUNC %s" buf)
       (ewm:history-add buf)
       (setq overrided (ewm:pst-pop-to-buffer (get-buffer-create buf)))))
    (if overrided
        (progn (set-buffer buf) (get-buffer-window buf))
      (ewm:message "#DISPLAY-BUFFER ") ;;適当な場所に表示する
      (set-window-buffer (selected-window) buf)
      (set-buffer buf) (selected-window) 
      )))

(defun ewm:kill-buffer-hook ()
  (ewm:message "#KILL HOOK %s" (current-buffer))
  (when (and (ewm:history-recordable-p (current-buffer))
             (ewm:managed-p))
    ;; killされたら履歴からも消す
    (ewm:history-delete (current-buffer))
    (ewm:pst-show-history-main)))

;; set-window-configuration 対策
;; いろいろ試行錯誤中。

(defun ewm:debug-windows (wm)
  (ewm:message " # WINDOWS : %s" 
               (loop for winfo in (wlf:wset-winfo-list wm)
                     collect (wlf:window-window winfo))))

;;ewm:$wcfg ウインドウ配置構造体
;; wcfg  : 本来のcurrent-window-configurationでとれるウインドウ配置オブジェクト
;; pst   : パースペクティブのインスタンスのコピー
;; count : デバッグ用カウンタ
(defstruct ewm:$wcfg wcfg pst count)

(defun ewm:override-custom-wcfg-p (cfg)
  (ewm:$wcfg-p cfg))

(defvar ewm:override-window-cfg-change-now nil) ; ewm:override-window-cfg-change 実行中ならt。再帰呼び出しを防ぐ。

(defun ewm:override-window-cfg-change ()
  ;; window-configuration-change-hook関数
  (when (and (ewm:managed-p) ; ewm管理中で
             (null ewm:override-window-cfg-change-now) ; 初回実行で
             (= (minibuffer-depth) 0) ; ミニバッファ実行中でなくて
             (and ewm:override-window-cfg-backup ; 補完前のウインドウ配置が空でなくて
                  (not (compare-window-configurations ; 配置が違ってたら
                        ewm:override-window-cfg-backup 
                        (current-window-configuration)))))
    (setq ewm:override-window-cfg-change-now t)
    (unwind-protect
        (ewm:override-restore-window-cfg) ; 配置を戻す
      (setq ewm:override-window-cfg-change-now nil))))

(defun ewm:override-setup-completion ()
  ;;completionバッファが終了したとき、set-window-configurationが呼ばれずに
  ;;window配置が元に戻される。なので、completionから戻ったときには
  ;;windwo-configuration-change-hookを捕まえて自前で
  ;;window配置を直すようにする。
  (when (and (ewm:managed-p) (null ewm:override-window-cfg-backup))
    (ewm:message "#OVERRIDE-SETUP-COMPLETION")
    ;;(ewm:debug-windows (ewm:pst-get-wm))
    (setq ewm:override-window-cfg-backup 
          (current-window-configuration))))

(defvar ewm:override-window-cfg-backup nil "[internal] Backup window configuration.")

(defun ewm:override-restore-window-cfg ()
  (interactive)
  (when ewm:override-window-cfg-backup
    (ewm:message "#RESTORE-WINDOW-CFG")
    (set-window-configuration ewm:override-window-cfg-backup)
    (setq ewm:override-window-cfg-backup nil)
    (let ((i (ewm:pst-get-instance)))
      (ewm:aif (ewm:$pst-main i)
        (wlf:select (ewm:$pst-wm i) it)))))

(defvar ewm:override-window-cfg-count 0 "[internal] Window configuration counter")

(defadvice current-window-configuration (around ewm:ad-override)
  (let ((cfg ad-do-it))
    (incf ewm:override-window-cfg-count)
    (ewm:message "#CURRENT-WINDOW-CONFIGURATION %s" 
                 ewm:override-window-cfg-count)
    (if (ewm:managed-p)
        (let ((data (ewm:pst-copy-instance)))
          (setq ad-return-value
                (make-ewm:$wcfg :wcfg cfg :pst data 
                      :count ewm:override-window-cfg-count))))))

(defadvice window-configuration-p (around ewm:ad-override-long (cfg))
  (setq ad-return-value (or (ewm:override-custom-wcfg-p cfg) ad-do-it)))

(defadvice window-configuration-frame (around ewm:ad-override-long (cfg))
  (when (ewm:override-custom-wcfg-p cfg)
    (ad-set-arg 0 (ewm:$wcfg-wcfg cfg)))
  ad-do-it)

(defadvice compare-window-configurations (around ewm:ad-override-long (cfg1 cfg2))
  (when (ewm:override-custom-wcfg-p cfg1)
    (ad-set-arg 0 (ewm:$wcfg-wcfg cfg1)))
  (when (ewm:override-custom-wcfg-p cfg2)
    (ad-set-arg 1 (ewm:$wcfg-wcfg cfg2)))
  ad-do-it
  (when (and ad-return-value (ewm:managed-p))
    (ewm:message "#COMPARE-WINDOW-CONFIGURATIONS = %s" ad-return-value)
    ;;(ewm:debug-windows (ewm:pst-get-wm))
    ))

(defadvice set-window-configuration (around ewm:ad-override-long (cfg))
  (ewm:message "#SET-WINDOW-CONFIGURATION -->")
  (cond 
   ((ewm:override-custom-wcfg-p cfg)
    ;;管理対象であればwindowオブジェクトを元に戻す
    (let ((pst-instance (ewm:$wcfg-pst cfg))
          (count (ewm:$wcfg-count cfg)))
      (ad-set-arg 0 (ewm:$wcfg-wcfg cfg))
      (ewm:message "#SET-WINDOW-CONFIGURATION (ad-do-it)")
      ad-do-it
      ;;(ewm:debug-windows (ewm:$pst-wm pst-instance))
      (when ewm:pst-minor-mode
        (cond
         ((ewm:managed-p)
          ;;(ewm:message "#AD-SET-WINDOW-CONFIGURATION SET %s" pst-instance)
          (ewm:message "#AD-SET-WINDOW-CONFIGURATION SET %s" count)
          (ewm:pst-set-instance pst-instance))
         (t
          (ewm:message "#AD-SET-WINDOW-CONFIGURATION RESUME %s" pst-instance)
          (ewm:pst-set-instance pst-instance)
          (ewm:pst-resume pst-instance))))))
   (t
    ;;管理してない配置の場合はパースペクティブを無効にする
    (when (ewm:managed-p)
      (ewm:message "#AD-SET-WINDOW-CONFIGURATION FINISH")
      (ewm:pst-finish)
      (ewm:pst-set-instance nil))
    ad-do-it))
  (ewm:message "#SET-WINDOW-CONFIGURATION <-- %s" ad-return-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Perspective Framework

(defvar ewm:pst-list nil "[internal] Perspective class registory.")
(setq ewm:pst-list nil)

;; ewm:$pst-class 構造体
;;   →この構造体でパースペクティブの定義を行う
;;     (*)は必須
;; name    : (*)このパースペクティブの名前、シンボル
;; init    : (*)このパースペクティブのコンストラクタ
;;         : 返値として wset 構造体を返す。
;;         : 基本的に wset 構造体だけを返すようにして、レイアウトや
;;         : 必要なフックなどのセットアップが必要であれば下のstartで行う。
;;         : init,start で使える dynamic bind 変数 : prev-selected-buffer
;; title   : (*)このパースペクティブのタイトル（人が読む用）
;; main    : wlfのウインドウレイアウトのうち、デフォルトでフォーカスを当てるべき場所の名前
;;         : nilなら適当に選ぶ。
;; start   : レイアウトや必要なフックなどのセットアップを行う。引数：wm。
;;         : この関数がnilなら何もしない。
;;         : （leaveで一時中断して後で再度startが呼ばれることがある。）
;; update  : wlfの各windowを更新する際に呼ばれる関数。引数：wm。
;;         : この関数がnilなら何もしない。
;;         : 各Windowのプラグインの更新が行われる前に呼ばれる。
;;         : ウインドウの構成の変更や履歴を戻ったりするたびに呼ばれる。
;; switch  : switch-to-bufferを乗っ取る関数。引数：buffer。
;;         : この関数がnilなら何もしない。返値でnilを返すと本来の動作、
;;         : それ以外なら動作を乗っ取ったものとみなしてそのまま終了する。
;;         : プラグインの更新などが必要であればewm:pst-update-windowsを呼ぶこと。
;; popup   : pop-to-buffer, special-display-func を乗っ取る関数。引数：buffer。
;;         : この関数がnilなら何もしない。返値でnilを返すと本来の動作、
;;         : それ以外なら動作を乗っ取ったものとみなしてそのまま終了する。
;;         : プラグインの更新などが必要であればewm:pst-update-windowsを呼ぶこと。
;; leave   : このパースペクティブを終了する際に呼ばれる関数。引数：wm。
;;         : この関数がnilなら何もしない。
;; keymap  : このパースペクティブで有効にするキーマップのシンボル。nilだと何も設定しない。

(defstruct ewm:$pst-class name title init main start update switch popup leave keymap)

(defun ewm:pst-class-register (pst-class)
  ;;パースペクティブクラスの登録
  (push pst-class ewm:pst-list))

(defun ewm:pst-class-get (name)
  ;;パースペクティブクラスの取得
  (ewm:find name 'ewm:$pst-class-name ewm:pst-list))

;;ewm:$pst(perspective) インスタンス構造体
;; name    : このパースペクティブの名前、シンボル
;; wm      : wlfレイアウトオブジェクト
;; type    : class オブジェクトへの参照

(defstruct ewm:$pst name wm type)

(defun ewm:$pst-title (pst)
  (ewm:$pst-class-title (ewm:$pst-type pst)))
(defun ewm:$pst-main (pst)
  (ewm:$pst-class-main (ewm:$pst-type pst)))
(defun ewm:$pst-start (pst)
  (ewm:$pst-class-start (ewm:$pst-type pst)))
(defun ewm:$pst-update (pst)
  (ewm:$pst-class-update (ewm:$pst-type pst)))
(defun ewm:$pst-switch (pst)
  (ewm:$pst-class-switch (ewm:$pst-type pst)))
(defun ewm:$pst-popup (pst)
  (ewm:$pst-class-popup (ewm:$pst-type pst)))
(defun ewm:$pst-leave (pst)
  (ewm:$pst-class-leave (ewm:$pst-type pst)))
(defun ewm:$pst-keymap (pst)
  (ewm:aif (ewm:$pst-class-keymap (ewm:$pst-type pst))
      (symbol-value it) nil))

(defun ewm:pst-get-instance ()
  (ewm:frame-param-get 'ewm:pst))
(defun ewm:pst-set-instance (pst-instance)
  (ewm:frame-param-set 'ewm:pst pst-instance))

(defun ewm:pst-get-prev-pst ()
  (ewm:frame-param-get 'ewm:prev-pst))
(defun ewm:pst-set-prev-pst (pst-name)
  (ewm:frame-param-set 'ewm:prev-pst pst-name))

(defun ewm:pst-copy-instance ()
  (let ((i (ewm:pst-get-instance)))
    (make-ewm:$pst
     :name   (ewm:$pst-name   i)
     :wm     (wlf:copy-windows (ewm:pst-get-wm))
     :type   (ewm:$pst-type   i))))

(defun ewm:pst-get-wm ()
  (ewm:$pst-wm (ewm:pst-get-instance)))

(defun ewm:pst-update-windows (&optional rebuild-windows)
  ;;全ウインドウを更新する。rebuild-windowがnon-nilであればウインドウの再構築を行う。
  (ewm:message "#PST-UPDATE")
  (ewm:with-advice
   (let* ((instance (ewm:pst-get-instance))
          (wm (ewm:$pst-wm instance)))
     ;;(ewm:debug-windows (ewm:pst-get-wm))
     (when (or rebuild-windows 
               (not (wlf:wset-live-p wm 1)))
       (ewm:message "  #PST-UPDATE > REBUILD")
       (wlf:refresh wm)
       (ewm:aif (ewm:$pst-main instance)
           (wlf:select wm it)))
     ;;パースペクティブ固有の処理
     (ewm:safe-call ewm:$pst-update instance wm)
     ;;プラグイン更新実行
     (ewm:plugin-exec-update (selected-frame) wm)
     )) t)

(defun ewm:pst-switch-to-buffer (buf)
  (ewm:message "#PST-SWITCH %s" buf)
  ;;switch-to-bufferを乗っ取ってパースペクティブ側に委譲する
  (ewm:safe-call ewm:$pst-switch (ewm:pst-get-instance) buf))

(defun ewm:pst-pop-to-buffer (buf)
  (ewm:message "#PST-POPUP %s" buf)
  ;;pop-to-bufferを乗っ取ってパースペクティブ側に委譲する
  (ewm:safe-call ewm:$pst-popup (ewm:pst-get-instance) buf))

(defun ewm:pst-change (next-pst-name)
  (ewm:message "#PST-CHANGE %s" next-pst-name)
  ;;パースペクティブを変更する
  ;;前のパースペクティブの終了処理と、新しい方の開始処理など
  (let ((prev-pst-instance (ewm:pst-get-instance))
        (next-pst-class (ewm:pst-class-get next-pst-name))
        (prev-selected-buffer (current-buffer)))
    (cond
     ((null next-pst-name)
      (error "Perspective [%s] is not found." next-pst-name))
     (t
      (ewm:aif prev-pst-instance
          (progn
            (ewm:safe-call ewm:$pst-leave it (ewm:$pst-wm it))
            (unless (eql next-pst-name (ewm:$pst-name it))
              (ewm:pst-set-prev-pst (ewm:$pst-name it)))))
      (let* ((next-pst-wm
              (funcall (ewm:$pst-class-init next-pst-class)))
             (next-pst-instance 
              (make-ewm:$pst :name next-pst-name
                             :wm next-pst-wm
                             :type next-pst-class)))
        (ewm:pst-set-instance next-pst-instance)
        (ewm:pst-change-keymap (ewm:$pst-keymap next-pst-instance))
        (ewm:safe-call ewm:$pst-start next-pst-instance 
                       (ewm:$pst-wm next-pst-instance)))))
    (ewm:pst-update-windows t)))

(defun ewm:pst-change-prev ()
  ;;前のパースペクティブに変える
  (ewm:aif (ewm:pst-get-prev-pst)
      (progn
        (ewm:message "#PREV-PST : %s" it)
        (ewm:pst-change it))))

(defun ewm:pst-change-keymap (new-keymap)
  (let ((map (copy-keymap
              (or new-keymap ewm:pst-minor-mode-keymap))))
    (when new-keymap
      (set-keymap-parent map ewm:pst-minor-mode-keymap))
    (ewm:aif (assq 'ewm:pst-minor-mode minor-mode-map-alist)
        (setf (cdr it) map))))

(defun ewm:pst-resume (pst-instance)
  (ewm:message "#PST-RESUME %s" pst-instance)
  ;;パースペクティブのインスタンスを戻してstartを呼ぶ
  ;;set-window-configurationでウインドウは元に戻っている仮定
  (ewm:pst-set-instance pst-instance)
  (ewm:pst-change-keymap (ewm:$pst-keymap pst-instance))
  (ewm:safe-call ewm:$pst-start pst-instance (ewm:$pst-wm pst-instance)))

(defun ewm:pst-finish ()
  (ewm:message "#PST-FINISH")
  ;;パースペクティブの終了処理のみ。全体の終了処理や
  ;;set-window-configurationで非管理対象画面に切り替えたときなど。
  (let ((prev-pst-instance (ewm:pst-get-instance)))
    (when prev-pst-instance
      (ewm:safe-call ewm:$pst-leave prev-pst-instance 
                     (ewm:$pst-wm prev-pst-instance)))
    (ewm:pst-set-instance nil)))

(defun ewm:pst-window-option-get (wm window-name)
  ;;指定したウインドウのオプション用plistを取ってくる
  (wlf:window-options 
   (wlf:get-winfo window-name (wlf:wset-winfo-list wm))))

(defun ewm:pst-window-plugin-get (wm window-name)
  ;;指定したウインドウのプラグイン名を取ってくる
  (plist-get (ewm:pst-window-option-get wm window-name)
             ':plugin))

(defun ewm:pst-window-plugin-set (wm window-name plugin-name)
  ;;指定したウインドウにプラグインを設定する
  (plist-put (ewm:pst-window-option-get wm window-name)
             ':plugin plugin-name))

(defun ewm:pst-buffer-set (window-name buffer &optional showp selectp)
  ;;指定したウインドウにバッファをセットする
  (let ((wm (ewm:pst-get-wm)))
    (when (wlf:window-name-p wm window-name)
      (when showp
        (wlf:show wm window-name))
      (wlf:set-buffer wm window-name buffer selectp))))

(defun ewm:pst-window-select (window-name)
  ;;指定したウインドウを選択する
  (let ((wm (ewm:pst-get-wm)))
    (when (wlf:window-name-p wm window-name)
      (wlf:select wm window-name))))

(defun ewm:pst-window-select-main ()
  ;;パースペクティブのデフォルトウインドウを選択する
  ;;main スロットが nil なら何もしない
  (let ((main (ewm:$pst-main (ewm:pst-get-instance)))
        (wm (ewm:pst-get-wm)))
    (when (and main (wlf:window-name-p wm main))
      (wlf:select wm main))))

(defun ewm:pst-window-toggle (window-name &optional selectp next-window)
  ;;指定したウインドウの表示をトグルする
  (let ((wm (ewm:pst-get-wm)))
    (when (wlf:window-name-p wm window-name)
      (wlf:toggle wm window-name)
      (if (wlf:window-displayed-p wm window-name)
          (and selectp (wlf:select wm window-name))
        (and next-window (wlf:select wm next-window))))))

(defun ewm:pst-show-history-main ()
  ;;パースペクティブの「メイン」ウインドウ（もしあれば）に履歴のトップ
  ;;のバッファを表示して ewm:pst-update-windows する。
  ;;履歴移動系のコマンドやバッファ切り替え乗っ取り系から呼ばれる。
  (ewm:with-advice
   (let* ((instance (ewm:pst-get-instance))
          (wm (ewm:$pst-wm instance)))
     (ewm:aif (ewm:$pst-main instance)
         (wlf:set-buffer wm it (ewm:history-get-main-buffer)))
     (ewm:pst-update-windows))))

;;; Commands / Key bindings / Minor Mode
;;;--------------------------------------------------

(defun ewm:pst-change-command ()
  (interactive)
  (let* ((pst-list (mapcar
                    (lambda (i)
                      (symbol-name (ewm:$pst-class-name i))) 
                    ewm:pst-list))
         (pst-name (completing-read "Chagne parspective: " pst-list)))
    (when pst-name
      (ewm:pst-change (intern pst-name)))))
(defun ewm:pst-window-select-main-command ()
  (interactive)
  (ewm:pst-window-select-main))
(defun ewm:pst-update-windows-command ()
  (interactive)
  (when (ewm:managed-p)
    (ewm:with-advice
     (wlf:reset-window-sizes (ewm:pst-get-wm))
     (ewm:pst-update-windows))))
(defun ewm:pst-change-prev-pst-command ()
  (interactive)
  (when (ewm:managed-p)
    (ewm:pst-change-prev)))
(defun ewm:pst-history-forward-command ()
  (interactive)
  (when (ewm:managed-p)
    (ewm:history-forward)
    (ewm:pst-show-history-main)))
(defun ewm:pst-history-back-command ()
  (interactive)
  (when (ewm:managed-p)
    (ewm:history-back)
    (ewm:pst-show-history-main)))

;;全パースペクティブに共通なキーマップ定義
;;各パースペクティブで指定したkeymapがこのkeymapのparentに置き換わる (ewm:pst-change)
(defvar ewm:pst-minor-mode-keymap
      (ewm:define-keymap
       '(("prefix Q"   . ewm:stop-management)
         ("prefix l"   . ewm:pst-update-windows-command)
         ("prefix n"   . ewm:pst-history-forward-command)
         ("prefix p"   . ewm:pst-history-back-command)
         ("prefix <DEL>" . ewm:pst-change-prev-pst-command)
         ) ewm:prefix-key))

(defvar ewm:pst-minor-mode-hook nil)

(defvar ewm:pst-minor-mode nil) ; dummy

;;グローバルでマイナーモードを定義
;;→フレームはひとつしかないことを仮定
;;→elscreenなどに対応する必要がある
(define-minor-mode ewm:pst-minor-mode
  "Perspective mode"
  :init-value nil
  :global t
  :lighter " EWM"
  :keymap ewm:pst-minor-mode-keymap
  :group 'ewm:pst-mode
  (if ewm:pst-minor-mode
      (progn
        (ewm:pst-minor-mode-setup)
        (run-hooks 'ewm:pst-minor-mode-hook))
    (ewm:pst-minor-mode-abort)))

(defun ewm:pst-minor-mode-setup ()
  ;;今のところ特になし
)

(defun ewm:pst-minor-mode-abort ()
  ;;今のところ特になし
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Plugin Framework

;; ewm:$plugin構造体
;; name   : プラグインの symbol 
;; title  : 人が読む用のプラグインの名前
;; update : プラグイン本体の関数
(defstruct ewm:$plugin name title update)

(defvar ewm:plugin-list nil "[internal] Plugin registory.")
(setq ewm:plugin-list nil)

(defun ewm:plugin-register (name title update-function)
  ;;プラグインの登録
  (push (make-ewm:$plugin
         :name name
         :title title
         :update update-function)
        ewm:plugin-list))

(defun ewm:plugin-delete (name)
  ;;プラグインの登録削除
  (ewm:delete! name ewm:$plugin-name ewm:plugin-list))

(defun ewm:plugin-get (name)
  ;;プラグイン構造体を名前から取ってくる
  (if name 
      (ewm:aif (ewm:find name 'ewm:$plugin-name ewm:plugin-list)
          it (ewm:message "Plugin not found [%s]." name) nil)))

(defun ewm:plugin-exec-update (frame wm)
  ;;各windowのプラグインを実行
  (loop for winfo in (wlf:wset-winfo-list wm)
        for plugin-name = (wlf:window-option-get winfo :plugin)
        for plugin = (ewm:plugin-get plugin-name)
        if (and (wlf:window-live-window winfo) plugin)
        do 
        (condition-case err
            (funcall (ewm:$plugin-update plugin) frame wm winfo)
          (nil (ewm:message "Plugin Error %s [%s]" plugin-name err)))))

(defun ewm:plugin-switch (plugin-name)
  ;;現在選択されているウインドウのプラグインを取り替える
  (let* ((wm (ewm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window)))
         (plugin-symbol (if (symbolp plugin-name) 
                            plugin-name
                          (intern plugin-name))))
    (when wname
      (ewm:pst-window-plugin-set wm wname plugin-symbol)
      (ewm:pst-update-windows))))

(defun ewm:plugin-switch-command ()
  (interactive)
  ;;プラグインを選択して取り替える
  (let* ((wm (ewm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window)))
         (cplg (or (if wname 
                       (ewm:pst-window-plugin-get wm wname)) 
                   "No plugin"))
         (plg-list (mapcar 
                    (lambda (i) (symbol-name (ewm:$plugin-name i)))
                    ewm:plugin-list))
         (completion-ignore-case t)
         (plg-name (completing-read
                    (format "Chagne plugin [current: %s] -> : " cplg)
                    plg-list)))
    (when plg-name
      (ewm:plugin-switch plg-name))))

(defun ewm:plugin-remove-command ()
  (interactive)
  ;;現在選択されているウインドウのプラグインを外す
  (let* ((wm (ewm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window))))
    (when wname
      (ewm:pst-window-plugin-set wm wname nil)
      (ewm:pst-update-windows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Menu Definition

(defvar ewm:pst-minor-mode-menu-spec nil)

(defun ewm:menu-pst-selected-p (name)
  ;;現在のパースペクティブがnameかどうか
  (eq name (ewm:$pst-name (ewm:pst-get-instance))))

(defun ewm:menu-plugin-selected-p (name)
  ;;現在のウインドウのプラグインがnameかどうか
  (let* ((wm (ewm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window))))
    (when wname
      (eq name (ewm:pst-window-plugin-get wm wname)))))

(defun ewm:menu-plugin-working-p ()
  ;;現在のウインドウにプラグインがあるかどうか
  (let* ((wm (ewm:pst-get-wm))
         (wname (wlf:get-window-name wm (selected-window))))
    (ewm:pst-window-plugin-get wm wname)))

(defun ewm:menu-define ()
  (let (perspectives plugins)
    (setq perspectives
          (loop for i in ewm:pst-list
                for n = (ewm:$pst-class-name i)
                collect 
                (vector (ewm:$pst-class-title i) 
                        `(lambda () (interactive) (ewm:pst-change ',n)) 
                        :selected `(ewm:menu-pst-selected-p ',n)
                        :style 'toggle)))
    (setq plugins
          (loop for i in ewm:plugin-list
                for n = (ewm:$plugin-name i)
                collect 
                (vector (ewm:$plugin-title i)
                        `(lambda () (interactive) (ewm:plugin-switch ',n))
                        :selected `(ewm:menu-plugin-selected-p ',n)
                        :style 'toggle)))

    (setq ewm:pst-minor-mode-menu-spec 
          `("EWM"
            ["History Forward" ewm:pst-history-forward-command t]
            ["History Back"    ewm:pst-history-back-command t]
            ["Update Windows"  ewm:pst-update-windows-command t]
            "----"
            ["Quit EWM"  ewm:stop-management t]
            "----"
            "Perspectives" ,@(nreverse perspectives)
            "----"
            "Plugins" ,@(nreverse plugins)
            ["Remove Current Plugin" ewm:plugin-remove-command
             (ewm:menu-plugin-working-p)]
            ))
    (easy-menu-define ewm-menu-map
      ewm:pst-minor-mode-keymap "EWM menu map" 
      ewm:pst-minor-mode-menu-spec)
    ;; (easy-menu-add ewm-menu-map ewm:pst-minor-mode-keymap)
    ))

;; (ewm:menu-define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Plugin Definition

;;; history-list / バッファ・履歴一覧
;;;--------------------------------------------------

(defun ewm:def-plugin-history-list (frame wm winfo)
  (let ((wname (wlf:window-name winfo))
        (buf (get-buffer " *WM:History*")))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create " *WM:History*"))
      (with-current-buffer buf
        (ewm:def-plugin-history-list-mode)
        (setq buffer-read-only t)
        (buffer-disable-undo buf)
        (hl-line-mode 1)))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (erase-buffer)
            (goto-char (point-min))
            (let ((history (ewm:history-get))
                  (history-backup (reverse (ewm:history-get-backup)))
                  (cnt 1))
              (loop for h in history-backup
                    for name = (if (stringp h) h (buffer-name h))
                    do (insert 
                        (ewm:tp (format
                                 "%3s %s %s\n" cnt name 
                                 (if (buffer-modified-p h) "*" ""))
                                'ewm:buffer h))
                    (incf cnt))
              (loop for h in history
                    for name = (if (stringp h) h (buffer-name h))
                    do (insert 
                        (ewm:tp (format 
                                 "%3s %s %s\n" cnt name 
                                 (if (buffer-modified-p h) "*" ""))
                                'ewm:buffer h))
                    (incf cnt))
              (goto-line (1+ (length history-backup)))
              (setq header-line-format
                    (format "Buffer History [%i]" (1- cnt))))
            (hl-line-highlight))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm wname buf)))

(defvar ewm:def-plugin-history-list-mode-map 
  (ewm:define-keymap 
   '(("k" . previous-line)
     ("j" . next-line)
     ("d" . ewm:def-plugin-history-list-kill-command)
     ("<SPC>" . ewm:def-plugin-history-list-show-command)
     ("C-m"   . ewm:def-plugin-history-list-select-command)
     ("q"     . ewm:pst-window-select-main-command)
     )))

(define-derived-mode ewm:def-plugin-history-list-mode fundamental-mode "History")

(defun ewm:def-plugin-history-list-kill-command ()
  (interactive)
  (when (ewm:managed-p)
    (save-excursion
      (beginning-of-line)
      (setq buf (get-text-property (point) 'ewm:buffer)))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf))))

(defun ewm:def-plugin-history-list-forward-command ()
  (interactive)
  (when (ewm:managed-p)
    (ewm:pst-history-forward-command)))

(defun ewm:def-plugin-history-list-back-command ()
  (interactive)
  (when (ewm:managed-p)
    (ewm:pst-history-back-command)))

(defun ewm:def-plugin-history-list-select-command ()
  (interactive)
  (when (ewm:managed-p)
    (ewm:def-plugin-history-list-show-command)
    (ewm:pst-window-select-main)))

(defun ewm:def-plugin-history-list-show-command ()
  (interactive)
  (when (ewm:managed-p)
    (save-excursion
      (beginning-of-line)
      (setq buf (get-text-property (point) 'ewm:buffer)))
    (when (and buf (buffer-live-p buf))
      (ewm:history-add buf)
      (ewm:pst-show-history-main))))

(ewm:plugin-register 'history-list 
                     "History List"
                     'ewm:def-plugin-history-list)

;;; dir-files / メインバッファの位置のファイル一覧
;;;--------------------------------------------------

;; 一番簡単なプラグイン
;; diredに丸投げ

(defun ewm:def-plugin-dir-files (frame wm winfo) 
  (let* ((buf (ewm:history-get-main-buffer))
         (dir (with-current-buffer buf 
                (or default-directory ".")))
         (dbuf (dired-noselect dir)))
    (with-current-buffer dbuf (revert-buffer))
    (wlf:set-buffer wm (wlf:window-name winfo) dbuf)))

(ewm:plugin-register 'dir-files 
                     "Dired"
                     'ewm:def-plugin-dir-files)

;;; imenu / Imenuで概要参照
;;;--------------------------------------------------

;; メインとプラグインの相互作用的なデモ
;; anything-config.el の imenu を参考に実装
;; （文字列で比較しているのがちょっとダサイ）

(defvar ewm:def-plugin-imenu-delimiter " / ")
(defvar ewm:def-plugin-imenu-cached-alist nil)
(make-variable-buffer-local 'ewm:def-plugin-imenu-cached-alist)
(defvar ewm:def-plugin-imenu-cached-entries nil)
(make-variable-buffer-local 'ewm:def-plugin-imenu-cached-entries)
(defvar ewm:def-plugin-imenu-cached-tick nil)
(make-variable-buffer-local 'ewm:def-plugin-imenu-cached-tick)

(defun ewm:def-plugin-imenu (frame wm winfo)
  (let ((entries (ewm:def-plugin-imenu-entries))
        (wname (wlf:window-name winfo))
        (buf (wlf:window-option-get winfo :buffer)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create " *WM:Imenu*"))
      (with-current-buffer buf
        (ewm:def-plugin-imenu-mode)
        (setq buffer-read-only t)
        (buffer-disable-undo buf)
        (hl-line-mode 1))
      (ewm:def-plugin-imenu-start-timer))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (erase-buffer)
            (goto-char (point-min))
              (loop for i in entries
                    do (insert (format "%s\n" i)))
              (setq header-line-format
                    (format "Imenu entries [%i]" (length entries)))
            (hl-line-highlight))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm wname buf)))

(defun ewm:def-plugin-imenu-entries ()
  (with-current-buffer (ewm:history-get-main-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (and (eq ewm:def-plugin-imenu-cached-tick tick)
               ewm:def-plugin-imenu-cached-entries)
          ewm:def-plugin-imenu-cached-entries
        (setq imenu--index-alist nil)
        (setq ewm:def-plugin-imenu-cached-tick tick
              ewm:def-plugin-imenu-cached-entries
              (condition-case nil
                  (mapcan
                   'ewm:def-plugin-imenu-create-entries
                   (setq ewm:def-plugin-imenu-cached-alist (imenu--make-index-alist)))
                (error nil)))
        (setq ewm:def-plugin-imenu-cached-entries
              (loop for x in ewm:def-plugin-imenu-cached-entries
                    collect (if (stringp x) x (car x))))))))

(defun ewm:def-plugin-imenu-create-entries (entry)
  (if (listp (cdr entry))
      (mapcan 
       (lambda (sub)
         (if (consp (cdr sub))
             (mapcar
              (lambda (subentry)
                (concat (car entry) ewm:def-plugin-imenu-delimiter subentry))
              (ewm:def-plugin-imenu-create-entries sub))
           (list (concat (car entry) ewm:def-plugin-imenu-delimiter (car sub)))))
       (cdr entry))
    (list entry)))
  
(setq imenu-default-goto-function 'imenu-default-goto-function)

(defun ewm:def-plugin-imenu-jump (elm)
  (let ((path (split-string elm ewm:def-plugin-imenu-delimiter))
        (alist ewm:def-plugin-imenu-cached-alist))
    (if (> (length path) 1)
        (progn
          (setq alist (assoc (car path) alist))
          (setq elm (cadr path))
          (imenu (assoc elm alist)))
      (imenu (assoc elm alist)))))

(defun ewm:def-plugin-imenu-jump-command ()
  (interactive)
  (let ((elm (ewm:string-trim (thing-at-point 'line))))
    (select-window (get-buffer-window (ewm:history-get-main-buffer)))
    (ewm:def-plugin-imenu-jump elm)))

(defun ewm:def-plugin-imenu-show-command ()
  (interactive)
  (let ((elm (ewm:string-trim (thing-at-point 'line)))
        (cwin (selected-window)))
    (select-window (get-buffer-window (ewm:history-get-main-buffer)))
    (ewm:def-plugin-imenu-jump elm)
    (select-window cwin)))

(defvar ewm:def-plugin-imenu-mode-map 
  (ewm:define-keymap 
   '(("C-m" . ewm:def-plugin-imenu-jump-command)
     ("j" . next-line)
     ("k" . previous-line)
     ("q" . ewm:pst-window-select-main-command)
     ("<SPC>" . ewm:def-plugin-imenu-show-command)
     )))

(define-derived-mode ewm:def-plugin-imenu-mode fundamental-mode "Imenu")

(defun ewm:def-plugin-imenu-which-func ()
  ;; which-func-modes から拝借
  (let ((alist ewm:def-plugin-imenu-cached-alist)
        (minoffset (point-max)) name
        offset pair mark imstack namestack)
    (while (or alist imstack)
      (if alist
          (progn
            (setq pair (car-safe alist)
                  alist (cdr-safe alist))
            (cond ((atom pair))
                  ((imenu--subalist-p pair)
                   (setq imstack   (cons alist imstack)
                         namestack (cons (car pair) namestack)
                         alist     (cdr pair)))
                  ((number-or-marker-p (setq mark (cdr pair)))
                   (if (>= (setq offset (- (point) mark)) 0)
                       (if (< offset minoffset)
                           (setq minoffset offset
                                 name (mapconcat 
                                       'identity
                                       (reverse (cons 
                                                 (car pair)
                                                 namestack)) 
                                       ewm:def-plugin-imenu-delimiter)))))))
        (setq alist     (car imstack)
              namestack (cdr namestack)
              imstack   (cdr imstack))))
    name))

(defvar ewm:def-plugin-imenu-timer nil)

(defun ewm:def-plugin-imenu-start-timer ()
  (interactive)
  (unless ewm:def-plugin-imenu-timer
    (setq ewm:def-plugin-imenu-timer
          (run-with-idle-timer 
           idle-update-delay t 
           'ewm:def-plugin-imenu-update-which-func))
    (ewm:message "Imenu timer started.")))

(defun ewm:def-plugin-imenu-stop-timer ()
  (interactive)
  (when (timerp ewm:def-plugin-imenu-timer)
    (cancel-timer ewm:def-plugin-imenu-timer))
  (setq ewm:def-plugin-imenu-timer nil)
  (ewm:message "Imenu timer stopped."))

(defun ewm:def-plugin-imenu-update-which-func ()
  (ewm:with-advice
   (let* ((main-buf (ewm:history-get-main-buffer))
          (win (selected-window))
          (imenu-buf (get-buffer " *WM:Imenu*"))
          (imenu-win (and imenu-buf (get-buffer-window imenu-buf))))
     (cond
      ((null imenu-buf)
       (ewm:def-plugin-imenu-stop-timer))
      ((eql win (get-buffer-window main-buf))
       (let ((name (ewm:def-plugin-imenu-which-func)))
         (when (and name (window-live-p imenu-win))
           (with-selected-window imenu-win
             (goto-char (point-min))
             (let ((ps (re-search-forward (concat "^" name "$"))))
               (when ps
                 (goto-char ps)
                 (beginning-of-line)
                 (hl-line-highlight)))))))
      (t
       ;;can not update
       )))))

(ewm:plugin-register 'imenu
                     "Outline"
                     'ewm:def-plugin-imenu)

;;; top / topでマシン状態表示
;;;--------------------------------------------------

;; 自動更新のデモ

(defvar ewm:def-plugin-top-buffer-name " *WM:Top*" "[internal use]")
(defvar ewm:def-plugin-top-timer-handle nil "[internal use]")
(defvar ewm:def-plugin-top-timer-interval 20 "Seconds for update.")

(defun ewm:def-plugin-top (frame wm winfo)
  ;;bufferが生きていればバッファを表示するだけ（タイマーに任せる）
  ;;bufferが無ければ初回更新してタイマー開始する
  (let ((buf (get-buffer ewm:def-plugin-top-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (ewm:def-plugin-top-update)))
    (unless ewm:def-plugin-top-timer-handle
      (setq ewm:def-plugin-top-timer-handle
            (run-at-time
             ewm:def-plugin-top-timer-interval
             ewm:def-plugin-top-timer-interval
             'ewm:def-plugin-top-timer))
      (ewm:message "WM: 'top' update timer started."))
    (wlf:set-buffer wm (wlf:window-name winfo) buf)))

(defun ewm:def-plugin-top-timer ()
  ;;bufferが死んでいれば、タイマー停止
  ;;bufferが生きていれば更新実行
  (let ((buf (get-buffer ewm:def-plugin-top-buffer-name)))
    (if (and (ewm:managed-p) buf (buffer-live-p buf))
        (when (= 0 (minibuffer-depth))
          (ewm:def-plugin-top-update))
      (when ewm:def-plugin-top-timer-handle
          (cancel-timer ewm:def-plugin-top-timer-handle)
          (setq ewm:def-plugin-top-timer-handle nil)
          (ewm:message "WM: 'top' update timer stopped.")))))

(defun ewm:def-plugin-top-update ()
  (lexical-let ((buf (get-buffer ewm:def-plugin-top-buffer-name))
                (tmpbuf (get-buffer-create " *WM:Top-temp*")))
    (buffer-disable-undo tmpbuf)
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create ewm:def-plugin-top-buffer-name))
      (with-current-buffer buf
        (buffer-disable-undo buf)))
    (let ((proc (start-process "WM:top" tmpbuf "top" "-b -n 1")))
      (set-process-sentinel
       proc (lambda(proc event)
              (if (equal event "finished\n")
                  (with-current-buffer buf
                    (erase-buffer)
                    (goto-char (point-min))
                    (insert-buffer-substring tmpbuf)
                    (kill-buffer tmpbuf))))))
    buf))

(ewm:plugin-register 'top 
                     "Top (System Stat)"
                     'ewm:def-plugin-top)

;;; history-nth / 履歴のN番目を表示
;;;--------------------------------------------------

;; 順番については以下のようにオプションで指定
;; 1でメインと同じ。2で1つ前（デフォルト）。
;; 例 (:name window-name :plugin history-nth :plugin-args 2)

(defun ewm:def-plugin-history-nth (frame wm winfo)
  (let* ((index (1- (or (wlf:window-option-get winfo :plugin-args) 2)))
         (buf (or (nth index (ewm:history-get)) 
                  (ewm:history-get-main-buffer))))
    (when buf
      (wlf:set-buffer wm (wlf:window-name winfo) buf))))

(ewm:plugin-register 'history-nth 
                     "History Back Buffer"
                     'ewm:def-plugin-history-nth)

;;; main-prev / ひとつ前にメインに表示していたバッファを表示
;;;--------------------------------------------------

;; history-nthのとの違いは、undoキューがあればそっちの頭を表示する。

(defun ewm:def-plugin-main-prev (frame wm winfo)
  (let* ((buf (or (car (ewm:history-get-backup))
                  (nth 1 (ewm:history-get)) 
                  (ewm:history-get-main-buffer))))
    (when buf
      (wlf:set-buffer wm (wlf:window-name winfo) buf))))

(ewm:plugin-register 'main-prev 
                     "Previous Buffer"
                     'ewm:def-plugin-main-prev)

;;; clock / 時計
;;;--------------------------------------------------

(defvar ewm:def-plugin-clock-timer-interval 60 "Seconds for update.")
(defvar ewm:def-plugin-clock-url "http://www.bijint.com/jp/img/clk/%H%M.jpg" "URL pattern.")
(defvar ewm:def-plugin-clock-referer "http://www.bijint.com/jp/" "referer URL")

(defvar ewm:def-plugin-clock-buffer-name " *WM:Clock*" "[internal use]")
(defvar ewm:def-plugin-clock-timer-handle nil "[internal use]")
(defvar ewm:def-plugin-clock-window nil "[internal use] Display window.") ; 表示するウインドウは1つであることを仮定（サイズ取得のため）
(defvar ewm:def-plugin-clock-download-file "/tmp/wmclock.jpg"  "[internal]")
(defvar ewm:def-plugin-clock-resized-file  "/tmp/wmclockt.jpg" "[internal]")
;;↑cygwin環境の場合は "C:/cygwin/tmp/wmclock.jpg" とかにすると良いかも

(defun ewm:def-plugin-clock (frame wm winfo)
  ;;bufferが生きていればバッファを表示するだけ（タイマーに任せる）
  ;;bufferが無ければ初回更新してタイマー開始する
  (let ((buf (get-buffer ewm:def-plugin-clock-buffer-name)))
    (setq ewm:def-plugin-clock-window 
          (wlf:get-window wm (wlf:window-name winfo)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (ewm:def-plugin-clock-update)))
    (unless ewm:def-plugin-clock-timer-handle
      (setq ewm:def-plugin-clock-timer-handle
            (run-at-time
             ewm:def-plugin-clock-timer-interval
             ewm:def-plugin-clock-timer-interval
             'ewm:def-plugin-clock-timer))
      (ewm:message "WM: 'clock' update timer started."))
    (wlf:set-buffer wm (wlf:window-name winfo) buf)))

(defun ewm:def-plugin-clock-timer ()
  ;;bufferが死んでいれば、タイマー停止
  ;;bufferが生きていれば更新実行
  (let ((buf (get-buffer ewm:def-plugin-clock-buffer-name)))
    (if (and (ewm:managed-p) buf (buffer-live-p buf) 
             (get-buffer-window buf))
        (when (= 0 (minibuffer-depth))
          (ewm:def-plugin-clock-update))
      (when ewm:def-plugin-clock-timer-handle
          (cancel-timer ewm:def-plugin-clock-timer-handle)
          (setq ewm:def-plugin-clock-timer-handle nil)
          (when buf (kill-buffer buf))
          (ewm:message "WM: 'clock' update timer stopped.")))))

(defun ewm:def-plugin-clock-update ()
  (let ((buf (get-buffer ewm:def-plugin-clock-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create ewm:def-plugin-clock-buffer-name))
      (with-current-buffer buf
        (buffer-disable-undo buf)))
    (ewm:def-plugin-clock-download)
    buf))

(defun ewm:def-plugin-clock-download ()
  (lexical-let ((tmpbuf (get-buffer-create " *WM:Clock-temp*")))
    (buffer-disable-undo tmpbuf)
    (let* ((url (format-time-string 
                 ewm:def-plugin-clock-url
                  (current-time)))
           (proc (start-process
                  "WM:clockw" tmpbuf "wget"
                  (concat "--referer=" ewm:def-plugin-clock-referer)
                  "-q" "-O" ewm:def-plugin-clock-download-file url)))
      (set-process-sentinel
       proc (lambda(proc event)
              (cond 
               ((string-match "exited abnormally" event)
                (kill-buffer tmpbuf)
                (ewm:def-plugin-clock-show-text "No network connection."))
               ((equal event "finished\n")
                (kill-buffer tmpbuf)
                (let ((f ewm:def-plugin-clock-download-file))
                  (if (and (file-exists-p f)
                           (< 0 (nth 7 (file-attributes f))))
                      (ewm:def-plugin-clock-resize)
                    (ewm:def-plugin-clock-show-text "No network connection."))))))))))

(defun ewm:def-plugin-clock-resize ()
  (lexical-let* 
      ((tmpbuf (get-buffer-create " *WM:Clock-temp*")) 
       (window ewm:def-plugin-clock-window)
       (w (* (window-width window) (frame-char-width)))
       (h (* (- (window-height window) 1) (frame-char-height)))
       (proc
        (start-process "WM:clockc" tmpbuf "convert" 
                       "-resize" 
                       (format "%ix%i" w h)
                       ewm:def-plugin-clock-download-file 
                       (concat "jpeg:" ewm:def-plugin-clock-resized-file))))
    (set-process-sentinel
     proc (lambda (proc event)
            (cond
               ((string-match "exited abnormally" event)
                (kill-buffer tmpbuf)
                (ewm:def-plugin-clock-show-text "Could not convert."))
               ((equal event "finished\n")
                (kill-buffer tmpbuf)
                (let ((f ewm:def-plugin-clock-resized-file))
                  (if (and (file-exists-p f)
                           (< 0 (nth 7 (file-attributes f))))
                      (ewm:def-plugin-clock-show-image)
                    (ewm:def-plugin-clock-show-text "Could not convert."))))
              )))))

(defun ewm:def-plugin-clock-show-image ()
  (clear-image-cache)
  (let ((buf (get-buffer ewm:def-plugin-clock-buffer-name))
        (img (create-image ewm:def-plugin-clock-resized-file 'jpeg))
        (map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ewm:def-plugin-clock-onclick)
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      (insert "clock image")
      (add-text-properties 
       (point-min) (point-max)
       (list 'display img 'keymap map 'mouse-face 'highlight)))))

(defun ewm:def-plugin-clock-onclick ()
  (interactive)
  (browse-url ewm:def-plugin-clock-referer))

(defun ewm:def-plugin-clock-show-text (&optional text)
  (let ((buf (get-buffer ewm:def-plugin-clock-buffer-name))
        (time (current-time)))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      (when text
        (insert 
         (ewm:rt-format "Status: %s\n" text)))
      (insert 
       (ewm:rt-format
        "\nSystem: %s\nLoad Average: %s\n\n"
        (system-name) (apply 'format "%.2f, %.2f, %.2f" (load-average t))))
      (insert 
       (ewm:rt-format
        "Date: %s\nTime: %s\n"
        (format-time-string "%Y/%m/%d" time)
        (cons (format-time-string "%H:%M" time) 'ewm:face-title)))
    )))

(ewm:plugin-register 'clock 
                     "Fancy Clock"
                     'ewm:def-plugin-clock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Perspective Definition

;;; code / Code editing perspective
;;;--------------------------------------------------

(defvar ewm:c-code-recipe
  '(| (:left-max-size 35)
      (- (:upper-size-ratio 0.7)
         files history)
      (- (:upper-size-ratio 0.7)
         (| (:right-max-size 30)
            main imenu)
         sub)))

(defvar ewm:c-code-winfo
  '((:name main)
    (:name files :plugin dir-files)
    (:name history :plugin history-list)
    (:name sub :buffer "*info*" :default-hide t)
    (:name imenu :plugin imenu :default-hide nil))
  )

(defvar ewm:c-code-show-main-regexp
   "\\*\\(vc-diff\\)\\*")

(ewm:pst-class-register 
  (make-ewm:$pst-class
   :name   'code
   :title  "Coding"
   :init   'ewm:dp-code-init
   :main   'main
   :switch 'ewm:dp-code-switch
   :popup  'ewm:dp-code-popup
   :keymap 'ewm:dp-code-minor-mode-map))

(defun ewm:dp-code-init ()
  (let* 
      ((code-wm 
        (wlf:no-layout 
         ewm:c-code-recipe
         ewm:c-code-winfo))
       (buf (or prev-selected-buffer
                (ewm:history-get-main-buffer))))

    (when (ewm:history-recordable-p prev-selected-buffer)
      (ewm:history-add prev-selected-buffer))
    
    (wlf:set-buffer code-wm 'main buf)
    code-wm))

(defun ewm:dp-code-switch (buf)
  (ewm:message "#DP CODE switch : %s" buf)
  (if (ewm:history-recordable-p buf)
      (ewm:pst-show-history-main)
    nil))

(defun ewm:dp-code-popup (buf)
  ;;とりあえず全部subで表示してみる
  (let ((cb (current-buffer)))
    (ewm:message "#DP CODE popup : %s (current %s / backup %s)" 
                 buf cb ewm:override-window-cfg-backup))
  (let ((buf-name (buffer-name buf))
        (wm (ewm:pst-get-wm)))
    (cond
     ((ewm:history-recordable-p buf)
      (ewm:pst-show-history-main)
      ;;記録対象なら履歴に残るのでupdateで表示を更新させる
      t)
     ((and ewm:override-window-cfg-backup
       (eq (selected-window) (wlf:get-window wm 'sub)))
      ;;現在subならmainに表示しようとする
      ;;minibuffer以外の補完バッファは動きが特殊なのでbackupをnilにする
      (setq ewm:override-window-cfg-backup nil)
      ;;一時的に表示するためにset-window-bufferを使う
      ;;(prefix) C-lなどで元のバッファに戻すため
      (set-window-buffer (wlf:get-window wm 'main) buf)
      t)
     ((and ewm:c-code-show-main-regexp
           (string-match ewm:c-code-show-main-regexp buf-name))
      (wlf:set-buffer wm 'main buf t)
      t)
     (t
      (ewm:dp-code-popup-sub buf)
      t))))

(defun ewm:dp-code-popup-sub (buf)
  (let ((wm (ewm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (ewm:with-advice
     (wlf:show wm 'sub)
     (wlf:set-buffer wm 'sub buf not-minibufp))))

;; Commands / Keybindings

(defun ewm:dp-code ()
  (interactive)
  (ewm:pst-change 'code))

(defun ewm:dp-code-imenu-toggle-command ()
  (interactive)
  (wlf:toggle (ewm:pst-get-wm) 'imenu)
  (ewm:pst-update-windows))
(defun ewm:dp-code-sub-toggle-command ()
  (interactive)
  (wlf:toggle (ewm:pst-get-wm) 'sub)
  (ewm:pst-update-windows))
(defun ewm:dp-code-navi-main-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'main))
(defun ewm:dp-code-navi-files-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'files))
(defun ewm:dp-code-navi-history-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'history))
(defun ewm:dp-code-navi-imenu-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'imenu))
(defun ewm:dp-code-navi-sub-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'sub))
(defun ewm:dp-code-main-maximize-toggle-command ()
  (interactive)
  (wlf:toggle-maximize (ewm:pst-get-wm) 'main)
  (ewm:pst-update-windows))

(defun ewm:dp-code-toggle-clock-command ()
  (interactive)
  (let* ((wm (ewm:pst-get-wm))
         (prev (ewm:pst-window-plugin-get wm 'history))
         (next (if (eq prev 'history-list)
                   'clock 'history-list)))
    (ewm:pst-window-plugin-set wm 'history next)
    (ewm:pst-update-windows)))

(defvar ewm:dp-code-minor-mode-map 
      (ewm:define-keymap
       '(("prefix I" . ewm:dp-code-imenu-toggle-command)
         ("prefix S" . ewm:dp-code-sub-toggle-command)
         ("prefix C" . ewm:dp-code-toggle-clock-command)
         ("prefix M" . ewm:dp-code-main-maximize-toggle-command))
       ewm:prefix-key))

;;; document / Document view perspective
;;;--------------------------------------------------

(defvar ewm:c-doc-recipe
      '(- (:upper-size-ratio 0.75)
        (| left right)
        sub))

(defvar ewm:c-doc-winfo
      '((:name left)
        (:name right)
        (:name sub :default-hide t)))

(setq ewm:c-doc-show-main-func
      (lambda (buf)
        (string-match "\\*\\(Help\\|info\\|w3m\\)\\*" buf)))

(ewm:pst-class-register 
  (make-ewm:$pst-class
   :name   'doc
   :init   'ewm:dp-doc-init
   :title  "Document"
   :main   'left
   :update 'ewm:dp-doc-update
   :switch 'ewm:dp-doc-switch
   :popup  'ewm:dp-doc-popup
   :leave  'ewm:dp-doc-leave
   :keymap 'ewm:dp-doc-minor-mode-map))

(defun ewm:dp-doc-set-doc-buffer (buf)
  (ewm:frame-param-set 'ewm:dp-doc-buffer buf))

(defun ewm:dp-doc-get-doc-buffer ()
  (ewm:frame-param-get 'ewm:dp-doc-buffer))

(defun ewm:dp-doc-init ()
  (let* 
      ((doc-wm 
        (wlf:no-layout 
         ewm:c-doc-recipe
         ewm:c-doc-winfo))
       (buf (ewm:dp-doc-get-doc-buffer)))

    (unless (and buf (buffer-live-p buf))
      (setq buf (or prev-selected-buffer
                    (ewm:history-get-main-buffer))))
    
    (wlf:set-buffer doc-wm 'left buf)
    (with-current-buffer buf
      (follow-mode 1))
    doc-wm))

(defun ewm:dp-doc-update (wm)
  (ewm:message "#DP DOC update")
  ;;左右を同じにする
  (let ((leftbuf  (wlf:get-buffer wm 'left))
        (rightbuf (wlf:get-buffer wm 'right)))
    (unless (eql leftbuf rightbuf)
        (with-current-buffer leftbuf
          (follow-mode 1))
        (wlf:set-buffer wm 'right leftbuf))))

(defun ewm:dp-doc-set-main-buffer (buf)
  (let ((wm (ewm:pst-get-wm)))
    (with-current-buffer buf
      (follow-mode 1))
    (wlf:set-buffer wm 'left buf)
    (wlf:set-buffer wm 'right buf)))

(defun ewm:dp-doc-switch (buf)
  ;;left,rightでswitch-to-bufferが起きたら、乗っ取って両方に表示する。
  (ewm:message "#DP DOC switch : %s" buf)
  (let ((wm (ewm:pst-get-wm))
        (curwin (selected-window)))
    (if (or (eql curwin (wlf:get-window wm 'left))
            (eql curwin (wlf:get-window wm 'right)))
        (progn 
          (ewm:dp-doc-set-main-buffer buf)
          t)
      nil)))

(defun ewm:dp-doc-popup (buf)
  (ewm:message "#DP DOC popup : %s" buf)
  (let ((buf-name (buffer-name buf)))
    (cond
     ((or (funcall ewm:c-doc-show-main-func buf-name)
          (ewm:history-recordable-p buf))
      (ewm:dp-doc-set-main-buffer buf)
      t)
     (t
      (ewm:dp-doc-popup-sub buf)
      t))))

(defun ewm:dp-doc-popup-sub (buf)
  (let ((wm (ewm:pst-get-wm)))
    (ewm:with-advice
     (wlf:show wm 'sub)
     (wlf:set-buffer wm 'sub buf))))

(defun ewm:dp-doc-leave (wm)
  (let ((buf (get-buffer (wlf:get-buffer wm 'left))))
    (when (and buf (buffer-live-p buf))
      (unless (ewm:history-recordable-p buf) ; ドキュメント的バッファだったら
        (ewm:dp-doc-set-doc-buffer buf)      ; あとで再表示できるようにして、
        (setq prev-selected-buffer nil))))   ; 次のパースペクティブは履歴から持ってきてもらう
  (loop for b in (buffer-list)
        do (with-current-buffer b
             (follow-mode -1))))

;; Commands / Keybindings

(defun ewm:dp-doc ()
  (interactive)
  (ewm:pst-change 'doc))

(defun ewm:dp-doc-navi-main-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'left))
(defun ewm:dp-doc-navi-sub-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'sub))
(defun ewm:dp-doc-sub-toggle-command ()
  (interactive)
  (wlf:toggle (ewm:pst-get-wm) 'sub)
  (ewm:pst-update-windows))
(defun ewm:dp-doc-main-maximize-toggle-command ()
  (interactive)
  (wlf:toggle-maximize (ewm:pst-get-wm) 'left)
  (ewm:pst-update-windows))

(defvar ewm:dp-doc-minor-mode-map 
      (ewm:define-keymap
       '(("prefix m" . ewm:dp-doc-navi-main-command)
         ("prefix s" . ewm:dp-doc-navi-sub-command)
         ("prefix S" . ewm:dp-doc-sub-toggle-command)
         ("prefix M" . ewm:dp-doc-main-maximize-toggle-command)
         ("prefix I" . info))
       ewm:prefix-key))

;;; two / Two column editing perspective
;;;--------------------------------------------------

(defvar ewm:c-two-recipe
      '(- (:upper-size-ratio 0.8)
          (| left
             (- (:upper-size-ratio 0.9)
                right history))
          sub))

(defvar ewm:c-two-winfo
      '((:name left )
        (:name right :plugin main-prev)
        (:name sub :buffer "*Help*" :default-hide t)
        (:name history :plugin history-list :default-hide nil)))

(defvar ewm:c-two-show-side-regexp "\\*\\(Help\\|info\\|w3m\\)\\*")

(ewm:pst-class-register
  (make-ewm:$pst-class
   :name   'two
   :title  "Two Columns"
   :init   'ewm:dp-two-init
   :main   'left
   :switch 'ewm:dp-two-switch
   :popup  'ewm:dp-two-popup
   :keymap 'ewm:dp-two-minor-mode-map))

(defun ewm:dp-two-init ()
  (let* 
      ((two-wm 
        (wlf:no-layout 
         ewm:c-two-recipe
         ewm:c-two-winfo))
       (buf (or prev-selected-buffer
                (ewm:history-get-main-buffer))))

    (when (ewm:history-recordable-p prev-selected-buffer)
      (ewm:history-add prev-selected-buffer))
    
    (wlf:set-buffer two-wm 'left buf)

    two-wm))

(defun ewm:dp-two-switch (buf)
  (ewm:message "#DP TWO switch : %s" buf)
  (let ((wm (ewm:pst-get-wm))
        (curwin (selected-window)))
    (if (or (eql curwin (wlf:get-window wm 'left))
            (eql curwin (wlf:get-window wm 'right)))
        ;;メイン画面の場合
        (cond 
         ((eql (get-buffer buf) (wlf:get-buffer wm 'left))
          ;;メインと同じなら並べる
          (ewm:pst-update-windows)
          (wlf:set-buffer wm 'right buf)
          t)
         ((ewm:history-recordable-p buf)
          ;;普通の編集対象なら履歴につっこんで更新
          (ewm:pst-show-history-main)
          t)
         (t 
          ;;それ以外ならとりあえず表示してみる
          nil))
      nil)))

(defun ewm:dp-two-popup (buf)
  ;;記録バッファ以外はsubで表示してみる
  (ewm:message "#DP TWO popup : %s" buf)
  (let ((buf-name (buffer-name buf)))
    (cond
     ((string-match ewm:c-two-show-side-regexp buf-name)
      (wlf:set-buffer (ewm:pst-get-wm) 'right buf)
      t)
     ((ewm:history-recordable-p buf)
      (ewm:pst-show-history-main)
      t)
     (t
      (ewm:dp-two-popup-sub buf)
      t))))

(defun ewm:dp-two-popup-sub (buf)
  (let ((wm (ewm:pst-get-wm)))
    (ewm:with-advice
     (wlf:show wm 'sub)
     (wlf:set-buffer wm 'sub buf))))

;; Commands / Keybindings

(defun ewm:dp-two ()
  (interactive)
  (ewm:pst-change 'two))

(defun ewm:dp-two-navi-left-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'left))
(defun ewm:dp-two-navi-right-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'right))
(defun ewm:dp-two-navi-sub-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'sub))
(defun ewm:dp-two-navi-history-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'history))

(defun ewm:dp-two-history-toggle-command ()
  (interactive)
  (wlf:toggle (ewm:pst-get-wm) 'history)
  (ewm:pst-update-windows))
(defun ewm:dp-two-sub-toggle-command ()
  (interactive)
  (wlf:toggle (ewm:pst-get-wm) 'sub)
  (ewm:pst-update-windows))

(defun ewm:dp-two-double-column-command ()
  (interactive)
  (wlf:set-buffer (ewm:pst-get-wm) 'right (ewm:history-get-main-buffer)))

(defun ewm:dp-two-main-maximize-toggle-command ()
  (interactive)
  (wlf:toggle-maximize (ewm:pst-get-wm) 'left)
  (ewm:pst-update-windows))

(defvar ewm:dp-two-minor-mode-map 
      (ewm:define-keymap
       '(("prefix d" . ewm:dp-two-double-column-command)
         ("prefix S" . ewm:dp-two-sub-toggle-command)
         ("prefix H" . ewm:dp-two-history-toggle-command)
         ("prefix M" . ewm:dp-two-main-maximize-toggle-command))
       ewm:prefix-key))

;;; dashboard / dashboard buffers perspective
;;;--------------------------------------------------


;;; array / arrange buffers perspective
;;;--------------------------------------------------

(defvar ewm:c-array-font-decrease 3) ; フォントのを小さくする相対サイズ
(defvar ewm:c-array-max-rows 4)  ; 並べる横最大数
(defvar ewm:c-array-max-cols 5)  ; 並べる縦最大数

(defvar ewm:c-array-more-buffers-pred
  (lambda (b)
    (let ((bn (buffer-name b)))
      (and 
       (not  ; 表示しないもの
        (memq (buffer-local-value 'major-mode b)  
              '(dired-mode)))
       (or
        (not (string-match "^ ?\\*" bn)) ; 内部バッファは表示しないが、
        (string-match ; 以下のものは表示する
         "\\*\\(Help\\|info\\|scratch\\|w3m\\|Messages\\)\\*" 
         bn))))))
      
(defun ewm:dp-array-make-recipe (cols rows)
  ;; cols x rows の recipe を作る
  (let* ((sz-summary 0.12)
         (sz-array (- 1.0 sz-summary)))
    (decf cols) (decf rows)
    (labels
        ((loop-rows
          (cols rows)
          (loop for i from rows downto 1
                with ret = nil
                if (< i rows)
                do (setq ret 
                         (list 
                          '- (list :upper-size-ratio 
                                   (* sz-array (/ 1.0 (- rows (- i 1.5)))))
                          (loop-cols cols i) ret))
                else
                do (setq ret 
                         (list '- (loop-cols cols i)
                               (loop-cols cols (1+ i))))
                finally return ret))
         (loop-cols
          (cols y)
          (loop for i from cols downto 1
                with ret = nil
                if (< i cols)
                do (setq ret (list 
                              '| (list :left-size-ratio
                                       (/ 1.0 (- cols (- i 2))))
                              (mk i y) ret))
                else
                do (setq ret (list '| (mk i y) (mk (1+ i) y)))
                finally return ret))
         (mk (x y) (intern (format "w-%i-%i" x y))))
      (let ((ar (cond
                 ((and (eql cols 0) (eql rows 0))
                  'w-1-1)
                 ((< rows 1)
                  (loop-cols 1 1))
                 (t
                  (loop-rows cols rows)))))
        (list '- (list ':upper-size-ratio sz-array) ar 'summary)))))

(defun ewm:dp-array-make-winfo (cols rows)
  ;; cols x rows の winfo を作る
  (labels
      ((loop-rows (cols rows)
                  (loop for i from 1 to rows
                        with ret = nil
                        do (setq ret (nconc ret (loop-cols cols i)))
                        finally return ret))
       (loop-cols (cols y)
                  (loop for i from 1 to cols
                        with ret = nil
                        do (setq ret (nconc ret (list (mk i y))))
                        finally return ret))
       (mk (x y) (list ':name (intern (format "w-%i-%i" x y)))))
    (let ((ar (loop-rows cols rows)))
      (add-to-list 'ar '(:name summary) t))))

(defun ewm:dp-array-calculate-size (num)
  ;;num個のサイズが入る縦横幅を計算する
  ;;なるべく縦横の数が離れすぎないような数を探す
  (labels
      ((loop-rows (cols rows)
                  (loop for i from 1 to rows
                        for ret = (loop-cols i i cols)
                        if ret
                        return ret
                        finally return (cons cols rows)))
       (loop-cols (cols y mx)
                  (loop for i from y to (min (1+ y) mx)
                        if (<= num (* i y))
                        return (cons i y)
                        finally return nil)))
    (loop-rows ewm:c-array-max-cols 
               ewm:c-array-max-rows)))

(defun ewm:dp-array-make-wm (buffers)
  (let* ((size (ewm:dp-array-calculate-size (length buffers)))
         (w (car size)) (h (cdr size))
         (recipe (ewm:dp-array-make-recipe w h))
         (wparams (ewm:dp-array-make-winfo w h))
         (wm (wlf:no-layout recipe wparams)))
    (ewm:dp-array-arrange-buffers wm buffers)
    wm))

(ewm:pst-class-register 
 (make-ewm:$pst-class
     :name   'array
     :title  "Buffer Array"
     :init   'ewm:dp-array-init
     :start  'ewm:dp-array-start
     :leave  'ewm:dp-array-leave
     :keymap 'ewm:dp-array-minor-mode-map))

(defun ewm:dp-array-init ()
  (let* 
      ((array-wm (ewm:dp-array-make-wm 
                  (funcall ewm:dp-array-buffers-function))))
    array-wm))

(defun ewm:dp-array-start (wm)
  (ewm:message "#ARRAY START")
  (ewm:dp-array-decrease-fontsize)
  (ewm:dp-array-update-summary))

(defun ewm:dp-array-arrange-buffers (wm buffers)
  (loop for winfo in (wlf:wset-winfo-list wm)
        with cnt = 0
        for opt = (wlf:window-options winfo)
        do (plist-put 
            opt ':buffer
            (ewm:aif (nth cnt buffers) 
                it ewm:c-blank-buffer))
        (incf cnt))
  wm)

(defvar ewm:dp-array-buffers-function
  'ewm:dp-array-get-recordable-buffers) ; この関数を切り替える

(defun ewm:dp-array-get-recordable-buffers ()
  ;;履歴に記録しそうなもの一覧
  (let ((ret 
         (append 
          (reverse (ewm:history-get))
          (copy-list (ewm:history-get-backup))
          )))
    (loop for b in (buffer-list)
          if (and (ewm:history-recordable-p b)
                  (not (member b ret)))
          do (push b ret))
    (nreverse ret)))

(defun ewm:dp-array-get-more-buffers ()
  ;;表示して意味がありそうなもの
  (let ((ret 
         (append 
          (reverse (ewm:history-get))
          (copy-list (ewm:history-get-backup))
          )))
    (loop for b in (buffer-list)
          if (and (funcall ewm:c-array-more-buffers-pred b)
                  (not (member b ret)))
          do (push b ret))
    (nreverse ret)))

(defun ewm:dp-array-leave (wm)
  (ewm:message "#ARRAY LEAVE")
  (when ewm:dp-array-overlay-focus
    (delete-overlay ewm:dp-array-overlay-focus))
  (ewm:dp-array-increase-fontsize))

(defun ewm:dp-array-decrease-fontsize ()
  (when (fboundp 'text-scale-decrease)
    (loop for b in (buffer-list)
          if (not (minibufferp b))
          do (with-current-buffer b
               (text-scale-decrease ewm:c-array-font-decrease)))))

(defun ewm:dp-array-increase-fontsize ()
  (when (fboundp 'text-scale-increase)
    (loop for b in (buffer-list)
          if (not (minibufferp b))
          do (with-current-buffer b
               (text-scale-increase ewm:c-array-font-decrease)))))

(defun ewm:dp-array-update-summary ()
  (let* ((bufname " *WM:ArraySummary*")
         (buf (get-buffer bufname))
         (wm (ewm:pst-get-wm))
         (win (wlf:get-window wm 'summary))
         (selected-buf (window-buffer (selected-window))))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create bufname))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (buffer-disable-undo buf)))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (erase-buffer)
            (goto-char (point-min))
            (unless (eql selected-buf buf)
              (ewm:dp-array-insert-summary-info
               selected-buf))
        (setq buffer-read-only t))))
    (wlf:set-buffer wm 'summary buf))
  (ewm:dp-array-hilite-focus))

(defun ewm:dp-array-insert-summary-info (selected-buf)
  (cond
   ((buffer-local-value 'buffer-file-name selected-buf)
    (let* ((f (buffer-local-value 'buffer-file-name selected-buf))
           (filename (file-name-nondirectory f))
           (dir (file-name-directory f))
           (mode (file-modes f))
           (attr (file-attributes f))
           (modified-time (nth 5 attr))
           (size (nth 7 attr))
           (strsize 
            (cond ((> size (* 1048576 4))
                   (format "%i Mb" (round (/ size 1048576))))
                  ((> size (* 1024 4))
                   (format "%i Kb" (round (/ size 1024))))
                  (t
                   (format "%i bytes" size)))))
      (insert
       (ewm:rt-format "File Name: %s  (Path: %s)\n"
               (cons  filename 'ewm:face-title) dir)
       (ewm:rt-format "Mode: %s  /  Modified Time: %s\n"
               (format "%o" mode) (ewm:strtime modified-time))
       (ewm:rt-format "File Size: %s /  Lines: %s"
               strsize 
               (int-to-string (with-current-buffer selected-buf 
                 (count-lines (point-min) (point-max))))))))
   (t
    (insert
     (ewm:rt-format 
      "Buffer Name: %s\nMajor Mode: %s"
      (substring-no-properties (buffer-name selected-buf))
      (format "%s" (buffer-local-value
                    'major-mode selected-buf)))))))

(defvar ewm:dp-array-overlay-focus nil "[internal]")

(defun ewm:dp-array-hilite-focus ()
  (when ewm:dp-array-overlay-focus
    (delete-overlay ewm:dp-array-overlay-focus))
  (setq ewm:dp-array-overlay-focus 
        (make-overlay (point-min) (point-max)))
  (overlay-put ewm:dp-array-overlay-focus 'face 'highlight))

;; Commands / Keybindings

(defun ewm:dp-array ()
  (interactive)
  (ewm:pst-change 'array))

(defun ewm:dp-array-move-left-command ()
  (interactive)
  (windmove-left)
  (ewm:dp-array-update-summary))
(defun ewm:dp-array-move-right-command ()
  (interactive)
  (windmove-right)
  (ewm:dp-array-update-summary))
(defun ewm:dp-array-move-up-command ()
  (interactive)
  (windmove-up)
  (ewm:dp-array-update-summary))
(defun ewm:dp-array-move-down-command ()
  (interactive)
  (let ((cwin (selected-window))
        (bwin (wlf:get-window (ewm:pst-get-wm) 'summary)))
    (windmove-down)
    (when (eql (selected-window) bwin)
      (select-window cwin)))
  (ewm:dp-array-update-summary))
(defun ewm:dp-array-goto-prev-pst-command ()
  (interactive)
  (ewm:pst-change-prev))
(defun ewm:dp-array-toggle-more-buffers-command ()
  (interactive)
  (setq ewm:dp-array-buffers-function
        (if (eq ewm:dp-array-buffers-function
                'ewm:dp-array-get-recordable-buffers)
            'ewm:dp-array-get-more-buffers
          'ewm:dp-array-get-recordable-buffers))
  (ewm:pst-change 'array))
(defun ewm:dp-array-cancel-command ()
  (interactive)
  (wlf:select (ewm:pst-get-wm) 'w-1-1)
  (ewm:pst-change-prev))

(defvar ewm:dp-array-minor-mode-map 
      (ewm:define-keymap
       '(("<SPC>"  . ewm:dp-array-toggle-more-buffers-command)
         ;;cursor
         ([left]  . ewm:dp-array-move-left-command)
         ([right] . ewm:dp-array-move-right-command)
         ([up]    . ewm:dp-array-move-up-command)
         ([down]  . ewm:dp-array-move-down-command)
         ;;emacs
         ("f"   . ewm:dp-array-move-left-command)
         ("b"   . ewm:dp-array-move-right-command)
         ("p"   . ewm:dp-array-move-up-command)
         ("n"   . ewm:dp-array-move-down-command)
         ("C-f" . ewm:dp-array-move-left-command)
         ("C-b" . ewm:dp-array-move-right-command)
         ("C-p" . ewm:dp-array-move-up-command)
         ("C-n" . ewm:dp-array-move-down-command)
         ;;vi
         ("h"   . ewm:dp-array-move-left-command)
         ("l"   . ewm:dp-array-move-right-command)
         ("k"   . ewm:dp-array-move-up-command)
         ("j"   . ewm:dp-array-move-down-command)
         ;;choose
         ("q"   . ewm:dp-array-cancel-command)
         ("C-m" . ewm:dp-array-goto-prev-pst-command)
         ("C-g" . ewm:dp-array-goto-prev-pst-command)
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Setup

(ewm:add-keymap 
 ewm:pst-minor-mode-keymap
 '(("prefix 1" . ewm:dp-code)
   ("prefix 2" . ewm:dp-two) 
   ("prefix 3" . ewm:dp-doc)
   ("prefix 4" . ewm:dp-array))
 ewm:prefix-key)

(defvar ewm:save-window-configuration nil) ; backup

(defun ewm:history-add-loaded-buffers ()
  (interactive)
  (loop for b in (buffer-list)
        for bo = (get-buffer b)
        if (ewm:history-recordable-p bo)
        do (ewm:history-add bo)))

(defun ewm:start-management ()
  ;;現在のフレームの管理を開始
  (interactive)
  (setq ewm:save-window-configuration 
        (current-window-configuration))

  (ewm:history-add-loaded-buffers) ; 全部つっこむ
  (ewm:history-save-backup nil)
  (ad-activate-regexp "^ewm:ad-override" t)
  (add-hook 'kill-buffer-hook 'ewm:kill-buffer-hook)
  (add-hook 'window-configuration-change-hook
            'ewm:override-window-cfg-change)
  (add-hook 'completion-setup-hook 'ewm:override-setup-completion)
  (setq display-buffer-function 'ewm:override-special-display-function)

  (ad-activate-regexp "^ewm:ad-debug" t) ; debug

  (ewm:pst-set-prev-pst nil)
  (ewm:dp-code)
  (ewm:pst-minor-mode 1)
  (ewm:menu-define))

(defun ewm:stop-management ()
  ;;現在のフレームの管理を終了
  (interactive)
  (when (ewm:managed-p)
    (ewm:pst-finish)
    (ewm:pst-minor-mode -1)
    (ewm:pst-set-prev-pst nil)

    (remove-hook 'kill-buffer-hook 'ewm:kill-buffer-hook)
    (remove-hook 'window-configuration-change-hook 
                 'ewm:override-window-cfg-change)
    (remove-hook 'completion-setup-hook 'ewm:override-setup-completion)
    (setq display-buffer-function nil)
    (ad-deactivate-regexp "^ewm:ad-override$")

    (ad-deactivate-regexp "^ewm:ad-debug") ; debug

    (ewm:aif ewm:save-window-configuration
        (set-window-configuration it))))

;; for dev
;; (progn (setq ewm:debug t) (toggle-debug-on-error))
;; (define-key ewm:pst-minor-mode-keymap (kbd "C-c ; C-m") 'ewm:message-mark)
;; (progn (kill-buffer (get-buffer-create "*ewm:debug*")) (eval-current-buffer) (ewm:start-management))
;; (ewm:stop-management)

(provide 'ewm)
;;; ewm.el ends here
