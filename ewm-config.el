;;; ewm-config.el --- ewm configuration

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai@kiwanami.net>
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

;; sample configuration
;; コメントアウトされているものはデフォルトの設定

;;; Code:

(setq woman-use-own-frame nil) ; womanで新規フレームを開かせない

;; (setq ewm:prefix-key "C-c ; ")

(require 'ewm)

;;; 全体設定

;; (setq ewm:debug nil) 

;; (setq ewm:c-max-history-num 20)  ; 履歴の保存数

;; (setq ewm:c-recordable-buffer-p  ; 履歴として記録したいBuffer
;;       (lambda (buf)
;;         (buffer-local-value 'buffer-file-name buf))) ; ファイル名に関連ついてるもの

;; (setq ewm:c-document-buffer-p ; 
;;       (lambda (buf)
;;         (string-match "\\*\\(Help\\|info\\|w3m\\|WoMan\\)" 
;;                       (buffer-name buf)))) ; ドキュメント的に扱いたいバッファ

;; (setq ewm:c-blank-buffer         ; 白紙バッファ
;;       (let ((buf (get-buffer-create " *ewm:blank*")))
;;         (with-current-buffer buf
;;           (setq buffer-read-only nil)
;;           (buffer-disable-undo buf)
;;           (erase-buffer)
;;           (setq buffer-read-only t)) buf))


;;; パースペクティブカスタマイズ

;;; code

;; ;; レイアウト
;; (setq ewm:c-code-recipe
;;   '(| (:left-max-size 35)
;;       (- (:upper-size-ratio 0.7)
;;          files history)
;;       (- (:upper-size-ratio 0.7)
;;          (| (:right-max-size 30)
;;             main imenu)
;;          sub)))

;; (setq ewm:c-code-winfo
;;   '((:name main)
;;     (:name files :plugin files)
;;     (:name history :plugin history-list)
;;     (:name sub :buffer "*info*" :default-hide t)
;;     (:name imenu :plugin imenu :default-hide nil))
;;   )

;; ;; メインに表示していいもの（それ以外はSubに表示される）
;; (setq ewm:c-code-show-main-regexp
;;    "\\*\\(vc-diff\\)\\*")

;; キーバインド
(ewm:add-keymap 
 ewm:pst-minor-mode-keymap
 '(("<M-right>" . ewm:dp-code) ; codeへ変更
   ("<M-left>"  . ewm:dp-two)  ; twoへ変更
   ("<M-up>"    . ewm:dp-doc)  ; docへ変更
   ("<M-down>"  . ewm:dp-dashboard) ; dashboardへ変更
   ("C-."       . ewm:pst-history-forward-command) ; 履歴を進む
   ("C-,"       . ewm:pst-history-back-command) ; 履歴をもどる
   ("prefix L"  . ielm)
   ("M-m"       . ewm:pst-window-select-main-command)
   ) ewm:prefix-key)

;;; two

;; ;; レイアウト
;; (setq ewm:c-two-recipe
;;       '(- (:upper-size-ratio 0.8)
;;           (| left
;;              (- (:upper-size-ratio 0.9)
;;                 right history))
;;           sub))

;; (setq ewm:c-two-winfo
;;       '((:name left )
;;         (:name right :plugin main-prev)
;;         (:name sub :buffer "*Help*" :default-hide t)
;;         (:name history :plugin history-list :default-hide nil)))

;; キーバインド
(ewm:add-keymap 
 ewm:dp-two-minor-mode-map 
 '(("prefix I" . info)) 
 ewm:prefix-key)

;;; doc

;; ;; レイアウト
;; (setq ewm:c-doc-recipe
;;       '(- (:upper-size-ratio 0.75)
;;         (| left right)
;;         sub))

;; (setq ewm:c-doc-winfo
;;       '((:name left)
;;         (:name right)
;;         (:name sub :default-hide t)))

;; キーバインド
(ewm:add-keymap 
 ewm:dp-doc-minor-mode-map 
 '(("prefix I" . info)) 
 ewm:prefix-key)

;;; dashboard

(setq ewm:c-dashboard-plugins
  '(
    (open :plugin-args (:command eshell :buffer "*eshell*"))
    (open :plugin-args (:command doctor :buffer "*doctor*"))
    ))

;;; pstset


;;; プラグインカスタマイズ

;;; top

;; (setq ewm:def-plugin-top-timer-interval 20 "Seconds for update.")

;;; clock

;; (defvar ewm:def-plugin-clock-download-file "/tmp/wmclock.jpg"  "[internal]")
;; (defvar ewm:def-plugin-clock-resized-file  "/tmp/wmclockt.jpg" "[internal]")
;;↑cygwin環境の場合は "C:/cygwin/tmp/wmclock.jpg" とかにすると良いかも

;; for bijin (default)
;; (setq ewm:def-plugin-clock-url "http://www.bijint.com/jp/img/clk/%H%M.jpg")
;; (setq ewm:def-plugin-clock-referer "http://www.bijint.com/jp/")

;; for binan
;; (setq ewm:def-plugin-clock-url "http://www.bijint.com/binan/img/clk/%H%M.jpg")
;; (setq ewm:def-plugin-clock-referer "http://www.bijint.com/binan/")


(provide 'ewm-config)
;;; ewm-config.el ends here
