;;; e2wm-config.el --- e2wm configuration

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

;; (setq e2wm:prefix-key "C-c ; ")

(require 'e2wm)

;;; 全体設定

;; (setq e2wm:debug nil) 

;; (setq e2wm:c-max-history-num 20)  ; 履歴の保存数

;; (setq e2wm:c-recordable-buffer-p  ; 履歴として記録したいBuffer
;;       (lambda (buf)
;;         (buffer-local-value 'buffer-file-name buf))) ; ファイル名に関連ついてるもの

;; (setq e2wm:c-document-buffer-p ; 
;;       (lambda (buf)
;;         (string-match "\\*\\(Help\\|info\\|w3m\\|WoMan\\)" 
;;                       (buffer-name buf)))) ; ドキュメント的に扱いたいバッファ

;; (setq e2wm:c-blank-buffer         ; 白紙バッファ
;;       (let ((buf (get-buffer-create " *e2wm:blank*")))
;;         (with-current-buffer buf
;;           (setq buffer-read-only nil)
;;           (buffer-disable-undo buf)
;;           (erase-buffer)
;;           (setq buffer-read-only t)) buf))


;;; パースペクティブカスタマイズ

;;; code

;; ;; レイアウト

;; ;; for 1440x900以上 (default)
;; (setq e2wm:c-code-recipe
;;   '(| (:left-max-size 35)
;;       (- (:upper-size-ratio 0.7)
;;          files history)
;;       (- (:upper-size-ratio 0.7)
;;          (| (:right-max-size 30)
;;             main imenu)
;;          sub)))

;; ;; for 1280x768
;; (setq e2wm:c-code-recipe
;;   '(| (:left-max-size 30)
;;       (- (:upper-size-ratio 0.7)
;;          files history)
;;       (- (:upper-size-ratio 0.7)
;;          (| (:right-max-size 25)
;;             main imenu)
;;          sub)))

;; ;; for 1024x768
;; (setq e2wm:c-code-recipe
;;   '(| (:left-max-size 35)
;;       (- (:upper-size-ratio 0.7)
;;          (- (:upper-size-ratio 0.6)
;;             files imenu)
;;          history)
;;       (- (:upper-size-ratio 0.7)
;;          main sub)))

;; (setq e2wm:c-code-winfo
;;   '((:name main)
;;     (:name files :plugin files)
;;     (:name history :plugin history-list)
;;     (:name sub :buffer "*info*" :default-hide t)
;;     (:name imenu :plugin imenu :default-hide nil))
;;   )

;; ;; メインに表示していいもの（それ以外はSubに表示される）
;; (setq e2wm:c-code-show-main-regexp
;;    "\\*\\(vc-diff\\)\\*")

;; キーバインド
(e2wm:add-keymap 
 e2wm:pst-minor-mode-keymap
 '(("<M-left>" . e2wm:dp-code) ; codeへ変更
   ("<M-right>"  . e2wm:dp-two)  ; twoへ変更
   ("<M-up>"    . e2wm:dp-doc)  ; docへ変更
   ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
   ("C-."       . e2wm:pst-history-forward-command) ; 履歴を進む
   ("C-,"       . e2wm:pst-history-back-command) ; 履歴をもどる
   ("prefix L"  . ielm)
   ("M-m"       . e2wm:pst-window-select-main-command)
   ) e2wm:prefix-key)

;;; two

;; ;; レイアウト
;; (setq e2wm:c-two-recipe
;;       '(- (:upper-size-ratio 0.8)
;;           (| left
;;              (- (:upper-size-ratio 0.9)
;;                 right history))
;;           sub))

;; (setq e2wm:c-two-winfo
;;       '((:name left )
;;         (:name right )
;;         (:name sub :buffer "*Help*" :default-hide t)
;;         (:name history :plugin history-list :default-hide nil)))

;; キーバインド
(e2wm:add-keymap 
 e2wm:dp-two-minor-mode-map 
 '(("prefix I" . info)
   ("C->"       . e2wm:dp-two-right-history-forward-command) ; 右側の履歴を進む
   ("C-<"       . e2wm:dp-two-right-history-back-command) ; 右側の履歴を進む
   ) e2wm:prefix-key)

;;; doc

;; ;; レイアウト
;; (setq e2wm:c-doc-recipe
;;       '(- (:upper-size-ratio 0.75)
;;         (| left right)
;;         sub))

;; (setq e2wm:c-doc-winfo
;;       '((:name left)
;;         (:name right)
;;         (:name sub :default-hide t)))

;; キーバインド
(e2wm:add-keymap 
 e2wm:dp-doc-minor-mode-map 
 '(("prefix I" . info)) 
 e2wm:prefix-key)

;;; dashboard

(setq e2wm:c-dashboard-plugins
  '(clock top
    (open :plugin-args (:command eshell :buffer "*eshell*"))
    (open :plugin-args (:command doctor :buffer "*doctor*"))
    ))

;;; pstset


;;; プラグインカスタマイズ

;;; top

;; (setq e2wm:def-plugin-top-timer-interval 20 "Seconds for update.")

;;; clock

;; (defvar e2wm:def-plugin-clock-download-file "/tmp/wmclock.jpg"  "[internal]")
;; (defvar e2wm:def-plugin-clock-resized-file  "/tmp/wmclockt.jpg" "[internal]")
;;↑cygwin環境の場合は "C:/cygwin/tmp/wmclock.jpg" とかにすると良いかも

;; for bijin (default)
;; (setq e2wm:def-plugin-clock-url "http://www.bijint.com/jp/img/clk/%H%M.jpg")
;; (setq e2wm:def-plugin-clock-referer "http://www.bijint.com/jp/")

;; for binan
;; (setq e2wm:def-plugin-clock-url "http://www.bijint.com/binan/img/clk/%H%M.jpg")
;; (setq e2wm:def-plugin-clock-referer "http://www.bijint.com/binan/")


(provide 'e2wm-config)
;;; e2wm-config.el ends here
