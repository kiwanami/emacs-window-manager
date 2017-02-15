;;; Code:

(require 'e2wm)
(require 'jedi-direx)

(defun e2wm:def-plugin-jedi-direx (frame wm winfo)
  (let* ((buf (e2wm:history-get-main-buffer))
	 (bname (buffer-name buf))
	 (jd-bname (format "*direx-jedi: %s*" bname))
	 (wname (wlf:window-name winfo)))
    (if (get-buffer jd-bname)
	(wlf:set-buffer wm wname jd-bname)
      (wlf:set-buffer wm wname (e2wm:jedi-direx-mk-jd-buf buf)))))

(defun e2wm:jedi-direx-mk-jd-buf (source-buf)
  (with-current-buffer source-buf
    (jedi-direx:make-buffer)))

(defun e2wm:update-jd-buf ()
  (interactive)
  (let* ((source-buf (e2wm:history-get-main-buffer))
	 (jd-bname (format "*direx-jedi: %s*" (buffer-name source-buf)))
	 (jd-win (get-buffer-window jd-bname)))
    (kill-buffer jd-bname)
    (set-window-buffer jd-win (e2wm:jedi-direx-mk-jd-buf source-buf))))  

(e2wm:plugin-register 'jedi-direx-plugin
                     "Outline"
                     'e2wm:def-plugin-jedi-direx)

(defvar e2wm:c-pycode-recipe
  '(| (:left-max-size 35)
      (- (:upper-size-ratio 0.7)
         files history)
      (- (:upper-size-ratio 0.8)
         (| (:right-max-size 30)
            main code-struct)
	 sub)))

(defvar e2wm:c-pycode-winfo
  '((:name main)
    (:name files :plugin files)
    (:name history :plugin history-list)
    (:name sub :buffer "*info*" :default-hide t)
;    (:name shell :plugin eshell :default-hide t)
    (:name code-struct :plugin jedi-direx-plugin :default-hide nil))
  )

(defvar e2wm:c-pycode-show-main-regexp
  "\\*\\(vc-diff\\)\\*")

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'pycode
   :extend 'base
   :title  "Pycode"
   :init   'e2wm:dp-pycode-init
   :main   'main
;   :start  'e2wm:dp-pycode-start
   :switch 'e2wm:dp-pycode-switch
   :popup  'e2wm:dp-pycode-popup
;   :leave  'e2wm:dp-pycode-leave
   :keymap 'e2wm:dp-pycode-minor-mode-map))


(defun e2wm:dp-pycode-init ()
  (let* ((pycode-wm
	  (wlf:no-layout
	   e2wm:c-pycode-recipe
	   e2wm:c-pycode-winfo))
	 (buf (or e2wm:prev-selected-buffer
		  (e2wm:history-get-main-buffer))))
    
    (when (e2wm:history-recordable-p e2wm:prev-selected-buffer)
      (e2wm:history-add e2wm:prev-selected-buffer))
    (wlf:set-buffer pycode-wm 'main buf)
    pycode-wm))

(defun e2wm:dp-pycode-switch (buf)
  (e2wm:message "#DP PYCODE switch : %s / %S" buf (e2wm:history-recordable-p buf))
  (if (e2wm:history-recordable-p buf)
      (progn
        (e2wm:pst-show-history-main)
        (e2wm:pst-window-select-main))
    nil)
  ;(e2wm:pst-update-windows)
  )

;(defun e2wm:dp-pycode-leave (wm)
;  (ad-deactivate-regexp "^e2wm:ad-override-pycode$")
;  (setq e2wm:prev-selected-buffer nil))

;(defun e2wm:dp-pycode-start (wm)
;  (ad-activate-regexp "^e2wm:ad-override-pycode$"))


(defun e2wm:dp-pycode-popup (buf)
  (let ((cb (current-buffer)))
    (e2wm:message "#DP CODE popup : %s (current %s / backup %s)"
                 buf cb e2wm:override-window-cfg-backup))
  (let ((buf-name (buffer-name buf))
        (wm (e2wm:pst-get-wm)))
    (cond
     ((e2wm:history-recordable-p buf)
      (e2wm:pst-show-history-main)
      t)
     ((and e2wm:override-window-cfg-backup
       (eq (selected-window) (wlf:get-window wm 'sub)))
      (setq e2wm:override-window-cfg-backup nil)
      (set-window-buffer (wlf:get-window wm 'main) buf)
      t)
     ((and e2wm:c-pycode-show-main-regexp
           (string-match e2wm:c-pycode-show-main-regexp buf-name))
      (e2wm:pst-buffer-set 'main buf t)
      t)
     ;; For completion buffer (ad-hoc fix)
     ((and (= 0 (minibuffer-depth))
           (string-match "\\*Completions\\*" (buffer-name buf)))
      (let ((wm (e2wm:pst-get-wm))
            (curwin (selected-window))
            (e2wm:delete-other-windows-permission t))
        (delete-other-windows curwin)
        (wlf:show wm 'sub)
        (wlf:set-buffer wm 'sub buf t))
      t)
     (t
      (e2wm:dp-pycode-popup-sub buf)
      t))))

(defun e2wm:dp-pycode-popup-sub (buf)
  (let ((wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t not-minibufp))))

;; Commands / Keybindings

;;;###autoload
(defun e2wm:dp-pycode ()
  (interactive)
  (e2wm:pst-change 'pycode))
  

(defvar e2wm:dp-pycode-minor-mode-map
  (e2wm:define-keymap
   '(("prefix U" . e2wm:update-jd-buf))
   e2wm:prefix-key))

;; (e2wm:add-keymap e2wm:pst-minor-mode-keymap '(("prefix v" . e2wm:dp-magit)) e2wm:prefix-key)

;; Commands / Keybindings

(provide 'e2wm-pycode)
;;; e2wm-pycode.el ends here
