;;; e2wm-vcs.el --- VCS perspectives

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: tools

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

;; These are e2wm perspectives for magit and dsvn.
;; One can change the perspective by M-x e2wm:dp-magit or e2wm:dp-svn.

;; Here is a sample code to add switching perspective key bindings:
;; (e2wm:add-keymap e2wm:pst-minor-mode-keymap '(("prefix v" . e2wm:dp-magit)) e2wm:prefix-key)
;; (e2wm:add-keymap e2wm:pst-minor-mode-keymap '(("prefix v" . e2wm:dp-svn)) e2wm:prefix-key)

;;; Code:

(require 'e2wm)
(require 'magit nil t)
(require 'dsvn nil t)


;;; Utilities
;;;--------------------------------------------------

(defface e2wm:face-vcs-na
  '((((class color) (background light))
     :foreground "Chocolate" :height 1.5 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "Chocolate3" :weight bold :height 1.5 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for e2wm:vcs-na title."
  :group 'e2wm)

(defun e2wm:def-plugin-vcs-na-buffer (title)
  (let ((buf (get-buffer-create " *e2wm:vcs-na*")))
    (with-current-buffer buf
      (let (buffer-read-only)
        (buffer-disable-undo buf)
        (erase-buffer)
        (insert (e2wm:rt (substring title 0) 'e2wm:face-vcs-na))
        buf))))


(defun e2wm:def-plugin-vcs-with-window (topdir-func body-func na-buffer-func)
  (let* ((buf (e2wm:history-get-main-buffer))
         (file (buffer-file-name buf))
         (dir (or (and file (file-name-directory file)) default-directory))
         (topdir (and dir (funcall topdir-func dir))))
    (e2wm:with-advice
     (cond
      (topdir
       (with-selected-window (wlf:get-window wm (wlf:window-name winfo))
         (with-current-buffer buf 
           (funcall body-func dir topdir))
         (wlf:set-buffer wm (wlf:window-name winfo)
                         (window-buffer (selected-window)))))
      (t
       (wlf:set-buffer wm (wlf:window-name winfo) 
                       (funcall na-buffer-func)))))))


;;; magit / plugins
;;;--------------------------------------------------

(defun e2wm:def-plugin-magit-branches (frame wm winfo)
  (e2wm:def-plugin-vcs-with-window
   'magit-get-top-dir
   (lambda (dir topdir) 
     (magit-show-branches))
   (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

(e2wm:plugin-register 'magit-branches
                      "Magit Branches"
                      'e2wm:def-plugin-magit-branches)

(defun e2wm:def-plugin-magit-logs (frame wm winfo)
  (e2wm:def-plugin-vcs-with-window
   'magit-get-top-dir
   (lambda (dir topdir)
     (magit-display-log nil))
   (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

(e2wm:plugin-register 'magit-logs
                      "Magit Logs"
                      'e2wm:def-plugin-magit-logs)

(defun e2wm:def-plugin-magit-status (frame wm winfo)
  (e2wm:def-plugin-vcs-with-window
   'magit-get-top-dir
   (lambda (dir topdir)
     (magit-status (file-name-as-directory dir)))
   (lambda () (e2wm:history-get-main-buffer))))

(e2wm:plugin-register 'magit-status
                      "Magit Status"
                      'e2wm:def-plugin-magit-status)


;;; magit / magit perspective
;;;--------------------------------------------------

(defvar e2wm:c-magit-recipe
  '(| (:left-max-size 35)
      (- (:upper-size-ratio 0.7)
         files history)
      (| (:right-max-size 45)
         (- (:upper-size-ratio 0.6) main sub)
         (- (:upper-size-ratio 0.4) branches logs))))

(defvar e2wm:c-magit-winfo
  '((:name main     :plugin magit-status)
    (:name files    :plugin files)
    (:name history  :plugin history-list)
    (:name sub      :buffer nil :default-hide t)
    (:name branches :plugin magit-branches)
    (:name logs     :plugin magit-logs)))

(defvar e2wm:c-magit-show-main-regexp
   "\\*\\(vc-diff\\)\\*")

(e2wm:pst-class-register 
  (make-e2wm:$pst-class
   :name   'magit
   :title  "Magit"
   :init   'e2wm:dp-magit-init
   :main   'main
   :start  'e2wm:dp-magit-start
   :switch 'e2wm:dp-magit-switch
   :popup  'e2wm:dp-magit-popup
   :leave  'e2wm:dp-vcs-leave
   :keymap 'e2wm:dp-magit-minor-mode-map))

(defadvice magit-log-edit-commit (after e2wm:ad-override-magit)
  (e2wm:pst-update-windows))
(ad-deactivate-regexp "^e2wm:ad-override-magit$")

(defun e2wm:dp-vcs-leave (wm)
  (ad-deactivate-regexp "^e2wm:ad-override-magit$")
  (setq prev-selected-buffer nil))

(defun e2wm:dp-magit-start (wm)
  (ad-activate-regexp "^e2wm:ad-override-magit$"))

(defun e2wm:dp-magit-init ()
  (let* ((magit-wm 
          (wlf:no-layout e2wm:c-magit-recipe e2wm:c-magit-winfo))
         (buf (or prev-selected-buffer
                  (e2wm:history-get-main-buffer))))
    magit-wm))

(defun e2wm:dp-magit-switch (buf)
  (e2wm:message "#DP MAGIT switch : %s" buf)
  nil)

(defun e2wm:dp-magit-popup (buf)
  (let ((cb (current-buffer)))
    (e2wm:message "#DP MAGIT popup : %s (current %s / backup %s)" 
                  buf cb e2wm:override-window-cfg-backup))
  (let ((buf-name (buffer-name buf))
        (wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (cond
      ((equal buf-name magit-commit-buffer-name)
       ;; displaying commit objects in the main window
       (e2wm:pst-buffer-set 'main buf t nil))
      (t
       ;; displaying other objects in the sub window
       (e2wm:pst-buffer-set 'sub buf t not-minibufp))))))

;; Commands / Keybindings

(defun e2wm:dp-magit ()
  (interactive)
  (e2wm:pst-change 'magit))

(defvar e2wm:dp-magit-minor-mode-map
  (e2wm:define-keymap '() e2wm:prefix-key))

;; (e2wm:add-keymap e2wm:pst-minor-mode-keymap '(("prefix v" . e2wm:dp-magit)) e2wm:prefix-key)


;;; Subversion / plugins
;;;--------------------------------------------------

(defvar e2wm:def-plugin-svn-log-arg "-l 4 -v")

(defun e2wm:def-plugin-svn-top-dir (dir)
  (let* ((expanded-dir (expand-file-name dir))
         (svndir (member ".svn" (directory-files expanded-dir))))
    (cond
     ((null svndir) nil)
     ((or 
       (string= expanded-dir "/")
       (string= expanded-dir (expand-file-name "~/"))) nil)
     (t (let ((updir (e2wm:def-plugin-svn-top-dir 
                      (concat (file-name-as-directory dir) ".."))))
          (if (null updir) expanded-dir updir))))))

(defvar e2wm:def-plugin-svn-logs-buffer-name " *WM:dsvn-logs*" "[internal]")

(defun e2wm:def-plugin-svn-logs (frame wm winfo)
    (e2wm:def-plugin-vcs-with-window
     'e2wm:def-plugin-svn-top-dir
     (lambda (dir topdir) 
       (let ((default-directory (file-name-as-directory topdir)))
         (svn-log e2wm:def-plugin-svn-log-arg))
       (let ((dbuf (get-buffer-create e2wm:def-plugin-svn-logs-buffer-name)))
         (with-current-buffer dbuf
           (setq buffer-read-only nil)
           (buffer-disable-undo buf)
           (erase-buffer)
           (insert (with-current-buffer (get-buffer "*svn output*")
                     (buffer-string)))
           (setq default-directory dir)
           (setq buffer-read-only t)
           (goto-char (point-min))
           (svn-log-mode))
         (set-window-buffer (selected-window) dbuf)))
     (lambda () (e2wm:def-plugin-vcs-na-buffer "Subversion N/A"))))

(e2wm:plugin-register 'svn-logs
                      "Svn Logs"
                      'e2wm:def-plugin-svn-logs)

(defun e2wm:def-plugin-svn-status (frame wm winfo)
  (e2wm:def-plugin-vcs-with-window
   'e2wm:def-plugin-svn-top-dir
   (lambda (dir topdir) 
     (svn-status (file-name-as-directory topdir)))
   (lambda () (e2wm:history-get-main-buffer))))

(e2wm:plugin-register 'svn-status
                      "Svn Status"
                      'e2wm:def-plugin-svn-status)


;;; Subversion status perspective
;;;--------------------------------------------------

(defvar e2wm:c-svn-recipe
  '(| (:left-max-size 35)
      (- (:upper-size-ratio 0.7)
         files history)
      (| (:right-max-size 45)
         (- (:lower-size-ratio 0.4) main sub)
         logs)))

(defvar e2wm:c-svn-winfo
  '((:name main     :plugin svn-status)
    (:name files    :plugin files)
    (:name history  :plugin history-list)
    (:name sub      :buffer nil :default-hide t)
    (:name logs     :plugin svn-logs :default-hide t)))

(defvar e2wm:c-svn-focus-buffer-regexp "\\*\\(svn commit\\)\\*")

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'svn
   :title  "Svn"
   :init   'e2wm:dp-svn-init
   :main   'main
   :switch 'e2wm:dp-svn-switch
   :popup  'e2wm:dp-svn-popup
   :leave  'e2wm:dp-vcs-leave
   :keymap 'e2wm:dp-svn-minor-mode-map))

(defun e2wm:dp-svn-init ()
  (let* ((svn-wm 
          (wlf:no-layout e2wm:c-svn-recipe e2wm:c-svn-winfo))
         (buf (or prev-selected-buffer
                  (e2wm:history-get-main-buffer))))
    svn-wm))

(defun e2wm:dp-svn-switch (buf)
  (e2wm:message "#DP SVN switch : %s" buf)
  nil)

(defun e2wm:dp-svn-popup (buf)
  (let ((cb (current-buffer)))
    (e2wm:message "#DP SVN popup : %s (current %s / backup %s)" 
                  buf cb e2wm:override-window-cfg-backup))
  (let* ((wm (e2wm:pst-get-wm))
         (bufname (buffer-name buf))
         (focus-set (and (= 0 (minibuffer-depth))
                         (string-match e2wm:c-svn-focus-buffer-regexp bufname))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t focus-set))))

;; Commands / Keybindings

(defun e2wm:dp-svn ()
  (interactive)
  (e2wm:pst-change 'svn))

(defvar e2wm:dp-svn-minor-mode-map
  (e2wm:define-keymap '() e2wm:prefix-key))

;; (e2wm:add-keymap e2wm:pst-minor-mode-keymap '(("prefix v" . e2wm:dp-svn)) e2wm:prefix-key)


(provide 'e2wm-vcs)
;;; e2wm-vcs.el ends here
