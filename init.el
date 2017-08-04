
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq mac-command-modifier 'meta)

;; emacs speaks statistics
(add-to-list 'load-path "~/.emacs.d/libs/ESS/")
(setq ess-eval-visibly nil) ; ESS will not print the evaluated
			    ; commands, also speeds up the evaluation 

(setq ess-ask-for-ess-directory nil) ;if you don't want to be
				      ;prompted each time you start an
				      ;interactive R session

;; Include my local emacs lisp library
(setq load-path (append '("~/.emacs.d/libs") load-path))

;; include R markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; disable auto-save
(setq auto-save-default nil)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph. Takes a multi-line paragraph and makes it into a
;;; single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;; Backups
;; This has been in my .emacs for years now, and I have no idea where
;; it came from.  This backs up files to
;; ~/.emacs.d/backups/<filename>, where <filename> is the original
;; filename used.  It seems to work very well.
(fset 'orthodox-make-backup-file-name
      (symbol-function 'make-backup-file-name))

(defun make-bak (file)
  "Intended for (fset 'make-backup-file-name 'make-bak)."
  (let* ((file-dir (abbreviate-file-name 
		    (or (file-name-directory file) default-directory)))
	 (backup-dir (concat (getenv "HOME") "/.emacs.d/backups/")))
    (or (file-directory-p backup-dir)
	(make-directory backup-dir))
    (concat backup-dir (file-name-nondirectory file))))

(defun bak-p (file)
  "Intended for (fset 'backup-file-name-p 'bak-p)."
  (string-match "Bak[/\\]" file))

(fset 'make-backup-file-name 'make-bak)
(fset 'backup-file-name-p 'bak-p)

;;;; Window cycling
(defun yic-ignore (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*ESS\\*$" str)
   (string-match "^\\*scratch\\*$" str)
   (string-match "^\\*Occur\\*$" str)
   (string-match "^\\*TeX silent\\*$" str)
   (string-match "^\\*.* output\\*$" str)
   (string-match "^ " str)))
(defun yic-next (ls)
  "Switch to next buffer in ls skipping unwanted ones."
  (let* ((ptr ls)
         bf 
	 bn 
	 go)
    (while (and ptr (null go))
      (setq bf (car ptr) bn (buffer-name bf))
      (if (null (yic-ignore bn))
	  (setq go bf)
        (setq ptr (cdr ptr))))
    (if go
        (switch-to-buffer go))))

(defun yic-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))

(defun yic-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (yic-next (reverse (buffer-list))))

(load "~/.emacs.d/libs/ess-12.09/ess-autoloads.el")
(load "~/.emacs.d/keys.el")
(load "~/.emacs.d/misc.el")
(load "~/.emacs.d/modes.el")
(load "~/.emacs.d/browse-kill-ring.el")

(message ".emacs loaded")
(put 'scroll-left 'disabled nil)

(put 'downcase-region 'disabled nil)
(require 'tramp)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(reftex-default-bibliography (quote ("refs")))
 '(safe-local-variable-values (quote ((TeX-PDF-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; Download source from aspell.net
;; Install it and its dictionary
;; Edit ~/.aspell.conf to say
;;   home-dir /Users/lkm
;; *not* with $HOME, or this will not work.
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-program-name "aspell")
;; (setq-default ispell-list-command "list") 
;; (setq-default ispell-extra-args  '("--lang=en_GB"))
 (setq-default ispell-extra-args  '("--lang=en_US"))
