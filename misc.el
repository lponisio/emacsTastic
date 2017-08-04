;;;; Miscellaneous interface improvements

;; Misc self-explanatory ones:
(blink-cursor-mode -1)
;;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Load the tab-bar code only if running not in a terminal
(cond (window-system
       (load "~/.emacs.d/tabbar-configuration.el")))

;; Time in the modeline:
(display-time-mode)

;; Highlight matching parantheses. Dead cool.
(show-paren-mode t)

;; Set comments in italics.
(set-face-italic-p 'font-lock-comment-face t)

;; Auto-fill all the time
(setq-default auto-fill-function 'do-auto-fill)

;; More informative window title
(setq frame-title-format
      (list "%b %* | " system-name " | emacs " emacs-version))

;; Prevent opening of new emacs windows by default
(setq ns-pop-up-frames nil)

;; Better duplicate buffer (by putting directories in front).
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Set font-locking to maximum, all the time.
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Delete unnecessary autosave files
(setq delete-auto-save-files t)

;; Turn off the beep sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Turning on transient-mark-mode
(transient-mark-mode 1)

;; Environmental variables (allows xdvi to be callable from emacs)
(setenv "DISPLAY" ":0.0")

;; Remote file editing via ssh
(require 'tramp)
(setq tramp-default-method "ssh")

(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")

;;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

;;; 2. Run the while loop.
      (while (and (< (point) end)
		  (re-search-forward "\\w+\\W*" end t))
	(setq count (1+ count)))

;;; 3. Send a message to the user.
      (cond ((zerop count)
	     (message
	      "The region does NOT have any words."))
	    ((= 1 count)
	     (message
	      "The region has 1 word."))
	    (t
	     (message
	      "The region has %d words." count))))))
