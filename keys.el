;; Keybindings

(global-set-key "\M-w" 'unfill-paragraph)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(global-set-key "\C-x\C-c" 'comment-region)
(global-set-key "\C-x\C-u" 'uncomment-region)

(global-set-key "\M-g" 'goto-line)

(global-set-key "\C-x\C-k" 'upcase-region)
(global-set-key "\C-x\C-l" 'downcase-region)

(global-set-key "\C-x\C-q" 'save-buffers-kill-emacs)

(when window-system
  (global-unset-key "\C-z")
  (global-unset-key "\C-x\C-z"))

(global-set-key "\M-c" 'copy-region-as-kill)
(global-set-key "\M-v" 'yank)
(global-set-key "\M-z" 'undo)

(global-set-key "\M-s" 'ispell-word)

(global-set-key "\C-x\C-w" 'kill-this-buffer)

(global-set-key "\M-e" 'execute-extended-command)

(global-set-key "\M-r" 'fill-individual-paragraphs)

(global-set-key "\C-a" 'fixup-whitespace)

(global-set-key "\M-0" 'tabbar-mode)

(global-set-key "\M-`" 'other-frame)

(global-set-key [\C-kp-delete] 'kill-word)

;; tab-bar:
(global-set-key [\C-tab] 'tabbar-forward-tab)
(global-set-key [(control shift tab)] 'tabbar-backward-tab)
(global-set-key [\C-prior] 'tabbar-forward-group)
(global-set-key [\C-next] 'tabbar-backward-group)
(global-set-key [\C-next] 'tabbar-backward-group)

;; browse-kill-ring:
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; select everything

(global-set-key (kbd "C-b") 'move-beginning-of-line)

(global-set-key "\M-a" 'mark-whole-buffer)


;; C-x C-e ;; current line
;; M-x eval-region ;; region
;; M-x eval-buffer ;; whole buffer
;; M-x load-file ~/.emacs.d/init.el
