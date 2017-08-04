;; Customise various "modes" (for want of a better name)

;; R
(require 'ess-site)
(defun Rnw-mode ()
  (require 'ess-noweb)
  (noweb-mode)
  ( if (fboundp 'R-mode)
      (setq noweb-default-code-mode 'R-mode)))

(setq auto-mode-alist
      (append '(("\\.Rnw\\'" . Rnw-mode)
                ("\\.Snw\\'" . Rnw-mode))
              auto-mode-alist))

;; Scheme
(autoload 'scheme-mode "cmuscheme" "Major mode for scheme" t)
(autoload 'run-scheme "cmuscheme" "Switch to interactive scheme buffer" t)
(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "/usr/local/Gambit-C/current/bin/gsi -:d-")

;; Common lisp
(add-hook 'lisp-mode-hook
          (lambda () (slime-mode t)
            (local-set-key "\r" 'newline-and-indent)
            (setq lisp-indent-function 'common-lisp-indent-function)
            (setq indent-tabs-mode nil)))

;; LaTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq reftex-cite-format 'natbib)

(eval-after-load "bibtex"
  '(progn 
     (setq bibtex-user-optional-fields
	   (append '(("doi" "Digital Object Identifier")
		     ("local-url" "URL for local files (e.g. pdf)"))
		   bibtex-user-optional-fields))
     (setq bibtex-include-OPTkey nil)))

(require 'bibtex)

(defun lkm-bibtex-next-entry ()
  (interactive)
  (progn
    (next-line)
    (bibtex-skip-to-valid-entry)))
(defun lkm-bibtex-prev-entry ()
  (interactive)
  (progn
    (previous-line)
    (bibtex-skip-to-valid-entry t)))

(add-hook 'bibtex-mode-hook
          '(lambda ()
	     (define-key bibtex-mode-map [\C-down] 'lkm-bibtex-next-entry)
	     (define-key bibtex-mode-map [\C-up] 'lkm-bibtex-prev-entry)))

(setq TeX-output-view-style
      (quote 
       (
        ("^dvi$" 
         ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") 
         "%(o?)dvips -t landscape %d -o && gv %f") 
        ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
        ("^dvi$" 
         ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") 
         "%(o?)xdvi %dS -paper a4r -s 0 %d") 
        ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" 
         "%(o?)xdvi %dS -paper a4 %d") 
        ("^dvi$" 
         ("^a5\\(?:comb\\|paper\\)$" "^landscape$") 
         "%(o?)xdvi %dS -paper a5r -s 0 %d") 
        ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") 
        ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") 
        ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") 
        ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") 
        ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") 
        ("^dvi$" "." "%(o?)xdvi %dS %d") 
        ("^pdf$" "." "repreview %o")
        ("^html?$" "." "/Applications/Safari.app/Contents/MacOS/Safari %o"))))

;; Maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(setq imaxima-use-maxima-mode-flag t)
(add-hook 'maxima-mode-hook
          (lambda () 
	    (local-set-key "\C-c\C-c" 'maxima-complete-symbol)))

;; ; HTML/XML
;; (load "rng-auto.el")
