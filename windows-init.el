;; ===================================
;; Basic Customization
;; ===================================
(setq inhibit-startup-message t)       ;; Hide the startup message

(global-linum-mode t)               ;; Enable line numbers globally
(setq default-directory "C:/Users/1370354/Videos/")
;; (setq-default cursor-type 'bar) ;; set cursor type to I
(setq-default indicate-empty-lines t) ;; show white lines
(setq-default show-trailing-whitespace t) ;; show only trailing white space
(setq whitespace-display-mappings '((space-mark 32 [?·])))
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; delete trailing white space before saving
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11")) ;; Change font style and size

(tool-bar-mode -1)     ;; Disable tool bar mode
(toggle-scroll-bar -1) ;; Scroll bar
(menu-bar-mode -1)     ;; Menubar
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; Add vertical line

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    flycheck-pycheckers
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    ein                             ;; Emacs IPython Notebook
    material-theme                  ;; Theme
    monokai-theme                   ;; Theme
    vscode-dark-plus-theme          ;; Theme
    iedit                           ;; multiple edic
    magit                           ;; for git
    highlight-indent-guides
    auctex
    org-bullets
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)


;; ===================================
;; Basic Customization
;; ===================================
(setq inhibit-startup-message t)       ;; Hide the startup message
;; (load-theme 'material t)            ;; Load material theme
;; (load-theme 'vscode-dark-plus t)    ;; Load material theme
;; (load-theme 'monokai t)


;; ---------------------------
;; Python setup
;; ---------------------------
(require 'elpy)
(elpy-enable)

(setq elpy-rpc-python-command "C:/Users/1370354/AppData/Local/Programs/Python/Python38/python")
(setq flycheck-python-flake8-executable "C:/Users/1370354/AppData/Local/Programs/Python/Python38/python")
(setq flycheck-python-pycompile-executable "C:/Users/1370354/AppData/Local/Programs/Python/Python38/python")
(setq flycheck-python-pylint-executable "C:/Users/1370354/AppData/Local/Programs/Python/Python38/python")

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-mode)
(setq py-autopep8-options '("--max-line-length=79"))

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")

;; Enable fLycheck
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;--------------------------------------------
;; Multiline edit
;;--------------------------------------------
(require 'iedit)

;;--------------------------------------------
;; highlight indent
;;--------------------------------------------
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;;--------------------------------------------
;; indent 4 space c/c++
;;--------------------------------------------
(setq-default c-basic-offset 4)
(setq c-default-style "linux"
      c-basic-offset 4)

(require 'flyspell)
(flyspell-mode +1)

;;--------------------------------------------
;; Latex
;;--------------------------------------------
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
(setq reftex-format-cite-function
          '(lambda (key fmt)
	     (let ((cite (replace-regexp-in-string "%l" key fmt)))
	       (if (or (= ?~ (string-to-char fmt))
		       (member (preceding-char) '(?\ ?\t ?\n ?~)))
	           cite
	         (concat "~" cite)))))




(add-hook 'plain-TeX-mode-hook
          (lambda () (set (make-local-variable 'TeX-electric-math)
                          (cons "$" "$"))))
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-local-variable 'TeX-electric-math)
                          (cons "\\(" "\\)"))))
;;--------------------------------------------
;; org
;;--------------------------------------------

;; Org as work processor
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; ----------------------------------------------------------------------------------------------------------------------
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "C:/Users/1370354/Videos/work-progress/weekly-planner.org"
			     "C:/Users/1370354/Videos/work-progress/personal.org"
			     "C:/Users/1370354/Videos/work-progress/monthly-planner.org"
			     "C:/Users/1370354/Videos/work-progress/02_february.org"))
(add-hook 'org-mode-hook 'toggle-truncate-lines)



;; (add-hook 'org-mode-hook (lambda () (setq truncate-lines t)))
;; https://www.emacswiki.org/emacs/TruncateLines

;; ===================================================
;; User init ends here
;; ===================================================
