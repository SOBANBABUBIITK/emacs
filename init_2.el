(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
;; some error with old package
(package-install 'use-package)

;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    ein                             ;; Emacs IPython Notebook
    auto-complete
    auto-complete-c-headers
    iedit
    modern-cpp-font-lock
    clang-format
    google-c-style
    format-all
    auctex
    auto-complete-auctex
    ac-math
    flycheck-google-cpplint
    flymake-cursor
    yasnippet
    magit
    vscode-dark-plus-theme
    material-theme                  ;; Theme
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
(set-language-environment "UTF-8")
(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default cursor-type 'bar)                              ;; set cursor type to I
(setq-default indicate-empty-lines t)                        ;; show white lines
(setq-default show-trailing-whitespace t)                    ;; show only trailing white space
(setq whitespace-display-mappings '((space-mark 32 [?·])))
(add-hook 'before-save-hook 'delete-trailing-whitespace)     ;; delete trailing white space before saving


;;===================================
;; Themes
;;===================================
(load-theme 'vscode-dark-plus t)
;; (load-theme 'material t)            ;; Load material theme

;;; Enable flycheck mode for all languages
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default tab-width 4)

;; ====================================
;; Python Development Setup
;; ====================================
;; Enable elpy
(package-initialize)
(elpy-enable)

;; Use IPython for REPL
(setq python-shell-interpreter "/usr/local/programfiles/anaconda3/bin/jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")
;; use Ipyhton
(setq python-shell-interpreter "/usr/local/programfiles/anaconda3/bin/ipython"
      python-shell-interpreter-args "-i --simple-prompt")
;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(setq elpy-remove-modeline-lighter t)

(advice-add 'elpy-modules-remove-modeline-lighter
            :around (lambda (fun &rest args)
                      (unless (eq (car args) 'flymake-mode)
                        (apply fun args))))
;; Auto-format code on save
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-format-code nil t)))
(put 'upcase-region 'disabled nil)
(global-flycheck-mode t)
;; (require 'iedit)

;;------------------------------
;;           c++
;;------------------------------
;; start auto-complete with emacs
(require 'auto-complete)

;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories
	(append '("/usr/include/c++/9"
		  "/usr/include/x86_64-linux-gnu/c++/9"
		  "/usr/include/c++/9/backward"
		  "/usr/lib/gcc/x86_64-linux-gnu/9/include"
		  "/usr/local/include"
		  "/usr/include/x86_64-linux-gnu"
		  "/usr/include"
		  "/usr/local/programfiles/trilinos/include")
		achead:include-directories))
  )
;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
(require 'iedit)

;; Fix iedit bug in ubuntu
(define-key global-map (kbd "C-c C-;") 'iedit-mode)

;; turn on Semantic
(semantic-mode 1)

;; let's define a function which adds semantic as a suggestion backend to auto complete
;; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; turn on ede mode
(global-ede-mode 1)

;; you can use system-include-path for setting up the system header file locations.
;; turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)
(modern-c++-font-lock-global-mode 1)

;; Set default indetation to 4 space
(setq-default c-basic-offset 4)

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))

;;------------------------------
;;           LATEX
;;------------------------------
(setq TeX-auto-save 1)
(setq TeX-parse-self 1)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX 1)
(setq LaTeX-item-indent 0)

;;------------------------------
;;  Sentinel erorr
;;------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(yasnippet-snippets find-file-in-project vscode-dark-plus-theme use-package py-autopep8 modern-cpp-font-lock material-theme magit jedi iedit google-c-style format-all flymake-cursor flycheck-google-cpplint elpy ein clang-format blacken better-defaults auto-complete-c-headers auto-complete-auctex auctex ac-math))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
