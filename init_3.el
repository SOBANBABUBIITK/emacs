(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
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
    jedi
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
    vscode-dark-plus-theme          ;; vscode-dark-theme
    material-theme                  ;; material-theme
    ubuntu-theme                    ;; Ubuntu-theme
    highlight-indent-guides         ;; Show vs code like line in indentation
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
;; (setq-default cursor-type 'bar)                              ;; set cursor type to I
(setq-default indicate-empty-lines t)                        ;; show white lines
(setq-default show-trailing-whitespace t)                    ;; show only trailing white space
(setq whitespace-display-mappings '((space-mark 32 [?·])))
(add-hook 'before-save-hook 'delete-trailing-whitespace)     ;; delete trailing white space before saving

;; show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Enable flycheck mode for all languages
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default tab-width 4)

;;===================================
;; Themes
;;===================================
(load-theme 'vscode-dark-plus t) ;; Load vscdode-dark themex
;; (load-theme 'material t)            ;; Load material theme


;; ====================================
;; Python Development Setup
;; ====================================
;; Enable elpy
(package-initialize)
(elpy-enable)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; ;;------------------------------
;; ;;           c++
;; ;;------------------------------
;; ;; start auto-complete with emacs
;; (require 'auto-complete)

;; ;; do default config for auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)

;; ;; start yasnippet with emacs
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
;; (defun my:ac-c-header-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   (setq achead:include-directories
;; 	(append '("/usr/include/c++/9"
;; 			  "/usr/include/x86_64-linux-gnu/c++/9"
;; 			  "/usr/include/c++/9/backward"
;; 			  "/usr/lib/gcc/x86_64-linux-gnu/9/include"
;; 			  "/usr/local/include"
;; 			  "/usr/include/x86_64-linux-gnu"
;; 			  "/usr/include"
;; 			  "/usr/local/programfiles/trilinos/include")
;; 			achead:include-directories))
;;   )
;; ;; now let's call this function from c/c++ hooks
;; (add-hook 'c++-mode-hook 'my:ac-c-header-init)
;; (add-hook 'c-mode-hook 'my:ac-c-header-init)
;; (require 'iedit)

;; ;; turn on Semantic
;; (semantic-mode 1)

;; ;; let's define a function which adds semantic as a suggestion backend to auto complete
;; ;; and hook this function to c-mode-common-hook
;; (defun my:add-semantic-to-autocomplete()
;;   (add-to-list 'ac-sources 'ac-source-semantic)
;;   )
;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; ;; turn on ede mode
;; (global-ede-mode 1)

;; ;; you can use system-include-path for setting up the system header file locations.
;; ;; turn on automatic reparsing of open buffers in semantic
;; (global-semantic-idle-scheduler-mode 1)

;; (modern-c++-font-lock-global-mode 1)
;; ;; Set default indetation to 4 space
;; (setq-default c-basic-offset 4)

;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      ;; Add Google C++ Style checker.
;;      ;; In default, syntax checked by Clang and Cppcheck.
;;      (flycheck-add-next-checker 'c/c++-clang
;;                                 'c/c++-googlelint 'append)))
;; ;;------------------------------
;; ;;           LATEX
;; ;;------------------------------
;; (setq TeX-auto-save 1)
;; (setq TeX-parse-self 1)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX 1)
;; (setq LaTeX-item-indent 0)

;; ;; -------------------------
;; ;;  highlight-indent-guides
;; ;; -------------------------
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; ;; Set the display method
;; (setq highlight-indent-guides-method 'character)

;; ;; -------------------------
;; ;;  Multiline edit
;; ;; -------------------------
;; (require 'iedit)
;; ;; Fix iedit bug in ubuntu
;; (define-key global-map (kbd "C-c C-;") 'iedit-mode)


;; ;; User init.el ends here
;; ;;=====================================================
