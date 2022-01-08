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
    flycheck-pycheckers             ;;
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    ein                             ;; Emacs IPython Notebook
    iedit                           ;; multiple edic
    magit                           ;; for git
    highlight-indent-guides         ;; Highlight like vs code
    vscode-dark-plus-theme          ;; vscode-dark-theme
    material-theme                  ;; Theme
    auto-complete                   ;; autocomplete
    auto-complete-c-headers         ;;
    modern-cpp-font-lock            ;;
    google-c-style                  ;; Provides the google C/C++ coding style.
    flycheck-google-cpplint         ;; This is extension for Flycheck according to the Google C++ Style Guide
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
;; (load-theme 'material t)            ;; Load material theme
(load-theme 'vscode-dark-plus t)            ;; Load material theme
(global-linum-mode t)               ;; Enable line numbers globally

(setq-default cursor-type 'bar) ;; set cursor type to I
(setq-default indicate-empty-lines t) ;; show white lines
(setq-default show-trailing-whitespace t) ;; show only trailing white space
(setq whitespace-display-mappings '((space-mark 32 [?·])))
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; delete trailing white space before saving
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11")) ;; Change font style and size

(tool-bar-mode -1)     ;; Disable tool bar mode
(toggle-scroll-bar -1) ;; Scroll bar
(menu-bar-mode -1)     ;; Menubar


;;------------------------------
;;           c++
;;------------------------------
;; start auto-complete with emacs
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

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
		  "/usr/local/programfiles/trilinos/include"
		  "/usr/local/programfiles/or-tools/include")
		achead:include-directories))
  )
;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; turn on Semantic
(semantic-mode 1)

;; you can use system-include-path for setting up the system header file locations.
;; turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)

;; let's define a function which adds semantic as a suggestion backend to auto complete
;; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-cppcheck
                                'c/c++-googlelint 'append)))

;; Set default indetation to 4 space
(setq-default c-basic-offset 4)
(setq-default c++-basic-offset 4)
(modern-c++-font-lock-global-mode 1)
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

;; -------------------------
;;  highlight-indent-guides
;; -------------------------
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; Set the display method
(setq highlight-indent-guides-method 'character)

;; -------------------------
;;  Multiline edit
;; -------------------------
(require 'iedit)
;; Fix iedit bug in ubuntu
(define-key global-map (kbd "C-c C-;") 'iedit-mode)

;; ------------------------
;; Magit c-x g is not working
;; ------------------------
(define-key global-map (kbd "C-x g") 'magit-status)

;; ------------------------
;; Org mode
;; ------------------------
;; Enable transient mark mode
(transient-mark-mode 1)
(require 'org)

;; ---------------------------
;; Python setup
;; ---------------------------
(elpy-enable)

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; Enable fLycheck
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)
;; User init ends here
;; ===================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-google-cpplint google-c-style modern-cpp-font-lock auto-complete-c-headers auto-complete material-theme vscode-dark-plus-theme highlight-indent-guides iedit ein blacken py-autopep8 flycheck-pycheckers flycheck better-defaults yasnippet-snippets magit find-file-in-project elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
