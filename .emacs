;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;; Add MELPA.
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Disable menu and tool bar.
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Enable IDO mode.
(ido-mode 1)

;; whitespace-mode
(global-whitespace-mode 1)
(setq whitespace-line-column 250)
(setq whitespace-identation '(face default))
(global-set-key (kbd "C-c w") 'global-whitespace-mode)

;; Enable relative line numbers.
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Put emacs save files in a directory out of the way
;; and don't crete interlock files since I'm a single user.
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs_saves" t)))
(setq create-lockfiles nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" default)))
 '(org-agenda-files (quote ("~/STACK.org")))
 '(package-selected-packages
   (quote
    (haskell-mode hindent terraform-mode company format-all markdown-mode rg go-autocomplete eglot auto-complete gh-md envrc smartparens dockerfile-mode pinentry magit magit-gh-pulls magit-todos yaml-mode go-mode nix-mode nixpkgs-fmt gruber-darker-theme smex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq transient-default-level 5)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defun disable-global-whitespace-mode ()
  (global-whitespace-mode -1))
(add-hook 'ediff-mode-hook #'disable-global-whitespace-mode)

;; pinentry
(pinentry-start)
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

;; smartparens
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode #'smartparens-mode)
(add-hook 'jsonnet-mode #'smartparens-mode)

;; envrc
(envrc-global-mode)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done 'note)

;; go-mode
(add-hook 'go-mode-hook '(auto-complete-mode 1))
(add-hook 'go-mode-hook 'eglot-ensure)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(with-eval-after-load 'go-mode (require 'go-autocomplete))
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

;; jsonnet-mode
;; TODO: install this with nix.
(add-to-list 'load-path "~/ext/jdbaldry/jsonnet-mode")
(load "jsonnet-mode")
(add-hook 'jsonnet-mode-hook #'format-all-mode)

;; I'm not into tabs but I may be working with a project that requires them.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(defun infer-indentation-style ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; rg
(require 'rg)
(rg-enable-default-bindings)

;; font
(set-face-attribute 'default nil :font "JetBrains Mono-14")

;; nix-mode
(add-hook 'nix-mode-hook #'format-all-mode)

;; company-mode
(add-hook 'after-init-hook #'global-company-mode)

;; terraform-mode
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; folding (really selective-display)
(global-set-key (kbd "C-c f") 'toggle-selective-display)
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

;; Configure ligatures
(when (window-system)
  (set-frame-font "Jetbrains Mono"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; haskell-mode
(add-hook 'haskell-mode #'hindent-mode)
