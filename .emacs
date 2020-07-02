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

;; Enable whitespace mode.
(global-whitespace-mode 1)

;; Enable relative line numbers.
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Put emacs save files in a directory out of the way.
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

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
 '(package-selected-packages
   (quote
    (go-autocomplete eglot auto-complete gh-md envrc smartparens dockerfile-mode pinentry magit magit-gh-pulls magit-todos yaml-mode go-mode nix-mode nixpkgs-fmt gruber-darker-theme smex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; pinentry
(pinentry-start)

;; smartparens
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode #'smartparens-mode)

;; envrc
(envrc-global-mode)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

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
