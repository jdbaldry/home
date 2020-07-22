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

;; Load theme.
(load-theme 'gruber-darker t)

;; Enable IDO mode.
(ido-mode 1)

;; whitespace-mode
(global-whitespace-mode 1)
(setq whitespace-line-column 250)
(setq whitespace-identation '(face default))
;; From: https://emacs.stackexchange.com/questions/38771/magit-status-does-not-open-when-using-global-whitespace-mode-1/38779.
(with-eval-after-load 'whitespace
  (add-function :before-while whitespace-enable-predicate
                (lambda ()
                  (not (derived-mode-p #'magit-mode #'shell-mode)))))

;; Enable relative line numbers.
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Put emacs save files in a directory out of the way
;; and don't crete interlock files since I'm a single user.
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs_saves" t)))
(setq create-lockfiles nil)

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

(set-face-attribute 'default nil :font "Fira Code-14")

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

;; haskell-mode
(add-hook 'haskell-mode #'hindent-mode)

;; YADM
;; From: https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/
;; Invoke magit with: (magit-status "/yadm::")
(require 'tramp)
(add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env (("SHELL") ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))
(defun yadm ()
  (interactive)
  (magit-status "/yadm::"))
(global-set-key (kbd "C-c y") #'yadm)

;; Move lines up and down.
;; From: https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
(indent-according-to-mode))
(global-set-key (kbd "M-n") #'move-line-down)
(global-set-key (kbd "M-p") #'move-line-up)
