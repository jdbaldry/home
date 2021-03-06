;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Start emacs server.
(server-start)

;; Add MELPA.
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

;; eglot
(require 'eglot)

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
(setq whitespace-line-column 160)
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
;; and don't create interlock files since I'm a single user.
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs_saves/" t)))
(setq create-lockfiles nil)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq transient-default-level 5)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defun disable-global-whitespace-mode ()
  "Disable 'whitespace-mode' everywhere."
  (global-whitespace-mode -1))
(add-hook 'ediff-mode-hook #'disable-global-whitespace-mode)

;; keychain-environment
(keychain-refresh-environment)

;; pinentry
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
(defun pinentry-emacs (desc prompt ok error)
  "Taken from https://github.com/ecraven/pinentry-emacs."
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
(pinentry-start)

;; smartparens
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode #'smartparens-mode)

;; typescript-mode
(require 'typescript-mode)
(setq typescript-indent-level 2)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Make sure the compilation buffer can handle colorized tsc output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; envrc
(envrc-global-mode)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-todo-keywords
      '((sequence "TODO" "PRGR" "DONE") (type "NOTD")))
(setq org-todo-keyword-faces '(("PRGR" . "orange") ("NOTD" . "blue")))
(setq org-log-done 'time)

(defun org-standup ()
  "Translate 'org-todo' entries into Slack standup message in kill ring."
  (interactive)
  (let ((org-link-regexp "\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\]"))
    (kill-new (string-join
               (org-map-entries
                '(format "- %s" (replace-regexp-in-string org-link-regexp
                                                          "<\\1\\|\\2>"
                                                          (org-entry-get (point) "ITEM")))
                t
                'region)
               "\n"))))

;; go-mode
(add-hook 'go-mode-hook 'auto-complete-mode)
(add-hook 'go-mode-hook 'eglot-ensure)
(setq gofmt-command "goimports")
(setq gofmt-args '("-local=github.com/grafana/backend-enterprise"))
(add-hook 'before-save-hook 'gofmt-before-save)

;; I'm not into tabs but I may be working with a project that requires them.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(defun infer-indentation-style ()
  "Infer whether a project uses spaces or tabs."
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(add-hook 'prog-mode-hook 'infer-indentation-style)

;; cue-mode
;; TODO: install this with nix.
(add-to-list 'load-path "~/ext/jdb/cue-mode")
(load "cue-mode")
(add-hook 'before-save-hook 'cue-format-before-save)

;; nix-mode
(add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))

;; format-all-mode
(add-hook 'prog-mode-hook #'format-all-mode)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)

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

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

;; ligatures
(global-fira-code-mode)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; perspective
(require 'perspective)

;; js2-mode (javascript)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))

;; graphviz-dot-mode
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 2))

(use-package company-graphviz-dot)
(put 'upcase-region 'disabled nil)

;; ivy/swiper/counsel
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-display-style 'fancy)
(setq ivy-use-virtual-buffers t)
(setq ivy-use-selectable-prompt t)
(setq enable-recursive-minibuffers t)
(setq search-default-mode #'char-fold-to-regexp)

;; org-roam
(setq org-roam-directory "~/zettelkasten")
(add-hook 'after-init-hook 'org-roam-mode)

;; Add delete to character function.
(require 'misc)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Increase font size.
(set-face-attribute 'default nil :height 140)

;; evil
(require 'evil)
(global-set-key (kbd "M-:") 'evil-ex)
(global-set-key (kbd "M-;") 'eval-expression)

;; org-pomodoro
(add-hook 'org-pomodoro-finished-hook '(message "Pomodoro complete!"))

;; modeline
(set-face-attribute 'mode-line nil :height 100)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; tetris
(defun disable-font-lock-mode () (font-lock-mode 0))
(add-hook 'tetris-mode-hook 'disable-font-lock-mode)

;; auth-source-pass
(require 'auth-source)
(require 'auth-source-pass)
(auth-source-pass-enable)

;; slack
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  (setq slack-render-image-p nil)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :token (auth-source-pass-get 'secret "grafana/raintank-corp.slack.com")
   :full-and-display-names t))
;; Redefine slack-image-block-element
;; from https://github.com/yuya373/emacs-slack/pull/532
(defun slack-image-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (image-height :initarg :image_height :type (or number null))
   (image-width :initarg :image_width :type (or number null))
   (image-bytes :initarg :image_bytes :type (or number null))))

;; (message (string-to-alphabet-emoji "test:smile:test" nil))
(defun string-to-alphabet-emoji (str white?)
  "Display the message STR as Slack alphabet emoji. WHITE? represents whether the character should be yellow (nil) or white (integer value)"

  (interactive "sMessage: \nP")
  ;; Taken from: https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
  (defun matches (regexp str)
    "Return a list of all regexp matches in STR"
    (let ((pos 0)
          matches)
      (while (string-match regexp str pos)
        (push (match-string 0 str) matches)
        (setq pos (match-end 0)))
      (reverse matches)))
  (message (mapconcat
            (lambda (token)
              (let ((color (if white? "yellow" "white"))
                    (emoji? (< 1 (length token))))
                (cond (emoji? token)
                      ((and (string< "A" token)
                            (string< token "z"))
                       (format ":alphabet-%s-%s:"
                               color
                               (downcase token)))
                      ((string-equal "!" token) (format ":alphabet-%s-exclamation:" color))
                      ((string-equal "?" token) (format ":alphabet-%s-question:" color))
                      ((string-equal "@" token) (format ":alphabet-%s-at:" color))
                      ((string-equal "#" token) (format ":alphabet-%s-hash:" color))
                      ((string-equal " " token) "   ")
                      (t token))))
            (matches "\\(:[a-z-_+-]+:\\|.\\)" str)
            "")))

;; flyspell
;; TODO configure (flyspell-auto-correct-word) and (flyspell-goto-next-error)
;; TODO: understand why some words are highlighted as being spelled incorrectly but ispell-word thinks they are fine.
;; (if (executable-find "aspell") (progn
;;                                 (setq ispell-program-name "aspell")
;;                                 (setq ispell-extra-args '("--camel-case" "--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))))
;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; org-agenda
(setq org-agenda-files '("~/org/"))

;; man
(setenv "MANPATH" (shell-command-to-string "manpath"))

;; compilation-mode
;; markdownlint-cli
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'markdownlint-cli)
(add-to-list 'compilation-error-regexp-alist-alist
             '(markdownlint-cli .
                                ("^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\) .*$"
                                 1 2 3)))

;; org-gcal
(require 'org-gcal)
(setq org-gcal-remove-api-cancelled-events t)

;; nyxt
(setq browse-url-generic-program (executable-find "nyxt"))
(setq browse-url-browser-function 'browse-url-generic)

;; org-babel
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

;; jsonnet-mode
(defun prettify-jsonnet()
  "Display some jsonnet keywords as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("function" . 955)))) ; λ
(add-hook 'jsonnet-mode-hook 'prettify-jsonnet)

;; origami-mode
(global-set-key (kbd "C-c C-i") 'origami-close-node)
(global-set-key (kbd "C-c C-u") 'origami-open-node)

;; flymake
;; (add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)

(provide 'emacs)
;;; .emacs ends here
