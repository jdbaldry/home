;;; .emacs --- Summary
;;; Commentary:
;;; Code:
(package-initialize)

;; Start emacs server.
(server-start)

;; Add MELPA.
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

;; Put emacs save files in a directory out of the way
;; and don't create interlock files since I'm a single user.
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs_saves/" t)))
(setq create-lockfiles nil)

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Also shrink fringes to 1 pixel.
(fringe-mode 1)

;; Enable ido-mode.
(ido-mode 1)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
(server-start)

;; exwm
(require 'exwm)
(require 'exwm-config)
(setenv "XDG_DATA_DIRS" (concat (getenv "XDG_DATA_DIRS") ":/home/jdb/.local/share/"))
(exwm-config-ido)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 4)

;; Enable the exwm systemtray.
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; Enable RandR support.
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(1 "eDP-1" 2 "DP-3"))
(defun autorandr ()
  "Change the monitor layout."
  (start-process-shell-command "autorandr" nil "autorandr --change"))
(add-hook 'exwm-randr-screen-change-hook 'autorandr)
(exwm-randr-enable)

;; Configure a logout function.
(defun exwm-logout ()
  "Log out of exwm."
  (interactive)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "pkill emacs"))

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
                                        ;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(defun exwm-rename-buffer ()
  "Add title to exwm buffer names.  From https://github.com/ch11ng/exwm/issues/198."
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50) exwm-title
             (concat (substring exwm-title 0 49) "...")))))
(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-d" to launch applications.
        ([?\s-d] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])
        ;; undo
        ([?\C-/] . [?\C-z])))

(exwm-enable)

;; browse-url
(setq browse-url-chromium-arguments '("--new-window"))

;; lsp-mode
(require 'lsp)
(require 'lsp-ui)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-file-watch-threshold 3000)

;; flycheck
(require 'flycheck)
;; Set flycheck to inherit the Emacs load path configured by Nix.
(setq flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook  #'flycheck-golangci-lint-setup)
(defvar-local flycheck-local-checkers nil)
(defun +flycheck-checker-get(fn checker property)
  "Return 'flycheck-local-checkers'[CHECKER][PROPERTY] or call FN.
FN is expected to be 'flycheck-checker-get'."
  (or (alist-get property (alist-get checker flycheck-local-checkers))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
(add-hook 'go-mode-hook (lambda()
                          (flycheck-golangci-lint-setup)
                          (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))

(defun display-buffer-window-below (buffer alist)
  "Display a reasonably sized buffer window below the current BUFFER.
ALIST is used by 'display-buffer-below-selected'."
  (let ((window (or (get-buffer-window buffer)
                    (display-buffer-below-selected buffer alist))))
    (when window
      (fit-window-to-buffer window 20 10)
      window)))
(add-to-list 'display-buffer-alist
             `(,(rx string-start (eval flycheck-error-list-buffer) string-end)
               (display-buffer-window-below . ((reusable-frames . t)))))

;; dap-mode
(require 'dap-mode)
(setq dap-print-io t)
(setq dap-auto-configure-features '(sessions locals controls tooltip))
(require 'dap-go)
;; (executable-find "dlv")

;; go-mode
(defun lsp-go-install-save-hooks ()
  "Hooks to run when saving a Go file."
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Disable menu and tool bar.
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Load theme.
(load-theme 'gruber-darker t)

;; Enable relative line numbers.
(require 'display-line-numbers)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; magit
(require 'magit)
(require 'transient)
(global-set-key (kbd "C-x g") 'magit-status)
(setq transient-default-level 5)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; forge
(with-eval-after-load 'magit
  (require 'forge))

;; whitespace-mode
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-line-column 160)
;; From: https://emacs.stackexchange.com/questions/38771/magit-status-does-not-open-when-using-global-whitespace-mode-1/38779.
(with-eval-after-load 'whitespace
  (add-function :before-while whitespace-enable-predicate
                (lambda ()
                  (not (derived-mode-p #'magit-mode #'shell-mode)))))

;; ediff
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defun disable-global-whitespace-mode ()
  "Disable 'whitespace-mode' everywhere."
  (global-whitespace-mode -1))
(add-hook 'ediff-mode-hook #'disable-global-whitespace-mode)
(setq whitespace-style '(face trailing tabs lines lines-tail newline indentation space-after-tab empty space-before-tab tab-mark newline-mark))
;; keychain-environment
(keychain-refresh-environment)

;; pinentry
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
(defun pinentry-emacs (desc prompt)
  "Taken from https://github.com/ecraven/pinentry-emacs.
DESC explains to the user what the password is required for.
PROMPT is used as the prompt to user when reading the password."
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
(pinentry-start)

;; smartparens
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode #'smartparens-mode)

;; typescript-mode
(require 'typescript-mode)
(setq typescript-indent-level 2)

;; org-mode
(require 'org)
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

;; I'm not into tabs but I may be working with a project that requires them.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(defun infer-indentation-style ()
  "Infer whether a project is indented with spaces or tabs."
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(add-hook 'prog-mode-hook 'infer-indentation-style)

;; nix-mode
(add-hook 'nix-mode-hook 'lsp)

;; (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))

;; format-all-mode
(add-hook 'prog-mode-hook #'format-all-mode)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)

;; company-mode
(add-hook 'after-init-hook #'global-company-mode)

;; terraform-mode
(require 'terraform-mode)
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; folding (really selective-display)
(global-set-key (kbd "C-c f") 'toggle-selective-display)
(defun toggle-selective-display (column)
  "Toggle folding with 'selective-display'.
COLUMN controls how deeply the display is folded."
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

;; haskell-mode
(require 'haskell)

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
  "Load magit for YADM."
  (interactive)
  (magit-status-setup-buffer "/yadm::"))
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
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
(require 'ivy)
(require 'swiper)
(require 'counsel)
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-display-style 'fancy)
(setq ivy-use-virtual-buffers t)
(setq ivy-use-selectable-prompt t)
(setq enable-recursive-minibuffers t)
(setq search-default-mode #'char-fold-to-regexp)

;; org-roam
;; (setq org-roam-directory "~/zettelkasten")
;; (add-hook 'after-init-hook 'org-roam-mode)

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
(defun disable-font-lock-mode () "Disable 'font-lock-mode' as it breaks tetris." (font-lock-mode 0))
(add-hook 'tetris-mode-hook 'disable-font-lock-mode)

;; auth-source-pass
(require 'auth-source)
(require 'auth-source-pass)
(setq auth-sources '("~/.authinfo" password-store))

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

;; (message (string-to-alphabet-emoji "test:smile:test" nil))
(defun string-to-alphabet-emoji (str white?)
  "Display the message STR as Slack alphabet emoji.  WHITE? represents whether the character should be yellow (nil) or white (integer value)."
  (interactive "sMessage: \nP")

  ;; Taken from: https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
  (let ((find-matches (lambda (regexp str)
                        "Return a list of all matches of REGEXP in STR."
                        (let ((pos 0) matches)
                          (while (string-match regexp str pos)
                            (push (match-string 0 str) matches)
                            (setq pos (match-end 0)))
                          (reverse matches)))))
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
              (funcall find-matches "\\(:[a-z-_+-]+:\\|.\\)" str)
              ""))))

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

;; org-babel
(require 'ob-async)
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;; jsonnet-mode
(setq jsonnet-command "jsonnet-tool")
(setq jsonnet-command-options '("eval"))
(flycheck-define-checker jsonnet
  "A Jsonnet syntax checker using jsonnet-tool.

See URL `https://jsonnet.org'."
  :command ("jsonnet-tool" "eval" source-inplace)
  :error-patterns
  ((error line-start "STATIC ERROR: " (file-name) ":"
          (or (seq line ":" column (zero-or-one (seq "-" end-column)))
              (seq "(" line ":" column ")" "-"
                   "(" end-line ":" end-column ")"))
          ": " (message) line-end)
   (error line-start "RUNTIME ERROR: " (message) "\n"
          (? "\t" (file-name) ":" ;; first line of the backtrace
             (or (seq line ":" column (zero-or-one (seq "-" end-column)))
                 (seq "(" line ":" column ")" "-"
                      "(" end-line ":" end-column ")"))))
   ;; file-name:(line:column)-(end-line:end-column) Computed imports are not allowed
   (error line-start
          (file-name) ":"
          (or (seq line ":" column (zero-or-one (seq "-" end-column)))
              (seq "(" line ":" column ")" "-"
                   "(" end-line ":" end-column ")"))
          (message)
          line-end))
  :error-filter
  (lambda (errs)
    ;; Some errors are missing line numbers. See URL
    ;; `https://github.com/google/jsonnet/issues/786'.
    (dolist (err errs)
      (unless (flycheck-error-line err)
        (setf (flycheck-error-line err) 1)))
    (flycheck-sanitize-errors errs))
  :modes jsonnet-mode)
(defun prettify-jsonnet()
  "Display some jsonnet keywords as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("function" . ?λ)
          (".o" . ?​) ;; Note this is a zero width space.
          ("_0: " . ?​) ;; Note this is a zero width space.
          ("_1: " . ?​) ;; Note this is a zero width space.
          ("_2: " . ?​) ;; Note this is a zero width space.
          ("_3: " . ?​) ;; Note this is a zero width space.
          ("_4: " . ?​) ;; Note this is a zero width space.
          ("_5: " . ?​) ;; Note this is a zero width space.
          )))
(add-hook 'jsonnet-mode-hook 'prettify-jsonnet)
;; (add-to-list 'lsp-language-id-configuration '(jsonnet-mode . "jsonnet"))
;; (lsp-register-client
;;  (make-lsp-client
;;   :new-connection (lsp-tcp-connection (lambda(port) `("jsonnet-language-server" "-m" "tcp" "-a" ,(format ":%d" port))))
;;   ;;:new-connection (lsp-stdio-connection '("jsonnet-language-server" "-m" "stdio"))
;;   :major-modes '(jsonnet-mode)
;;   :server-id 'jsonnet))

;; origami-mode
(global-set-key (kbd "C-c C-i") 'origami-close-node)
(global-set-key (kbd "C-c C-u") 'origami-open-node)

;; direnv
(require 'direnv)
(direnv-mode)

;; sh-mode
(defun sh-set-indent () "Set 'sh-mode' indent." (setq tab-width 2))
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)
(add-hook 'sh-mode-hook 'sh-set-indent)

;; browser
(setq browse-url-browser-function 'browse-url-chromium)

;; (open-on-github)
(defun open-on-github(project)
  "Open the current file in GitHub.

PROJECT is the Github repository owner."
  (interactive "sProject: \n")
  (let ((url "https://github.com")
        (repo (car (last (delete "" (split-string (projectile-project-root) "/")))))
        (ref (shell-command-to-string "git rev-parse HEAD"))
        (file (string-remove-prefix (projectile-project-root) (buffer-file-name)))
        (line (line-number-at-pos)))
    (browse-url (format "%s/%s/%s/tree/%s/%s#L%s" url project repo ref file line))))

(defun open-in-zendesk(id)
  "Open a Zendesk ticket.  ID is the Zendesk ticket number."
  (interactive "sID: \n")
  (let ((url "https://grafana.zendesk.com/agent/tickets"))
    (browse-url (format "%s/%s" url id))))

;; modeline
(require 'battery)
(setq display-time-day-and-date t)
(display-time)
(setq battery-mode-line-format " [BAT %b%p%% %t]")
(display-battery-mode)

;; ansi-color
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

;;; mue4e
(with-eval-after-load 'f
  (let ((mu4epath
         (concat
          (f-dirname
           (file-truename
            (executable-find "mu")))
          "/../share/emacs/site-lisp/mu4e")))
    (when (and
           (string-prefix-p "/nix/store/" mu4epath)
           (file-directory-p mu4epath))
      (add-to-list 'load-path mu4epath))))
(require 'mu4e)
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Grafana"
          :enter-func (lambda ()
                        (mu4e-message "Entering Grafana context")
                        (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                          (revert-buffer)))
          :leave-func (lambda ()
                        (mu4e-message "Leaving Grafana context")
                        (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                          (revert-buffer)))
          :match-func (lambda (msg)
                        (when msg
                          (or (mu4e-message-contact-field-matches msg :to "jack.baldry@grafana.com")
                              (mu4e-message-contact-field-matches msg :from "jack.baldry@grafana.com")
                              (mu4e-message-contact-field-matches msg :cc "jack.baldry@grafana.com")
                              (mu4e-message-contact-field-matches msg :bcc "jack.baldry@grafana.com"))))
          :vars '((user-full-name . "Jack Baldry")
                  (user-mail-address . "jack.baldry@grafana.com")
                  (mu4e-compose-signature . (format "Cheers,\n\njdb"))
                  (mu4e-get-mail-command . "mbsync grafana")
                  (mu4e-maildir-shortcuts . ((:maildir "/grafana/Archive" :key ?a)))
                  (mu4e-bookmarks .
                                  ((:name  "Unread messages"
                                           :query "maildir:/grafana/Inbox AND flag:unread AND NOT flag:trashed"
                                           :key ?u)
                                   (:name "Archive"
                                          :query "maildir:/grafana/Archive"
                                          :key ?a)
                                   (:name "Last 2 days"
                                          :query "maildir:/grafana/Inbox AND date:2d..now AND NOT flag:trashed"
                                          :key ?t)
                                   (:name "Last 7 days"
                                          :query "maildir:/grafana/Inbox AND date:7d..now AND NOT flag:trashed"
                                          :hide-unread t
                                          :key ?w)
                                   (:name "Deleted"
                                          :query "flag:trashed"
                                          :key ?d)))))
        ,(make-mu4e-context
          :name "gmail"
          :enter-func (lambda ()
                        (mu4e-message "Entering gmail context")
                        (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                          (revert-buffer)))
          :leave-func (lambda ()
                        (mu4e-message "Leaving gmail context")
                        (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                          (revert-buffer)))
          :match-func (lambda (msg)
                        (when msg
                          (or (mu4e-message-contact-field-matches msg :to "jdbaldry@gmail.com")
                              (mu4e-message-contact-field-matches msg :from "jdbaldry@gmail.com")
                              (mu4e-message-contact-field-matches msg :cc "jdbaldry@gmail.com")
                              (mu4e-message-contact-field-matches msg :bcc "jdbaldry@gmail.com"))))
          :vars '((user-full-name . "Jack Baldry")
                  (user-mail-address . "jdbaldry@gmail.com")
                  (mu4e-compose-signature . (format "Cheers,\n\njdb"))
                  (mu4e-get-mail-command . "mbsync gmail")
                  (mu4e-maildir-shortcuts . ((:maildir "/gmail/Archive" :key ?a)))
                  (mu4e-bookmarks .
                                  ((:name  "Unread messages"
                                           :query "maildir:/gmail/Inbox AND flag:unread AND NOT flag:trashed"
                                           :key ?u)
                                   (:name "Archive"
                                          :query "maildir:/gmail/Archive"
                                          :key ?a)
                                   (:name "Last 2 days"
                                          :query "maildir:/gmail/Inbox AND date:2d..now AND NOT flag:trashed"
                                          :key ?t)
                                   (:name "Last 7 days"
                                          :query "maildir:/gmail/Inbox AND date:7d..now AND NOT flag:trashed"
                                          :hide-unread t
                                          :key ?w)
                                   (:name "Deleted"
                                          :query "flag:trashed"
                                          :key ?d)))))
        ,(make-mu4e-context
          :name "fastmail"
          :enter-func (lambda ()
                        (mu4e-message "Entering fastmail context")
                        (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                          (revert-buffer)))
          :leave-func (lambda ()
                        (mu4e-message "Leaving fastmail context")
                        (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                          (revert-buffer)))
          :match-func (lambda (msg)
                        (when msg
                          (or (mu4e-message-contact-field-matches msg :to "jdbaldry@fastmail.com")
                              (mu4e-message-contact-field-matches msg :from "jdbaldry@fastmail.com")
                              (mu4e-message-contact-field-matches msg :cc "jdbaldry@fastmail.com")
                              (mu4e-message-contact-field-matches msg :bcc "jdbaldry@fastmail.com"))))
          :vars '((user-full-name . "Jack Baldry")
                  (user-mail-address . "jdbaldry@fastmail.com")
                  (mu4e-compose-signature . (format "Cheers,\n\njdb"))
                  (mu4e-get-mail-command . "mbsync fastmail")
                  (mu4e-maildir-shortcuts . ((:maildir "/fastmail/Archive" :key ?a)))
                  (mu4e-bookmarks . ((:name "Unread messages"
                                            :query "maildir:/fastmail/Inbox AND flag:unread AND NOT flag:trashed"
                                            :key ?u)
                                     (:name "Archive"
                                            :query "maildir:/fastmail/Archive"
                                            :key ?a)
                                     (:name "Last 2 days"
                                            :query "maildir:/fastmail/Inbox AND date:2d..now AND NOT flag:trashed"
                                            :key ?t)
                                     (:name "Last 7 days"
                                            :query "maildir:/fastmail/Inbox AND date:7d..now AND NOT flag:trashed"
                                            :hide-unread t
                                            :key ?w)
                                     (:name "Deleted"
                                            :query "flag:trashed"
                                            :key ?d)))))))
(setq send-mail-function 'sendmail-send-it)
(setq sendmail-program (executable-find "msmtp"))
(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-headers-date-format "%F")
(setq mu4e-headers-time-format "%H:%M:%S")
(setq mu4e-headers-include-related nil)

;; shell-mode
;; (setq shell-file-name "scsh")
;; (setq explicit-scsh-args '("--"))
;; (defun add-mode-line-dirtrack ()
;;   "From https://www.emacswiki.org/emacs/ModeLineDirtrack."
;;   (add-to-list 'mode-line-buffer-identification
;;                '(:propertize (" " default-directory " ") face dired-directory)))
;; (add-hook 'shell-mode-hook 'add-mode-line-dirtrack)
;;
;; (defun scsh-doc ()
;;   "Browse the scsh documentation."
;;   (interactive)
;;   (browse-url "https://scsh.net/docu/html/man.html"))
;;

;; safe-local-variable-values
(put 'dap-go-delve-path 'safe-local-variable #'stringp)
(put 'lsp-go-build-flags 'safe-local-variable #'vectorp)

(provide 'emacs)
;;; .emacs ends here
