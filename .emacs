;;; .emacs --- Init file

;;; Commentary:
;;; Code:
;; From: https://blog.d46.us/advanced-emacs-startup/
;; To test "best possible" startup time
;; emacs -q --eval='(message "%s" (emacs-init-time))'
;; Emacs ready in 2.00 seconds with 24 garbage collections.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Require use-package which is used to manage all other packages.
(eval-when-compile
  (require 'use-package)
  ;; Disabling as this significantly slows startup.
  ;; (setq use-package-always-ensure t)
  (setq use-package-expand-minimally byte-compile-current-file))

;; Save and restore Emacs session.
(use-package desktop
  :config
  (setq desktop-save 1
        desktop-load-locked-desktop t
        desktop-dirname user-emacs-directory)
  (desktop-save-mode 1))

;; Unbind the jdb function that interferes with my namespacing.
(fmakunbound 'jdb)

(defgroup jdb nil "Personal group."
  :group 'emacs
  :version "27"
  :prefix "jdb/")

(defcustom jdb/slack-status--collection nil
  "Collection of emoji strings useful in Slack statuses."
  :type '(repeat string)
  :group 'jdb)

(defcustom jdb/co-authored-by--collection nil
  "Collection of author strings."
  :type '(repeat string)
  :group 'jdb)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(column-number-mode t)
 '(exwm-input-global-keys
   '(([8388722]
      . exwm-reset)
     ([8388727]
      . exwm-workspace-switch)
     ([8388656]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 0))
     ([8388657]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 1))
     ([8388658]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 2))
     ([8388659]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 3))
     ([8388660]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 4))
     ([8388708]
      lambda
      (command)
      (interactive
       (list
        (read-shell-command "$ ")))
      (start-process-shell-command command nil command))
     ([s-tab]
      . jdb/switch-to-last-buffer)))
 '(jdb/co-authored-by--collection
   '("achatterjee-grafana <70489351+achatterjee-grafana@users.noreply.github.com>" "Brenda Muir <brenda.muir@grafana.com>" "Garrett Guillotte <garrett.guillote@grafana.com>" "Matt Dodson <MattDodsonTeacher@gmail.com>" "Martin Disibio <martin.disibio@grafana.com>" "Miguel Ángel Ortuño <ortuman@gmail.com>" "Patrick Oyarzun <patrick.oyarzun@grafana.com>" "George Krajcsovits <krajorama@users.noreply.github.com>" "Fiona Artiaga <89225282+GrafanaWriter@users.noreply.github.com>" "eleijonmarck <eric.leijonmarck@gmail.com>" "Karen Miller <karen.miller@grafana.com>" "Dimitar Dimitrov <dimitar.dimitrov@grafana.com>" "Bryan Boreham <bryan@weave.works>" "Gilles De May <gilles.de.mey@gmail.com>" "Peter Štibraný <peter.stibrany@grafana.com>" "Chris Moyer <chris.moyer@grafana.com>" "Nick Pillitteri <nick.pillitteri@grafana.com>" "Archie Baldry <archiebaldry@gmail.com>" "Marco Pracucci <marco@pracucci.com>" "replay <mauro.stettler@gmail.com>" "Jennifer Villa <jen.villa@grafana.com>" "Ursula Kallio <ursula.kallio@grafana.com>"))
 '(jdb/slack-status--collection
   '(":tada:" ":headphones:" ":tomato:" ":spiral_note_pad:" ":grafana:" ":docker:" ":coffee:" ":notifications:" ":tooth:" ":helm4:" ":smile:" ":slack:" ":hotel:" ":gem-metrics:" ":face_with_thermometer:" ":sick:" ":mimir:" ":airplane:" ":eyes:" ":calendar:" ":writing_hand:" ":sleuth_or_spy:" ":tea:" ":email:" ":github:" ":reading:"))
 '(markdown-filename-translate-function 'markdown-relref-translate)
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda ""
               ((org-agenda-span 1)))
       (alltodo "" nil))
      nil)))
 '(org-capture-templates
   '(("t" "Add a TODO to the current day" entry
      (file jdb/org-file)
      "* TODO %?")
     ("n" "Add a TODO to the next org file." entry
      (file
       (lambda nil
         (jdb/org-file
          (jdb/next-working-day))))
      "* TODO %?")
     ("p" "Add a TODO to the personal org file." entry
      (file "~/org/personal.org")
      "* TODO %?")
     ("y" "Add a YouTube video to the watchlist." entry
      (file "~/org/youtube.org")
      "** %?")))
 '(warning-suppress-log-types '(((fira-code-ligatures))))
 '(zoneinfo-style-world-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/UTC" "UTC")
     ("America/Chicago" "Chicago")
     ("America/Colorado" "Colorado"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(setq save-interprogram-paste-before-kill t)

(setenv "XDG_DATA_DIRS" (concat (getenv "XDG_DATA_DIRS") ":/home/jdb/.local/share/"))

;; exwm
(require 'exwm)
;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;;   all windows are probably the same.  Using window titles for them makes
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
(add-hook 'exwm-update-class-hook #'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook #'exwm-rename-buffer)

;; Enable the exwm systemtray.
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(exwm-enable)

;; Configure a logout function.
(use-package recentf
  :commands (jdb/exwm-logout recentf-save-list)
  :config
  (defun jdb/exwm-logout ()
    "Log out of exwm."
    (interactive)
    (recentf-save-list)
    (save-some-buffers)
    (save-buffers-kill-emacs)))

(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

(defun jdb/switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; browse-url
(setq browse-url-chromium-arguments '("--new-window"))

;; lsp-mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (go-mode . jdb/lsp-go-install-save-hooks)
  (nix-mode . lsp-deferred)
  (jsonnet-mode . lsp-deferred)
  :config
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-file-watch-threshold 3000)
  (setq lsp-auto-guess-root t)
  (add-to-list 'lsp-language-id-configuration '(jsonnet-mode . "jsonnet"))
  (defun jdb/lsp-go-install-save-hooks ()
    "Hooks to run when saving a Go file."
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () lsp-jsonnet-executable))
    :activation-fn (lsp-activate-on "jsonnet")
    :server-id 'jsonnet)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-lens
  :commands lsp-avy-lens
  :config
  (defun go-run-test ()
    "Run go test where indicated by an LSP codelens."
    (interactive)
    (save-some-buffers)
    (lsp-avy-lens)))

;; flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  ;; Set flycheck to inherit the Emacs load path configured by Nix.
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-golangci-lint
  :hook
  (flycheck-mode . flycheck-golangci-lint-setup)
  :config
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    "Return 'flycheck-local-checkers'[CHECKER][PROPERTY] or call FN.
FN is expected to be 'flycheck-checker-get'."
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  (add-hook 'go-mode-hook (lambda()
                            (flycheck-golangci-lint-setup)
                            (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))

(defun jdb/display-buffer-window-below (buffer alist)
  "Display a reasonably sized buffer window below the current BUFFER.
ALIST is used by 'display-buffer-below-selected'."
  (let ((window (or (get-buffer-window buffer)
                    (display-buffer-below-selected buffer alist))))
    (when window
      (fit-window-to-buffer window 20 10)
      window)))
(add-to-list 'display-buffer-alist
             `(,(rx string-start (eval flycheck-error-list-buffer) string-end)
               (jdb/display-buffer-window-below . ((reusable-frames . t)))))

;; dap-mode
(use-package dap-mode
  :config
  (setq dap-print-io t)
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package dap-dlv-go :after dap-mode :mode "\\.go\\'")
;; (executable-find "dlv")

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Disable menu and tool bar.
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Load theme.
(load-theme 'modus-vivendi t)
(set-face-attribute 'mode-line nil  :height 100)

;; Enable relative line numbers.
(use-package display-line-numbers
  :init
  (global-display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;; magit
(use-package magit
  :init
  (global-set-key (kbd "C-x g") #'magit-status)
  ;; difftastic
  ;; From https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
  (defun th/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc event)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))
  (defun th/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (th/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))
  (defun th/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (th/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))
  (transient-define-prefix jdb/add ()
    (interactive)
    (ivy-read "Add file: "
              #'(lambda (_ _ _)
                  (let ((prune '(games .yarn .runelite ext .npm .Garmin Maildir .emacs_saves node_modules Slack BraveSoftware .zoom retroarch Code emojis elpa CacheStorage .git chromium go .cache .mozilla .kube .local nix vendor)))
                    (split-string (shell-command-to-string (concat "find " (projectile-project-root) " " (string-join (mapcar (lambda (base) (format "-name '%s' -prune -o" base)) prune) " ") " -type f -print")) "\n")))
              :action #'(lambda (file) (magit-run-git "add" "--" file))
              :caller 'jdb/add))
  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("a" "Add" jdb/add)
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" th/magit-aux-commands))
  (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)
  :commands (magit-status magit-display-buffer-same-window-except-diff-v1)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package transient
  :after magit
  :config
  (setq transient-default-level 5))

;; forge
(use-package forge :after (magit transient))

;; nixos
(defun jdb/nixos-flake-update (directory)
  "Update the system configuration flake.
DIRECTORY configures which directory to update the flake in."
  (interactive "sDirectory: \n")
  (let ((compilation-buffer-name-function (lambda (_) "*jdb/nixos-flake-update*")))
    (compile (format "cd %s && nix flake update" (if (string-blank-p directory)  "~/.config/nixos" directory)))))

(defun jdb/nixos-rebuild ()
  "Rebuild and switch to the new generation."
  (interactive)
  (let ((compilation-buffer-name-function (lambda (_) "*jdb/nixos-rebuild*")))
    (compile "cd ~/.config/nixos && nix flake lock --update-input jdb && sudo nixos-rebuild switch --flake ~/.config/nixos" t)))

(defun jdb/nix-collect-garbage ()
  "Collect Nix garbage."
  (interactive)
  (let ((compilation-buffer-name-function (lambda (_) "*jdb/nix-collect-garbage*")))
    (compile "sudo nix-collect-garbage" t)))

(defun jdb/co-authored-by--grep-authors (regexp)
  "Find all authors in the git log that match REGEXP."
  (interactive "sRegexp: \n")
  (with-current-buffer (generate-new-buffer "grep-authors")
    (switch-to-buffer (current-buffer))
    (start-process-shell-command "grep-authors" (current-buffer) (format "git log | grep %s | sort -u" regexp))))

(defun jdb/co-authored-by ()
  "Add a Co-authored-by line to a commit message."
  (interactive)
  (let ((tag "Co-authored-by: "))
    (ivy-read tag
              (lambda (&rest _) jdb/co-authored-by--collection)
              :action (lambda (author)
                        (customize-set-variable 'jdb/co-authored-by--collection
                                                (add-to-list 'jdb/co-authored-by--collection author))
                        (customize-save-customized)
                        (insert (concat tag author)))
              :caller 'co-authored-by)))

;; whitespace-mode
(use-package whitespace
  :after magit
  :init
  (global-whitespace-mode 1)
  :config
  (setq whitespace-line-column 160)
  (setq whitespace-style '(face trailing tabs lines lines-tail newline indentation space-after-tab empty space-before-tab tab-mark newline-mark))
  ;; From: https://emacs.stackexchange.com/questions/38771/magit-status-does-not-open-when-using-global-whitespace-mode-1/38779.
  (add-function :before-while whitespace-enable-predicate
                (lambda ()
                  (not (derived-mode-p #'magit-mode #'shell-mode)))))

;; shell-mode
;; Enable bash completion.
;; From https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/.
(use-package company-native-complete
  :hook
  (shell-mode . (lambda () (setq comint-prompt-regex "^.+[$%>] ")))
  :config
  (add-to-list 'company-backends 'company-native-complete)
  (with-eval-after-load 'shell
    (native-complete-setup-bash)))

;; ediff
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (defun jdb/disable-global-whitespace-mode ()
    "Disable 'whitespace-mode' everywhere."
    (global-whitespace-mode -1))
  (add-hook 'ediff-mode-hook #'jdb/disable-global-whitespace-mode))

;; keychain-environment
(use-package keychain-environment :init (keychain-refresh-environment))

;; pinentry
(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (defun pinentry-emacs (desc prompt)
    "Taken from https://github.com/ecraven/pinentry-emacs.
DESC explains to the user what the password is required for.
PROMPT is used as the prompt to user when reading the password."
    (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
      str))
  (pinentry-start))

;; smartparens
;; (use-package smartparens
;;   :mode "\\.el\\'"
;;   ;; origami-mode
;;   :bind
;;   (("C-c C-i" . origami-close-node)
;;    ("C-c C-u" . origami-open-node)))

;; typescript-mode
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

;; org-mode
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-l u" . jdb/urgent)
   ("C-l i" . jdb/important))
  :commands (jdb/org-30m jdb/org-1h jdb/org-insert-link-with-title)
  :hook
  (org-clock-in . jdb/org-slack-status)
  (org-clock-in . (lambda () (org-todo "PRGR")))
  (org-clock-out . (lambda () (jdb/slack-status "" "")))
  :config
  (setq org-startup-folded t)
  (setq org-adapt-indentation nil)
  (setq org-todo-keywords
        '((sequence "TODO" "PRGR" "DONE") (type "NOTD")))
  (setq org-todo-keyword-faces '(("PRGR" . "orange") ("NOTD" . "blue")))
  (setq org-log-done 'time)

  (defun jdb/org-30m () "Update effort to 30 minutes." (interactive) (org-set-effort nil "0:30"))
  (org-defkey org-mode-map (kbd "C-c C-x 3") #'jdb/org-30m)

  (defun jdb/org-1h () "Update effort to one hour." (interactive) (org-set-effort nil "1:00"))
  (org-defkey org-mode-map (kbd "C-c C-x 1") #'jdb/org-1h)
  (defun jdb/org-insert-link-with-title (url)
    "Insert URL with a description from the title."
    (interactive "sURL: \n")
    (org-insert-link nil url (jdb/tag-for-url url 'title))))

(defun jdb/org-timetable ()
  "Append a time table to the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-max))
    (insert (string-join '("* Total"
                           "  #+COLUMNS: %ITEM %TODO %4EFFORT(EST){:} %CLOCKSUM(ACT)"
                           "  #+BEGIN: columnview :hlines 1 :id global"
                           "  | ITEM | TODO |  EST | ACT |"
                           "  #+TBLFM:@>$3=vsum(@2..@-1);T::@>$4=vsum(@2..@-1);T"
                           "  #+END:") "\n"))))

(use-package request)
(defvar jdb/slack-api-url "https://slack.com/api")
(defun jdb/slack-url-post (endpoint data &optional callback)
  "Make a POST request to Slack.
ENDPOINT is a Slack RPC endpoint such as users.profile.set.
DATA is the request body.
CALLBACK is called on completion."
  (request (format "%s/%s" jdb/slack-api-url endpoint)
    :type "POST"
    :headers `(("Content-Type" . "application/json; charset=utf-8")
               ("Authorization" . ,(concat "Bearer " (auth-source-pass-get 'secret "grafana/raintank-corp.slack.com"))))
    :data data
    :parser 'json-read
    :complete (or callback (cl-function
                            (lambda (&key response &allow-other-keys)
                              (message "%s: %s"
                                       (request-response-status-code response)
                                       (request-response-data response)))))))

(defun jdb/slack-standup (text)
  "Post a standup message TEXT to the standup channel."
  (interactive "sText: \n")
  (jdb/slack-url-post "chat.postMessage"
                      (json-encode
                       `((channel . "C039JG5NDLP")
                         (text . ,text)))))

(defun jdb/slack-react (channel timestamp name)
  "React to the TIMESTAMP in CHANNEL with emoji identified by NAME."
  (jdb/slack-url-post "react.add"
                      (json-encode
                       `((channel . ,channel)
                         (timestamp . ,timestamp)
                         (name . ,name)))))

(defun jdb/slack-react-callback (channel timestamp text)
  "Return a 'cl-function' that can be used as a request callback.
The callback reacts to the TIMESTAMP message in CHANNEL with the
alphabet emoji of the first character in TEXT."
  (lambda (&key response &allow-other-keys)
    (if (> (length text) 1)
        (funcall (jdb/slack-react-callback channel timestamp (substring text 1)) :response response :other-keys)
      (jdb/slack-react channel timestamp (string-to-char text)))))

(defun jdb/slack-post-with-react (text channel &optional message)
  "Post TEXT to CHANNEL and react with TEXT alphabet emoji.
If MESSAGE is non-nil, post that instead of TEXT."
  (interactive "sText: \nsChannel: \n")
  (jdb/slack-url-post
   "chat.postMessage"
   (json-encode
    `((channel . ,channel)
      (text . ,text)))
   (cl-function (lambda (&key response &allow-other-keys)
                  (let ((channel (alist-get 'channel (request-response-data response)))
                        (timestamp (alist-get 'ts (request-response-data response)))
                        (text (alist-get 'text (alist-get 'message (request-response-data response)))))
                    (funcall (jdb/slack-react-callback channel timestamp text) :response response :other-keys))))))

(defvar jdb/slack-status-last-emoji nil "Last emoji used in a Slack status API request.")
(defun jdb/slack-status (text &optional emoji)
  "Update Slack status.  TEXT is the status message.  EMOJI is the status emoji."
  (interactive "sText: \n")
  (let ((emoji (or emoji
                   (ivy-read "Emoji: "
                             (lambda (&rest _) jdb/slack-status--collection)
                             :action (lambda (emoji) (setq jdb/slack-status-last-emoji emoji))
                             :caller 'jdb/slack-status))))
    (jdb/slack-url-post
     "users.profile.set"
     (json-encode
      `(("profile" . (("status_text" . ,text) ("status_emoji" . ,emoji)))))
     (cl-function
      (lambda (&key response &allow-other-keys)
        (if (and
             (equal (request-response-status-code response) 200)
             (equal (alist-get 'ok (request-response-data response)) :json-false)
             jdb/slack-status-last-emoji)
            (customize-set-variable
             'jdb/slack-status--collection
             (add-to-list 'jdb/slack-status--collection jdb/slack-status-last-emoji))
          (customize-save-customized)))))
    (jdb/slack-url-post
     "users.setPresence"
     (json-encode `(("presence" . ,(if (string-empty-p text) "auto" "away")))))))

(defun jdb/slack-clear ()
  "Clear Slack status."
  (interactive)
  (jdb/slack-status "" ""))

(defun jdb/slack-status-with-time (text)
  "Update Slack status with TEXT formatted with the current time.
'%s' should be used for the text substition."
  (interactive "sText: \n")
  (funcall-interactively 'jdb/slack-status (format text (format-time-string "%H:%M %Z"))))

(defun jdb/slack-tea ()
  "Update Slack status to reflect the fact I am making a cup of tea."
  (interactive)
  (jdb/slack-status (format "started making tea at %s, back in five minutes" (format-time-string "%H:%M %Z")) ":tea:"))

(defun jdb/slack-long-pomodoro ()
  "Update Slack status to reflect that I am on a long Pomodoro break."
  (interactive)
  (jdb/slack-status (format "started a long Pomodoro break at %s, back in fifteen minutes" (format-time-string "%H:%M %Z")) ":tomato:"))

(defun jdb/slack-lunch ()
  "Update Slack status to reflect the fact I am having lunch."
  (interactive)
  (let ((today (string-to-number (format-time-string "%u"))))
    (jdb/slack-status (format "started lunch at %s, back in one hour" (format-time-string "%H:%M %Z"))
                      (if (>= today 5) ":beer:" ":shallow_pan_of_food:"))))

(defun jdb/slack-done ()
  "Update Slack status to reflect the fact I am no longer working."
  (interactive)
  (jdb/slack-status "not working" ":checkered_flag:"))

(defconst
  org-link-regexp
  (rx "[[" (group (one-or-more anything)) "][" (group (one-or-more anything)) "]]")
  "Regexp to match 'org-mode' links in the form [[link][text]].
There are capture groups for the link and text components.")

(defun jdb/conjugate-verb (verb)
  "Conjugate VERB into present tense.  attend -> attending."
  (cond ((string-suffix-p "e" verb) (replace-regexp-in-string "e$" "ing" verb))
        (t (concat verb "ing"))))

(defun jdb/org-slack-status ()
  "Update Slack status with the current org item.  EMOJI is the status emoji."
  (let* ((todo (replace-regexp-in-string org-link-regexp
                                         "\\2"
                                         (org-entry-get (point) "ITEM")))
         (words (split-string todo))
         (verb (car words))
         (conjugated (jdb/conjugate-verb verb))
         (text (string-join (cons conjugated (cdr words)) " ")))
    (funcall-interactively 'jdb/slack-status text)))

(defun jdb/format-YYYY-mm-dd (&optional time)
  "Format TIME to YYYY-mm-dd.
If TIME is not provided, it defaults to the current time."
  (format-time-string "%Y-%m-%d" time))

(defun jdb/next-working-day ()
  "Return the time of the next working day."
  (let ((today (string-to-number (format-time-string "%u"))))
    (if (>= today 5) (+ (time-convert nil 'integer) (* (- 8 today) 86400))
      (+ (time-convert nil 'integer) 86400))))

(defun jdb/org-file (&optional time)
  "Return the org file for the day that TIME falls within.
If TIME is not provided it defaults to the current time."
  (format "~/org/%s.org" (jdb/format-YYYY-mm-dd time)))

(defun jdb/org-today ()
  "Create or open the org file for today."
  (interactive)
  (find-file (jdb/org-file)))

(defun jdb/org-tomorrow ()
  "Create or open the org file for tomorrow."
  (interactive)
  (find-file (jdb/org-file (+ (time-convert nil 'integer) 86400))))

(defun jdb/org-next ()
  "Create or open the next org file, only considering work days."
  (interactive)
  (find-file (jdb/org-file (jdb/next-working-day))))

(defun jdb/org-file-prev ()
  "Return the org file for the previous working day.
This relies on the sorted file names as 'yesterday' isn't necessary the
last file when files are only created on weekdays."
  (let ((yesterday (jdb/org-file (- (time-convert nil 'integer) 86400))))
    (cond ((file-exists-p yesterday) yesterday)
          ((file-exists-p (jdb/org-file))
           (car (last (butlast (directory-files "~/org" t "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.org$")))))
          (t (car (last (directory-files "~/org" t "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.org$")))))))

(defun jdb/org-prev ()
  "Open the org file for the previous working day."
  (interactive)
  (find-file (jdb/org-file-prev)))

(defun jdb/org-skip ()
  "Skip subtrees with a :personal: tag."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (member "personal" (org-get-tags (point) t))
        subtree-end
      nil)))

(defun jdb/add-tag (tag)
  "Add TAG to current headline."
  (org-set-tags (sort (cons tag (org-get-tags (point) t)) 'string-lessp)))

(defun jdb/org-urgent () "Add urgent tag to headline." (interactive) (jdb/add-tag "urgent"))
(defun jdb/org-important () "Add important tag to headline." (interactive) (jdb/add-tag "important"))


(defun jdb/org-standup--remaining-effort ()
  "Return the remaining effort in minutes for the org-entry at point."
  (max
   (-
    (org-duration-to-minutes (or (org-entry-get (point) "EFFORT") "0:00"))
    (org-duration-to-minutes (org-clock-sum-current-item)))
   0.0))

(ert-deftest jdb/org-standup--remaining-effort/returns-effort-if-no-logbook ()
  "Remaining effort is effort if there is no clocked time."
  (let ((entry "* TODO todo entry
  :PROPERTIES:
  :Effort:   1:00
  :END:
"))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (should (equal (jdb/org-standup--remaining-effort) 60.0)))))
(ert-deftest jdb/org-standup--remaining-effort/returns-effort-sub-clock-if-logbook ()
  "Remaining effort should be the defined effort minus any clocked time."
  (let ((entry "* PRGR todo entry
  :PROPERTIES:
  :Effort:   1:00
  :END:
  :LOGBOOK:
  CLOCK: [1970-01-01 Thu 00:00]--[1970-01-01 Thu 01:00] =>  1:00
  :END:
"))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (should (equal (jdb/org-standup--remaining-effort) 0.0)))))
(ert-deftest jdb/org-standup--remaining-effort/returns-zero-for-negative-remaining-effort ()
  "Negative effort remaining is not useful for discussing estimated times."
  (let ((entry "* PRGR todo entry
  :PROPERTIES:
  :Effort:   1:00
  :END:
  :LOGBOOK:
  CLOCK: [1970-01-01 Thu 01:00]--[1970-01-01 Thu 02:00] =>  1:00
  CLOCK: [1970-01-01 Thu 00:00]--[1970-01-01 Thu 01:00] =>  1:00
  :END:
  "))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (should (equal (jdb/org-standup--remaining-effort) 0.0)))))
(ert-deftest jdb/org-standup--remaining-effort/handles-other-entries-in-buffer ()
  "The original implementation would subtract the clock sum of all entries in the buffer."
  (let ((entry "* PRGR todo entry
  :PROPERTIES:
  :Effort:   1:00
  :END:

* PRGR other todo entry
  :PROPERTIES:
  :Effort:   1:00
  :END:
  :LOGBOOK:
  CLOCK: [1970-01-01 Thu 01:00]--[1970-01-01 Thu 02:00] =>  1:00
  CLOCK: [1970-01-01 Thu 00:00]--[1970-01-01 Thu 01:00] =>  1:00
  :END:
  "))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (should (equal (jdb/org-standup--remaining-effort) 60.0)))))

(defun jdb/org-standup--org-to-standup ()
  "Translate the 'org-todo' entry at point into a standup entry."
  (format "- %s (EST %s) %s%s"
          (org-entry-get (point) "TODO")
          (format-seconds "%02h:%02m" (* 60 (jdb/org-standup--remaining-effort)))
          (replace-regexp-in-string org-link-regexp
                                    "[\\2](\\1)"
                                    (org-entry-get (point) "ITEM"))
          (let* ((tags (split-string (or (car (last (org-heading-components))) "") ":"))
                 (tag-string (concat
                              (if (member "urgent" tags) "U")
                              (if (member "important" tags) "I"))))
            (if (string-empty-p tag-string) "" (format " [%s]" tag-string)))))

(ert-deftest jdb/org-standup--org-to-standup/should-include-urgency-if-present ()
  "If a TODO entry has an `urgent` tag, it should be included in the standup entry."
  (let ((org-entry "* TODO must do                                   :urgent:important:")
        (want "- TODO (EST ) 00:00 must do [UI]"))
    (with-temp-buffer
      (insert org-entry)
      (goto-char (point-min))
      (let ((got (jdb/org-standup--org-to-standup)))
        (print got)
        (should (equal got want))))))

(defun jdb/org-standup ()
  "Translate 'org-todo' entries into Slack standup message in kill ring."
  (interactive)
  (let ((total 0))
    (org-map-entries (lambda () (set 'total (+ total (* 60 (jdb/org-standup--remaining-effort)))))
                     t
                     `(,(jdb/org-file))
                     #'jdb/org-skip)
    (kill-new (string-join
               `("*Today*"
                 ,@(org-map-entries 'jdb/org-standup--org-to-standup
                                    t
                                    `(,(jdb/org-file))
                                    #'jdb/org-skip)
                 ,(format "TOTAL %s" (format-seconds "%02h:%02m" total)))
               "\n"))))

(defun jdb/org-standup-last ()
  "Translate 'org-todo' entries into a Slack standup for time spent yesterday."
  (interactive)
  (let ((total 0)
        (yesterday-start (-
                          (string-to-number
                           (shell-command-to-string "date -d '' +%s"))
                          86400)))
    (kill-new (string-join
               `("*Last*"
                 ,@(org-map-entries
                    '(format "- %s (ACT %s ACC %3d٪) %s"
                             (org-entry-get (point) "TODO")
                             (progn
                               (set 'total (+ total (* 60 (or (org-clock-sum-current-item yesterday-start) 0))))
                               (format-seconds "%02h:%02m" (* 60 (or (org-clock-sum-current-item yesterday-start) 0))))
                             (let* ((split (split-string (or (org-entry-get (point) "EFFORT") "00:00") ":"))
                                    (hours (string-to-number (car split)))
                                    (mins (string-to-number (cadr split)))
                                    (effort-in-seconds (+ (* 3600 hours) (* 60 mins))))
                               (* 100
                                  (+ 1
                                     (/
                                      (- (* 60.0 (or (org-clock-sum-current-item yesterday-start) 0)) effort-in-seconds)
                                      effort-in-seconds))))
                             (replace-regexp-in-string org-link-regexp
                                                       "[\\2](\\1)"
                                                       (org-entry-get (point) "ITEM")))
                    t
                    `(,(jdb/org-file-prev))
                    #'jdb/org-skip)
                 ,(format "TOTAL %s" (format-seconds "%02h:%02m" total)))
               "\n"))))

(defun jdb/org-carryover ()
  "Carry over unfinished tasks from the previous day."
  (interactive)
  (let ((curr (jdb/org-file))
        (prev (jdb/org-file-prev)))
    (org-map-entries
     (lambda ()
       (let ((entry (buffer-substring-no-properties (org-entry-beginning-position) (org-entry-end-position))))
         (with-current-buffer (find-file curr)
           (goto-char (point-max))
           (insert entry)
           (save-buffer))))
     nil
     (list prev)
     (lambda ()
       (let ((subtree-end (save-excursion (org-end-of-subtree t)))
             (todo (org-entry-get (point) "TODO")))
         (if (or (string= todo "DONE") (string= todo "NOTD"))
             subtree-end
           nil))))))

(defun jdb/org-pomodoro-kill () "Cancel a Pomodoro countdown." (interactive) (org-pomodoro-kill))

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

;; format-all-mode
(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (markdown-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)
  (nix-mode . jdb/disable-format-all-mode)
  :config
  (defun jdb/disable-format-all-mode () "Disable format-all-mode." (format-all-mode 0)))

;; company-mode
(use-package company :init (global-company-mode))

;; terraform-mode
(use-package terraform-mode
  :hook (terraform-mode-hook . terraform-format-on-save-mode))

;; folding (really selective-display)
(defun jdb/toggle-selective-display (column)
  "Toggle folding with 'selective-display'.
  COLUMN controls how deeply the display is folded."
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))
(global-set-key (kbd "C-c f") #'jdb/toggle-selective-display)

;; haskell-mode
(use-package haskell)

;; YADM
;; From: https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/
;; Invoke magit with: (magit-status "/yadm::")
(use-package tramp
  :bind ("C-c y" . jdb/yadm)
  :config
  (defun jdb/yadm ()
    "Load magit for YADM."
    (interactive)
    (magit-status-setup-buffer "/yadm::"))
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

;; Move lines up and down.
;; From: https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun jdb/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun jdb/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-n") #'jdb/move-line-down)
(global-set-key (kbd "M-p") #'jdb/move-line-up)

;; multiple-cursors
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; expand-region
(use-package expand-region
  :bind
  (("C-." . er/expand-region)
   ("C-," . er/contract-region)))

;; ligatures
(use-package fira-code-mode
  :config
  (global-fira-code-mode))

;; projectile
(use-package projectile
  :after transient
  :config
  (defun jdb/projectile-project-buffers ()
    (projectile-project-buffers (projectile-acquire-root)))
  (defun projectile-help ()
    (interactive)
    ;; From: https://stackoverflow.com/questions/3480173/show-keys-in-emacs-keymap-value
    (with-output-to-temp-buffer "*keymap: projectile-command-map*"
      (princ "projectile-command-map\n\n")
      (princ (substitute-command-keys "\\{projectile-command-map}"))))
  (transient-define-prefix projectile-search-transient ()
    "Transient for searching a projectile project"
    ["Search with?"
     ("r" "Ripgrep" projectile-ripgrep)])

  (transient-define-prefix projectile-transient ()
    "Transient for projectile-mode-map"
    ["Projectile actions"
     ("c" "Compile" projectile-compile-project)
     ("f" "Find" projectile-find-file)
     ("p" "Switch project" projectile-switch-project)
     ("q" "Switch open project" projectile-switch-open-project)
     ("r" "Replace" projectile-replace)
     ("s" "Search" projectile-search-transient)
     ("S" "Save" projectile-save-project-buffers)
     ("x" "Shell" projectile-run-shell)
     ("?" "Help" projectile-help)])
  :bind
  ((:map projectile-mode-map ("C-c p" . projectile-transient)))
  :init
  (projectile-mode))

;; js2-mode (javascript)
(use-package js2-mode
  :mode
  (("\\.js\\'" . js2-mode)
   ("\\.ts\\'" . js2-mode)))

;; graphviz-dot-mode
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 2)
  (use-package company-graphviz-dot))

(put 'upcase-region 'disabled nil)

;; ivy/swiper/counsel
(use-package ivy
  :bind
  ("C-c v" . ivy-push-view)
  ("C-c V" . ivy-pop-view)
  :init
  (ivy-mode)
  :commands (ivy-mode)
  :config
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp))
(use-package swiper :bind (("C-s" . swiper)) :after ivy)

(use-package counsel :config (counsel-mode))

(use-package avy :bind (("M-s" . avy-goto-word-1)))

;; org-roam
;; (setq org-roam-directory "~/zettelkasten")
;; (add-hook 'after-init-hook 'org-roam-mode)

;; Add delete to character function.
(use-package misc :bind (("M-Z" . zap-up-to-char)))

;; Increase font size.
(set-face-attribute 'default nil :height 140)

;; org-pomodoro
(use-package org-pomodoro
  :hook (org-pomodoro-finished . (lambda () (message "Pomodoro complete!"))))

;; modeline
(set-face-attribute 'mode-line nil :height 100)

;; rainbow-delimiters
(use-package rainbow-delimiters :hook (prog-mode-hook . rainbow-delimiters-mode))

;; tetris
(defun disable-font-lock-mode () "Disable 'font-lock-mode' as it breaks tetris." (font-lock-mode 0))
(add-hook 'tetris-mode-hook 'disable-font-lock-mode)

;; auth-source-pass
(use-package auth-source
  :commands (auth-source-pass-get)
  :config
  (use-package auth-source-pass)
  (setq auth-sources '("~/.authinfo" password-store)))

;; slack
;; (require 'slack)
;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t)
;;   (setq slack-prefer-current-team t)
;;   (setq slack-render-image-p nil)
;;   :config
;;   (slack-register-team
;;    :name "emacs-slack"
;;    :default t
;;    :token (auth-source-pass-get 'secret "grafana/raintank-corp.slack.com")
;;    :full-and-display-names t))

;; (message (jdb/string-to-alphabet-emoji "test:smile:test" nil))
(defun jdb/string-to-alphabet-emoji (str &optional white?)
  "Display the message STR as Slack alphabet emoji.
WHITE? represents whether the character should be yellow (nil)
or white (integer value)."
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
                        ((and (org-string<= "A" token)
                              (org-string<= token "z"))
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

;; org-agenda
(setq org-agenda-files '("~/org"))
;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

;; pager
(setenv "PAGER" "cat")

;; compilation-mode
;; markdownlint-cli
(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist 'markdownlint-cli)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(markdownlint-cli .
                                  ("^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\) .*$"
                                   1 2 3)))
  (add-to-list 'compilation-error-regexp-alist 'monkeyc)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(monkeyc .
                         ("^ERROR: [^:]+: \\([^:]+\\):\\([0-9]+\\),\\([0-9]+\\): .*$"
                          1 2 3)))
  (add-to-list 'compilation-error-regexp-alist 'doc-validator)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(doc-validator .
                               ("^ERROR: \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\) .*$"
                                1 2 3))))

;; markdown-mode
(defconst jdb/relref-regexp (rx "{{<" (? " ") "relref \\" (group (* (not ?\))) "\\" (? " ") ">}}")))
;; TODO: Understand why this isn't being called.
(defun markdown-relref-translate (filename)
  "Translate FILENAME into a link that can be followed.
Specifically, translating Hugo relrefs into filenames."
  (message "testing: %s" filename))

;; org-gcal
(use-package org-gcal
  :hook
  (org-gcal-after-update-entry-functions . my-org-gcal-set-effort)
  :config
  (setq org-gcal-recurring-events-mode 'nested)
  (setq org-gcal-remove-api-cancelled-events t)
  (setq org-gcal-client-id (auth-source-pass-get "client_id" "grafana/org-gcal"))
  (setq org-gcal-client-secret (auth-source-pass-get "client_secret" "grafana/org-gcal"))
  (setq org-gcal-file-alist '(("jack.baldry@grafana.com" .  "~/org/jack.baldry@grafana.com.org")))
  (defun my-org-gcal-set-effort (_calendar-id event _update-mode)
    "Set Effort property based on EVENT if not already set."
    (when-let* ((stime (plist-get (plist-get event :start)
                                  :dateTime))
                (etime (plist-get (plist-get event :end)
                                  :dateTime))
                (diff (float-time
                       (time-subtract (org-gcal--parse-calendar-time-string etime)
                                      (org-gcal--parse-calendar-time-string stime))))
                (minutes (floor (/ diff 60))))
      (let ((effort (org-entry-get (point) org-effort-property)))
        (unless effort
          (message "need to set effort - minutes %S" minutes)
          (org-entry-put (point)
                         org-effort-property
                         (apply #'format "%d:%02d" (cl-floor minutes 60))))))))

;; org-babel
(use-package ob-async
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil))

;; jsonnet-mode
(use-package jsonnet-mode
  :hook (jsonnet-mode . prettify-jsonnet)
  :config
  (defun prettify-jsonnet()
    "Display some jsonnet keywords as pretty Unicode symbols."
    (setq prettify-symbols-alist
          '(("function" . ?λ)
            ("std." . ?.) ;; Note this is a zero width space.
            (".o" . ?​) ;; Note this is a zero width space.
            ("_0: " . ?​) ;; Note this is a zero width space.
            ("_1: " . ?​) ;; Note this is a zero width space.
            ("_2: " . ?​) ;; Note this is a zero width space.
            ("_3: " . ?​) ;; Note this is a zero width space.
            ("_4: " . ?​) ;; Note this is a zero width space.
            ("_5: " . ?​) ;; Note this is a zero width space.
            (": { " . ?.)
            ))))

;; LSP server
(defcustom lsp-jsonnet-executable "jsonnet-language-server"
  "Command to start the Jsonnet language server."
  :group 'lsp-jsonnet
  :risky t
  :type 'file)

(defun jdb/docs-jsonnet-stdlib ()
  "Open the Jsonnet stdlib documentation."
  (interactive)
  (browse-url "https://jsonnet.org/ref/stdlib.html"))

;; direnv
(use-package direnv :init (direnv-mode))

;; sh-mode
(defun sh-set-indent () "Set 'sh-mode' indent." (setq tab-width 2))
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)
(add-hook 'sh-mode-hook 'sh-set-indent)

;; browser
(setq browse-url-browser-function 'browse-url-chromium)

;; Regexp for HTTP URLs in the BNF in RFC1738.
;; rx definitions are created to match all the nonterminals.
;; https://datatracker.ietf.org/doc/html/rfc1738#section-5."
(rx-define alphadigit alphanumeric)
(rx-define digits (+ digit))
(rx-define domainlabel (| alphadigit (* alphadigit (? (| alphadigit "-")) alphadigit)))
(rx-define toplabel (| alpha (* alpha (? (| alphadigit "-")) alphadigit)))
(rx-define hostname (seq (* domainlabel ".") toplabel))
(rx-define hostnumber (seq digits "." digits "." digits "." digits))
(rx-define host (| hostname hostnumber))
(rx-define port digits)
(rx-define hostport (seq host (? ":" port)))
(rx-define safe (any "$-_.+"))
(rx-define extra (any "!*',()"))
(rx-define unreserved (| alpha digit safe extra))
(rx-define escape (seq "%" hex hex))
(rx-define uchar (| unreserved escape))
(rx-define hsegment (* (| uchar (any ";:@&="))))
(rx-define hpath (seq hsegment (* "/" hsegment)))
(rx-define search (* (| uchar (any ";:@&="))))
(rx-define httpurl (seq "http://" hostport (? "/" hpath (? "?" search))))
;; httpsurl matches HTTPS URLs.
(rx-define httpsurl (seq "https://" hostport (? "/" hpath (? "?" search))))
;; https-urlish matches HTTPS or HTTP URLs that are just missing a scheme.
(rx-define https-urlish (seq (? (seq "http" (? "s") "://")) hostport (? "/" hpath (? "?" search))))

(defun jdb/chr--switch-buffer-action (buffer)
  "Switch to a buffer named BUFFER if it is live.
If there is nota live buffer, and BUFFER looks an HTTP URL,
open it in a browser.
Otherwise, search for BUFFER with DuckDuckGo."
  (cond ((buffer-live-p (get-buffer buffer)) (switch-to-buffer buffer 'force-same-window))
        ;; Open URL from history.
        ((string-match (rx string-start https-urlish " | ") buffer) (jdb/chr current-prefix-arg (nth 0 (split-string buffer))))
        ((string-match (rx string-start https-urlish string-end) buffer) (jdb/chr current-prefix-arg buffer))
        (t (jdb/ddg current-prefix-arg (string-trim buffer)))))

;; (require 'emacsql-sqlite-builtin)
;; TODO: emacsql-sqlite-builtin-connection
(defun jdb/chr--read-history ()
  "Read Chromium history file and add the URLs to ivy completion."
  (let* ((wd "/home/jdb/.config/chromium/Default")
         (history-file (string-join (list wd "History") "/"))
         (copy-file (concat history-file ".copy")))
    ;; Copy the file because Chromium keeps the database locked when running.
    (copy-file history-file copy-file "overwrite-if-exists")
    (butlast (split-string (shell-command-to-string (format "sqlite3 %s -separator ' | ' 'SELECT url,title FROM urls'" copy-file)) "\n" nil))))

(require 's)
(defun jdb/chr-read ()
  "Switch to a chromium process or start a new one.
INCOGNITO controls whether the window is opened incognito.
URL is the optional URL to open the process on."
  (interactive)
  (ivy-read "Switch to buffer: "
            #'(lambda (_ _ _) (append (internal-complete-buffer "Chromium-browser" nil t) (jdb/chr--read-history)))
            :action #'jdb/chr--switch-buffer-action
            :caller 'jdb/chr-read))

(global-set-key (kbd "C-x c") 'jdb/chr-read)
;; C-x C-c is originally bound to save-buffers-kill-terminal which is a little too
;; dangerous to have as a typo for jdb/chr-read.
(global-unset-key (kbd "C-x C-c"))

(defun jdb/chr (incognito url)
  "Start a chromium process at URL.
If INCOGNITO is non-nil, start the chromium incognito."
  (interactive "P\nsURL: \n")
  (let ((browse-url-chromium-arguments (if incognito (cons "--incognito" browse-url-chromium-arguments) browse-url-chromium-arguments)))
    (browse-url url)))

(use-package cl-lib)
(defun jdb/chr-open (url buffer-regexp)
  "Open the buffer matching BUFFER-REGEXP or a new Chromium window for URL."
  (let ((meet-buffers (cl-remove-if-not (lambda (buffer)  (string-match-p buffer-regexp (buffer-name buffer))) (buffer-list))))
    (if (> (length meet-buffers) 0)
        (switch-to-buffer (buffer-name (car meet-buffers)) 'force-same-window)
      (browse-url url))))
(defun jdb/meet () "Start a Google Meet." (interactive) (jdb/chr-open "https://meet.new" ".*Meet.*"))
(defun jdb/whatsapp () "Open Whatsapp." (interactive) (jdb/chr-open "https://web.whatsapp.com" ".*Whatsapp.*"))
(defun jdb/g-calendar () "Open Google Calendar." (interactive) (jdb/chr-open "https://calendar.google.com" ".*Calendar.*"))
(defun jdb/github-notifications () "Open GitHub notifications." (interactive) (jdb/chr-open "https://github.com/notifications" ".*Notifications.*"))

(defun jdb/yt (query)
  "Search YouTube with QUERY."
  (interactive "sQuery: \n")
  (jdb/chr t (concat "https://youtube.com/results?search_query=" (replace-regexp-in-string " " "+" query))))

(defun jdb/yt-local (url)
  "Convert a YouTube URL into one for the local player."
  (interactive "sURL: \n")
  (string-match (rx (seq "v=" (group (= 11 (any alphanumeric))))) url)
  (kill-new (format "file:///home/jdb/youtube.html?v=%s&t=0" (match-string-no-properties 1 url))))

(defun jdb/ddg (incognito query)
  "Search DuckDuckGo for QUERY.
IF INCOGNITO is non-nil, search incognito."
  (interactive "P\nsQuery: \n")
  (jdb/chr incognito (concat "https://duckduckgo.com/?q=" (replace-regexp-in-string " " "+" query))))

(defun jdb/search-go (package)
  "Search for PACKAGE on https://pkg.go.dev."
  (interactive "sPackage: \n")
  (browse-url (concat "https://pkg.go.dev/search?q=" package)))

(defun jdb/search-nixos (package)
  "Search for PACKAGE in NixOS packages."
  (interactive "sPackage: \n")
  (browse-url (concat "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=" package)))

(defun jdb/open-pulls-on-github (org repo)
  "Open my Pull Requests on GitHub for a specific ORG and REPO."
  (interactive "sOrg: \nsRepository: \n")
  (browse-url (format "https://github.com/%s/%s/pulls/@me" org repo)))

;; (jdb/open-on-github)
(defun jdb/open-on-github (org)
  "Open the current file in GitHub.
ORG is the Github repository owner."
  (interactive "sOrg: \n")
  (let ((url "https://github.com")
        (repo (car (last (delete "" (split-string (projectile-project-root) "/")))))
        (ref (shell-command-to-string "git rev-parse HEAD"))
        (file (string-remove-prefix (projectile-project-root) (buffer-file-name)))
        (line (line-number-at-pos)))
    (browse-url (format "%s/%s/%s/tree/%s/%s#L%s" url org repo ref file line))))

(defun jdb/open-mimir-issue (issue)
  "Open the Mimir issue or PR ISSUE."
  (interactive "nIssue: \n")
  (browse-url (format "https://github.com/grafana/mimir/issues/%s" issue)))

(defun jdb/open-in-zendesk(id)
  "Open a Zendesk ticket.  ID is the Zendesk ticket number."
  (interactive "sID: \n")
  (let ((url "https://grafana.zendesk.com/agent/tickets"))
    (browse-url (format "%s/%s" url id))))

(defun jdb/docs-home-manager ()
  "Open the home-manager documentation."
  (interactive)
  (browse-url "https://nix-community.github.io/home-manager/"))

;; modeline
(use-package battery
  :config
  (setq display-time-day-and-date t)
  (display-time)
  (setq battery-mode-line-format " [BAT %b%p%% %t]")
  (display-battery-mode))

;; ansi-color
(use-package ansi-color
  :commands (endless/colorize-compilation)
  :hook
  (compilation-filter . endless/colorize-compilation)
  :config
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point)))))

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
(use-package mu4e
  :defer t
  :commands (mu4e mu4e-message mu4e-message-contact-field-matches)
  :config
  (setq mu4e-change-filenames-when-moving t)
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
                                            :key ?w)
                                     (:name "This year"
                                            :query "maildir:/grafana/Inbox AND date:20220101..now AND NOT flag:trashed"
                                            :key ?y)
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
  (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)
  (use-package ace-link
    :defer t
    :config
    (add-to-list 'mu4e-view-actions `("Open link" . ,(lambda (_) (ace-link-mu4e))) t)))

;; safe-local-variable-values
(put 'dap-go-delve-path 'safe-local-variable #'stringp)
(put 'lsp-go-build-flags 'safe-local-variable #'vectorp)

(defun jdb/sh ()
  "Create a new shell in the current project."
  (interactive)
  (shell (format "*shell*<%s>" (projectile-project-root))))

;; debugger
(setq debugger-stack-frame-as-list t)

;; abbrev-mode
;; Save abbreviations when files are saved.
(setq save-abbrevs 'silent)
(setq abbrev-mode t)
(define-abbrev-table 'global-abbrev-table
  '(("teseting" (string-join '("testing"
                               "second") "\n")
     nil 1)))

;; atomic-chrome
(use-package atomic-chrome
  :defer t
  :commands atomic-chrome-start-server
  :config
  (setq atomic-chrome-extension-type-list '(ghost-text))
  (atomic-chrome-start-server))

;; keycast
;; (keycast-mode)
(use-package keycast
  :defer t)

;; ghub
(use-package ghub
  :defer t
  :commands ghub-post
  :config
  (defun jdb/gh-create-repo(repo)
    "Create a new GitHub repository REPO."
    (interactive "sRepository: \n")
    (ghub-post "/user/repos" nil :payload `((name . ,repo)))))

;; sound
(defun jdb/amixer-message ()
  "Output the current volume as a message."
  (message "%s" (shell-command-to-string "amixer get Master")))

(defun jdb/amixer-set-vol (vol)
  "Set the volume to VOL percent."
  (shell-command (format "amixer sset Master %s%%" vol)))

(defun jdb/amixer-mute ()
  "Mute sound."
  (interactive)
  (jdb/amixer-set-vol 0)
  (jdb/amixer-message))

(defun jdb/amixer-unmute (vol)
  "Set sound level to 100% if VOL is nil otherwise, set to the value of VOL."
  (interactive "P")
  (jdb/amixer-set-vol (or vol 100))
  (jdb/amixer-message))

;; autorandr
(defun jdb/autorandr-work ()
  "Change to work displays."
  (interactive)
  (shell-command "autorandr --change work"))
(defun jdb/autorandr-laptop ()
  "Change to laptop displays."
  (interactive)
  (shell-command "autorandr --change laptop"))

;; dired
(setq dired-listing-switches "-alh")
(defun jdb/open-marked-with (command)
  "Open marked files in using the shell command.
COMMAND should be a function accepting a list of file names that
returns a shell command string to open those files."
  (start-process-shell-command "open-with" nil (funcall command (dired-get-marked-files))))

(defun jdb/open-marked-with-chromium ()
  "Open Dired marked files with chromium."
  (interactive)
  (jdb/open-marked-with '(lambda (files) (string-join (cons "chromium" files) " "))))

(defun jdb/open-marked-with-mupdf ()
  "Open Dired marked files with mupdf."
  (interactive)
  (jdb/open-marked-with '(lambda (files) (mapconcat (lambda (file) (format "mupdf '%s'" file)) files ";"))))

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode))

;; code-review
(use-package code-review
  :after magit
  :config
  (defun code-review-open-file ()
    "Open file of section at point."
    (interactive)
    ;; TODO: open at section of file.
    (find-file-other-window (alist-get 'path (oref (magit-current-section) value)))))

;; agda
(use-package agda2-mode)

;; ace-link
(use-package ace-link
  :config
  (ace-link-setup-default))
;; (define-key gnus-summary-mode-map (kbd "M-o") 'ace-link-gnus)
;; (define-key gnus-article-mode-map (kbd "M-o") 'ace-link-gnus)

;; windows, buffers, and frames
(defun make-dedicated ()
  "Make the current window but not frame dedicated to the current buffer."
  (interactive)
  ;; (set-frame-parameter nil 'unsplittable t)
  (set-window-dedicated-p nil t))

;; flycheck-aspell
(use-package flycheck-aspell
  :config
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic 'append)
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic 'append)
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic 'append)
  (add-to-list 'flycheck-checkers 'xml-aspell-dynamic 'append)
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic 'append))

;; vale
(flycheck-define-checker vale-error
  "A checker for prose"
  :command ("~/bin/vale" "--minAlertLevel" "error" source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode)
  :next-checkers '(no-errors . vale-warning))
(flycheck-define-checker vale-warning
  "A checker for prose"
  :command ("~/bin/vale" "--minAlertLevel" "warning" source)
  :standard-input nil
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode)
  :next-checkers '(markdown-aspell-dynamic))
(add-to-list 'flycheck-checkers 'vale-error 'append)
(add-to-list 'flycheck-checkers 'vale-warning 'append)

;; html
(use-package dom)
(defun jdb/tag-for-url (url tag)
  "Fetch the HTML TAG for a URL.
TODO: strip off #edit from at least GDocs URLs as it breaks the request."
  (interactive "sURL: \nSTag: \n")
  (let ((buffer (generate-new-buffer "title-for-url-as-kill")))
    (with-temp-file "/tmp/gdoc"
      (let ((effective-url
             (shell-command-to-string (format "curl -A 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -Lsb <(kooky -d %s -o /dev/stdout %s) -w %%{url_effective} -o /dev/null %s"  (url-host (url-generic-parse-url url)) url url))))
        (shell-command (format "curl -Lb <(kooky -d %s -o /dev/stdout) %s" (url-host (url-generic-parse-url effective-url)) effective-url) (current-buffer))
        (dom-text (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) tag))))))

(defun jdb/title-for-url-as-kill (url)
  "Fetch the HTML title for a URL."
  (interactive "sURL: \n")
  (kill-new (jdb/tag-for-url url 'title)))

(defun jdb/title-for-url-as-kill-md (url)
  "Fetch the HTML title for a URL."
  (interactive "sURL: \n")
  (kill-new (format "[%s](%s)" (jdb/tag-for-url url 'title) url)))

(defun jdb/h1-for-url-as-kill (url)
  "Fetch the first HTML H1 for a URL."
  (interactive "sURL: \n")
  (kill-new (jdb/tag-for-url url 'h1)))

(defun jdb/h1-for-url-as-kill-md (url)
  "Fetch the HTML h1 for a URL."
  (interactive "sURL: \n")
  (kill-new (format "[%s](%s)" (jdb/tag-for-url url 'h1) url)))

(defun jdb/new-scratch ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (create-file-buffer (concat (temporary-file-directory) "scratch"))))

;; hl-mode
(global-hl-line-mode)

;; brightness
(use-package f)
(defun jdb/brightness (percentage)
  "Adjust the brightness to PERCENTAGE."
  (interactive "p")
  (let* ((backlight-path "/sys/class/backlight/intel_backlight")
         (path-join (lambda (&rest paths) (string-join paths "/")))
         (max-brightness-file (funcall path-join backlight-path "max_brightness"))
         (max-brightness (string-to-number (f-read-text max-brightness-file)))
         (brightness-file (funcall path-join backlight-path "brightness"))
         (brightness (* percentage (/ max-brightness 100))))
    (start-process-shell-command "brightness" nil
                                 (format "tee %s <<<%s" brightness-file brightness))))

;; hibernate
(defun jdb/hibernate () "Hibernate the system." (interactive)
       (start-process-shell-command "hibernate" nil "systemctl hibernate"))

(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(defun jdb/clear-kill-ring ()
  "Clear the 'kill-ring'."
  (interactive)
  (setq kill-ring nil))

;;
(use-package deadgrep
  :commands (deadgrep)
  :config
  (setq deadgrep-executable "rga"))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; From https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder.
(require 're-builder)
(setq reb-re-syntax 'rx)

(provide 'emacs)
;;; .emacs ends here
