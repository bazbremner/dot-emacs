;; -*- lisp -*-

(setq warning-suppress-log-types '((package reinitialization)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/")

(setq package-list
      '(dockerfile-mode docker scala-mode yaml-mode terraform-mode
 solarized-theme smex rspec-mode puppet-mode projectile markdown-mode
 magit helm haml-mode groovy-mode go-mode git-blamed flymake-yaml
 flymake-shell flymake-ruby flx-ido feature-mode exec-path-from-shell
 diminish solarized-theme better-defaults auto-complete ag)
      )

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General options.

;; tune GC and process output values for LSP as per
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024 2)) ;; 2MiB
(setq gc-cons-threshold 100000000)

;; disable splash screen, *scratch* by default.
(setq inhibit-startup-screen t)

;; Set PATH, MANPATH and exec-path from the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; OS X's ls doesn't have a --dired option.
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(setq ag-reuse-buffers t)

(line-number-mode t)
(column-number-mode t)
(global-display-line-numbers-mode)

;; y or n, rather than yes/no to prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)

;; Use real tabs in Makefiles and Go
(add-hook 'make-mode     ( setq indent-tabs-mode t ))
(add-hook 'makefile-mode ( setq indent-tabs-mode t ))
(add-hook 'go-mode       ( setq indent-tabs-mode t ))

;; Don't wrap lines
(setq default-truncate-lines t)

;; No more blasted ~ files.
(setq-default make-backup-files nil)

;; Ensure final newline on files
(setq require-final-newline t)

(add-hook 'text-mode-hook 'auto-fill-mode)

(add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)

(when (and (eq system-type 'darwin) (executable-find "aspell"))
      (setq ispell-program-name (executable-find "aspell")))

;; autocomplete and other helpers.
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; sane buffer names for dup files. foo/foo and bar/foo, not foo, foo<2>
(setq-default uniquify-buffer-name-style 'forward)

(condition-case ()
    (quietly-read-abbrev-file)
  (file-error nil))

(global-set-key "\C-cg" 'magit-status)
(eval-after-load 'magit
  '(diminish 'magit-auto-revert-mode))
(setq magit-last-seen-setup-instructions "1.4.0")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up color-theme to use Solarised theme.
(load-theme 'solarized-dark t)
(setq solarized-termcolors 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc file extension to mode associations
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)" . markdown-mode))

;; Cucumber feature-mode
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(global-set-key "\C-cc" 'org-capture)
(setq org-directory (expand-file-name "~/.org"))
(setq org-default-notes-file (concat org-directory "/todo.org"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defun go-mode-setup ()
   (setq compile-command "go build -v && go test -v && go vet && golint"))

(add-hook 'go-mode-hook 'go-mode-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby-mode stuff

(setq ruby-deep-indent-paren nil)

;; Gem-, Cap- and Rake- files etc are Ruby files:
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Rake\\|Gem\\|Berks\\)file$" . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cperl-mode stuff
(setq cperl-hairy t)

;; Prefer cperl-mode over perl-mode
(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python-mode

;; Ignoring electric indentation - I'm littering trailing whitespace
;; all over the place. Borrowed from
;; http://emacswiki.org/emacs/AutoIndentation

(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dos-to-unix ()
  "Convert a DOS buffer to Unix format."
  (interactive)
  (beginning-of-buffer)
  (replace-string "\r\n" "\n"))

(defun global-trim ()
  "Trim all trailing whitespace in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" t t))))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
            (fill-paragraph nil region)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C# mode

(add-to-list 'auto-mode-alist '("\.cs$" . csharp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epg-gpg-program "gpg")
 '(frame-background-mode 'dark)
 )

(custom-set-faces
 '(default ((t (:background "nil")))))
