;; -*- lisp -*-

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "/opt/boxen/homebrew/share/emacs/site-lisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;; Cask/Pallet initialisation. Deals with packages automagically
(require 'cask)
(cask-initialize)
(require 'pallet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General options.

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
(global-linum-mode t)

(setq-default tab-width 2)
(setq-default js-indent-level 2)

;; Use real tabs in Makefiles
(add-hook 'make-mode     ( setq indent-tabs-mode t ))
(add-hook 'makefile-mode ( setq indent-tabs-mode t ))

;; Don't wrap lines
(setq default-truncate-lines t)

;; No more blasted ~ files.
(setq-default make-backup-files nil)

(add-hook 'text-mode-hook 'auto-fill-mode)

;; autocomplete and other helpers.
(ac-config-default)
(projectile-global-mode)

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; sane buffer names for dup files. foo/foo and bar/foo, not foo, foo<2>
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

(condition-case ()
    (quietly-read-abbrev-file)
  (file-error nil))

(global-set-key "\C-cg" 'magit-status)
(eval-after-load 'magit
  '(diminish 'magit-auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up Puppet mode
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up color-theme to use Solarised theme.
(load-theme 'solarized-dark t)
(setq solarized-termcolors 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mode
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby-mode stuff

(setq ruby-deep-indent-paren nil)

;; Gem-, Cap- and Rake- files etc are Ruby files:
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Rake\\|Gem\\|Berks\\)file$" . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cucumber feature-mode

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cperl-mode stuff
(setq cperl-hairy t)

;; Prefer cperl-mode over perl-mode
(defalias 'perl-mode 'cperl-mode)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(frame-background-mode (quote dark)))


