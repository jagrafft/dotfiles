(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(global-display-line-numbers-mode)
(setq-default indent-tabs-mode nil)
(show-paren-mode 1)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Iteration Example
;;(package-refresh-contents)
;;(mapc
;; (lambda (p)
;;   (unless (package-installed-p p)
;;     (package-install p)))
;; '(stan-mode
;;   company-stan
;;   flycheck-stan))

;; deadgrep
(unless (package-installed-p 'deadgrep)
  (package-refresh-contents)
  (package-install 'deadgrep))
(require 'deadgrep)
(global-set-key (kbd "<f5>") 'deadgrep)

;; Delve Mode
(add-to-list 'load-path "~/.emacs.d/delve-mode/")
(require 'delve-mode)
(add-to-list 'auto-mode-alist '("\\.delve" . delve-mode))

;; Dracula Theme
(unless (package-installed-p 'dracula-theme)
  (package-refresh-contents)
  (package-install 'dracula-theme))
(load-theme 'dracula t)

;; J Mode
(add-to-list 'load-path "~/.emacs.d/j-mode/")
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
(add-to-list 'auto-mode-alist '("\\.ij(kbd "<rstp>")$" . j-mode))

;; Julia Mode
(unless (package-installed-p 'julia-mode)
  (package-refresh-contents)
  (package-install 'julia-mode))
(require 'julia-mode)

;; Markdown Mode
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'markdown-mode))
(require 'markdown-mode)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;;:init (setq markdown-command "multimarkdown"))
  :init (setq markdown-command "pandoc --from=markdown --to=html5"))
(setq markdown-enable-math t)

;; Monokai Theme
;;(unless (package-installed-p 'monokai-theme)
;;  (package-refresh-contents)
;;  (package-install 'monokai-theme))
;;(load-theme 'monokai t)

;; NeoTree
(unless (package-installed-p 'neotree)
  (package-refresh-contents)
  (package-install 'neotree))
(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; Rust Mode
(unless (package-installed-p 'rust-mode)
  (package-refresh-contents)
  (package-install 'rust-mode))
(autoload 'rust-mode "rust-mode" nil t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; Stan Mode
(unless (package-installed-p 'stan-mode)
  (package-refresh-contents)
  (package-install 'stan-mode))
(require 'stan-mode)

;; vlang-mode
(add-to-list 'load-path "~/.emacs.d/vlang-mode/")
(require 'vlang-mode)
(add-to-list 'auto-mode-alist '("\\.v" . vlang-mode))

;; which-key
(use-package which-key
  :defer 10
  :ensure t
  :config
  (progn
    (setq which-key-popup-type 'side-window) ;Default
    ;; (setq which-key-popup-type 'minibuffer)

    (setq which-key-compute-remaps t) ;Show correct descriptions for remapped keys
    (setq which-key-allow-evil-operators t)
    (setq which-key-show-operator-state-maps t)

    (setq which-key-allow-multiple-replacements t) ;Default = nil
    (which-key-mode 1)
    )
  )

;; Wordcount Mode
(add-to-list 'load-path "~/.emacs.d/wc-mode/")
(require 'wc-mode)
(global-set-key "\C-cw" 'wc-mode)

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
