;;; crafted-emacs.el -- Emacs customization file, based on the Crafted config -*- lexical-binding: t; -*-
;; This file is generated from crafted-emacs.org. If you want to edit the
;; configuration, DO NOT edit crafted-emacs.el, edit crafted-emacs.org, instead.

;; Tangle the code blocks to crafted-emacs.el on save.
(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "crafted-emacs.org" dotfiles-path))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle-config)))

(require 'crafted-defaults)    ; Sensible default settings for Emacs
;; (require 'crafted-use-package) ; Configuration for `use-package`
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.
(require 'crafted-evil)        ; An `evil-mode` configuration
(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.
(require 'crafted-project)     ; built-in alternative to projectile
(require 'crafted-speedbar)    ; built-in file-tree
(require 'crafted-screencast)  ; show current command and binding in modeline
;; (require 'crafted-compile)     ; automatically compile some emacs lisp files

(customize-set-variable 'crafted-startup-inhibit-splash t)

(when (or
        (file-directory-p (expand-file-name "custom-modules/" crafted-config-path))
        (file-symlink-p (expand-file-name "custom-modules/" crafted-config-path)))

(add-to-list 'load-path (expand-file-name "custom-modules/" crafted-config-path))

(let ((default-directory (expand-file-name "custom-modules/" crafted-config-path)))
    (normal-top-level-add-subdirs-to-load-path))
)

; Follow best practice by setting your environment variables so that they are available to both interactive and non-interactive shells. In practical terms, for most people this means setting them in ~/.profile, ~/.bash_profile, ~/.zshenv instead of ~/.bashrc and ~/.zshrc.
(when (memq window-system '(mac darwin))
  (progn (crafted-package-install-package 'exec-path-from-shell)
         (setq exec-path-from-shell-arguments nil)
         (exec-path-from-shell-initialize)))

(when (eq system-type 'darwin)
  (progn (setq mac-command-modifier 'meta)
         (setq mac-option-modifier 'super)
         (setq mac-right-option-modifier 'none)))

(crafted-package-install-package 'doom-themes)
(progn
  (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
  (load-theme 'doom-snazzy t))       ; load the doom-palenight theme

(add-hook '
 emacs-startup-hook
 (lambda ()
            (custom-set-faces
             `(default ((t (:font "FiraCode Nerd Font" :weight light :height 120))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Iosevka Aile" :height 120 :weight light)))))))

;;;; Make the titlebar on macOS transparent
(when (eq system-type 'darwin)
  (crafted-package-install-package 'ns-auto-titlebar)
  (ns-auto-titlebar-mode))

;; Make new buffers split vertically instead of horizontally
;; Taken from https://stackoverflow.com/a/2081978/3802589
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Enabling line numbers
(customize-set-variable 'crafted-ui-display-line-numbers t)

(crafted-package-install-package 'super-save)
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)

(setq-default fill-column 100)

(crafted-package-install-package 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(require 'tdtron-org)

(customize-set-variable 'crafted-evil-discourage-arrow-keys t)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

(require 'tdtron-keybinds)

(require 'tdtron-org-roam)

;; (require 'tdtron-racket)
