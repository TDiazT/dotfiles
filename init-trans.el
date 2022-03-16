(require 'rational-defaults)
(require 'rational-screencast)
(require 'rational-ui)
(require 'rational-editing)
(require 'rational-evil)
(require 'rational-completion)
(require 'rational-windows)
(require 'rational-org)

(straight-use-package 'project)

(when (or
       (file-directory-p (expand-file-name "custom-modules/" rational-config-path))
       (file-symlink-p (expand-file-name "custom-modules/" rational-config-path)))

  (add-to-list 'load-path (expand-file-name "custom-modules/" rational-config-path))

  (let ((default-directory (expand-file-name "custom-modules/" rational-config-path)))
    (normal-top-level-add-subdirs-to-load-path))
  )

; Follow best practice by setting your environment variables so that they are available to both interactive and non-interactive shells. In practical terms, for most people this means setting them in ~/.profile, ~/.bash_profile, ~/.zshenv instead of ~/.bashrc and ~/.zshrc.
(when (memq window-system '(mac ns x))
  (straight-use-package 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-right-option-modifier 'none))

(load-theme 'doom-snazzy t)

(custom-set-variables
 '(rational-ui-default-font
   '(:font "JetBrains Mono" :weight light :height 150)))

(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile"
                    :height 150
                    :weight 'light)

;;;; Make the titlebar on macOS transparent
(when (eq system-type 'darwin)
  (straight-use-package 'ns-auto-titlebar)
  (ns-auto-titlebar-mode))

;; Enabling line numbers
(customize-set-variable 'rational-ui-display-line-numbers t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(straight-use-package 'super-save)
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)

(setq-default fill-column 100)

(straight-use-package 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(require 'tdtron-org)

(require 'tdtron-keybinds)

(require 'tdtron-org-roam)

(require 'tdtron-racket)
