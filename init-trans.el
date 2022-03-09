(require 'rational-defaults)
(require 'rational-screencast)
(require 'rational-ui)
(require 'rational-editing)
(require 'rational-evil)
(require 'rational-completion)
(require 'rational-windows)

(require 'rational-use-package)

; Follow best practice by setting your environment variables so that they are available to both interactive and non-interactive shells. In practical terms, for most people this means setting them in ~/.profile, ~/.bash_profile, ~/.zshenv instead of ~/.bashrc and ~/.zshrc.
(when (memq window-system '(mac ns x))
  (progn
    (straight-use-package 'exec-path-from-shell)
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize))
  )

(setq mac-command-modifier 'meta)

(load-theme 'doom-snazzy t)

(set-face-attribute 'default nil
                  :font "JetBrains Mono"
                  :weight 'light
                  :height 150)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile"
                    :height 150
                    :weight 'light)

;;;; Make the titlebar on macOS transparent
(when (eq system-type 'darwin)
  (progn (straight-use-package 'ns-auto-titlebar)
   (ns-auto-titlebar-mode)))

;; Enabling line numbers
(customize-set-variable 'rational-ui-display-line-numbers t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(setq-default fill-column 100)

(straight-use-package 'perspective)

(global-set-key (kbd "C-M-k") 'persp-switch)
(global-set-key (kbd "C-M-n") 'persp-next)
(global-set-key (kbd "C-x k") 'persp-kill-buffer*)
(global-set-key (kbd "C-x b") 'persp-counsel-switch-buffer)

(setq persp-initial-frame-name "Main")
;; Running `persp-mode' multiple times resets the perspective list...
(unless (equal persp-mode t)
  (persp-mode 1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)

  :custom
  ; group-by-directory-first is not available in OS X apparently
  ; https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-899157143
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq ;;dired-omit-files "^\\.[^.].*"
   dired-omit-verbose nil
   dired-hide-details-hide-symlink-targets nil
   delete-by-moving-to-trash t)
  :config
  (setq dired-dwim-target t)
  :bind ("C-x C-j" . dired-jump)
  )

(straight-use-package 'dired-single)

(add-hook 'dired-mode 'all-the-icons-dired-mode)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(straight-use-package 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(global-set-key (kbd "C-M-e") 'recentf-open-files)

(straight-use-package 'general)
(general-create-definer tdtron/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC"
  )

(straight-use-package 'hydra)

(defhydra hydra-coq ()
  "Coq actions"
  ("j" proof-assert-next-command-interactive "Next")
  ("k" proof-undo-last-successful-command "Undo")
  ("f" nil "finished" :exit t)
  )

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t)
  )

(tdtron/leader-keys
  "c"   '(:ignore t :which-key "coq")
  "cs"  '(coq-Search :which-key "search")
  "ci"  '(hydra-coq/body :which-key "interactive")
  )

(tdtron/leader-keys
  "s" '(hydra-text-scale/body :which-key "scale text"))

(tdtron/leader-keys
 "t"  '(:ignore t :which-key "toggles")
 "tw" 'whitespace-mode
 "tt" '(counsel-load-theme :which-key "choose theme"))

(defun tdtron/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'light :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; Turn on indentation and auto-fill mode for Org files
(defun tdtron/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  )

(use-package org
  :defer t
  :commands (org-capture org-agenda)
  :hook (org-mode . tdtron/org-mode-setup)
  ;; Consider using :custom instead and not setq
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)
  ;; (setq org-agenda-files
  ;;       '("~/Projects/Coq-PL/Org/"))
  (tdtron/org-font-setup)
  :custom
  ;; Check whether it's worth having this shift support
  (org-support-shift-select t)

  )

;; Change headers * for other symbols
(with-eval-after-load 'org
  (progn
    (straight-use-package 'org-superstar)
    (org-superstar-mode 1)
    (add-hook 'org-mode 'org-superstar-mode)
    (setq org-superstar-remove-leading-stars t)
    (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

(defun tdtron/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(straight-use-package 'visual-fill-column)
(add-hook 'org-mode 'tdtron/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; Coq apparently is not working with Babel - https://emacs.stackexchange.com/q/58369/34589
     ;; With newer Coq versions, the file 'coq-inferior.el' is no longer packaged with it
     ;; (coq . t)
     )
   )
  )

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  ;; See org lang load - TLDR: Coq is not working with babel rn
  (add-to-list 'org-structure-template-alist '("coq" . "src coq"))
  )

;; Let's you insert without confirming.
;; Uses the first template for the immediate nodes (ie. default right now)
;; Source: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)

  :config
  (org-roam-db-autosync-mode)

  :custom
  (org-roam-directory "~/Projects/Notes")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)

     ("p" "Paper" plain
      (file "~/Projects/Notes/templates/PaperTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: Paper")
      :unnarrowed t)

     ("q" "Question" plain
      (file "~/Projects/Notes/templates/QuestionTemplate.org")
      :if-new (file+head "~/Projects/Notes/questions/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: Question")
      :unnarrowed t)

     )
   )
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r I" . org-roam-node-insert-immediate)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         )
  )

(straight-use-package 'flycheck)
(add-hook 'coq-mode 'flycheck-mode)
(add-hook 'racket-mode 'flycheck-mode)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(save-place-mode 1)

(defun tdtron/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (magit-status))

(straight-use-package 'projectile)
(projectile-mode 1)
(global-set-key (kbd "C-M-o") 'projectile-find-file)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(when (file-directory-p "~/Projects")
  (setq projectile-project-search-path '("~/Projects")))
(setq projectile-switch-project-action #'tdtron/switch-project-action)

(with-eval-after-load 'projectile
  (progn
    (straight-use-package 'counsel-projectile)
    (counsel-projectile-mode 1)
    (global-set-key (kbd "C-S-f") 'counsel-projectile-rg)
    ))

(straight-use-package 'proof-general)
(setq proof-three-window-mode-policy 'hybrid) ;; Set default layout to hybrid
(setq proof-three-window-enable t) ;; Set 3 window enabled
;; proof-locked-face allows customizing background color I think

(straight-use-package 'company-coq)
;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)

(straight-use-package 'racket-mode)
(add-hook 'racket-mode 'racket-xp-mode)
