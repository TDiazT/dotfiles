;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Rational Emacs loaded in %s."
;;                      (emacs-init-time))))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

(setq mac-command-modifier 'meta)

; Follow best practice by setting your environment variables so that they are available to both interactive and non-interactive shells. In practical terms, for most people this means setting them in ~/.profile, ~/.bash_profile, ~/.zshenv instead of ~/.bashrc and ~/.zshrc.
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize))
  )

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; Comment/uncomment this line to enable MELPA Stable if desired.
                         ;; See `package-archive-priorities`
                         ;; and `package-pinned-packages`.
                         ;; Most users will not need or want to do this.
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Update package contents right away if not updated
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package right away if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; All packages with use-package are :ensure t
(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; Make new buffers split vertically instead of horizontally
;; Taken from https://stackoverflow.com/a/2081978/3802589
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(use-package doom-themes
  :init
  (load-theme 'doom-snazzy t)
  ;; Not sure what this does exactly... and if I'm calling it correctly
  (doom-themes-visual-bell-config)
  )

(set-face-attribute 'default nil :font "Fira Mono" :height 150)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height 150)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile"
                    :height 150
                    :weight 'light)

;; Not sure if this is doing anything...
(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(column-number-mode)

(use-package diminish)

;; Necessary for doom modeline
(use-package all-the-icons)

;; Doom modeline
;; You must run (M-x all-the-icons-install-fonts) one time after
;; installing this package!
;; Not sure if necessary with the previous use-package call

;; Double check what this is for lol
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil)
  )

(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; (desktop-save-mode 1)

(delete-selection-mode 1)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Multiple-cursors, let's you have multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c"   . mc/edit-lines)
   )
  )

;; Taken from https://github.com/anachronic/emacs.d/blob/master/elisp/setup-editor.el
;; Let's you transpose lines like IntelliJ
(defun ach-move-line-up ()
  "Transpose the current line with the one above leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-beginning-of-line 1)
    (forward-char col)))


(defun ach-move-line-down ()
  "Transpose the current line with the one below leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-beginning-of-line 1)
    (forward-char col)))

(global-set-key (kbd "C-S-<up>") 'ach-move-line-up)
(global-set-key (kbd "C-S-<down>") 'ach-move-line-down)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatically clean whitespaces
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(setq-default fill-column 100)

(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(customize-set-variable 'kill-do-not-save-duplicates t)

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*)
         ("C-x b" . persp-counsel-switch-buffer))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

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

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package no-littering
  :custom
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  )

(recentf-mode 1)
(global-set-key (kbd "C-M-e") 'recentf-open-files)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-create-definer tdtron/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
    )
  )

;; Sets initial evil mode to emacs for these modes
;; (defun tdtron/evil-hook ()
;;   (dolist (mode '(custom-mode
;;                   eshell-mode
;;                   git-rebase-mode
;;                   term-mode))
;;     (add-to-list 'evil-emacs-state-modes mode)))
 (use-package undo-tree
   :init
   (global-undo-tree-mode 1)
   :config
   (setq undo-tree-auto-save-history nil))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)

  ;; :hook (evil-mode . tdtron/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package hydra)

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
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "???"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

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
  (setq org-ellipsis " ???"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)
  (setq org-agenda-files
        '("~/Projects/Coq-PL/Org/"))
  (tdtron/org-font-setup)
  :custom
  ;; Check whether it's worth having this shift support
  (org-support-shift-select t)

  )

;; Change headers * for other symbols
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("???" "???" "???" "???" "???" "???" "???")))


(defun tdtron/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . tdtron/org-mode-visual-fill))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(use-package org-download)

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

;; (use-package org-roam-ui
;;   ;; :straight
;;   ;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;     :after org-roam
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1) ;; This sets everything up, including remapping find file to use Ivy
  )

;; Ivy rich
(use-package ivy-rich
  :init (ivy-rich-mode 1)
  )


;; Counsel
;; General user interface (UI) to narrow down a list of selections by typing.
;; Same devs as Ivy
(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ;; ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^


;; Swiper
;; I think Swiper comes with ivy installation
(use-package swiper)

;; Syntax check
(use-package flycheck
  :defer t
  :hook (coq-mode . flycheck-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; (use-package smartparens
  ;;   :hook (prog-mode . smartparens-mode))

;; Checking paredit for Racket dev
(use-package paredit
  :ensure t
  :config
  (dolist (m '(emacs-lisp-mode-hook
               racket-mode-hook
               racket-repl-mode-hook))
    (add-hook m #'paredit-mode))
  (bind-keys :map paredit-mode-map
             ("{"   . paredit-open-curly)
             ("}"   . paredit-close-curly))
  (unless terminal-frame
    (bind-keys :map paredit-mode-map
               ("M-[" . paredit-wrap-square)
               ("M-{" . paredit-wrap-curly))))
;; ;;
;; Highlight matching parens
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1)

(defun tdtron/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . tdtron/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun tdtron/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind ("C-M-o" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)

  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'tdtron/switch-project-action)
  ;; (setq projectile-switch-project-action #'projectile-dired)

  )

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode)
  :bind ("C-S-f" . counsel-projectile-rg)
  )

(use-package magit
  :bind ("C-M-;" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  ;; Forge
  ;; (use-package forge)

(tdtron/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(use-package proof-general
  :config
  (setq coq-prog-name "/Users/tomas/.opam/_coq-platform_.2021.02.1/bin/coqtop")
  ;; (setq coq-prog-name "/Users/tomas/.opam/coq.8.13/bin/coqtop")
  (setq proof-three-window-mode-policy 'hybrid) ;; Set default layout to hybrid
  (setq proof-three-window-enable t) ;; Set 3 window enabled
;; proof-locked-face allows customizing background color I think
  )

(use-package company-coq
  :init
  ;; Load company-coq when opening Coq files
  (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package tuareg)

(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook (racket-mode . racket-xp-mode)   )

(with-eval-after-load 'lsp-mode
  (require 'lsp-racket)
  (add-hook 'racket-mode-hook #'lsp))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ws-butler which-key visual-fill-column use-package undo-tree tuareg super-save smartparens rainbow-delimiters racket-mode proof-general perspective paredit org-superstar org-roam org-download no-littering multiple-cursors minions magit lsp-ui lsp-treemacs lsp-ivy ivy-rich helpful general geiser flycheck expand-region exec-path-from-shell evil-surround evil-nerd-commenter evil-collection emojify dumb-jump doom-themes doom-modeline dired-single dired-hide-dotfiles diminish counsel-projectile company-coq company-box all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 0.85))))
 '(mode-line-inactive ((t (:height 0.85)))))
