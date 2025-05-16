(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

(setq mac-command-modifier 'meta)

; Follow best practice by setting your environment variables so that they are available to both interactive and non-interactive shells. In practical terms, for most people this means setting them in ~/.profile, ~/.bash_profile, ~/.zshenv instead of ~/.bashrc and ~/.zshrc.
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize))
  )

;;;; Make the titlebar on macOS transparent
(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar
    :init
    (ns-auto-titlebar-mode)))

;; Set up the visible bell
(customize-set-variable 'visible-bell 1)

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

;; Make it full screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

(use-package doom-themes
  :init
  (disable-theme 'deeper-blue)
  (load-theme 'doom-snazzy t)
  (doom-themes-visual-bell-config)
  )

(add-hook '
 emacs-startup-hook
 (lambda ()
            (custom-set-faces
             `(default ((t (:font "FiraCode Nerd Font" :weight light :height 120))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Iosevka Aile" :height 120 :weight light)))))))

(use-package helpful
             :bind
             ([remap describe-function] . helpful-callable)
             ([remap describe-symbol] . helpful-symbol)
             ([remap describe-variable] . helpful-variable)
             ([remap describe-command] . helpful-command)
             ([remap describe-key] . helpful-key)
             ("C-h f" . helpful-function)
             )

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
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  )

;; Bind extra `describe-*' commands
(global-set-key (kbd "C-h K") #'describe-keymap)

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(desktop-save-mode 1)

(delete-selection-mode 1)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
;; (use-package ws-butler
;;   :hook ((text-mode . ws-butler-mode)
;;          (prog-mode . ws-butler-mode)))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(setq-default fill-column 100)

(setq-default indent-tabs-mode nil)

;; N.B. Emacs 28 has a variable for using short answers, which should
;; be preferred if using that version or higher.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(customize-set-variable 'kill-do-not-save-duplicates t)

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1) ; turn on paren match highlighting

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(defun tdtron/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)
    (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

(use-package consult
  :bind
  ("C-s" . consult-line)
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region)
  )

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides  '((file (styles . (partial-completion))))))

(use-package embark
  :bind
  ("C-." . embark-act)
  ([remap describe-bindings] . embark-bindings)
  :config
  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

;; (use-package corfu-doc)

(use-package corfu
    :init
    (global-corfu-mode)
    :custom
    (corfu-cycle t) ; Allows cycling through candidates
    (corfu-auto t)  ; Enable auto completion
    (corfu-auto-prefix 2) ; Complete with less prefix keys
    (corfu-auto-delay 0.0) ; No delay for completion
    (corfu-echo-documentation 0.25) ; Echo docs for current completion option

    :hook (corfu-mode . corfu-popupinfo-mode)

    :bind (:map corfu-map
                ("M-p" . corfu-popupinfo-scroll-down)
                ("M-n" . corfu-popupinfo-scroll-up)
                ("M-d" . corfu-popupinfo-toggle))
    )

(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                            corfu-quit-no-match t
                            corfu-auto nil)
            (corfu-mode)))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )

(use-package dired
  :ensure nil
  :straight nil
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
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  )

(add-hook 'after-init-hook #'recentf-mode)
(global-set-key (kbd "C-M-e") 'recentf-open-files)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; (use-package which-key
;;   :init (which-key-mode)
;;   :diminish which-key-mode
;;   :config
;;   (setq which-key-idle-delay 0.3))

(use-package general
    :config
    (general-create-definer tdtron/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC"
      )
    )

;; General purpose Toggles
  (tdtron/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tw" 'whitespace-mode
    "tt" '(load-theme :which-key "choose theme"))

  ;; Org-roam
  (with-eval-after-load 'org-roam-mode
    (tdtron/leader-keys
      "r" '(:ignore t :which-key "org-roam")
      "rl"  '(org-roam-buffer-toggle :which-key "buffer toggle")
      "rf" '(org-roam-node-find :which-key "find")
      "rg" '(org-roam-graph :which-key "graph")
      "ri" '(org-roam-node-insert :which-key "insert")
      "rI" '(org-roam-node-insert-immediate :which-key "insert immediate")
      "rc" '(org-roam-capture :which-key "capture")
      ;; Dailies
      "rj" '(org-roam-dailies-capture-today :which-key "capture today")
      "ry" '(org-roam-dailies-capture-yesterday :which-key "capture yesterday")
      )
    (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)
    )

  ;; Magit
  (with-eval-after-load 'magit
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
    )

(use-package hydra)


(with-eval-after-load 'coq-mode
  (defhydra hydra-coq ()
    "Coq actions"
    ("j" proof-assert-next-command-interactive "Next")
    ("k" proof-undo-last-successful-command "Undo")
    ("f" nil "finished" :exit t)
    )
  (tdtron/leader-keys
    "c"   '(:ignore t :which-key "coq")
    "cs"  '(coq-Search :which-key "search")
    "ci"  '(hydra-coq/body :which-key "interactive")
    ))

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

;; Sets initial evil mode to emacs for these modes
(defun tdtron/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))


  (use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    (setq evil-respect-visual-line-mode t)

    :hook (evil-mode . tdtron/evil-hook)
    :custom
    ;; C-h is backspace in insert state
    (customize-set-variable 'evil-want-C-h-delete t)

    :config
    (evil-mode 1)
    ;; Make C-g revert to normal state
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

    ;; Make evil search more like vim
    (evil-select-search-module 'evil-search-module 'evil-search)

    ;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
    (global-set-key (kbd "C-M-u") 'universal-argument)
    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

  ;; Turn on undo-tree globally
  (when (< emacs-major-version 28)
    (use-package undo-tree
      :after evil
      :init
      (global-undo-tree-mode 1)
      :config
      (setq undo-tree-auto-save-history nil)))

  (if (< emacs-major-version 28)
      (customize-set-variable 'evil-undo-system 'undo-tree)
    (customize-set-variable 'evil-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
       ; (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
       (evil-collection-outline-bind-tab-p nil)
       :config
       (setq evil-collection-mode-list
             (remove 'lispy evil-collection-mode-list))
       (evil-collection-init))

(use-package evil-nerd-commenter
  :init
  ;; Turn on Evil Nerd Commenter
  (evilnc-default-hotkeys))


(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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

;; Tangle on save
 (defun org-babel-tangle-config ()
   (when (string-equal (buffer-file-name)
                       (expand-file-name "Emacs.org"
                                         (expand-file-name "Projects/dotfiles" (getenv "HOME"))))
     ;; Dynamic scoping to the rescue
     (let ((org-confirm-babel-evaluate nil))
       (org-babel-tangle))))

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
   :hook (org-mode . (lambda ()
                       (add-hook 'after-save-hook #'org-babel-tangle-config)))
   :hook (org-mode . org-toggle-pretty-entities)
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
   (tdtron/org-font-setup)

   :custom
   ;; Check whether it's worth having this shift support
   (org-support-shift-select t)
   ;; Return or left-click with mouse follows link
   (org-return-follows-link t)
   (org-mouse-1-follows-link t)
   ;; Display links as the description provided
   (org-link-descriptive t)
   ;; Hide markup markers
   (org-hide-emphasis-markers t)
   (org-entities-user
    '(("square" "\\square" t "&#9633" "■" "" "□")
      ("sqsubseteq" "\\sqsubseteq" t "&#2291" "⊑" "" "⊑")))
   )

;; Change headers * for other symbols
 (use-package org-superstar
   :after org
   :hook (org-mode . org-superstar-mode)
   :custom
   (org-superstar-remove-leading-stars t)
   (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))


 (defun tdtron/org-mode-visual-fill ()
   (setq visual-fill-column-width 110
         visual-fill-column-center-text t)
   (visual-fill-column-mode 1))

 (use-package visual-fill-column
   :defer t
   :hook (org-mode . tdtron/org-mode-visual-fill))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; (use-package org-download)

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

;; Syntax check
;; (use-package flycheck
;;   :defer t
;;   :hook (coq-mode . flycheck-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(savehist-mode 1)

(save-place-mode 1)

;; (defun tdtron/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook (lsp-mode . tdtron/lsp-mode-setup)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
;;   :config
;;   (lsp-enable-which-key-integration t))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; (use-package lsp-treemacs
;;   :after lsp)

;; (use-package lsp-ivy)

;; (use-package company
;;   :after lsp-mode
;;   :hook (lsp-mode . company-mode)
;;   :bind (:map company-active-map
;;          ("<tab>" . company-complete-selection))
;;         (:map lsp-mode-map
;;          ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; (defun tdtron/switch-project-action ()
;;   "Switch to a workspace with the project name and start `magit-status'."
;;   ;; TODO: Switch to EXWM workspace 1?
;;   (persp-switch (projectile-project-name))
;;   (magit-status))

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :demand t
;;   :bind ("C-M-o" . projectile-find-file)
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)

;;   :init
;;   (when (file-directory-p "~/Projects")
;;     (setq projectile-project-search-path '("~/Projects")))
;;   (setq projectile-switch-project-action #'tdtron/switch-project-action)
;;   ;; (setq projectile-switch-project-action #'projectile-dired)

;;   )

;; (use-package counsel-projectile
;;   :after projectile
;;   :config
;;   (counsel-projectile-mode)
;;   :bind ("C-S-f" . counsel-projectile-rg)
;;   )

(use-package magit
  :bind ("C-M-;" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  ;; Forge
;;   ;; (use-package forge)

(use-package proof-general
  :config
  (setq proof-three-window-mode-policy 'hybrid) ;; Set default layout to hybrid
  )

(use-package company-coq
  :hook
  ;; Load company-coq when opening Coq files
  (coq-mode . company-coq-mode))

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (use-package tuareg)

;; (use-package racket-mode
;;              :mode "\\.rkt\\'"
;;   :hook (racket-mode . racket-xp-mode)   )

;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-racket)
;;   (add-hook 'racket-mode-hook #'lsp))
