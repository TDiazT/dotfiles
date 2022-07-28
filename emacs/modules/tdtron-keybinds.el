;;; tdtron-keybinds.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Tomas Diaz

;; Commentary

;; General Keybinds configuration

;;; Code:

;; (straight-use-package 'which-key)

(global-set-key (kbd "C-M-;") 'magit-status)

(crafted-package-install-package 'general)

(general-create-definer tdtron/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC"
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

(crafted-package-install-package 'hydra)

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
  "s" '(hydra-text-scale/body :which-key "scale text"))

(provide 'tdtron-keybinds)
;;; tdtron-keybinds.el ends here
