;;; tdtron-keybinds.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Tomas Diaz

;; Commentary

;; General Keybinds configuration

;;; Code:

;; (straight-use-package 'which-key)

(straight-use-package 'general)

(general-create-definer tdtron/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC"
  )

(straight-use-package 'hydra)

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

(tdtron/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(load-theme :which-key "choose theme"))

(provide 'tdtron-keybinds)
;;; tdtron-keybinds.el ends here
