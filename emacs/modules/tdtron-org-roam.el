;;; tdtron-org-roam.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Tomas Diaz

;; Commentary

;; General Org Roam configuration

;;; Code:

;; Let's you insert without confirming.
;; Uses the first template for the immediate nodes (ie. default right now)
;; Source: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(crafted-package-install-package 'org-roam)

(setq org-roam-v2-ack t)
(setq org-roam-directory "~/Projects/Notes")
(setq org-roam-completion-everywhere t)
(setq org-roam-completion-system 'default)
(setq org-roam-capture-templates
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

;; Run this after setting the roam directory
(org-roam-db-autosync-mode 1)

(provide 'tdtron-org-roam)
;;; tdtron-org-roam.el ends here
