;;; tdtron-racket.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Tomas Diaz

;; Commentary

;; General Keybinds configuration

;;; Code:

(straight-use-package 'racket-mode)
(add-hook 'racket-mode 'racket-xp-mode)

(provide 'tdtron-racket)
;;; tdtron-racket.el ends here
