;;; tdtron-coq.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: Tomas Diaz

;; Commentary

;; Coq configuration

;;; Code:

(crafted-package-install-package 'proof-general)
(setq proof-three-window-mode-policy 'hybrid) ;; Set default layout to hybrid
;; proof-locked-face allows customizing background color I think

(crafted-package-install-package 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)

(provide 'tdtron-coq)
;;; tdtron-coq.el ends here
