;;; keys.el --- Global keys

;;; Commentary:

;;; Code:

(map! "C-s"   #'swiper-isearch)
(map! "C-c <" #'lsp-find-references)

(require 'bind-key)

(bind-key "C-\\" 'psv/toggle-russian-input-method)
;; (bind-key "C-x k" 'kill-this-buffer)
;; (bind-key "C-x \\" 'align-regexp)
(bind-key "C-c d" 'psv/duplicate-current-line-or-region)
(bind-key "C-<f6>" 'whitespace-mode)
(bind-key "C-<f12>" 'toggle-truncate-lines)
;; (bind-key "M-[" 'backward-paragraph)
;; (bind-key "M-]" 'forward-paragraph)
(bind-key "C-M-=" 'psv/diff-current-buffer-with-file)
;; (bind-key "C-x l" 'magit-blame)
(bind-key "C-x C-j" 'dired-jump)
(bind-key "M-RET" '+company/complete)
;; (bind-key "C-;" 'avy-goto-word-1)
;; (bind-key "C-x f" 'helm-recentf)
;; (bind-key "C-x g" 'magit-status)
(bind-key "C-x =" 'magit-diff-buffer-file)
(bind-key "M-\\" 'psv/goto-match-paren)

;; unbound dangerous keys
(global-unset-key (kbd "C-x C-c"))

;; unbound "C-x 5 0 <-> O" typo
(global-unset-key (kbd "C-x 5 0"))

;; unbound suspend-frame function
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(provide 'keys)
;;; keys.el ends here
