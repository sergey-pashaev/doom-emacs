;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)
(setq doom-inhibit-indent-detection t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
(use-package google-translate
  :init
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-output-destination 'echo-area)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
    (setq google-translate-backend-method 'curl))

(setq read-quoted-char-radix              10 ; use decimal, not octal

      ;; dired
      dired-dwim-target                   t ; guess target directory
      dired-listing-switches              "-alh" ; --group-directories-first

      ;; stop asking whether to save newly added abbrev when quitting emacs
      save-abbrevs                        nil

      ;; Save whatever’s in the current (system) clipboard before
      ;; replacing it with the Emacs’ text.
      ;; https://github.com/dakrone/eos/blob/master/eos.org
      save-interprogram-paste-before-kill t

      ;; changing the recentering order
      recenter-positions                  '(top middle bottom)
)

;;; common minor modes
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; subtle highlighting of matching parens
(show-paren-mode t)

;; don't delete the selection with a keypress
(delete-selection-mode -1)

(defun psv/update-cursor-color ()
  "Change cursor color with keyboard layout change."
  (set-cursor-color (if current-input-method
                        "red"
                      "black")))

(when (display-graphic-p)
  (add-hook 'post-command-hook 'psv/update-cursor-color))

;; delete trailing whitespace before save
;; (defun psv/before-save-hook ()
;;   "Delete trailing whitespace everywhere."
;;   (delete-trailing-whitespace))

;; (add-hook 'before-save-hook 'psv/before-save-hook)

;; russian input
(defun psv/toggle-russian-input-method ()
  "Toggle internal input method between default and russian."
  (interactive)
  (if (string= current-input-method "russian-computer")
      (deactivate-input-method)
      (set-input-method "russian-computer")))

(defun psv/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun psv/diff-current-buffer-with-file ()
  "Diff current buffer with associated file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

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

;; unbound dangerous keys
(global-unset-key (kbd "C-x C-c"))

;; unbound "C-x 5 0 <-> O" typo
(global-unset-key (kbd "C-x 5 0"))

;; unbound suspend-frame function
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(require 'windmove)
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; Show buffer filepath at frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq ivy-height 10) ; for `swiper-isearch'
