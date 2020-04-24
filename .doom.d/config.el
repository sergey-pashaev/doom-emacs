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
(setq doom-font (font-spec :family "Liberation Mono" :size 12))

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

(load! "elisp/functions")
(load! "elisp/keys")
(load! "elisp/projectile-extra")
(load! "elisp/ccls-extra")
(load! "elisp/yandex-browser")


;; ws-butler
(use-package ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point nil))


;; google-translate
(use-package google-translate
  :init
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-output-destination 'echo-area)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
    (setq google-translate-backend-method 'curl))


;; various settings
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

      scroll-margin                       5
      c-default-style                     "linux"
)

;; helm
(use-package helm
  :config
  (setq helm-split-window-inside-p t)
  (setq helm-buffer-max-length 60))


;; ivy
(use-package ivy
  :config
  (setq ivy-height 10)) ; for `swiper-isearch'


;; Show buffer filepath at frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;;; common minor modes
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; subtle highlighting of matching parens
(show-paren-mode t)

;; don't delete the selection with a keypress
(delete-selection-mode -1)


;; russian input indication
(defun psv/update-cursor-color ()
  "Change cursor color with keyboard layout change."
  (set-cursor-color (if current-input-method
                        "red"
                      "black")))

(when (display-graphic-p)
  (add-hook 'post-command-hook 'psv/update-cursor-color))

(defun psv/toggle-russian-input-method ()
  "Toggle internal input method between default and russian."
  (interactive)
  (if (string= current-input-method "russian-computer")
      (deactivate-input-method)
      (set-input-method "russian-computer")))


;; move around windows with meta
(require 'windmove)
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)


;; Browse with yandex-browser
(defconst *psv/yandex-browser-program* "yandex-browser")
(defconst *psv/browser-url-yandex-browser-arguments* nil)

(defun psv/browse-url-yandex-browser (url &optional _new-window)
  "Ask the Yandex Browser WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `*psv/browser-url-yandex-browser-arguments*' are also
passed to browser.  The optional argument NEW-WINDOW is not
used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
       (concat *psv/yandex-browser-program* " " url)
           nil
       *psv/yandex-browser-program*
       (append
        *psv/browser-url-yandex-browser-arguments*
        (list url)))))

(setq browse-url-browser-function 'psv/browse-url-yandex-browser)
