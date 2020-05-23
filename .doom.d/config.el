;;; $DOOMDIR/config.el --- Private configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

(setq doom-font (font-spec :family "Liberation Mono" :size 12))
(setq doom-theme 'doom-solarized-light)
(setq doom-inhibit-indent-detection t)

(setq org-directory "~/org/")

;; Don't display line numbers.
(setq display-line-numbers-type nil)

;; Use decimal, not octal.
(setq read-quoted-char-radix 10)

;; Guess target directory.
(setq dired-dwim-target t)

;; Show all files with human readable sizes.
(setq dired-listing-switches "-alh") ; --group-directories-first

;; Stop asking whether to save newly added abbrev when quitting emacs.
(setq save-abbrevs nil)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;; Change the recentering order.
(setq recenter-positions '(top middle bottom))

;; Show some lines below "last" line before start scrolling.
(setq scroll-margin 5)

(setq c-default-style "linux")

;; Show buffer filepath at frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;;; Load external files.
(load! "elisp/functions")
(load! "elisp/keys")
(load! "elisp/ccls-extra")
(load! "elisp/yandex-browser")


;;; Packages configurations:
(use-package ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point nil))


(use-package google-translate
  :init
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-output-destination 'echo-area)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
    (setq google-translate-backend-method 'curl))


(use-package helm
  :config
  (setq helm-split-window-inside-p t)
  (setq helm-buffer-max-length 60))


(use-package ivy
  :config
  (setq ivy-height 10)) ; for `swiper-isearch'


(use-package windmove
  :init
  (progn
    (require 'windmove)
    (setq windmove-wrap-around t)
    (windmove-default-keybindings 'meta)))


(use-package cprg
  :init
  (require 'cprg)
  (cprg-set-globs "_c_++"          '("*.h" "*.c" "*.cc"))
  (cprg-set-globs "_t_ests"        '("*unittest.cc" "*test.cc" "*tests.cc"))
  (cprg-set-globs "bro_w_sertests" '("*browsertest.cc" "*browsertests.cc"))
  (cprg-set-globs "_m_ojom"        '("*.mojom"))
  (cprg-set-globs "_b_uild"        '("*.gn" "DEPS"))
  (cprg-set-globs "_y_aml"         '("*.yaml" "*.yml"))
  (cprg-set-globs "_j_ava"         '("*.java"))
  (cprg-set-globs "_p_ython"       '("*.py"))
  (cprg-set-globs "_e_lisp"        '("*.el"))
  (cprg-set-globs "_x_ml"          '("*.xml"))
  (cprg-load-hydra))


;;; Common minor modes:
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t) ; subtle highlighting of matching parens
(delete-selection-mode -1) ; don't delete the selection with a keypress


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
