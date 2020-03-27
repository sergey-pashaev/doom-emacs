;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;;* [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp buffers.")


;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)


;;
;;; Config

(use-package! elisp-mode
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :config
  (set-repl-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+emacs-lisp/open-repl)
  (set-eval-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+emacs-lisp-eval)
  (set-lookup-handlers! '(emacs-lisp-mode helpful-mode)
    :definition    #'+emacs-lisp-lookup-definition
    :documentation #'+emacs-lisp-lookup-documentation)
  (set-docsets! '(emacs-lisp-mode lisp-interaction-mode) "Emacs Lisp")
  (set-pretty-symbols! 'emacs-lisp-mode :lambda "lambda")
  (set-rotate-patterns! 'emacs-lisp-mode
    :symbols '(("t" "nil")
               ("let" "let*")
               ("when" "unless")
               ("advice-add" "advice-remove")
               ("defadvice!" "undefadvice!")
               ("add-hook" "remove-hook")
               ("add-hook!" "remove-hook!")
               ("it" "xit")
               ("describe" "xdescribe")))

  (setq-hook! 'emacs-lisp-mode-hook
    tab-width (or lisp-indent-offset 2)
    ;; shorter name in modeline
    mode-name "Elisp"
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp +emacs-lisp-outline-regexp)

  ;; variable-width indentation is superior in elisp
  (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode nil #'eq)

  ;; Use helpful instead of describe-* from `company'
  (advice-add #'elisp--company-doc-buffer :around #'doom-use-helpful-a)

  (add-hook! 'emacs-lisp-mode-hook
             #'outline-minor-mode
             ;; initialization
             #'+emacs-lisp-extend-imenu-h)

  (autoload 'straight-register-file-modification "straight")
  (add-hook! 'emacs-lisp-mode-hook
    (defun +emacs-lisp-init-straight-h ()
      (when (file-in-directory-p (or buffer-file-name default-directory) doom-local-dir)
        (add-hook 'after-save-hook #'straight-register-file-modification
                  nil 'local))))

  ;; Flycheck's two emacs-lisp checkers produce a *lot* of false positives in
  ;; emacs configs, so we disable `emacs-lisp-checkdoc' and reduce the
  ;; `emacs-lisp' checker's verbosity.
  (add-hook 'flycheck-mode-hook #'+emacs-lisp-reduce-flycheck-errors-in-emacs-config-h)

  ;; Special syntax highlighting for elisp...
  (font-lock-add-keywords
   'emacs-lisp-mode
   (append `(;; custom Doom cookies
             ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
           ;; highlight defined, special variables & functions
           (when +emacs-lisp-enable-extra-fontification
             `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

  ;; Recenter window after following definition
  (advice-add #'elisp-def :after #'doom-recenter-a)

  (map! :localleader
        :map emacs-lisp-mode-map
        (:prefix ("d" . "debug")
          "f" #'+emacs-lisp/edebug-instrument-defun-on
          "F" #'+emacs-lisp/edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
          "b" #'eval-buffer
          "d" #'eval-defun
          "e" #'eval-last-sexp
          "r" #'eval-region
          "l" #'load-library)
        (:prefix ("g" . "goto")
          "f" #'find-function
          "v" #'find-variable
          "l" #'find-library)))

;; Adapted from http://www.modernemacs.com/post/comint-highlighting/
(add-hook! 'ielm-mode-hook
  (defun +emacs-lisp-init-syntax-highlighting-h ()
    (font-lock-add-keywords
     nil (cl-loop for (matcher . match-highlights)
                  in (append lisp-el-font-lock-keywords-2 lisp-cl-font-lock-keywords-2)
                  collect
                  `((lambda (limit)
                      (and ,(if (symbolp matcher)
                                `(,matcher limit)
                              `(re-search-forward ,matcher limit t))
                           ;; Only highlight matches after the prompt
                           (> (match-beginning 0) (car comint-last-prompt))
                           ;; Make sure we're not in a comment or string
                           (let ((state (sp--syntax-ppss)))
                             (not (or (nth 3 state)
                                      (nth 4 state))))))
                    ,@match-highlights)))))


;;
;;; Packages


(use-package! flycheck-cask
  :when (featurep! :checkers syntax)
  :defer t
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t)))


(use-package! elisp-demos
  :defer t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  :config
  (defadvice! +emacs-lisp--add-doom-elisp-demos-a (orig-fn symbol)
    "Add Doom's own demos to help buffers."
    :around #'elisp-demos--search
    (or (funcall orig-fn symbol)
        (when-let (demos-file (doom-glob doom-docs-dir "api.org"))
          (with-temp-buffer
            (insert-file-contents demos-file)
            (goto-char (point-min))
            (when (re-search-forward
                   (format "^\\*\\*\\* %s$" (regexp-quote (symbol-name symbol)))
                   nil t)
              (let (beg end)
                (forward-line 1)
                (setq beg (point))
                (if (re-search-forward "^\\*" nil t)
                    (setq end (line-beginning-position))
                  (setq end (point-max)))
                (string-trim (buffer-substring-no-properties beg end)))))))))


(use-package! buttercup
  :defer t
  :minor ("/test[/-].+\\.el$" . buttercup-minor-mode)
  :preface
  ;; buttercup.el doesn't define a keymap for `buttercup-minor-mode', as we have
  ;; to fool its internal `define-minor-mode' call into thinking one exists, so
  ;; it will associate it with the mode.
  (defvar buttercup-minor-mode-map (make-sparse-keymap))
  :config
  (set-popup-rule! "^\\*Buttercup\\*$" :size 0.45 :select nil :ttl 0)
  (set-yas-minor-mode! 'buttercup-minor-mode)
  (when (featurep 'evil)
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map buttercup-minor-mode-map
        :prefix "t"
        "t" #'+emacs-lisp/buttercup-run-file
        "a" #'+emacs-lisp/buttercup-run-project
        "s" #'buttercup-run-at-point))


;;
;;; Project modes

(def-project-mode! +emacs-lisp-ert-mode
  :modes '(emacs-lisp-mode)
  :match "/test[/-].+\\.el$")
