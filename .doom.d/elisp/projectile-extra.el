;;; projectile-extra.el --- Projectile extra functions

;;; Commentary:

;;; Code:

(require 'hydra)

(defvar prg-set-c++ '("*.h" "*.c" "*.cc"))
(defvar prg-set-c++-t '("*test.cc" "*tests.cc"))
(defvar prg-set-c++-bt '("*browsertest.cc" "*browsertests.cc"))
(defvar prg-set-mojom '("*.mojom"))
(defvar prg-set-build '("*.gn" "DEPS"))
(defvar prg-set-yaml '("*.yaml" "*.yml"))
(defvar prg-set-java '("*.java"))
(defvar prg-set-python '("*.py"))
(defvar prg-set-elisp '("*.el"))

(defvar prg-include-set prg-set-c++)
(defvar prg-exclude-set '())

(defun prg-clean ()
  "Reset `prg-set' list."
  (setq prg-include-set '())
  (setq prg-exclude-set '()))

(defun prg-include-toggle (wildcards)
  "Add or remove `WILDCARDS' into `prg-include-set' list."
  (dolist (wildcard wildcards)
    (if (member wildcard prg-include-set)
      (setq prg-include-set (delete wildcard prg-include-set))
    (setq prg-include-set (append (list wildcard) prg-include-set)))))

(defun prg-exclude-toggle (wildcards)
  "Add or remove `WILDCARDS' into `prg-exclude-set' list."
  (dolist (wildcard wildcards)
    (if (member wildcard prg-exclude-set)
      (setq prg-exclude-set (delete wildcard prg-exclude-set))
    (setq prg-exclude-set (append (list wildcard) prg-exclude-set)))))

(defun prg-check-include (wildcards)
  "Return t if all of `WILDCARDS' exists in `prg-include-set'."
  (let ((match 0))
    (dolist (wildcard wildcards)
      (if (member wildcard prg-include-set)
          (setq match (+ match 1))))
    (equal match (length wildcards))))

(defun prg-check-exclude (wildcards)
  "Return t if all of `WILDCARDS' exists in `prg-exclude-set'."
  (let ((match 0))
    (dolist (wildcard wildcards)
      (if (member wildcard prg-exclude-set)
          (setq match (+ match 1))))
    (equal match (length wildcards))))

(defun prg-status-string (wildcards)
  (format "[%s%s]"
          (if (prg-check-include wildcards) "I" " ")
          (if (prg-check-exclude wildcards) "E" " ")))

(defhydra prg (:hint none)
  "
?c? _c_++    ?b? _b_uild    ?m? _m_ojom    ?y? _y_aml    ?j? _j_ava   ?p? _p_ython    ?e? _e_lisp
?t? c++ _t_ests  c++ ?w? bro_w_sertests   [_s_earch _f_ilename _r_eset _q_uit]
Include: %`prg-include-set
Exclude: %`prg-exclude-set
"
  ("c" (prg-include-toggle prg-set-c++) (prg-status-string prg-set-c++))
  ("C" (prg-exclude-toggle prg-set-c++) nil)
  ("t" (prg-include-toggle prg-set-c++-t) (prg-status-string prg-set-c++-t))
  ("T" (prg-exclude-toggle prg-set-c++-t) nil)
  ("w" (prg-include-toggle prg-set-c++-bt) (prg-status-string prg-set-c++-bt))
  ("W" (prg-exclude-toggle prg-set-c++-bt) nil)
  ("b" (prg-include-toggle prg-set-build) (prg-status-string prg-set-build))
  ("B" (prg-exclude-toggle prg-set-build) nil)
  ("m" (prg-include-toggle prg-set-mojom) (prg-status-string prg-set-mojom))
  ("M" (prg-exclude-toggle prg-set-mojom) nil)
  ("y" (prg-include-toggle prg-set-yaml) (prg-status-string prg-set-yaml))
  ("Y" (prg-exclude-toggle prg-set-yaml) nil)
  ("j" (prg-include-toggle prg-set-java) (prg-status-string prg-set-java))
  ("J" (prg-exclude-toggle prg-set-java) nil)
  ("p" (prg-include-toggle prg-set-python) (prg-status-string prg-set-python))
  ("P" (prg-exclude-toggle prg-set-python) nil)
  ("e" (prg-include-toggle prg-set-elisp) (prg-status-string prg-set-elisp))
  ("E" (prg-exclude-toggle prg-set-elisp) nil)
  ("s" prg-search :exit t)
  ("f" psv/projectile-ripgrep-current-filename nil)
  ("r" (prg-clean))
  ("q" nil :exit t))

(map! "C-x f" #'prg/body)

(require 'rx)

(defun prg-search (regexp)
  "Run a custom ripgrep serach with `REGEXP'."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          prg-include-set
                          prg-exclude-set))

(defun psv/buffer-file-path ()
  "Return current buffer filename or default directory."
  (interactive)
  (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))


(defun psv/projectile-ripgrep-current-filename ()
  "Run a Ripgrep serach with current buffer filename at the current projectile project root."
  (interactive)
  (psv/projectile-ripgrep (regexp-quote (file-name-nondirectory (directory-file-name (psv/buffer-file-path))))
                          nil
                          nil
                          nil))

(defun psv/projectile-ripgrep (regexp args include exclude)
  "Run a Ripgrep search with `REGEXP' rooted at the current project root.

Pass list of strings `ARGS' to command line arguments, pass
`INCLUDE' & `EXCLUDE' list of strings as included/excluded glob
patterns."
  (psv/ripgrep regexp
               (projectile-project-root)
               args
               include
               (append exclude
                       projectile-globally-ignored-files
                       projectile-globally-ignored-directories)))

(defun psv/ripgrep (regexp dir args include exclude)
  "Run a Rupgrep search with `REGEXP'.

Rooted at the `DIR' with list of included globs `INCLUDE' and
  list of excluded globs `EXCLUDE'.' Pass list of strings `ARGS'
  to command line arguments."
  (ripgrep-regexp regexp
                  dir
                  (append '("-S")
                          args
                          (mapcar (lambda (val) (concat "--glob " val))
                                  include)
                          (mapcar (lambda (val) (concat "--glob !" val))
                                  exclude))))

(defun psv/copy-projectile-buffer-relative-path-to-clipboard ()
  "Put the current file name to clipboard."
  (interactive)
  (let ((filename (psv/projectile-buffer-relative-path)))
    (psv/put-to-clipboard filename)
    (message "Copied: %s" filename)))

(provide 'projectile-extra)
;;; projectile-extra.el ends here
