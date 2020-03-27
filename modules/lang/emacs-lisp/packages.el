;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

(package! elisp-def :pin "368b04da68")
(package! elisp-demos :pin "bec206bf1b")

(when (featurep! :checkers syntax)
  (package! flycheck-cask :pin "3457ae553c"))

(package! buttercup :pin "178c7954f8")
