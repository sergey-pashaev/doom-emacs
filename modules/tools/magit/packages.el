;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "55c5c7cb83")
  (package! magit-gitflow :pin "cc41b561ec")
  (package! magit-todos :pin "ad5663aa26")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "0b79aa33a4")))
