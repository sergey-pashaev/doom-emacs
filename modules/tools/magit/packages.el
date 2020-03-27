;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "55c5c7cb83")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "0b79aa33a4")))
