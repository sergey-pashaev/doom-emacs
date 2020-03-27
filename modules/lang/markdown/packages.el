;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "e9dff50d57")
(package! edit-indirect :pin "935ded353b")

(when (featurep! +grip)
  (package! grip-mode :pin "1a61bb71a7"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown") :pin "46cd81b379"))
