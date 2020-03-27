;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "3bf9a3b1af")
(package! doom-snippets
  :recipe (:host github
           :repo "hlissner/doom-snippets"
           :files ("*.el" "*"))
  :pin "2781b782a3")
