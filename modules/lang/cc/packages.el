;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "bfe85bc009")
(if (featurep! +lsp)
    (package! ccls :pin "e5cc4c3e6f")
  (when (package! irony :pin "8387098286")
    (when (featurep! :completion company)
      (package! company-irony :pin "b44711dfce")
      (package! company-irony-c-headers :pin "72c386aeb0")))
  (when (package! rtags :pin "31f7842015")
    (when (featurep! :completion ivy)
      (package! ivy-rtags :pin "31f7842015"))
    (when (featurep! :completion helm)
      (package! helm-rtags :pin "31f7842015"))))
