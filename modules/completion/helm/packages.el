;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm :pin "21e778bc88")
(package! helm-rg :pin "785a80fe5c")
(package! helm-c-yasnippet :pin "65ca732b51")
(package! helm-company :pin "6eb5c2d730")
(package! helm-projectile :pin "5328b74ddd")
(package! swiper-helm :pin "93fb6db87b")
(when (featurep! +childframe)
  (package! posframe :pin "8a9af547e6"))
