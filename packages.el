;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! nose :disable t)

(package! dockerfile-mode)
(package! groovy-mode)
(package! yaml-mode)
(package! olivetti)
(package! zpresent)
(package! plantuml-mode)
(package! prism)
(package! emojify)
(package! lsp-treemacs)
(package! powershell)
(package! squirrel-mode
  :recipe (:host github :repo "thechampagne/squirrel-mode"))
