(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(haskell-mode grandshell-theme zenburn-theme auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(menu-bar-mode -1)
(show-paren-mode 1)

(load-theme 'grandshell t)

(require 'display-line-numbers)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Haskell Config
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
      '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
