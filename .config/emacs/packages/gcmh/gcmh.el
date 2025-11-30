(use-package gcmh
  :hook (after-init . gcmh-mode)
  :delight
  :custom
  gcmh-auto-idle-delay-factor 10
  gcmh-low-cons-threshold (* 16 1024 1024) ; 16MB
  gcmh-high-cons-threshold (* 1024 1024 1024) ; 1GB
  :config
  (remove-hook 'after-init-hook #'ox/revert-gc))
