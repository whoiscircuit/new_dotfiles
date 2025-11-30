(use-package tab-line
  :demand t
  :custom
  tab-line-switch-cycling t
  :bind
  ("C-<tab>" . tab-line-switch-to-next-tab)
  ("C-S-<tab>" . tab-line-switch-to-prev-tab)
  :config
  (global-tab-line-mode -1))
