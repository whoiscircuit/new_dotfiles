(use-package eat
  :bind (:map eat-semi-char-mode-map
	       ("C-c C-c" . nil)
	       ("C-c C-e" . nil)
	       ("C-c" . eat-self-input))
  :commands eat)
