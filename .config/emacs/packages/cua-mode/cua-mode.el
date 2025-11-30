(use-package cua-base
  :disabled
  :ensure nil
  :demand t
  :config
  (cua-mode t)
  (setq cua-keep-region-after-copy t
	  cua-enable-cursor-indications t
	  cua-normal-cursor-color 'box
	  cua-overwrite-cursor-color 'hbar
	  cua-read-only-cursor-color 'hollow
	  cua-prefix-override-inhibit-delay 0.05
	  cua-auto-tabify-rectangles nil)
  (setq-default cursor-in-non-selected-windows nil))
