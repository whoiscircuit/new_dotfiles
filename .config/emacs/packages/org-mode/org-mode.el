(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :custom
  org-support-shift-select t
  :commands (org-mode)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  
  (defun ox/async-tangle-and-compile ()
    (interactive)
    (message "Compiling...")
    (make-process
     :name "async-tangle-and-compile"
     :buffer nil
     :noquery t
     :sentinel (lambda (process event)
		 (when (equal event "finished\n")
		   (load (concat (file-name-sans-extension (buffer-file-name)) ".el"))))
     :command `("emacs" "-Q"
		"--batch"
		"--file" ,(buffer-file-name)
		"--eval" "(dolist (file (org-babel-tangle)) (byte-compile-file file))")))
  (defun ox/automatic-tangle-and-compile-for-config-buffers ()
    (let ((config-dir (if (boundp 'real-emacs-directory) real-emacs-directory user-emacs-directory)))
      (when (string-prefix-p (expand-file-name config-dir) (buffer-file-name))
	  (add-hook 'after-save-hook #'ox/async-tangle-and-compile t t))))
  (defun ox/enable-variable-pitch-mode-in-org-mode ()
    (variable-pitch-mode 1))
  (add-hook 'org-mode-hook #'ox/automatic-tangle-and-compile-for-config-buffers)
  (add-hook 'org-mode-hook #'ox/enable-variable-pitch-mode-in-org-mode))
