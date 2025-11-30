(use-package keybindings
  :ensure nil
  :no-require
  :demand t
  :config

(defvar ox/fast-keyseq-timeout 50) ; this timeout determines if a ESC is a Alt combination or a single <escape>
(defun ox/-tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
	   (sit-for (/ ox/fast-keyseq-timeout 1000.0)))
      [escape] map))

(defun ox/-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun ox/catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
    (let ((esc-binding (ox/-lookup-key input-decode-map ?\e)))
      (define-key input-decode-map
		  [?\e] `(menu-item "" ,esc-binding :filter ox/-tty-ESC-filter)))))
(ox/catch-tty-ESC)

(setq map (make-keymap))

)
