(use-package fonts
  :ensure nil
  :no-require
  :if (display-graphic-p)
  :demand t
  :config

(defun ox/create-fontset (name)
  "create an empty fontset named NAME"
  (create-fontset-from-fontset-spec
   (font-xlfd-name
    (font-spec :registry name))))

(defun ox/set-default-font (&rest fonts)
  "set the default/main emacs font with fallbacks"
  (unless (null fonts)
    (if (find-font (font-spec :name (car fonts)))
	  (set-face-attribute 'default nil
			      :font (car fonts)
			      :height 90)
	(apply #'ox/set-default-font (cdr fonts)))))

(defun ox/set-font (fontset script fonts)
  "set a font for a specific SCRIPT in FONTSET with fallbacks.
   if non of the fonts are available look for an appropriate font on the system"
  (set-fontset-font fontset script (car fonts))
  (dolist (font (cdr fonts))
    (set-fontset-font fontset script font nil 'append))
  (set-fontset-font fontset script (font-spec :script script) nil 'append))

(ox/create-fontset "fontset-sans")

(setq ox/fonts-fallback-latin '("ioZevka Quasi"
			 "Noto Sans"
			 "Arimo"
			 "Liberation Sans"
			 "FreeSans"
			 "DejaVu Sans"
			 "Segoe UI"))
(ox/set-font "fontset-sans" 'latin ox/fonts-fallback-latin)

(setq ox/fonts-fallback-arabic '("Vazirmatn UI NL"
			       "Vazirmatn UI"
			       "Vazirmatn NL"
			       "Vazirmatn"
			       "Noto Sans Arabic"
			       "Noto Naskh Arabic"
			       "IRANSansWeb"
			       "Segoe UI")) ; for windows
(ox/set-font "fontset-sans" 'arabic ox/fonts-fallback-arabic)

(setq ox/fonts-fallback-han '("Noto Sans CJK SC"
				"MS Gothic")) ; for windows
(ox/set-font "fontset-sans" 'han ox/fonts-fallback-han)

(setq ox/fonts-fallback-kana '("Noto Sans CJK JP"
				 "MS Gothic")) ; for windows
(ox/set-font "fontset-sans" 'han ox/fonts-fallback-kana)

(setq ox/fonts-fallback-hangul '("Noto Sans CJK KR"
				   "Malgun Gothic")) ; for windows
(ox/set-font "fontset-sans" 'hangul ox/fonts-fallback-hangul)

(setq ox/fonts-fallback-hebrew '("Noto Sans Hebrew"))
(ox/set-font "fontset-sans" 'hebrew ox/fonts-fallback-hebrew)

(setq ox/fonts-fallback-khmer '("Noto Sans Khmer"
				  "Leelawadee UI")) ; for windows
(ox/set-font "fontset-sans" 'khmer ox/fonts-fallback-khmer)

(setq ox/fonts-fallback-lao '("Noto Sans Lao"
				"Leelawadee UI")) ; for windows
(ox/set-font "fontset-sans" 'lao ox/fonts-fallback-lao)

(setq ox/fonts-fallback-burmese '("Noto Sans Myanmar"
				    "Myanmar Text")) ; for windows
(ox/set-font "fontset-sans" 'burmese ox/fonts-fallback-burmese)

(setq ox/fonts-fallback-thai '("Noto Sans Thai"
				 "Leelawadee UI")) ; for windows
(ox/set-font "fontset-sans" 'thai ox/fonts-fallback-thai)

(setq ox/fonts-fallback-ethiopic '("Noto Sans Ethiopic"
				     "Ebrima")) ; for windows
(ox/set-font "fontset-sans" 'ethiopic ox/fonts-fallback-ethiopic)

(setq ox/fonts-fallback-gujarati '("Noto Sans Gujarati"
				     "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'gujarati ox/fonts-fallback-gujarati)

(setq ox/fonts-fallback-devanagari '("Noto Sans Devanagari"
				      "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'devanagari ox/fonts-fallback-devanagari)

(setq ox/fonts-fallback-kannada '("Noto Sans Kannada"
				   "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'kannada ox/fonts-fallback-kannada)

(setq ox/fonts-fallback-malayalam '("Noto Sans Malayalam"
				      "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'malayalam ox/fonts-fallback-malayalam)

(setq ox/fonts-fallback-oriya '("Noto Sans Oriya"
				  "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'oriya ox/fonts-fallback-oriya)

(setq ox/fonts-fallback-sinhala '("Noto Sans Sinhala"
				    "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'sinhala ox/fonts-fallback-sinhala)

(setq ox/fonts-fallback-tamil '("Noto Sans Tamil"
			      "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'tamil ox/fonts-fallback-tamil)

(setq ox/fonts-fallback-telugu '("Noto Sans Telugu"
			       "Nirmala UI")) ; for windows
(ox/set-font "fontset-sans" 'telugu ox/fonts-fallback-telugu)

(setq ox/fonts-fallback-tibetan '("Noto Sans Tibetan"
				"Microsoft Himalaya")) ; for windows
(ox/set-font "fontset-default" 'tibetan ox/fonts-fallback-tibetan)

(set-face-attribute 'variable-pitch nil
		      :font "fontset-sans"
		      :fontset "fontset-sans"
		      :height 1.2)

(ox/create-fontset "fontset-serif")

(ox/set-font "fontset-serif"
	       'latin
	       `("ioZevka Slabs"
		 "Noto Serif"
		 "Liberation Serif"
		 "FreeSerif"
		 "Dejavu Serif"
		 "Times New Roman" ; for windows
		 ,@ox/fonts-fallback-latin))

(ox/set-font "fontset-serif"
	       'arabic
	       `("Noto Naskh Arabic"
		 "Arial" ; for windows
		 ,@ox/fonts-fallback-arabic))

(ox/set-font "fontset-serif"
	       'han
	       `("Noto Serif CJK SC"
		 ,@ox/fonts-fallback-han))

(ox/set-font "fontset-serif"
	       'kana
	       `("Noto Serif CJK JP"
		 ,@ox/fonts-fallback-kana))

(ox/set-font "fontset-serif"
	       'hangul
	       `("Noto Serif CJK KP"
	       ,@ox/fonts-fallback-hangul))

(ox/set-font "fontset-serif"
	       'hebrew
	       `("Noto Serif Hebrew"
		 ,@ox/fonts-fallback-hebrew))

(ox/set-font "fontset-serif"
	       'khmer
	       `("Noto Serif Khmer"
		 ,@ox/fonts-fallback-khmer))

(ox/set-font "fontset-serif"
	       'lao
	       `("Noto Serif Lao"
		 ,@ox/fonts-fallback-lao))

(ox/set-font "fontset-serif"
	       'burmese
	       `("Noto Serif Myanmar"
		 ,@ox/fonts-fallback-burmese))

(ox/set-font "fontset-serif"
		   'thai
		   `("Noto Serif Thai"
		     ,@ox/fonts-fallback-thai))

(ox/set-font "fontset-serif"
	       'ethiopic
	       `("Noto Serif Ethiopic"
		 ,@ox/fonts-fallback-ethiopic))

(ox/set-font "fontset-serif"
	       'gujarati
	       `("Noto Serif Gujarati"
		 ,@ox/fonts-fallback-gujarati))

(ox/set-font "fontset-serif"
	       'devanagari
	       `("Noto Sans Devanagari"
		 ,@ox/fonts-fallback-devanagari))

(ox/set-font "fontset-serif"
	       'kannada
	       `("Noto Serif Kannada"
		 ,@ox/fonts-fallback-kannada))

(ox/set-font "fontset-serif"
	       'malayalam
	       `("Noto Serif Malayalam"
		 ,@ox/fonts-fallback-malayalam))

(ox/set-font "fontset-serif"
	       'oriya
	       `("Noto Serif Oriya"
		 ,@ox/fonts-fallback-oriya))

(ox/set-font "fontset-serif"
	       'sinhala
	       `("Noto Serif Sinhala"
		 ,@ox/fonts-fallback-sinhala))

(ox/set-font "fontset-serif"
	       'tamil
	       `("Noto Serif Tamil"
		 ,@ox/fonts-fallback-tamil))

(ox/set-font "fontset-serif"
	       'telugu
	       `("Noto Serif Telugu"
		 ,@ox/fonts-fallback-telugu))

(ox/set-font "fontset-serif"
	       'tibetan
	       `("Noto Sans Tibetan"
		 ,@ox/fonts-fallback-tibetan))

(ox/set-default-font "ioZevka Code"
		       "ioZevka Mono"
		       "JetBrains Mono"
		       "Noto Sans Mono"
		       "Courier New" ; for windows
		       "monospace")

;; سلام چطوری؟
(ox/set-font "fontset-default"
	       'arabic
	       `(,(font-spec :name "Vazir Code Extra Height WOL" :size 13)
		 ,(font-spec :name "Vazir Code Extra Height" :size 13)
		 ,(font-spec :name "Vazir Code WOL" :size 13)
		 ,(font-spec :name "Vazir Code" :size 13)
		 "Courier New" ; for windows
		 "monospace"
		 ,@ox/fonts-fallback-arabic))

;; 你好 早晨
(ox/set-font "fontset-default"
	       'han
	       `("Sarasa Mono SC"
		 "Source Han Mono SC"
		 ,@ox/fonts-fallback-han))
;; こんにちは
(ox/set-font "fontset-default"
	       'kana
	       `("Sarasa Mono J"
		 ,@ox/fonts-fallback-kana))
;; 안녕하세요
(ox/set-font "fontset-default"
	       'hangul
	       `("Sarasa Mono K"
		 "Source Han Mono K"
		 ,@ox/fonts-fallback-hangul))

;; Hebrew:  חפש סתם אהב
(ox/set-font "fontset-default"
	       'hebrew
	       '("FreeMono"
		 "Courier New"))

(ox/set-font "fontset-default" 'khmer   ox/fonts-fallback-khmer)
(ox/set-font "fontset-default" 'lao     ox/fonts-fallback-lao)
(ox/set-font "fontset-default" 'burmese ox/fonts-fallback-burmese)
(ox/set-font "fontset-default" 'thai    ox/fonts-fallback-thai)

(ox/set-font "fontset-default" 'ethiopic ox/fonts-fallback-ethiopic)

(ox/set-font "fontset-default" 'gujarati  ox/fonts-fallback-gujarati)
(ox/set-font "fontset-default" 'devanagari ox/fonts-fallback-devanagari)
(ox/set-font "fontset-default" 'kannada   ox/fonts-fallback-kannada)
(ox/set-font "fontset-default" 'malayalam ox/fonts-fallback-malayalam)
(ox/set-font "fontset-default" 'oriya     ox/fonts-fallback-oriya)
(ox/set-font "fontset-default" 'sinhala   ox/fonts-fallback-sinhala)
(ox/set-font "fontset-default" 'tamil     ox/fonts-fallback-tamil)
(ox/set-font "fontset-default" 'telugu    ox/fonts-fallback-telugu)
(ox/set-font "fontset-default" 'tibetan   ox/fonts-fallback-tibetan)

(set-face-attribute 'fixed-pitch nil
		      :font "fontset-default"
		      :fontset "fontset-default")

(ox/set-font "fontset-default"
	       'emoji
	       '("Apple Color Emoji"
		 "Noto Color Emoji"
		 "Noto Emoji"))
;;emacs on windows does not support colored emojis
(when (eq system-type 'windows-nt)
  (ox/set-font "fontset-default"
		 'emoji
		 '("Noto Emoji"
		   "Segoe UI Emoji")))

) ; end of use-package
