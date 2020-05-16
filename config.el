(defvar show-keyboard-layout nil
  "If non nil, show keyboard layout in special buffer.")

(setq utf-translate-cjk-mode nil)       ; disable CJK coding/encoding

(set-language-environment "Korean")
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (spacemacs/system-is-mswindows)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defvar korean-default-fonts
  `(,(car dotspacemacs-default-font)
    :unicode           "Symbola"
    :chinese           "WenQuanYi Zen Hei Mono"
    :japanese          "Migu 2M"
    :korean            "D2Coding"
    :korean-yethangul  "HCR Dotum"))

(defconst korean-font-scale-alist
  '((5.0  .  6.0)
    (6.0  .  8.0) (7.0  .  8.0) (8.0  . 10.0) (9.0  . 10.5) (10.0 . 12.0)
    (11.0 . 14.0) (12.0 . 14.0) (13.0 . 16.0) (14.0 . 16.0) (15.0 . 18.0)
    (16.0 . 20.0) (17.0 . 20.0) (18.0 . 22.0) (19.0 . 22.0) (20.0 . 24.0)
    (21.0 . 26.0) (22.0 . 26.0) (23.0 . 28.0) (24.0 . 28.0) (25.0 . 30.0)
    (26.0 . 32.0) (27.0 . 32.0) (28.0 . 34.0) (29.0 . 34.0) (30.0 . 36.0)
    (31.0 . 38.0) (32.0 . 38.5) (33.0 . 40.0) (34.0 . 40.0) (35.0 . 42.0)
    (36.0 . 44.0))
  "Pair list for rescale font (latin . cjk).")

(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

(spacemacs|do-after-display-system-init
 (setq korean-default-font-size
       (font-get (face-attribute 'default :font) :size))
 (korean//set-fonts))
