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

(defvar cjk-default-fonts
  `(,(car dotspacemacs-default-font)
    :korean            "D2Coding"
    :korean-yethangul  "HCR Dotum"
    :chinese           "WenQuanYi Zen Hei Mono"
    :japanese          "Migu 2M"))

(defconst font-scale-alist
  '((5  .  6.0)
    (6  .  8.0) (7  .  8.0) (8  . 10.0) (9  . 10.5) (10 . 12.0)
    (11 . 14.0) (12 . 14.0) (13 . 16.0) (14 . 16.0) (15 . 18.0)
    (16 . 20.0) (17 . 20.0) (18 . 22.0) (19 . 22.0) (20 . 24.0)
    (21 . 26.0) (22 . 26.0) (23 . 28.0) (24 . 28.0) (25 . 30.0)
    (26 . 32.0) (27 . 32.0) (28 . 34.0) (29 . 34.0) (30 . 36.0)
    (31 . 38.0) (32 . 38.5) (33 . 40.0) (34 . 40.0) (35 . 42.0)
    (36 . 44.0))
  "Alist stored matching font-size (latin . cjk)")

(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

(spacemacs|do-after-display-system-init
 (let ((scale (assoc (plist-get (cdr dotspacemacs-default-font) :size)
                     font-scale-alist)))
   (set-cjk-fonts scale))
 )
