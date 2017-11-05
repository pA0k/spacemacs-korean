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

(setq font-scale-alist
  '((9    . 10.5)
    (10   . 12.0)
    ;; (11   . 13.5)
    ;; (12   . 15.0)
    (13   . 16.0)
    ;; (14   . 17.0)
    (15   . 18.0)
    ;; (16   . 19.5)
    (17   . 20.0)
    ;; (18   . 21.0)
    (19   . 22.0)
    (20   . 24.0)
    (21   . 26.0)
    ;; (22   . 27.0)
    (24   . 28.0)
    (26   . 32.0)
    (28   . 34.0)
    (30   . 36.0)
    ;; (32   . 39.0)
    (34   . 40.0)
    (36   . 44.0)))

(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

(spacemacs|do-after-display-system-init
 (let ((scale
        (car (assoc (plist-get (cdr dotspacemacs-default-font) :size)
                    font-scale-alist))))
   (set-cjk-font scale))
 )
