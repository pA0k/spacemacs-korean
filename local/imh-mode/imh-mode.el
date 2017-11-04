;;;; imh-mode is input method helper.
;; https://github.com/hillwithsmallfields/JCGS-emacs/blob/master/config/config-international.el

(require 'quail)

(defvar imh-buffer-name "*Keyboard layout*")

(defvar imh-evil-p (and (featurep 'evil) evil-mode))

(defconst imh-beautify-matrix-alist
  '(("standard"                        ; latin
     (6 5) (6 5) (6 4) (6 2))
    ("korean-hangul390"
     ((3 4) (2 6) (1 1))
     ((3 4) (2 1) (3 1) (2 3) (1 5))
     ((3 4) (2 1) (3 1) (2 1) (3 1) (2 1) (1 6))
     ((3 4) (2 1) (3 1) (2 1) (3 1) (2 1) (1 2) (2 1)))
    ("korean-hangul3f"
     ((3 6) (2 1) (3 1) (2 1) (3 1) (2 4) (1 1))
     ((3 4) (2 1) (3 1) (2 1) (3 1) (2 1) (3 1) (1 5))
     ((3 4) (2 1) (3 1) (2 1) (3 1) (2 2) (1 6))
     ((3 4) (2 1) (3 1) (2 1) (3 1) (2 1) (1 2) (2 1))))
  "자판에 표기된 한글의 첫소리,가운뎃소리와 끝소리의 연속된 개수를 나타낸다.
한글의 자모가 아닌 것(숫자와 특수문자)은 무시하였다.")

(defvar imh-sebul-jamo-colors
  '("limegreen" "royalblue" "indianred")
  "첫·가운뎃·끝소리 색깔 정의")

(defvar imh-display-custom-fn-alist
  '(("japanese" . imh-show-hiragana-table))
  "Input methods for which I'd like diagrams displayed.")


(add-to-list 'quail-keyboard-layout-alist
             `("korean-hangul3f" .
               ,(concat "                              "
                        "  ㅎㄲㅆㄺㅂㅈㅛㄿㅠㄾㅑ=ㅖ“ㅢ”ㅜ'ㅋ~);>+:\\  "
                        "  ㅅㅍㄹㅌㅕㄵㅐㅀㅓㄽㄹ5ㄷ6ㅁ7ㅊ8ㅍ9(%</    "
                        "  ㅇㄷㄴㄶㅣㄼㅏㄻㅡㅒㄴ0ㅇ1ㄱ2ㅈ3ㅂ4ㅌ·      "
                        "  ㅁㅊㄱㅄㅔㅋㅗㄳㅜ?ㅅ-ㅎ\",,..ㅗ!          "
                        "                              ")))
(add-to-list 'quail-keyboard-layout-alist
             `("korean-hangul390" .
               ,(concat "                              "
                        "  ㅎㅈㅆ@ㅂ#ㅛ$ㅠ%ㅑ^ㅖ&ㅢ*ㅜ(ㅋ)-_=+\\|  "
                        "  ㅅㅍㄹㅌㅕㅋㅐㅒㅓ;ㄹ<ㄷ7ㅁ8ㅊ9ㅍ>[{]}    "
                        "  ㅇㄷㄴㄶㅣㄺㅏㄲㅡ/ㄴ'ㅇ4ㄱ5ㅈ6ㅂ:ㅌ\"      "
                        "  ㅁㅊㄱㅄㅔㄻㅗㅀㅜ!ㅅ0ㅎ1,2.3ㅗ?          "
                        "                              ")))

(add-to-list 'quail-keyboard-layout-alist
             `("english-dvorak" .
               ,(concat "                              "
                        "  1!2@3#4$5%6^7&8*9(0)[{]}`~  "
                        "  '\",<.>pPyYfFgGcCrRlL/?=+    "
                        "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
                        "  ;:qQjJkKxXbBmMwWvVzZ        "
                        "                              ")))
(add-to-list 'quail-keyboard-layout-alist
             `("programmer-dvorak" .
               ,(concat "                              "
                        "  %&7[5{3}1(9=0*2)4+6]8!`#    "
                        "  ;:,<.>pPyYfFgGcCrRlL/?@^     "
                        "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
                        "  'qQjJkKxXbBmMwWvVzZ\"       "
                        "                              ")))

(defconst imh-hiragana-table
  "あ	か	さ	た	な	は	ま	や	ら	わ
a		ka	sa	ta	na	ha	ma	ya	ra	wa
い	き	し	ち	に	ひ	み		り
i		ki	si	chi	ni	hi	mi		ri
う	く	す	つ	ぬ	ふ	む	ゆ	る
u		ku	su	tsu	nu	fu	mu	yu	ru
え	け	せ	て	ね	へ	め		れ
e		ke	se	te	ne	he	me		re
お	こ	そ	と	の	ほ	も	よ	ろ	を
o		ko	so	to	no	ho	mo	yo	ro	wo
"
  "히라가나 50음도")


(defmacro imh-with-face (beg end fg-color)
  "Return propertized text with FG-COLOR from BEG to END."
  `(put-text-property ,beg ,end 'face (cons 'foreground-color ,fg-color)))

(defun imh-beautify-sebul-layout (sori repeat)
  (let ((color (cond ((= sori 1)
                      (car imh-sebul-jamo-colors))
                     ((= sori 2)
                      (cadr imh-sebul-jamo-colors))
                     ((= sori 3)
                      (caddr imh-sebul-jamo-colors)))))
    (while (< 0 repeat)
      (search-forward-regexp "[ㄱ-ㅎㅏ-ㅣ]" (point-max) t 1)
      (let ((end (point))
            (beg (save-excursion
                   (backward-char) (point))))
        (imh-with-face beg end color))
      (setq repeat (1- repeat)))))

(defun imh-beautify-standard-layout (left-keys right-letters)
  "Colour the background of LEFT-KEYS and RIGHT-LETTERS in this row."
  ;; (back-to-indentation)
  (let* ((row-start (point))
         (row-middle (search-forward "|"
                                     (point-max) t left-keys))
         (row-letters-end (search-forward "|"
                                          (point-max) t right-letters))
         (row-end (line-end-position)))
    ;; (mapcar (lambda (color)
    ;;           (while (< 0 left-keys)
    ;;             (search-forward-regexp "[[:graph:]]" (line-end-position) t 3)
    ;;             (let ((end (point))
    ;;                   (beg (save-excursion
    ;;                          (backward-char 3)
    ;;                          (point))))
    ;;               (imh-with-face beg end color))
    ;;             (setq left-keys (1- left-keys))))
    ;;         '("red"))
    (imh-with-face row-start row-middle (car imh-sebul-jamo-colors))
    (imh-with-face row-middle row-letters-end (cadr imh-sebul-jamo-colors))
    (imh-with-face row-letters-end row-end (caddr imh-sebul-jamo-colors))))

(defun imh-show-hiragana-table ()
  "Show Hiragana table."
  (with-output-to-temp-buffer imh-buffer-name
    (princ imh-hiragana-table)))

(defun imh-show-kbd-layout ()
  (if (get-buffer imh-buffer-name)
      ;; FIXME
      (kill-buffer imh-buffer-name))
  (when imh-mode
    (imh-set-keyboard-layout)
    (if quail-keyboard-layout-type
        (unless (minibufferp)
          (let ((custom-fn (cdr (assoc current-input-method
                                       imh-display-custom-fn-alist))))
            (if custom-fn
                (funcall custom-fn)
              (imh-beautify-keyboard-layout)))))))

(defun imh-hide-kbd-layout ()
  (let ((win (get-buffer-window imh-buffer-name)))
    (when (and imh-mode win)
      (if (and (featurep 'popwin)
               (assoc imh-buffer-name popwin:special-display-config))
          (popwin:close-popup-window)
        (delete-other-windows win))
      ;; FIXME
      (kill-buffer imh-buffer-name)
      (setq quail-keyboard-layout-type nil))))

(defun imh-set-keyboard-layout ()
  (if (or (assoc current-input-method quail-keyboard-layout-alist)
          (assoc current-input-method imh-display-custom-fn-alist))
      (quail-set-keyboard-layout (if (string= "japanese" current-input-method)
                                     "jp106"
                                   current-input-method))
    (setq quail-keyboard-layout-type nil)))

(defun imh-beautify-keyboard-layout ()
  "Show the physical layout of the current keyboard type.
The variable `quail-keyboard-layout-type' holds the currently selected
keyboard type."
  (let ((layout (assoc quail-keyboard-layout-type quail-keyboard-layout-alist))
        (hangul (string-match "hangul" quail-keyboard-layout-type)))
    (or layout
        (error "Unknown keyboard type: %s" quail-keyboard-layout-type))
    (save-excursion
      (with-output-to-temp-buffer imh-buffer-name
        (with-current-buffer standard-output
          (insert "Keyboard layout (keyboard type: "
                  quail-keyboard-layout-type
                  ")\n")
          (quail-insert-kbd-layout (cdr layout))))
      (set-buffer imh-buffer-name)
      (let ((inhibit-read-only t)
            (layout-data (or (cdr (assoc quail-keyboard-layout-type
                                         imh-beautify-matrix-alist))
                             (cdr (assoc "standard"
                                         imh-beautify-matrix-alist)))))
        (goto-char (point-min))
        (search-forward "+")
        (beginning-of-line 2)
        (mapcar (lambda (row)
                  (if hangul
                      (mapcar (lambda (col)
                                (apply 'imh-beautify-sebul-layout col))
                              row)
                    (apply 'imh-beautify-standard-layout row))
                  (beginning-of-line 3))
                layout-data)))))

(defun imh-mode-activate ()
  (add-hook 'input-method-activate-hook   #'imh-show-kbd-layout)
  (add-hook 'input-method-deactivate-hook #'imh-hide-kbd-layout)
  )

(defun imh-mode-deactivate ()
  (remove-hook 'input-method-activate-hook  #'imh-show-kbd-layout)
  (remove-hook 'input-method-deactivate-hook #'imh-hide-kbd-layout)
  )


;;;###autoload
(define-minor-mode imh-mode
  "Show current keyboard layout, when activate input method."
  :group 'quail
  :global t
  :lighter nil
  (if imh-mode
      (imh-mode-activate)
    (imh-mode-deactivate)))

(provide 'imh-mode)

;; imh-mode.el ends here.
