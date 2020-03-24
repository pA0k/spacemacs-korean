;;; Fonts
(defun korean//set-fonts ()
  "Set font for unicode, CJK and Korean YetHangul."
  (let* ((font-list (korean//get-font-list))
         (charsets `(unicode
                     ;; Chinese
                     ,(list 'han 'cjk-misc 'bopomofo)
                     (;; Japanese
                      (#x3040 . #x309f)  ; Hiragana
                      (#x30a0 . #x30ff)  ; Katakana
                      (#x3190 . #x319f)  ; Kanbun
                      (#x31f0 . #x31ff)  ; Katakana Phonetic Extensions
                      (#xff5f . #xff9f)) ; Halfwidth katakana
                     (;; Korean
                      (#x1100 . #x11ff)  ; Hangul Jamo (첫/가/끝 자모 - 옛한글 포함)
                      ;; #x20a9 ; Won Sign
                      (#x302e . #x302f)  ; Hangul Dot Tone Mark (옛한글 방점)
                      (#x3130 . #x318f)  ; Hangul Compatiblity Jamo (ksx1001 호환)
                      (#x3200 . #x321e)  ; Enclosed CJK Letters and Months (괄호 기호)
                      (#x3260 . #x327f)  ; Enclosed CJK Letters and Months (원 기호)
                      (#xa960 . #xa97f)  ; Hangul Jamo Extended-A (옛한글 첫소리)
                      (#xac00 . #xd7af)  ; Hangul Syllables (한글 음절)
                      (#xd7b0 . #xd7ff)  ; Hangul Jamo Extended-B (옛한글 가운뎃/끝소리)
                      (#xffa0 . #xffdc)  ; Halfwidth Hangul variants
                      ;; #xffe6 ; Fullwidth Won Sign
                      )
                     (;; 옛한글 첫·가·끝 코드
                      (#x1113 . #x1159)  ; 옛한글 첫소리
                      (#x1176 . #x11a2)  ; 옛한글 가운뎃소리
                      (#x11c3 . #x11f9)  ; 옛한글 끝소리
                      ;; 옛한글 완성형
                      (#x3130 . #x318f)  ; Hangul Compatiblity Jamo (ksx1001 호환)
                      ;; 한양 PAU
                      (#xE0BC . #xEFFF) (#xF100 . #xF66E) ; 옛한글 완성형 글자
                      (#xF784 . #xF800)                   ; 옛한글 첫소리(조합형)
                      (#xF806 . #xF864)                   ; 옛한글 가운뎃소리(조합형)
                      (#xF86A . #xF8F7)                   ; 옛한글 끝소리(조합형)
                      )))
         (font-name (pop font-list)))
    (while (car charsets)
      (when (member font-name (font-family-list))
        (if (listp (car charsets))
            (mapc (lambda (c)
                    (korean//set-fontset-font c font-name))
                  (car charsets))
          (korean//set-fontset-font (car charsets) font-name)))
      (setq charsets (cdr charsets)
            font-name (pop font-list))))
  ;; rescale CJK fonts
  (korean//rescale-fonts (korean//get-font-scale 'current)))

(defun korean//set-fontset-font (charset font-name)
  (set-fontset-font nil charset
                    (font-spec :name font-name)
                    nil (if (or (listp charset)
                                (string= (symbol-name charset) "unicode"))
                            'append
                          'prepared)))

(defun korean//resize-font (action)
  "Increase/Decrease font size."
  (let (scale
        (scale-steps korean-font-scale-alist)
        (default-scale (korean//get-font-scale))
        (current-scale (korean//get-font-scale 'CURRENT)))
    (if (eq action 'reset)
        (setq scale default-scale)
      (if (eq action 'decrease)
          (setq scale-steps (reverse scale-steps)))
      (if (eq current-scale (car (last scale-steps)))
          (error "There is not enough scale data, Font size cannot be changed."))
      (setq scale (cadr (member current-scale scale-steps))))
    ;; Latin font
    (set-frame-font (format "%s:pixelsize=%d"
                            (car dotspacemacs-default-font) (car scale)))
    (korean//rescale-fonts scale)))

(defun korean//rescale-fonts (pair)
  "Rescale CJK fonts."
  (mapc (lambda (font)
          (let ((data (/ (cdr pair) (car pair))))
            (if (assoc font face-font-rescale-alist)
                (setcdr (assoc font face-font-rescale-alist) data)
              (add-to-list 'face-font-rescale-alist `(,font . ,data)))))
        (korean//get-font-list 'rescale)))

(defun korean//get-font-list (&optional rescale)
  "Return font list.
If RESCALE is non-nil, exclude unicode and Korean YetHangul."
  (cl-loop for     (key value)
           on      (if rescale
                       (spacemacs/mplist-remove
                        ;; Note: 엣한글은 가변폭이므로 제외
                        (spacemacs/mplist-remove
                         (cdr korean-default-fonts) :korean-yethangul)
                        ;; exclude unicode
                        :unicode)
                     (cdr korean-default-fonts))
           by      'cddr
           collect value))

(defun korean//get-font-scale (&optional current)
  (if current
      (assoc (float (font-get (face-attribute 'default :font) :size))
             korean-font-scale-alist)
    (assoc (float korean-default-font-size) korean-font-scale-alist)))

(defun korean/increase-font-size ()
  (interactive)
  (korean//resize-font 'increase))

(defun korean/decrease-font-size ()
  (interactive)
  (korean//resize-font 'decrease))

(defun korean/reset-font-size ()
  (interactive)
  (korean//resize-font 'reset))

;;; input method
(defun korean/change-input-method (arg)
  (interactive (list (completing-read "Which is your choice: "
                                      '("2" "3f" "390"))))
  (if (string-equal "2" arg)
      (setq default-korean-keyboard "")
    (setq default-korean-keyboard arg))
  (when (and evil-mode
             (or (evil-insert-state-p)
                 (evil-emacs-state-p)))
    (set-input-method (format "korean-hangul%s" default-korean-keyboard)))
  (message "Current input method is `korean-hangul%s'."
           default-korean-keyboard))

(defun korean//replace-hangul-indicator ()
  "Replace indicator of Korean input method 한 to ."
  (if (and (display-graphic-p)
           (member (plist-get (cdr korean-default-fonts) :korean-yethangul)
                   (font-family-list))
           current-input-method)
      (setq current-input-method-title
            (replace-regexp-in-string "한" "" current-input-method-title))))

(defun korean//turn-off-input-method (&rest _)
  (if current-input-method
      (deactivate-input-method)))

(add-hook 'input-method-activate-hook #'korean//replace-hangul-indicator)
