(defun korean//set-cjk-fonts (plist)
  ""
  (let ((ko-hangul    (car plist))
        (ko-yethangul (plist-get (cdr plist) :ko-yethangul))
        (zh-hanja     (plist-get (cdr plist) :zh-hanja))
        (ja-kana      (plist-get (cdr plist) :ja-kana))
        (scale        (if (= 13 (plist-get (cdr dotspacemacs-default-font) :size))
                          1.2307692307692308
                        (plist-get (cdr plist) :scale))))

    (when ko-yethangul
      ;; 옛글 첫가끝 코드
      (set-fontset-font t '(#x1100 . #x11ff) (font-spec :name ko-yethangul))
      (set-fontset-font t '(#xa960 . #xa97c) (font-spec :name ko-yethangul))
      (set-fontset-font t '(#xd7b0 . #xd7fb) (font-spec :name ko-yethangul))
      ;; 옛글 한양 사용자 정의 영역
      (set-fontset-font t '(#xe0bc . #xefff) (font-spec :name ko-yethangul))  ; 완성형
      (set-fontset-font t '(#xf100 . #xf66e) (font-spec :name ko-yethangul))  ; 완성형
      (set-fontset-font t '(#xf784 . #xf800) (font-spec :name ko-yethangul))  ; 첫소리(조합형)
      (set-fontset-font t '(#xf806 . #xf864) (font-spec :name ko-yethangul))  ; 가운뎃소리(조합형)
      (set-fontset-font t '(#xf86a . #xf8f7) (font-spec :name ko-yethangul))) ; 끝소리(조합형)
    ;; (add-to-list 'face-font-rescale-alist `(,ko-yethangul . ,scale))

    (set-fontset-font t 'hangul              (font-spec :name ko-hangul)) ;  Korean
    (add-to-list 'face-font-rescale-alist   `(,ko-hangul . ,scale))

    (when ja-kana
      (set-fontset-font t 'japanese-jisx0208 (font-spec :name ja-kana))   ; Japanese
      (set-fontset-font t 'japanese-jisx0212 (font-spec :name ja-kana))
      (set-fontset-font t 'katakana-jisx0201 (font-spec :name ja-kana))
      (add-to-list 'face-font-rescale-alist `(,ja-kana . ,scale)))

    (when zh-hanja
      (set-fontset-font t 'kana              (font-spec :name zh-hanja))  ; chinese
      (set-fontset-font t 'han               (font-spec :name zh-hanja))
      (set-fontset-font t 'cjk-misc          (font-spec :name zh-hanja))
      (set-fontset-font t 'bopomofo          (font-spec :name zh-hanja))
      (add-to-list 'face-font-rescale-alist `(,zh-hanja . ,scale)))

    (set-fontset-font t 'symbol              (font-spec :name "Source Code Pro"))))
