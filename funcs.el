(defun korean//set-cjk-fonts (plist)
  ""
  (let ((ko-yethangul (plist-get plist :ko-yethangul))
        (ko-hangul    (plist-get plist :ko-hangul))
        (zh-hanja     (plist-get plist :zh-hanja))
        (ja-kana      (plist-get plist :ja-kana))
        (scale        (plist-get plist :scale)))

    (set-fontset-font t '(#x1100 . #xffdc) (font-spec :name ko-yethangul)) ; 옛글
    (set-fontset-font t 'hangul            (font-spec :name ko-hangul)) ;;  Korean
    (when ja-kana
      (set-fontset-font t 'japanese-jisx0208 (font-spec :name ja-kana)) ;; Japanese
      (set-fontset-font t 'japanese-jisx0212 (font-spec :name ja-kana))
      (set-fontset-font t 'katakana-jisx0201 (font-spec :name ja-kana)))
    (when zh-hanja
      (set-fontset-font t 'kana              (font-spec :name zh-hanja)) ;; chinese
      (set-fontset-font t 'han               (font-spec :name zh-hanja))
      ;; (set-fontset-font t 'symbol            (font-spec :name zh-hanja))
      (set-fontset-font t 'cjk-misc          (font-spec :name zh-hanja))
      (set-fontset-font t 'bopomofo          (font-spec :name zh-hanja)))

    ;; one CJK char width = two latin chars :fixed width fonts
    (dolist (elt `((,ko-hangul    . ,scale)
                   ;; (,ko-yethangul . ,scale) ;; variable width
                   (if ,ja-kana (,ja-kana   . ,scale))
                   (if ,zh-hanja (,zh-hanja . ,scale))))
      (add-to-list 'face-font-rescale-alist elt))))
