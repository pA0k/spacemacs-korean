(defvar korean-default-fonts '("NanumGothicCoding"
                               :ko-yethangul "NanumBarunGothic YetHangul"
                               :ja-kana
                               :zh-hanja
                               :scale        1.2))

(defvar korean-default-input-method nil
  "default is `nil', it means du-beol-sik.
 If you are se-beol-sik user, you could set the `3f' or `390'.")

(defvar korean-input-method-modeline-position nil)

(spacemacs|do-after-display-system-init
 ;; Source Code Pro 보다 DejaVu Sans Mono가 더 익숙해서 바꾸고 보니,
 ;; 일부 알파벳이 네모로 나온다. 대표적으로 볼 일 없는 첫 화면과 info 문서가 그렇다.
 ;; 이 문제는 아래 두 줄이면 해결된다.
 (set-face-attribute 'variable-pitch nil
                     :family (car dotspacemacs-default-font))

 (korean//set-cjk-fonts korean-default-fonts)
 )
