;;; * Fonts
(defun set-cjk-font (scale)
  (let ((default   (car cjk-default-fonts))
        (korean    (plist-get (cdr cjk-default-fonts) :korean))
        (yethangul (plist-get (cdr cjk-default-fonts) :korean-yethangul))
        (chinese   (plist-get (cdr cjk-default-fonts) :chinese))
        (japanese  (plist-get (cdr cjk-default-fonts) :japanese)))
    (set-face-attribute 'default nil :family default)
    ;; Korean Hangul
    (set-fontset-font t 'hangul (font-spec :name korean))
    ;; Korean YetHangul
    (--apply-fonts yethangul
      '((#x1100 . #x11ff) (#xa960 . #xa97c) (#xd7b0 . #xd7fb) ; 옛한글 첫·가·끝 코드
        (#xe0bc . #xefff) (#xf100 . #xf66e)                   ; 옛한글 완성형
        (#xf784 . #xf800) (#xf806 . #xf864) (#xf86a . #xf8f7) ; 옛한글 조합형 첫·가·끝
        ))
    ;; Chinese
    (set-fontset-font t '(#x4e00 . #x9fff) chinese)
    ;; Japanese
    (--apply-fonts japanese
      '((#x3000 . #x303f)               ; punctuation
        (#x3040 . #x309f)               ; Hiragana
        (#x30a0 . #x30ff)               ; Katakan
        (#xff00 . #xffef)               ; full-width roman and half-width katakana
        ))
    ;; unicode characters
    (set-fontset-font t nil "Symbola" nil 'prepend)
    ;; rescale CJK fonts
    (rescale-cjk-fonts scale)))

(defmacro --apply-fonts (font-name charsets)
  (declare (indent defun))
  `(mapcar (lambda (charset)
             (set-fontset-font t charset (font-spec :name ,font-name)))
           ,charsets))

(defun rescale-cjk-fonts (font-size)
  (let ((fontlist (cl-loop for (key value)
                           ;; Note: 옛한글은 가변폭이므로 제외한다.
                           on (spacemacs/mplist-remove
                               (cdr cjk-default-fonts) :korean-yethangul)
                           by 'cddr
                           collect value)))
    (mapcar (lambda (font)
              (let ((scale (/ (alist-get font-size font-scale-alist)
			                        (float font-size))))
                (if (assoc font face-font-rescale-alist)
                    (setcdr (assoc font face-font-rescale-alist) scale)
                  (add-to-list 'face-font-rescale-alist `(,font . ,scale)))))
            fontlist)))

(defun adjust-font-size (step)
  "Increase/Decrease font size."
  (let ((scale-steps font-scale-alist)
        (default-scale
          (assoc (plist-get (cdr dotspacemacs-default-font) :size) font-scale-alist))
        (current-scale
         (assoc (font-get (face-attribute 'default :font) :size) font-scale-alist)))
    (let (size)
      (if (= step 0)
          (setq size (car default-scale))
        (if (< step 0)
            (setq scale-steps (reverse scale-steps)))
        (if (eq current-scale (car (last scale-steps)))
            (error "There is not enough scale data, Font size cannot be changed."))
        (setq size (caadr (member current-scale scale-steps))))
      ;; Latin font
      (set-frame-font (format "%s:pixelsize=%d"
                              (car dotspacemacs-default-font) size))
      ;; font size
      (set-cjk-font size)
      ;; (modify-frame-parameters
      ;;  (selected-frame)
      ;;  (list (cons 'fullscreen 'fullheight)
      ;;        (cons 'width 86)
      ;;        (cons 'left -1)))
      )))

(defun step-up-font-size ()
  (interactive)
  (adjust-font-size 1))

(defun step-down-font-size ()
  (interactive)
  (adjust-font-size -1))

(defun reset-font-size ()
  (interactive)
  (adjust-font-size 0))

;;; input method
(when korean-want-ims
  (setq korean-input-methods (make-ring (length korean-want-ims)))
  (mapcar (lambda (im)
            (ring-insert korean-input-methods im))
          korean-want-ims)

  (defun cycle-korean-input-methods ()
    (interactive)
    (let ((im (ring-ref korean-input-methods -1)))
      (ring-insert korean-input-methods im)
      (if (member im '("" "390" "3f"))
          (progn
            (setq default-korean-keyboard im)
            (set-input-method (concat "korean-hangul" im)))
        (set-input-method im))
      (let ((msg (cond ((string= im "")
                        "Dubeol(두벌식)")
                       ((string= im "3f")
                        "Sebeol391(세벌최종)")
                       ((string= im "390")
                        "Sebeol390(세벌390)")
                       (t
                        im))))
        (message "Current input method is %s." msg))))

  (global-set-key [?\C-\\] #'cycle-korean-input-methods))

(defun replace-hangul-indicator ()
  "Replace indicator of Korean input method 한 to ."
  (if (and (display-graphic-p)
           current-input-method)
      (setq current-input-method-title
            (replace-regexp-in-string "한" "" current-input-method-title))))

(defun turn-off-input-method (&rest _)
  (if current-input-method
      (deactivate-input-method)))

(add-hook 'input-method-activate-hook #'replace-hangul-indicator)
