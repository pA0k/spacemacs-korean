;;; * Fonts
(defun set-cjk-fonts (scale)
  (let* ((font-list (cl-loop for     (key value)
                             on      (cdr cjk-default-fonts)
                             by      'cddr
                             collect value))
         (charsets '((hangul)
                     (;; 옛한글 첫·가·끝 코드
                      (#x1100 . #x11ff) (#xa960 . #xa97c) (#xd7b0 . #xd7fb)
                      ;; 옛한글 완성형
                      (#xe0bc . #xefff) (#xf100 . #xf66e)
                      ;; 옛한글 조합형 첫·가·끝
                      (#xf784 . #xf800) (#xf806 . #xf864) (#xf86a . #xf8f7))
                     ((#x4e00 . #x9fff))
                     (;(#x3000 . #x303f)   ; punctuation
                      (#x3040 . #x309f)    ; Hiragana
                      (#x30a0 . #x30ff)    ; Katakan
                      (#xff00 . #xffef)))) ; full-width roman and half-width katakana
         (font-name (pop font-list)))
    (while (and font-name
                (member font-name (font-family-list)))
      (mapc (lambda (charset)
              (set-fontset-font t charset (font-spec :name font-name)))
            (pop charsets))
      (setq font-name (pop font-list))))
  ;; unicode symbols
  (set-fontset-font t nil "Symbola" nil 'prepend)
  ;; rescale CJK fonts
  (rescale-cjk-fonts scale))

(defun rescale-cjk-fonts (scale)
  (let ((fontlist (cl-loop for (key value)
                           ;; Note: 옛한글은 가변폭이므로 제외한다.
                           on (spacemacs/mplist-remove
                               (cdr cjk-default-fonts) :korean-yethangul)
                           by 'cddr
                           collect value)))
    (mapc (lambda (font)
            (let ((data (/ (cdr scale) (float (car scale)))))
              (if (assoc font face-font-rescale-alist)
                  (setcdr (assoc font face-font-rescale-alist) data)
                (add-to-list 'face-font-rescale-alist `(,font . ,data)))))
          fontlist)))

(defun resize-font-size (direction)
  "Increase/Decrease font size."
  (let ((scale-steps font-scale-alist)
        (default-scale
          (assoc (plist-get (cdr dotspacemacs-default-font) :size) font-scale-alist))
        (current-scale
         (assoc (font-get (face-attribute 'default :font) :size) font-scale-alist)))
    (let (scale)
      (if (eq direction 'reset)
          (setq scale default-scale)
        (if (eq direction 'decrease)
            (setq scale-steps (reverse scale-steps)))
        (if (eq current-scale (car (last scale-steps)))
            (error "There is not enough scale data, Font size cannot be changed."))
        (setq scale (cadr (member current-scale scale-steps))))
      ;; Latin font
      (set-frame-font (format "%s:pixelsize=%d"
                              (car dotspacemacs-default-font) (car scale)))
      (rescale-cjk-fonts scale)
      ;; (modify-frame-parameters
      ;;  (selected-frame)
      ;;  (list (cons 'fullscreen 'fullheight)
      ;;        (cons 'width 86)
      ;;        (cons 'left -1)))
      )))

(defun increase-font-size ()
  (interactive)
  (resize-font-size 'increase))

(defun decrease-font-size ()
  (interactive)
  (resize-font-size 'decrease))

(defun reset-font-size ()
  (interactive)
  (resize-font-size 'reset))

;;; input method
(when korean-want-ims
  (setq korean-input-methods (make-ring (length korean-want-ims)))
  (mapc (lambda (im)
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
