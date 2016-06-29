;;; packages.el --- korean Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Changlog:
;; 29-06-2016
;; - spaceline의 변경점 반영
;;

;; 19-01-2016
;; - new function `korean/popup-translation', and change `google-translate-to-korean' to non-interactive

;; 17-01-2016
;; - 문서에 변경된 글꼴 관련 변수 반영
;; - `spaceline-left' 바뀐 부분 반영

;; 13-01-2016
;; - input-method 위치 조정 코드 단순화
;; - `ispell' 사전 추가 함수 개선

;; 08-01-2016
;; - `ispell' 설정이 적용되지 않던 문제 수정/ 함수 추가
;; - 글꼴 관련 부분 일부 수정 (중국 한자나 일본 가나는 옵션으로)
;; - readme.org 작성 - 말도 안되는 영어로 써 봤다

;; 06-01-2016
;; - `input-method'가 추가되었지만 위치가 마음에 들지 않으므로,
;;  왼쪽의 마이너 모드 앞에 위치시켜 두었다.

;; 05-01-2016
;; - powerline에서 spaceline으로 대체됨에 따라 관련 부분 수정
;; - core-display-init.el의 도입으로 starter-kit에서 가져와 사용하던 daemon관련 부분을
;;  `spacemacs|do-after-display-system-init'으로 교체

;;; Code:
;;;; Require packages
(setq korean-packages
      '(
        (korea-util :location built-in)
        spaceline
        (cal-korea-x :location (recipe :fetcher github :repo "cinsk/cal-korea-x"))
        google-translate
        (ispell :location built-in)
        ))

;;;; Configrations
;;;;; fonts
(spacemacs|do-after-display-system-init
 ;; Source Code Pro 보다 DejaVu Sans Mono가 더 익숙해서 바꾸고 보니,
 ;; 일부 알파벳이 네모로 나온다. 대표적으로 볼 일 없는 첫 화면과 info 문서가 그렇다.
 ;; 이 문제는 아래 두 줄이면 해결된다.
 (set-face-attribute 'variable-pitch nil
                     :family (car dotspacemacs-default-font))

 (korean//set-cjk-fonts korean-default-fonts))

;;;;; input method
(defun korean/init-korea-util ()
  (use-package korea-util
    :init
    (progn
      (when (or (eq korean-default-input-method '3f)
                (eq korean-default-input-method '390))
        (setq default-korean-keyboard (symbol-name korean-default-input-method))
        (let ((im (format "korean-hangul%s"
                          (symbol-name korean-default-input-method))))
          (setq-default default-input-method im)
          (set-input-method im)))

      (let ((ims '("" "3f" "390")))
        (setq korean-input-methods (make-ring (length ims)))
        (dolist (im ims) (ring-insert korean-input-methods im)))

      (defun korean/cycle-input-methods ()
        (interactive)
        (let ((im (ring-ref korean-input-methods -1)))
          (ring-insert korean-input-methods im)
          (setq-default default-input-method (concat "korean-hangul" im))
          (setq default-korean-keyboard im)
          (set-input-method (concat "korean-hangul" im))))

      ;; TODO show keyboard layout like virtual keyboard?
      ;; (defun korean//show-keyboard-layout (input-method)
      ;;   (if (or (string= "3f" im)
      ;;           (string= "390" im))
      ;;       (progn
      ;;         (quail-show-keyboard-layout (concat "korean-hangul" im))
      ;;         (and (eq (current-buffer) (get-buffer-window "*Help*"))
      ;;              (call-interactively 'other-window)))
      ;;     (and (get-buffer-window "*Help*")
      ;;          (delete-window (get-buffer-window "*Help*")))))

      (defun korean//deactivate-input-method ()
        (and current-input-method
             (deactivate-input-method)))

      (add-hook 'minibuffer-with-setup-hook #'korean//deactivate-input-method)
      (add-hook 'helm-before-initialize-hook #'korean//deactivate-input-method)
      (advice-add 'save-buffers-kill-emacs :before #'korean//deactivate-input-method)
      (defadvice isearch-mode (before turn-off-im activate)
        "turn off input method isearch-mode."
        (korean//deactivate-input-method))

      (when (configuration-layer/package-usedp 'visual-regexp)
        (with-eval-after-load "visual-regexp"
          (add-hook 'vr/initialize-hook #'korean//deactivate-input-method)))

      (when (configuration-layer/package-usedp 'swiper)
        (with-eval-after-load "swiper"
          (advice-add 'swiper :before #'korean//deactivate-input-method)))

      ;; TODO: `S-SPC' not working in terminal.
      (global-set-key [?\S-\ ] 'toggle-korean-input-method)
      (global-set-key [?\C-\\] 'korean/cycle-input-methods))))

;;;;; modeline
(defun korean/post-init-spaceline ()
  (add-hook 'input-method-activate-hook
            (lambda ()
              (if (and (display-graphic-p) current-input-method)
                  (setq current-input-method-title
                        (replace-regexp-in-string "한" "" current-input-method-title)))))

  (when (eq korean-input-method-modeline-position 'left)
    (delete 'input-method (assq 'main spaceline--mode-lines))
    (unless (eq 'input-method (car (nth 7 (cadr (assq 'main spaceline--mode-lines)))))
      (push 'input-method (nth 7 (cadr (assq 'main spaceline--mode-lines)))))
    (spaceline-install)))

;;;;; calendar
;; TODO 대체휴일 - 설날/한가위/어린이날
(defun korean/init-cal-korea-x ()
  (use-package cal-korea-x
    :init
    (setq calendar-week-start-day 0)
    :config
    (progn
      (setq calendar-holidays cal-korea-x-korean-holidays)

      (copy-face 'default 'calendar-sunday-face)
      (copy-face 'default 'calendar-saturday-face)
      (set-face-attribute 'calendar-sunday-face nil
                          :foreground "#ff4500")
      (set-face-attribute 'calendar-saturday-face nil
                          :foreground "#eeee00")

      (defadvice calendar-generate-month
          (after highlight-weekend-days (month year indent) activate)
        "Highlight weekend days"
        (dotimes (i 31)
          (let ((date (list month (1+ i) year)))
            (cond ((= (calendar-day-of-week date) 0)
                   (calendar-mark-visible-date date 'calendar-sunday-face))
                  ((= (calendar-day-of-week date) 6)
                   (calendar-mark-visible-date date 'calendar-saturday-face))))))

      (copy-face 'default 'calendar-iso-week-header-face)
      (copy-face 'calendar-iso-week-header-face 'calendar-iso-week-number-face)
      (set-face-attribute 'calendar-iso-week-header-face nil
                          :foreground "black"
                          :height 1.0)
      (set-face-attribute 'calendar-iso-week-number-face nil
                          :foreground "black"
                          :height 1.0)

      (setq calendar-intermonth-header
            (propertize "Wk"                  ; or e.g. "KW" in Germany
                        'font-lock-face 'calendar-iso-week-header-face))

      (setq calendar-intermonth-text
            '(propertize
              (format "%2d"
                      (car
                       (calendar-iso-from-absolute
                        (calendar-absolute-from-gregorian (list month day year)))))
              'font-lock-face 'calendar-iso-week-number-face))

      (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
      (add-hook 'today-visible-calendar-hook 'calendar-mark-holidays))))

;;;;; Spell checking
(defun korean/init-ispell ()
  (use-package ispell
    :init
    (progn
      (when (executable-find "hunspell")
        (setq-default ispell-program-name "hunspell")
        (setq ispell-really-hunspell t))

      (setq ispell-local-dictionary-alist
            '(("ko_KR"
               "[가-힣]" "[^가-힣]" "[0-9a-zA-Z]" nil
               ("-d" "ko_KR")
               nil utf-8)
              ("en_US"
               "[A-Za-z]" "[^A-Za-z]" "[0-9a-zA-Z]" nil
               ("-d" "en_US")
               nil utf-8)))

      (setq ispell-dictionary "en_US")
      (setq ispell-local-dictionary "en_US")

      (let ((dicts '("en_US" "ko_KR")))
        (setq ispell-dictionaries (make-ring (length dicts)))
        (dolist (dict dicts)
          (ring-insert ispell-dictionaries dict)))

      (defun korean/ispell-cycle-dictionaries ()
        ""
        (interactive)
        (let ((dict (ring-ref ispell-dictionaries -1)))
          (ring-insert ispell-dictionaries dict)
          (ispell-change-dictionary dict t)))

      (defun korean//ispell-add-dictionary (dict)
        "Easy to add ispell dictionary."
        (let ((dict-length (1+ (ring-size ispell-dictionaries)))
              (old-dicts   (ring-elements ispell-dictionaries))
              (new-dict    (car dict)))
          (setq ispell-dictionaries (make-ring dict-length))
          (dolist (dict old-dicts)
            (ring-insert ispell-dictionaries dict))
          (ring-insert ispell-dictionaries new-dict))

        (add-to-list 'ispell-local-dictionary-alist dict)))
    :config
    (progn
      ;; TODO: spacemacs와 어울리도록 바꿀 필요가 있다.
      (bind-keys :map ispell-minor-keymap
        ("C-S-SPC" . korean/ispell-cycle-dictionaries)
        ;; ("M-D"     . ispell-region)
        ("M-$"     . ispell-word)))))

;;;;; translation
(defun korean/post-init-google-translate ()
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "ko")

  (autoload 'google-translate-translate "google-translate-core-ui" "google-translate-translate" nil nil)
  (autoload 'popup-tip "popup" "popup-tip" nil nil)

  (defun google-translate-to-korean (&optional str)
    "Translate given string automatically without language selection prompt."
    (let ((lang (cond
                 ((string-match "[가-힣]" str)
                  "ko")
                 ((or (string-match "[ァ-ヶー]" str)
                      (string-match "[ぁ-んー]" str)
                      ;; (string-match "[亜-瑤]" str)
                      )
                  "ja")
                 ((string-match "[一-龥]" str)
                  "zh-CN")
                 (t
                  "en"))))
      (google-translate-translate lang
                                  (if (string= "ko" lang) "en" "ko")
                                  str)))

  (defun delete-leading-whitespace (str)
    (when (stringp str)
      (replace-regexp-in-string "^\\s-+" "" str)))

  (defun korean/popup-translation (&optional str)
    "Display Google translation in tooltip."
    (interactive)
    (let* ((str (cond ((stringp str) str)
                      (current-prefix-arg
                       (read-string "Google Translate: "))
                      ((use-region-p)
                       (buffer-substring (region-beginning) (region-end)))
                      (t
                       (save-excursion
                         (let (s)
                           (forward-char 1)
                           (backward-sentence)
                           (setq s (point))
                           (forward-sentence)
                           (buffer-substring s (point)))))))
           (translated-str (save-window-excursion
                             (funcall 'google-translate-to-korean (delete-leading-whitespace str))
                             (switch-to-buffer "*Google Translate*")
                             (buffer-string))))
      (if (region-active-p)
          (run-at-time 0.1 nil 'deactivate-mark))
      (kill-buffer "*Google Translate*")
      (popup-tip translated-str
                 :point (point)
                 :around t
                 ;; :height 30
                 :scroll-bar t
                 :margin t)))

  (global-set-key    "\C-ct" 'korean/popup-translation)
  (evil-leader/set-key "xgg" 'korean/popup-translation))

   
;;; packages.el ends here.
