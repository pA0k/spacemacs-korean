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
;; 05-01-2016
;; - powerline에서 spaceline으로 대체됨에 따라 관련 부분 수정
;; - core-display-init.el의 도입으로 starter-kit에서 가져와 사용하던 daemon관련 부분을
;;  `spacemacs|do-after-display-system-init'으로 교체
;; 06-01-2016
;; - `input-method'가 추가되었지만 위치가 마음에 들지 않으므로,
;;  왼쪽의 마이너 모드 앞에 위치시켜 두었다.
;; 08-01-2016
;; - `ispell' 설정이 적용되지 않던 문제 수정/ 함수 추가
;; - 글꼴 관련 부분 일부 수정 (중국 한자나 일본 가나는 옵션으로)
;; - readme.org 작성 - 말도 안되는 영어로 써 봤다


;;; packages
(setq korean-packages
      '(
        (korea-util :location built-in)
        spaceline
        (cal-korea-x :location (recipe :fetcher github :repo "cinsk/cal-korea-x"))
        google-translate
        (ispell :location built-in)
        ))

;;; fonts
(spacemacs|do-after-display-system-init
 ;; 알파벳이 네모로 나오는 경우가 있더라. Source Code Pro를 제외하고 대부분이...
 ;; 인쇄 품질은 잘 모르겠고, 우선 눈에 더 편한 글꼴이 DejaVu Sans Mono라서...
 ;; 소스코드프로는 자체에 고정폭/가변폭 둘 다 있기 때문에 문제가 없는 것이고,
 ;; 그 외의 글꼴들은 그렇지 못 하기에 생기는 문제인 듯 하다.
 (set-face-attribute 'variable-pitch nil
                     :family (car dotspacemacs-default-font))
 (korean//set-cjk-fonts korean-default-fonts))

;;; input method
(defun korean/init-korea-util ()
  (use-package korea-util
    :init
    (when (or (eq korean-default-input-method '3f)
              (eq korean-default-input-method '390))
      (setq default-korean-keyboard (symbol-name korean-default-input-method))
      (let ((im (format "korean-hangul%s"
                        (symbol-name korean-default-input-method))))
        (setq-default default-input-method im)
        (set-input-method im)))

    (let ((ims '("" "3f" "390")))
      (setq im-ring (make-ring (length ims)))
      (dolist (e ims) (ring-insert im-ring e)))

    (defun korean/cycle-korean-input-method ()
      (interactive)
      (let ((im (ring-ref im-ring -1)))
        (ring-insert im-ring im)
        (setq-default default-input-method (concat "korean-hangul" im))
        (setq default-korean-keyboard im)
        (set-input-method (concat "korean-hangul" im))
        ;; TODO show keyboard layout like virtual keyboard?
        ;; (if (or (string= "3f" im)
        ;;         (string= "390" im))
        ;;     (progn
        ;;       (quail-show-keyboard-layout (concat "korean-hangul" im))
        ;;       (and (eq (current-buffer) (get-buffer-window "*Help*"))
        ;;            (call-interactively 'other-window)))
        ;;   (and (get-buffer-window "*Help*")
        ;;        (delete-window (get-buffer-window "*Help*"))))
        ))

    ;; TODO: 콘솔에서 `S-SPC'가 동작하지 않는다.
    (global-set-key [?\S-\ ] 'toggle-korean-input-method)
    (global-set-key [?\C-\\] 'korean/cycle-korean-input-method)

    (defun korean//deactivate-input-method ()
      (and current-input-method
           (deactivate-input-method)))

    (add-hook 'minibuffer-with-setup-hook #'korean//deactivate-input-method)
    (add-hook 'helm-before-initialize-hook #'korean//deactivate-input-method)
    (defadvice isearch-mode (before turn-off-im activate)
      "turn off input method isearch-mode."
      (korean//deactivate-input-method))
    ))

;;; modeline
(defun korean/post-init-spaceline ()
  (add-hook 'input-method-activate-hook
            (lambda()
              (if (and (display-graphic-p) current-input-method)
                  (setq current-input-method-title
                        (replace-regexp-in-string "한" "" current-input-method-title)))))

  (when (eq korean-input-method-modeline-position 'left)
    ;; FIXME check
    (delq (nth 3 spaceline-right) spaceline-right)
    (setf (car (nth 6 spaceline-left))
          (append (list 'input-method
                        (caar (nth 6 spaceline-left))
                        (cdar (nth 6 spaceline-left)))))))

;;; calendar
;; TODO 대체휴일
(defun korean/init-cal-korea-x ()
  (use-package cal-korea-x
    :init
    (setq calendar-week-start-day 0)
    :config
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
    (add-hook 'today-visible-calendar-hook 'calendar-mark-holidays)))

;;; spell check
(defun korean/init-ispell ()
  (use-package ispell
    :init
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

    (defun korean/add-ispell-dictionary (new-dict)
      ""
      (interactive)
      (let ((size (1+ (ring-size lang-ring)))
            (old-dict (ring-elements lang-ring)))
        (setq lang-ring (make-ring size))
        (dolist (dict old-dict)
          (ring-insert lang-ring dict))
        (ring-insert lang-ring new-dict)))

    (let ((langs '("en_US" "ko_KR")))
      (setq lang-ring (make-ring (length langs)))
      (dolist (elem langs) (ring-insert lang-ring elem)))
    (defun cycle-ispell-languages ()
      (interactive)
      (let ((lang (ring-ref lang-ring -1)))
        (ring-insert lang-ring lang)
        (ispell-change-dictionary lang t)))

    ;; TODO: spacemacs와 어울리도록 바꿀 필요가 있다.
    (bind-keys ("C-S-SPC" . cycle-ispell-languages)
               ;; ("M-D"     . ispell-region)
               ("M-$"     . ispell-word))))

;;; translation
(defun korean/post-init-google-translate ()
  (autoload 'google-translate-translate "google-translate-core-ui" "google-translate-translate" t t)

  (setq google-translate-default-source-language "En")
  (setq google-translate-default-target-language "ko")

  (defun google-translate-to-korean (&optional string)
    "문장 혹은 선택영역을 자동으로 번역한다."
    ;; TODO: 한자와 가나가 섞여 있으면 일본어로, 한자만 있다면 중국어로 인식하도록
    (interactive)
    (setq string
          (cond ((stringp string) string)
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
    (let* ((lang (cond
                  ((string-match "[가-힣]" string)
                   "ko")
                  ((or (string-match "[ァ-ヶー]" string)
                       (string-match "[ぁ-んー]" string)
                       ;; (string-match "[亜-瑤]" string)
                       )
                   "ja")
                  ((string-match "[一-龥]" string)
                   "zh-CN")
                  (t
                   "en"))))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate lang
                                  (if (string= "ko" lang) "en" "ko")
                                  string)))

  (global-set-key "\C-ct" 'google-translate-to-korean)
  (evil-leader/set-key "xgg" 'google-translate-to-korean))
