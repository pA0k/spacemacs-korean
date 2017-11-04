;;; packages.el --- korean Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Byungsu Seo <pa0k.su@gmail.com>
;; URL: https://github.com/pA0k/spacemacs-korean
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:
;;;; Require packages
(defconst korean-packages
  '(
    (imh-mode :location local :toggle show-keyboard-layout)
    (cal-korea-x :location (recipe :fetcher github :repo "cinsk/cal-korea-x"))
    google-translate
    (ispell :location built-in)
    evil
    popwin
    ))

;;;; Configrations

;;;;; input method
(defun korean/init-imh-mode ()
  (use-package imh-mode
    :config (imh-mode 1)))

(defun korean/post-init-evil ()
  (advice-add 'evil-normal-state :before #'turn-off-input-method)

  (mapcar (lambda (mode)
            (let ((keymap (intern (format "evil-%s-state-map" mode))))
              (define-key (symbol-value keymap) [?\S- ]
                #'(lambda () (interactive)
                    (message
                     (format "Input method is disabled in %s state." evil-state))))))
          '(motion normal visual))
  )

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

(defun korean/post-init-popwin ()
  (push '("*Keyboard layout*" :dedicated t :position bottom :stick t :noselect t :height 13) popwin:special-display-config))

;;; packages.el ends here.
