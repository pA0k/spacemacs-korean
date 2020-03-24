(global-set-key [?\C-\\] 'korean/change-input-method)

(if (configuration-layer/package-usedp 'swiper)
    (define-key swiper-map [?\S- ] 'toggle-input-method))

(spacemacs|define-transient-state scale-cjk-font
  :title "Font Scaling for CJK Transient State"
  :doc "\n[_+_/_=_] scale up [_-_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" korean/increase-font-size)
  ("=" korean/increase-font-size)
  ("-" korean/decrease-font-size)
  ("0" korean/reset-font-size)
  ("q" nil :exit t))
;; (global-set-key (kbd "C-=") #'spacemacs/scale-cjk-font-transient-state/body)
(evil-leader/set-key "zX" #'spacemacs/scale-cjk-font-transient-state/body)
