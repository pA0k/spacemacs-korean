(if (configuration-layer/package-usedp 'swiper)
    (define-key swiper-map [?\S- ] 'toggle-input-method))

(spacemacs|define-transient-state scale-cjk-font
  :title "Font Scaling Transient State"
  :doc "\n[_+_/_=_] scale up [_-_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" increase-font-size)
  ("=" increase-font-size)
  ("-" decrease-font-size)
  ("0" reset-font-size)
  ("q" nil :exit t))
(global-set-key (kbd "C-=") #'spacemacs/scale-cjk-font-transient-state/body)
(evil-leader/set-key "zX" #'spacemacs/scale-cjk-font-transient-state/body)
