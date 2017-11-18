(deftheme org-mode-helvetica
  "Created 2015-12-12.")

(custom-theme-set-faces
 'org-mode-helvetica
 '(org-table ((nil (:inherit 'fixed-pitch))))
 '(org-link ((nil (:inherit 'fixed-pitch))))
 '(org-checkbox ((nil (:inherit 'fixed-pitch))))
 '(org-verbatim ((nil (:inherit 'fixed-pitch))))
 '(org-code ((nil (:inherit 'fixed-pitch))))
 '(org-indent ((t (:background "#FBF8EF" :foreground "#FBF8EF")) (nil (:inherit 'fixed-pitch))))
 '(variable-pitch
   ((t (:inherit nil :stipple nil :background nil :foreground "#000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Noto Sans")))))

(provide-theme 'org-mode-helvetica)
