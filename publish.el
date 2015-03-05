
(require 'ox-publish)

(defconst home (file-name-directory (or load-file-name
                                        buffer-file-name)))
(setq vc-follow-symlinks nil
      org-publish-use-timestamps-flag nil)

(setq org-publish-project-alist
      `(("main-page"
         :base-directory ,home
         :base-extension "org"
         :publishing-directory ,home
         :publishing-function org-html-publish-to-html
         :headline-levels 4)
        ("emacs-pe" :components ("main-page"))))
