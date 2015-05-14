(require 'ox-publish)

(defconst base-directory (file-name-directory (if load-in-progress
                                                  load-file-name
                                                (buffer-file-name))))

(setq org-publish-project-alist
      `(("main-page"
         :base-directory ,base-directory
         :base-extension "org"
         :publishing-directory ,base-directory
         :publishing-function org-html-publish-to-html
         :headline-levels 4)
        ("emacs-pe" :components ("main-page"))))
