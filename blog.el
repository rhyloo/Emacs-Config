(require 'ox-publish)
(setq org-publish-project-alist
      '(("org-notes"
		     :base-directory "~/Documents/Github/Blog/blog/"
		     :base-extension "org"
		     :auto-sitemap t                ; Generate sitemap.org automagically...
		     :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
		     :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
		     :publishing-directory "~/Documents/Github/Blog/public_html"
		     :recursive t
		     :publishing-function org-html-publish-to-html
		     :headline-levels 4             ; Just the default for this project.
		     :auto-preamble t
		 )
		("org-static"
		 :base-directory "~/Documents/Github/Blog/blog/"
		 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
		 		 :publishing-directory "~/Documents/Github/Blog/public_html"
		 :recursive t
		 :publishing-function org-publish-attachment
		 )
		("org" :components ("org-notes" "org-static"))
		))
