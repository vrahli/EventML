(defvar eventml-emacs-directory   (expand-file-name "ui"      eventml-repos))
(defvar eventml-bin-directory     (expand-file-name "bin"     eventml-repos))
(defvar eventml-lib-directory     (expand-file-name "lib"     eventml-repos))
(defvar eventml-sources-directory (expand-file-name "sources" eventml-repos))

(load-file (expand-file-name "eventml-mode.el" eventml-emacs-directory))
(load-file (expand-file-name "eventml-main.el" eventml-emacs-directory))