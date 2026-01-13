;;; metal-loader.el --- Universal Metal Loader -*- lexical-binding: t; -*-

(unless (featurep 'metal-loader)
  (load "metal-loader-core"))

(defun metal-loader-start (metallib-name)
  "Start Metal filter with METALLIB-NAME."
  (metal-loader-load (concat (file-name-directory (or load-file-name buffer-file-name))
                             metallib-name)))

(provide 'metal-loader)
