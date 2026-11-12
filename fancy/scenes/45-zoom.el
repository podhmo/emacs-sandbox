;;; 45-zoom -- zoom-mode: focused window takes the golden share
(let ((mk (lambda (name)
            (with-current-buffer (get-buffer-create name)
              (insert name "\n\nThe focused window keeps ~62% of the frame;\n"
                      "the others shrink to the remaining edge.")
              (current-buffer)))))
  (switch-to-buffer (funcall mk "*focused*"))
  (delete-other-windows)
  (select-window (split-window-right))
  (switch-to-buffer (funcall mk "*other-1*"))
  (select-window (split-window-right))
  (switch-to-buffer (funcall mk "*other-2*"))
  ;; focus the leftmost window, then let zoom resize
  (select-window (frame-first-window))
  (when (fboundp 'zoom--update)
    (zoom--update)))
(fancy:shoot (getenv "FANCY_OUT"))
