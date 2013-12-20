;; mako
(require 'mmm-mode)
(require 'mmm-auto)
(require 'mmm-mako)
(setq mmm-global-mode 'maybe)

(add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'mako)
(setq mmm-submode-decoration-level 1)

(defface individual-comment-face  '((t (:foreground "gray70")))
  "comment face")
(setq individual-comment-face 'individual-comment-face)

(setq individual-comment-font-lock-keywords
      '((".*" . individual-comment-face)))

(define-derived-mode individual-comment-mode text-mode "individual-comment"
  "comment mode"
  (set (make-local-variable 'indent-line-function) (lambda ()))
  (set (make-local-variable 'font-lock-defaults)
       `(individual-comment-font-lock-keywords)))
;;hmm.
                                        ;see also:http://www.fides.dti.ne.jp/~oka-t/emacs.html

(defun mmm-individual-indent-region (start end &optional column)
  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (catch 'break 
        (let ((pt (point)))
          (while (progn (forward-line 1) (< pt (point)))
            (when (re-search-forward "<%doc>" (point-at-eol) t 1)
              (unless (re-search-forward "</%doc>" nil t 1)
                (throw 'break nil)
                )
              )
            (when (re-search-forward "<script" (point-at-eol) t 1)
              (unless (re-search-forward "</script>" nil t 1)
                (throw 'break nil)
                )
              )
            (when (re-search-forward "<style +type=\"text/css\" *>" (point-at-eol) t 1)
              (unless (re-search-forward "</style>" nil t 1)
                (throw 'break nil)
                )
              )
            (beginning-of-line)
            (while (re-search-forward "<%!?[ \t]" (point-at-eol) t 1)
              (unless (re-search-forward "%>" nil t 1)
                (throw 'break nil)
                )
              )
            (beginning-of-line)
            (while (re-search-forward "<%!?$" (point-at-eol) t 1)
              (unless (re-search-forward "%>" nil t 1)
                (throw 'break nil)
                )
              )
            (beginning-of-line)
            (funcall indent-line-function)
            (setq pt (point))
            )))
      (when column
        (indent-rigidly (point-min) (point-max) column)
        )
      ))
  )

(defun mmm-individual-indent-line () (interactive)
  (unless (and mmm-current-overlay
               (or (overlay-get mmm-current-overlay 'display-name)
                   (get mmm-current-submode 'mmm-mode-name)))
    (sgml-indent-line)
    ))

(defun mmm-individual-indent-for-tab-command (&optional arg)
  (interactive "P")
  (cond ((use-region-p)
         (mmm-individual-indent-region (region-beginning) (region-end)))
        (t
         (funcall indent-line-function)
         )))

(defun mmm-update-class-definition (definition)
  (let1 k (car definition)
    (setcdr (assoc k mmm-classes-alist) definition)))

;; (mmm-update-class-definition
;;    '(mako-doc
;;     :submode individual-comment-mode
;;     :face mmm-comment-submode-face
;;     :front "<%doc>"
;;     :back "</%doc>"
;;     :insert ((?o mako-<%doc> nil @ "<%doc>" @ "\n" _ "\n" @ "</%doc>"
;;                  @)))
;;    )

;; (mmm-update-class-definition
;;  '(mako-one-line-comment
;;     :submode individual-comment-mode
;;     :face mmm-comment-submode-face
;;     :front "^[ \t]*##"
;;     :back "\n"
;;     :front-delim 0
;;     :insert ((?# mako-comment nil @ "##" @ " " _ @
;;                  '(mmm-mako-end-line) "\n" @)))
;;  )


(defun execute-parent-makefile/python () (interactive)
  "for reveal.js slide generating" 
  (and-let* ((dir (target-in-path "Makefile"))
             (py-bin (target-in-path "bin/python")))
    (shell-command (format "source %s/bin/activate && make -C %s" py-bin dir))))

(defun setup-html-mode ()
  ;; (setq indent-line-function 'mmm-individual-indent-line)
  (define-insert-pair-binding html-mode-map my:html-key-pair)
  (define-many-keys html-mode-map
    `(("C-c C-@" . execute-parent-makefile/python)
       ("\t" . mmm-individual-indent-for-tab-command)))
  )

(setq my:html-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("<" . ">")
        ("'" . "'")
        ("{"  "}" "{")
        ("[" "]" "["))
      )

(add-hook 'html-mode-hook 'setup-html-mode)
(add-hook 'html-mode-hook ;; move-it
            (lambda ()
              (modify-syntax-entry ?% "w_")))
