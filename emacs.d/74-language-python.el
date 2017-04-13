(provide '74-language-python)
;;; 74-language-python.el ends here

;; * todo
;; ok ** quickrun 
;; ok ** flymake (pyflake)
;; ok *** flymake eldoc 
;; ok ** ffap module
;; *** anything
;; ok ** insert auto pair element
;; ok ** indentation setting
;; ok ** automode alist
;; ok ** quick syntax delete
;; ** toggle file
;; ** auto magic comment
;; ** yasnipet

(require-and-fetch-if-not 'pickup)

;; async-io-support
(defun format-message (fmt &rest args)
  (message (apply #'format fmt args)))
(add-to-list 'load-path (concat (current-directory) "python-async-support"))
(autoload 'python-mode "python" nil t)



;;; ffap module
(require-and-fetch-if-not 'ffap-python :url "https://gist.githubusercontent.com/podhmo/8133843/raw/54f0e1ad64a817b1b3b7315a44493e04cf311650/ffap-python.el")
(defvar ffap-python-disable-confirm-before-open t)
;;; flymake

(defun flymake-create-temp--tmpdirectory (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let ((prefix (or prefix "flymake")))
    (let* ((ext (file-name-extension file-name))
           (temp-name (file-truename (concat (file-name-sans-extension file-name)
                                             "_" prefix
                                             (and ext (concat "." ext)))))
           (temp-path (concat temporary-file-directory (file-name-nondirectory temp-name))))
      (flymake-log 3 "create-temp-tmpdirectory: file=%s temp=%s" file-name temp-path)
      temp-path)))

(setq flymake-python:program "pyflakes")
(setq flymake-python:program "flake8")
(defun flymake-python:find-program ()
  (or (ffap-python:find-program flymake-python:program)
      (error "%s is not found in %s" flymake-python:program default-directory)))

(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp--tmpdirectory))
         (check-program (flymake-python:find-program)))
    (cond ((string-match-p "flake8" check-program)
           (list check-program (list "--ignore" "E501" temp-file)))
          (t
           (list check-program (list temp-file))))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.py$" flymake-python-init))

;;; quick-run
(progn
  (setq quickrun/language-alist 
        (remove* "python" quickrun/language-alist :key 'car :test 'equal))
  (add-to-list 'quickrun/language-alist
               '("python" . ((:command . ffap-python:find-python)
                             (:compile-only . flymake-python:find-program)
                             (:description . "Run Python script")))
               )
  )
(defun quickrun-python:compile-only () (interactive)
  (shell-command (format "%s %s" (flymake-python:find-program) buffer-file-name)))

(defun my:python-insert-comma () (interactive)
  (insert ",")
  (unless (looking-at-p "$")
    (insert " ")))


;; yapf
(defun my:py-yapf-buffer ()
  (interactive)
  (lexical-let ((yapf (or (pickup-file "bin/yayapf") "yapf")))
    (flet ((executable-find (cmd) t))
      (py-yapf-bf--apply-executable-to-buffer
       "yapf"
       (lambda (errbuf file)
         (apply 'call-process yapf nil errbuf nil
                       (append py-yapf-options `("--in-place", file))))
       nil "py" t))))

;;; auto-pair
(require 'insert-pair-element nil t)
(setq my:python-key-map
      `(("`" . ,(ilambda (insert "_")))
        ("_" . my:python-insert-comma )
        ("\\" . insert-pair-escaped-after)
        ("," . my:python-insert-comma)
        ("C-x C-s" . my:py-yapf-buffer)
        ("C-c @" . quickrun-python:compile-only)
        ("C-c C-f" . ffap-python:import-ffap)
        ("C-c C-c" . toggle-file)))

(setq my:python-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("'" . "'")
        ("{"  "}" "{")
        ("[" "]" "["))
      )

(defun flymake-python-load () (interactive)
  (defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (flymake-mode t)
)



(defun my:python-setup ()
  ;; indentation
  (setq indent-tabs-mode nil
        python-indent-offset 4
        tab-width 4)
  (flymake-python-load)
  (define-insert-pair-binding python-mode-map my:python-key-pair)
  (define-many-keys python-mode-map my:python-key-map)
  (require 'py-yapf)

  ;;; hmm
  (save-excursion 
    (goto-char (point-min))
    (forward-line 30)
    (unless (search-backward "-*- coding:" nil t 1)
      (flet ((y-or-n-p (x) t))
        (auto-insert))))
  )

(add-hook 'python-mode-hook 'my:python-setup)

(unless (boundp 'python-initialize-settings-once)
  (require 'autoinsert)
  (setq python-initialize-settings-once t)

  (add-to-list 'auto-insert-alist
               '(("\\.py\\'" . "python skeleton")
                 (concat )
                 "# -*- coding:utf-8 -*-\nimport logging\nlogger = logging.getLogger(__name__)\n"))
  ;; (pop auto-insert-alist)
  )
