(provide '74-language-python)
;;; 74-language-python.el ends here

;; * todo
;; ok ** quickrun
;; ** flycheck (flake8)
;; ok ** ffap module
;; *** anything
;; ok ** insert auto pair element
;; ok ** indentation setting
;; ok ** automode alist
;; ok ** quick syntax delete
;; ** toggle file

(require-and-fetch-if-not 'pickup)

;;; ffap module
(use-package ffap-python  ;; mine
  :commands (ffap-python ffap-python:find-program ffap-python:find-python)
  :init
  (with-eval-after-load 'quickrun
    (setq my:check-python-program "pyflakes")
    (setq my:check-python-program "flake8")
    (defun my:check-python-find-program ()
      (or (ffap-python:find-program my:check-python-program)
          (error "%s is not found in %s" my:check-python-program default-directory)))

    (defun quickrun-python:compile-only () (interactive)
           (async-shell-command (format "%s %s" (my:check-python-find-program) buffer-file-name)))

    (unless (boundp 'quickrun--language-alist)
      (error "quickrun ins not activated yet"))

    (setq quickrun--language-alist
          (remove* "python" quickrun--language-alist :key 'car :test 'equal))
    (add-to-list 'quickrun--language-alist
                 '("python" . ((:command . ffap-python:find-python)
                               (:compile-only . my:check-python-find-program)
                               (:description . "Run Python script")))
                 ))
  :config
  (setq ffap-python-disable-confirm-before-open t)
  )

;; flycheck
(with-eval-after-load 'python
  (require 'flycheck)

  (defun my:python-flycheck-setup ()
    (make-local-variable 'my:flake8-path)
    (make-local-variable 'my:mypy-path)

    ;; this is buffer local
    (setq flycheck-disabled-checkers '(python-pycompile python-mypy python-pylint))
    (flycheck-mode 1)
    )

  ;; virtualenvのflake8を使う
  (my:flycheck-executable-find-function-register
   "flake8"
   (lambda ()
     (cond ((boundp 'my:flake8-path) my:flake8-path)
           (t (setq my:flake8-path (or (pickup:pickup-file "bin/flake8") "flake8"))
              my:flake8-path))))

  ;; TODO: use dmypy or mypyc
  (my:flycheck-executable-find-function-register
   "mypy"
   (lambda ()
     (cond ((boundp 'my:mypy-path) my:mypy-path)
           (t (setq my:mypy-path (or (pickup:pickup-file "bin/mypy") "mypy"))
              my:mypy-path))))

  ;; max-length
  (setq flycheck-flake8-maximum-line-length 100)

  ;; (flycheck-checker-get 'python-flake8 'next-checkers)
  (add-hook 'python-mode-hook 'my:python-flycheck-setup)
  )

(defun my:python-insert-comma () (interactive)
       (insert ",")
       (unless (looking-at-p "$")
         (insert " ")))


;; formatter
(defun my:py-yapf-buffer (beg end)
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min))
    (setq end (point-max))
    )
  (let ((yapf (or (pickup-file "bin/yayapf") (pickup-file "bin/yapf") "yapf")))
    (my:execute-formatter-command yapf yapf  beg end)))
(defvar my:py-black-command nil)
(defun my:py-black-buffer (beg end)
  (interactive "r")
  (unless (or (region-active-p) (= 0 (abs (- end beg))))
    (setq beg (point-min))
    (setq end (point-max))
    )
  (let* ((cmd-name (or my:py-black-command (pickup-file "bin/black") "black"))
         (cmd (format "%s %s" cmd-name "-")))
    (my:execute-formatter-command cmd-name cmd  beg end)))
(defalias 'my:py-formatter-buffer (symbol-function 'my:py-black-buffer))

;; jedi
(with-eval-after-load 'python
  (require 'pickup) ; individual package

  (defun my:python-jedi-setup ()
    (let ((alternative-python (pickup:pickup-file "bin/python")) ; pickup:pickup-file is magical function (custom defined)
          (cmds jedi:server-command)
          (args nil))

      (when alternative-python
        ;; venv/foo/bin/python -> venv/foo
        (let ((venv-path (file-name-directory (substring-no-properties (file-name-directory alternative-python) 0 -1))))
          (setq args (append args `("--virtual-env" ,venv-path))))
        (setq-local jedi:server-command cmds)
        (setq-local jedi:server-args args)
        )
      )
    (jedi-mode 1)

    (let ((map python-mode-map))
      (define-key map (kbd "M-.") 'jedi:goto-definition)
      (define-key map (kbd "M-,") 'jedi:goto-definition-pop-marker)
      (define-key map (kbd "C-c C-d") 'jedi:show-doc)
      )

    (require 'company-jedi)
    (add-to-list 'company-backends 'company-jedi)
    (company-mode-on)
    )

  ;; python-envorinment
  (require 'python-environment)
  (custom-set-variables
   '(python-environment-virtualenv (list "python" "-m" "venv" "--system-site-packages")))

  (require 'jedi-core)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)

  ;; this is work-around
  ;; (defun my:safe-python-jedi-setup ()
  ;;   (let ((p (start-process "find jedi" nil (pickup:pickup-file "bin/python")  "-c" "import jedi; import epc")))
  ;;     (set-process-sentinel
  ;;      p
  ;;      (lambda (p status)
  ;;        (cond ((= 0 (process-exit-status p)) (my:python-jedi-setup))
  ;;              (t (message "jedi is not found. please install `pip install jedi epc`"))))))
  ;;   )
  (add-hook 'python-mode-hook 'my:python-jedi-setup))

;;; auto-pair
(require 'insert-pair-element nil t)
(setq my:python-key-map
      `(("`" . ,(ilambda (insert "_")))
        ("_" . my:python-insert-comma )
        ("\\" . insert-pair-escaped-after)
        ("," . my:python-insert-comma)
        ("C-x C-s" . my:py-formatter-buffer)
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

(defun my:python-setup ()
  ;; indentation
  (setq indent-tabs-mode nil
        python-indent-offset 4
        tab-width 4)

  (define-insert-pair-binding python-mode-map my:python-key-pair)
  (define-many-keys python-mode-map my:python-key-map)

  ;;; hmm
  (save-excursion
    (goto-char (point-min))
    (forward-line 30)
    (unless (search-backward "-*- coding:" nil t 1)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (x) t)))
        (auto-insert))))
  )

(add-hook 'python-mode-hook 'my:python-setup)
(add-to-list 'auto-mode-alist '("\\.py[i]?$" . python-mode))
(unless (boundp 'python-initialize-settings-once)
  (require 'autoinsert)
  (setq python-initialize-settings-once t)

  (add-to-list 'auto-insert-alist
               '(("\\.py\\'" . "python skeleton")
                 (concat )
                 "import logging\nlogger = logging.getLogger(__name__)\n"))
  ;; (pop auto-insert-alist)
  )
