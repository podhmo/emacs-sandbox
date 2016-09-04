(require-and-fetch-if-not 'initchart :url "https://raw.githubusercontent.com/yuttie/initchart/master/initchart.el")

;; (defmacro my:initchart-record-execution-all (excludes)
;;   (let ((targets (list)))
;;     (mapatoms (lambda (sym)
;;                 (when (and (functionp sym) (not (subrp (symbol-function sym))))
;;                   (if (not (memq sym excludes))
;;                     (push sym targets) (message "*skip: %s" sym))
;;                   )
;;                 ))
;;     (let ((body (mapcar (lambda (sym) `(initchart-record-execution-time-of ,sym)) targets)))
;;       `(progn
;;          ,@body
;;          )))
;;   )
;; (defmacro initchart-record-execution-time-of (fn &rest args)
;;   `(defadvice ,fn (around ,(intern (concat "initchart-record-execution-time-of-" (symbol-name fn))) compile activate)
;;      (let ((start-time (current-time)))
;;        ad-do-it
;;        (let ((end-time (current-time)))
;;          (initchart-log (symbol-name ',fn)
;;                         start-time
;;                         end-time
;;                         ,(or (car args) "-")))
;;        )))

;; (defun my:first-atom (xs)
;;   (and xs (if (listp xs) (my:first-atom (car xs)) xs)))

;; (defun initchart-log (name start-time end-time &rest args)
;;   (let ((sub-name (my:first-atom args)))
;;     (with-current-buffer "*initchart*"
;;       (insert (format "exec-time: %s %f %f\n"
;;                       (if sub-name (format "%s(%s)" name sub-name) name)
;;                       (float-time start-time)
;;                       (float-time end-time))))))

(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)
(initchart-record-execution-time-of init-loader-load)
;; (my:initchart-record-execution-all '(blink-cursor-timer-function))
;;; (ad-disable-regexp "^initchart-record-execution")

