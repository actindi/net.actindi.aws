(in-package :net.actindi.aws)

(defun sh (control-string &rest format-arguments)
  (let ((command (apply #'format nil control-string format-arguments)))
    (format *terminal-io* "~&~a~%" command)
   (multiple-value-bind (stdout stderr exit-code)
       (trivial-shell:shell-command command)
     (values (zerop exit-code)
             exit-code
             stdout
             stderr))))

(defvar *ssh-remote* "foo@example.com")
(defvar *ssh-option* "")

(defun ssh (control-string &rest format-arguments)
  (sh "ssh ~a ~a ~?" *ssh-option* *ssh-remote* control-string format-arguments))
#+nil
(let ((*ssh-remote* "rep"))
  (ssh "ls -l"))

(defun ensure-sh-arg (arg)
  (typecase arg
    (keyword (format nil "~:[--~;-~]~(~a~)"
                     (= 1 (length (symbol-name arg)))
                     arg))
    (symbol (string-downcase arg))
    (t arg)))

(defmacro def-command (name path)
  `(defun ,(intern (string-upcase name)) (&rest args)
     #+nil
     (sh-sync "~a~{ ~a~}" ,path
              (collect 'list (ensure-sh-arg (scan 'list args))))
     (sh "~a~{ ~a~}" ,path
         (collect 'list (ensure-sh-arg (scan 'list args))))))

(defmacro def-commands (bin-directory)
  `(progn
     ,@(collect
           (let ((path (scan-directory (merge-pathnames "*" bin-directory))))
             `(def-command ,(file-namestring path) ,path)))))

(def-commands "~/local/opt/ec2-api-tools/bin/")
(ec2ver)
;;=> T
;;   0
;;   "1.5.2.5 2012-03-01
;;   "
;;   ""
