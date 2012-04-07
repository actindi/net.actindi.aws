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
;;(ec2ver)
;;=> T
;;   0
;;   "1.5.2.5 2012-03-01
;;   "
;;   ""

(def-commands "~/local/opt/AutoScaling/bin/")
;;(as-cmd)

(defun make-ami-from-instance (instance-id)
  "指定したインスタンスの EBS のスナップショットをとって AMI を作る。"
  (let (kernel-id)
    (labels ((volume-id (instance-id)
               (multiple-value-bind (ok exit-code stdout)
                   (ec2-describe-instances :show-empty-fields instance-id)
                 (declare (ignore ok exit-code))
                 (ppcre:register-groups-bind ($kernel-id $volume-id)
                     ((ppcre:create-scanner
                       "INSTANCE\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+(\\S+).*BLOCKDEVICE\\s+\\S+\\s+(\\S+)"
                       :single-line-mode t)
                      stdout)
                   (setf kernel-id $kernel-id)
                   $volume-id)))
             (make-snapshot (volume-id)
               (multiple-value-bind (ok exit-code stdout)
                   (ec2-create-snapshot
                    :show-empty-fields
                    :d (format nil "\"~a(app) ~a\"" instance-id volume-id)
                    volume-id)
                 (declare (ignore ok exit-code))
                 (ppcre:register-groups-bind (snapshot-id)
                     ("SNAPSHOT\\s+(\\S+)" stdout)
                   snapshot-id)))
             (make-ami (snapshot-id)
               (ec2-register
                :n "\"app-`date +\\%Y-\\%m-\\%d-\\%H-\\%M-\\%S`\""
                :d "\"app image\""
                :root-device-name "/dev/sda1"
                :a "x86_64"
                :kernel kernel-id
                :b (format nil "/dev/sda=~a" snapshot-id))))
      (make-ami
       (make-snapshot
        (volume-id instance-id))))))
