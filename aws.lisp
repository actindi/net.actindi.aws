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
  `(progn
     (defun ,(intern (format nil "%~:@(~a~)" name)) (&rest args)
       #+nil
       (sh-sync "~a~{ ~a~}" ,path
                (collect 'list (ensure-sh-arg (scan 'list args))))
       (sh "~a~{ ~a~}" ,path
           (collect 'list (ensure-sh-arg (scan 'list args)))))
     (defun ,(intern (format nil "~:@(~a~)" name)) (&rest args)
       (multiple-value-bind (ok exit-code stdout stderr)
           (sh "~a~{ ~a~}" ,path
               (collect 'list (ensure-sh-arg (scan 'list args))))
         (unless ok
           (error "~a is failed!~%exit: ~a~%~a~%~a"
                  (format nil "~a~{ ~a~}"
                          ,path
                          (collect 'list (ensure-sh-arg (scan 'list args))))
                  exit-code stdout stderr))
         stdout))))

(defmacro def-commands (bin-directory)
  `(progn
     ,@(collect
           (let ((path (scan-directory (merge-pathnames "*" bin-directory))))
             `(def-command ,(file-namestring path) ,path)))))

(def-commands "~/local/opt/ec2-api-tools/bin/")
;;(%ec2ver)
;;=> T
;;   0
;;   "1.5.2.5 2012-03-01
;;   "
;;   ""
;;(ec2ver)
;;=> "1.5.2.5 2012-03-01
;;   "

(def-commands "~/local/opt/AutoScaling/bin/")
;;(%as-cmd)
;;(as-cmd)


(defun make-ami-from-instance (instance-id)
  "指定したインスタンスの EBS のスナップショットをとって AMI を作る。"
  (let (kernel-id)
    (labels ((volume-id (instance-id)
               (ppcre:register-groups-bind ($kernel-id $volume-id)
                   ((ppcre:create-scanner
                     "INSTANCE\\t(?:[^\\t]+\\t){11}([^\\t]+).*BLOCKDEVICE\\s+[^\\t]+\\s+([^\\t]+)"
                     :single-line-mode t)
                    (ec2-describe-instances :show-empty-fields instance-id))
                 (setf kernel-id $kernel-id)
                 $volume-id))
             (make-snapshot (volume-id)
               (ppcre:register-groups-bind (snapshot-id)
                   ("SNAPSHOT\\s+(\\S+)"
                    (ec2-create-snapshot
                     :show-empty-fields
                     :d (format nil "\"~a(app) ~a\"" instance-id volume-id)
                     volume-id))
                 (loop for out = (ec2-describe-snapshots snapshot-id)
                       until (search "completed" out)
                       do (sleep 10))
                 snapshot-id))
             (make-ami (snapshot-id)
               (ec2-register
                :n "\"app-`date +\\%Y-\\%m-\\%d-\\%H-\\%M-\\%S`\""
                :d "\"app image\""
                :root-device-name "/dev/sda1"
                :a "x86_64"
                :kernel kernel-id
                :b (format nil "/dev/sda=~a" snapshot-id)
                :b "/dev/sdc=ephemeral0")))
      (make-ami
       (make-snapshot
        (volume-id instance-id))))))
;; wheezy
;;(make-ami-from-instance "i-45b29643")
;;⇒ "IMAGE	ami-3f9cd63e
;;   "


(defun update-launch-confgi (auto-scaling-group-name ami-id
                             &key (instance-type "c3.large")
                               (group "web-server")
                               (region "ap-northeast-1")
                               (key "actindi")
                               (launch-config-name "outing-lc"))
  "オートスケールの AMI を変更する。"
  (let ((new-launch-config (format nil "lc-~(~a~)" (uuid:make-v1-uuid))))
    (as-create-launch-config new-launch-config
                             :image-id ami-id
                             :instance-type instance-type
                             :group group
                             :key key
                             :region region)
    (ppcre:register-groups-bind (old-launch-config)
        ("AUTO-SCALING-GROUP\\s+\\S+\\s+(\\S+)"
         (as-describe-auto-scaling-groups :show-empty-fields auto-scaling-group-name
                                          :region region))
      (as-update-auto-scaling-group auto-scaling-group-name
                                    :launch-configuration new-launch-config
                                    :region region)
      (as-delete-launch-config old-launch-config :force :region region))
    (as-create-launch-config launch-config-name
                             :image-id ami-id
                             :instance-type instance-type
                             :group group
                             :key key
                             :region region)
    (as-update-auto-scaling-group auto-scaling-group-name
                                  :launch-configuration launch-config-name
                                  :region region)
    (as-delete-launch-config new-launch-config :force :region region)
    (values
     (multiple-value-list (as-describe-launch-configs :region region))
     (multiple-value-list (as-describe-auto-scaling-groups auto-scaling-group-name :region region)))))
;; wheezy
;; (update-launch-confgi "outing-grp" "ami-3f9cd63e" :instance-type "c3.xlarge")
;;⇒ ("LAUNCH-CONFIG  outing-lc  ami-3f9cd63e  c3.xlarge
;;   ")
;;   ("AUTO-SCALING-GROUP  outing-grp  outing-lc  ap-northeast-1a  outing  1  20  1
;;   INSTANCE  i-93bc9895  ap-northeast-1a  InService  Healthy
;;   TAG  outing-grp  auto-scaling-group  name  app  true
;;   ")
