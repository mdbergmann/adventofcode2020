(ql:quickload "trivia")

(in-package :cl-user)

(defun eval-expr (expr env)
  (trivia:match expr
    ;;(`(* ,e1 ,e2) (* (eval-expr e1 env)
    ;;                 (eval-expr e2 env)))
    ((trivia:guard x (symbolp x)) (funcall env x))
    (`(lambda (,x) ,body)
      (progn
        (format t "x: ~a, body: ~a~%" x body)
        (lambda (arg) (eval-expr body
                            (lambda (y)
                              (if (eq x y)
                                  arg
                                  (funcall env y)))))))
    (`(,rator ,rand)
      (progn
        (format t "rator: ~a, rand: ~a~%" rator rand)
        (let ((result (apply (eval-expr rator env) 
                             (list rand))))
          (format t "result: ~a~%" result)
          result)))))

;; (eval-expr '(((lambda (!)
;;                 (lambda (n)
;;                   ((! !) n)))
;;               (lambda (!)
;;                 (lambda (n)
;;                   (if (zerop n)
;;                       1
;;                       (* n ((! !) (1- n))))))) 5)
;;            (lambda (y) (error "lookup unbound")))
