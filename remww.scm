(define strange_anaylsis
   (lambda (insts)
       (letrec ((analysis (list (get-all-vars insts)))
                (last-anal (get-all-vars insts))
                (loop (lambda (lst)
                          (if (null? lst)
                              (cdr analysis) ;we dont really care about the first one
                              (begin
                                   (if (inst? (car lst))
                                       (begin (set! last-anal (add-to-anal (inst->read (car lst)) (remove-from-anal (inst->write (car lst)) last-anal)))
                                              (set! analysis (append (list last-anal) analysis)))
                                       (void))
                                   (loop (cdr lst)))))))
           (loop (reverse insts)))))
           
(define inst->read
   (lambda (inst)
      (cadr inst)))
      
(define inst->write
   (lambda (inst)
      (caddr inst)))
      
(define add-to-anal
   (lambda (vars anal)
      (letrec ((new_anal anal)
               (loop (lambda (cur_vars)
                   (if (null? cur_vars)
                        new_anal
                        (begin
                               (if (not (member (car cur_vars) new_anal))
                                   (set! new_anal (append new_anal (list (car cur_vars)))))
                               (loop (cdr cur_vars)))))))
          (loop vars))))
          
(define get-all-vars
   (lambda (insts)
      (letrec ((vars (list))
               (loop (lambda (lst)
                        (if (null? lst)
                             vars
                            (begin
                                   (set! vars (add-to-anal (inst->read (car lst)) (add-to-anal (inst->write (car lst)) vars)))
                                   (loop (cdr lst)))))))
         (loop insts))))
          
(define remove-from-anal
   (lambda (vars anal)
      (letrec ((new_anal anal)
               (loop (lambda  (cur_vars)
                   (if (null? cur_vars)
                        new_anal
                        (begin
                             (set! new_anal (remove (car cur_vars) new_anal))
                             (loop (cdr cur_vars)))))))
          (loop vars))))
     
(define inst?
  (lambda (test)
     (and (list? test) (not (null? test)) (= (length test) 3) (list? (cadr test)) (list? (caddr test)))))
     
(define dce
  (lambda (insts analysis)
    (letrec ((after_dce insts)
             (loop (lambda (cur_insts cur_analysis)
                        (if (null? cur_insts)
                            after_dce
                            (begin
                                   (if (null? (filter (lambda (e) (member e (car cur_analysis))) (inst->write (car cur_insts))))
                                          (set! after_dce (remove (car cur_insts) after_dce)))
                                   (loop (cdr cur_insts) (cdr cur_analysis)))))))
        (loop insts analysis))))
     
(define remww
   (lambda (insts)
      (let* ((analysis (strange_anaylsis insts))
            (insts_after_dce (dce insts analysis)))
        (if (equal? insts insts_after_dce)
            insts_after_dce
            (remww insts_after_dce)))))

