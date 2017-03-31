(load "./pattern-matcher.scm")
(load "./pc.scm")

;PROJECT

(define prologue
   (string-append "#include <stdio.h>\n"
                  "#include <stdlib.h>\n"
                  "#include <string.h>\n"
                  "#define DO_SHOW 1\n"
                  "#include \"arch/cisc.h\"\n"
                  "int main()\n"
                  "{\n"))

(define middlouge
   (string-append "#define SOB_VOID 1\n"
                  "#define SOB_NIL 2\n"
                  "#define SOB_FALSE 3\n"
                  "#define SOB_TRUE 5\n"
                  " JUMP(CONTINUE);\n"
                  "#include \"arch/char.lib\"\n"
                  "#include \"arch/io.lib\"\n"
                  "#include \"arch/math.lib\"\n"
                  "#include \"arch/string.lib\"\n"
                  "#include \"arch/system.lib\"\n"
                  "#include \"arch/scheme.lib\"\n"
                  " CONTINUE: \n"))
(define epilogue
  (string-append 
                ;"    END_OF_COMPILER:\n"
                 " STOP_MACHINE;\n"
                 "return 0;\n"
                 "}"))

(define file-string->sexprs
  (lambda (string)
    (<sexpr> (string->list string)
	    (lambda (e s)
	      e)
	    (lambda (w) `(failed with report: ,@w)))))
	    
(define parse_and_optimize
   (lambda (sexpr)
      (annotate-tc
          (pe->lex-pe
             (box-set
                (remove-applic-lambda-nil
                  (eliminate-nested-defines
                     (parse sexpr))))))))
(define compile-scheme-file
   (lambda (file_name output-file_name)
      (let* ((file_string (file->string file_name))
             (b_p_sexprs (append scheme-primitive-fvars (file-string->sexprs (string-append "(" file_string ")"))))
             (sexprs (map parse_and_optimize b_p_sexprs))
             (const-table (build-const-table sexprs))
             (c-table-size (const-table->size const-table))
             (fvar-table (build-fvar-table sexprs (+ 1 c-table-size)))
             (f-table-size (fvar-table->size fvar-table))
             (symbol-list-arr (const-table->symbol-list const-table (+ 1 c-table-size f-table-size)))
             (symbol-list-arr-size (const-table->symbol-list-size const-table))
             (output-port (open-output-file output-file_name 'replace)))
         (display (string-append
                        prologue
                        (const-table->array const-table) "\n"
                        (if (zero? f-table-size) "" (string-append (fvar-table->array fvar-table) "\n"))
                        symbol-list-arr "\n"
                        "START_MACHINE;\n"
                        "ADD(IND(0), IMM(" (number->string (+ c-table-size f-table-size symbol-list-arr-size)) "));\n"
                        "memcpy(&IND(1), CONSTANTS, " (number->string c-table-size) " * WORD_SIZE);\n"
                        (if (zero? f-table-size)
                            ""
                            (string-append "memcpy(&IND(" (number->string (+ 1 c-table-size)) "), FVARS, " 
                                             (number->string f-table-size) " * WORD_SIZE);\n"))
                        "memcpy(&IND(" (number->string (+ 1 c-table-size f-table-size)) "), SYMBOL_LIST, " 
                        (number->string symbol-list-arr-size) " * WORD_SIZE);\n"
                        "#define SYMBOL_LIST_START " (number->string (+ 1 c-table-size f-table-size)) "\n"
                        middlouge
                        (build-primitive-fvars fvar-table)
                        (fold-left string-append "" (map (lambda(expr) (string-append (codegen expr 0 0 const-table fvar-table 1) (print-sob 1))) sexprs))
                        epilogue) output-port)
          (close-output-port output-port))))

(define make-spaces
  (lambda (level)
     (make-string (* 4 level) #\space)))
          
(define make-make-label
  (lambda (prefix)
    (lambda ()
      (let ((n 0))
        (lambda ()
          (set! n (+ n 1))
            (string-append prefix (number->string n)))))))
            
(define make-label-if3-else
  (make-make-label "L_if3_else_"))
(define L_if3_else (make-label-if3-else))

(define make-label-if3-end
  (make-make-label "L_if3_end_"))
(define L_if3_end (make-label-if3-end))

(define make-label-or-exit
  (make-make-label "L_or_exit_"))
(define L_or_exit (make-label-or-exit))

(define make-label-applic-no-error
  (make-make-label "L_applic_no_error_"))
(define L_applic_no_error (make-label-applic-no-error))

(define make-label-closure-code
  (make-make-label "L_closure_code_"))
(define L_closure_code (make-label-closure-code))

(define make-label-closure-exit
  (make-make-label "L_closure_exit_"))
(define L_closure_exit (make-label-closure-exit))

(define make-label-closure-loop-one
  (make-make-label "L_closure_loop_one_"))
(define L_closure_loop_one (make-label-closure-loop-one))

(define make-label-closure-loop-one-end
  (make-make-label "L_closure_loop_one_end_"))
(define L_closure_loop_one_end (make-label-closure-loop-one-end))


(define make-label-closure-loop-two
  (make-make-label "L_closure_loop_two_"))
(define L_closure_loop_two (make-label-closure-loop-two))

(define make-label-closure-loop-two-end
  (make-make-label "L_closure_loop_two_end_"))
(define L_closure_loop_two_end (make-label-closure-loop-two-end))

(define make-label-closure-loop-three
  (make-make-label "L_closure_loop_three_"))
(define L_closure_loop_three (make-label-closure-loop-three))

(define make-label-closure-loop-three-end
  (make-make-label "L_closure_loop_three_end_"))
(define L_closure_loop_three_end (make-label-closure-loop-three-end))


(define make-label-dont-print-sob
  (make-make-label "L_dont_print_sob_"))
(define L_dont_print_sob (make-label-dont-print-sob))

(define make-label-tc-applic-loop
  (make-make-label "L_tc_applic_loop_"))
(define L_tc_applic_loop (make-label-tc-applic-loop))

(define make-label-tc-applic-loop-end
  (make-make-label "L_tc_applic_loop_end_"))
(define L_tc_applic_loop_end (make-label-tc-applic-loop-end))


(define build-cons
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF CONS BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_cons);\n"
                "    L_prim_cons:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        PUSH(FPARG(3));\n" ;pushing cdr
                "        PUSH(FPARG(2));\n" ;pushing car
                "        CALL(MAKE_SOB_PAIR);\n"
                "        DROP(2);\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_cons:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_cons));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'cons fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF CONS BUILDING --------------- */\n")))
                
(define build-car
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF CAR BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_car);\n"
                "    L_prim_car:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(2));\n" ;now R1 is the pair we want to do car on
                "        MOV(R0, INDD(R1, 1));\n" ;now R1 is the car of the pair
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_car:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_car));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'car fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF CAR BUILDING --------------- */\n")))

(define build-cdr
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF CDR BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_cdr);\n"
                "    L_prim_cdr:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(2));\n" ;now R1 is the pair we want to do cdr on
                "        MOV(R0, INDD(R1, 2));\n" ;now R1 is the cdr of the pair
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_cdr:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_cdr));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'cdr fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF CDR BUILDING --------------- */\n")))
                

(define build-predict
 (lambda (fvar-table predict-pair)
  (let* ((predict-str-before (symbol->string (car predict-pair)))
         (predict (string-append (substring predict-str-before 0 (- (string-length predict-str-before) 1)) "_pred"))
        (type (cdr predict-pair)))
   (string-append 
                "    /*--------------- START OF " predict " BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_" predict ");\n"
                "    L_prim_" predict ":\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R0, FPARG(2));\n" ;now R0 is the expr we want to check the predict on
                "        CMP(IND(R0), IMM(" (number->string type) "));\n" ;checking the is the current type
                "        JUMP_EQ(L_prim_" predict "_is_true);\n"
                "        MOV(R0, IMM(SOB_FALSE));\n"
                "        JUMP(L_prim_" predict "_return);\n"
                "        L_prim_" predict "_is_true:\n"
                "        MOV(R0, IMM(SOB_TRUE));\n"
                "        L_prim_" predict "_return:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_" predict ":\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_" predict "));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr (car predict-pair) fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF predict BUILDING --------------- */\n"))))
                
(define build-char_to_integer
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF char_to_integer BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_char_to_integer);\n"
                "    L_prim_char_to_integer:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R0, FPARG(2));\n" ;now R0 is a pointer to the T_CHAR
                "        MOV(R0, INDD(R0, 1));\n" ;now R0 is the number of the char
                "        PUSH(R0);\n"
                "        CALL(MAKE_SOB_INTEGER);\n" ;now R0 is the integer repersnation of the char
                "        DROP(1);\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_char_to_integer:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_char_to_integer));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'char->integer fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF char_to_integer BUILDING --------------- */\n")))
                
(define build-integer_to_char
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF integer_to_char BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_integer_to_char);\n"
                "    L_prim_integer_to_char:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R0, FPARG(2));\n" ;now R0 is a pointer to the T_INTEGER
                "        MOV(R0, INDD(R0, 1));\n" ;now R0 is the number of the integer
                "        PUSH(R0);\n"
                "        CALL(MAKE_SOB_CHAR);\n" ;now R0 is the char repersnation of the integer
                "        DROP(1);\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_integer_to_char:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_integer_to_char));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'integer->char fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF integer_to_char BUILDING --------------- */\n")))
                
                
(define build-number_and_rational-pred
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF number_pred BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_number_pred);\n"
                "    L_prim_number_pred:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R0, FPARG(2));\n" ;now R0 is the expr we want to check the predict on
                "        CMP(IND(R0), IMM(" (number->string T_INTEGER) "));\n"
                "        JUMP_EQ(L_prim_number_pred_is_true);\n"
                "        CMP(IND(R0), IMM(" (number->string T_FRACTION) "));\n"
                "        JUMP_EQ(L_prim_number_pred_is_true);\n"
                "        MOV(R0, IMM(SOB_FALSE));\n"
                "        JUMP(L_prim_number_pred_is_return);\n"
                "        L_prim_number_pred_is_true:\n"
                "        MOV(R0, IMM(SOB_TRUE));\n"
                "        L_prim_number_pred_is_return:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_number_pred:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_number_pred));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'number? fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    MOV(R1, IMM(" (number->string (fvar->addr 'rational? fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF number_pred BUILDING --------------- */\n")))
                
                
(define build-denominator
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF denominator BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_denominator);\n"
                "    L_prim_denominator:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R0, FPARG(2));\n" ;now R0 is the expr we want to check the predict on
                "        CMP(IND(R0), IMM(" (number->string T_FRACTION) "));\n"
                "        JUMP_EQ(L_prim_denominator_is_fraction);\n"
                "        PUSH(IMM(1));\n"
                "        CALL(MAKE_SOB_INTEGER);\n"
                "        DROP(1);\n"
                "        JUMP(L_prim_denominator_return);\n"
                "        L_prim_denominator_is_fraction:\n"
                "        MOV(R0, INDD(R0, 2));\n"
                "        PUSH(R0);\n"
                "        CALL(MAKE_SOB_INTEGER);\n"
                "        DROP(1);\n"
                "        L_prim_denominator_return:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_denominator:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_denominator));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'denominator fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF denominator BUILDING --------------- */\n")))

(define build-numerator
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF numerator BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_numerator);\n"
                "    L_prim_numerator:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R0, FPARG(2));\n" ;now R0 is the expr we want to check the predict on
                "        CMP(IND(R0), IMM(" (number->string T_FRACTION) "));\n"
                "        JUMP_EQ(L_prim_numerator_is_fraction);\n"
                "        JUMP(L_prim_numerator_return);\n" ;if num is integer the the numerator is the number
                "        L_prim_numerator_is_fraction:\n"
                "        MOV(R0, INDD(R0, 1));\n"
                "        PUSH(R0);\n"
                "        CALL(MAKE_SOB_INTEGER);\n"
                "        DROP(1);\n"
                "        L_prim_numerator_return:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_numerator:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_numerator));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'numerator fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF numerator BUILDING --------------- */\n")))
                
(define build-string_and_vector_length
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF string_and_vector_length BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_string_and_vector_length);\n"
                "    L_prim_string_and_vector_length:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R0, FPARG(2));\n" ;now R0 is the vector/string we want to find its length
                "        MOV(R0, INDD(R0, 1));\n" ;now R0 is the length
                "        PUSH(R0);\n"
                "        CALL(MAKE_SOB_INTEGER);\n"
                "        DROP(1);\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_string_and_vector_length:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_string_and_vector_length));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'string-length fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    MOV(R1, IMM(" (number->string (fvar->addr 'vector-length fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF string_and_vector_length BUILDING --------------- */\n")))
                
(define build-set_car
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF set_car BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_set_car);\n"
                "    L_prim_set_car:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R2, FPARG(3));\n" ;the new car
                "        MOV(R1, FPARG(2));\n" ;the pair
                "        MOV(INDD(R1,1), R2);\n" ;doing the set
                "        MOV(R0, IMM(SOB_VOID));\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_set_car:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_set_car));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'set-car! fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF set_car BUILDING --------------- */\n")))
                
(define build-set_cdr
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF set_cdr BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_set_cdr);\n"
                "    L_prim_set_cdr:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R2, FPARG(3));\n" ;the new cdr
                "        MOV(R1, FPARG(2));\n" ;the pair
                "        MOV(INDD(R1,2), R2);\n" ;doing the set
                "        MOV(R0, IMM(SOB_VOID));\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_set_cdr:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_set_cdr));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'set-cdr! fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF set_cdr BUILDING --------------- */\n")))
                
                
(define build-string_ref
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF string_ref BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_string_ref);\n"
                "    L_prim_string_ref:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R2, FPARG(3));\n" ;the integer represtion of the index we need to get from the string
                "        MOV(R2, INDD(R2, 1));\n" ;now R2 is the real index
                "        ADD(R2, IMM(2));\n" ;now R2 is the index in the memory
                "        MOV(R1, FPARG(2));\n" ;the string
                "        MOV(R0, INDD(R1, R2));\n" ;now R0 is the number represnting the char in the index
                ;creating the char:
                "        PUSH(R0);\n"
                "        CALL(MAKE_SOB_CHAR);\n"
                "        DROP(1);\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_string_ref:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_string_ref));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'string-ref fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF string_ref BUILDING --------------- */\n")))
                
(define build-string_set
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF string_set BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_string_set);\n"
                "    L_prim_string_set:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R3, FPARG(4));\n" ;the t_char that we want to set to
                "        MOV(R3, INDD(R3,1));\n" ;the integer that repsrestes the char
                "        MOV(R2, FPARG(3));\n" ;the integer represtion of the index we need to change in the string
                "        MOV(R2, INDD(R2, 1));\n" ;now R2 is the real index
                "        ADD(R2, IMM(2));\n" ;now R2 is the index in the memory
                "        MOV(R1, FPARG(2));\n" ;the string
                ;doing the set
                "        MOV(INDD(R1, R2), R3);\n" ;now R0 is the number represnting the char in the index
                "        MOV(R0, IMM(SOB_VOID));\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_string_set:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_string_set));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'string-set! fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF string_set BUILDING --------------- */\n")))
                
(define build-vector_ref
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF vector_ref BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_vector_ref);\n"
                "    L_prim_vector_ref:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R2, FPARG(3));\n" ;the integer represtion of the index we need to get from the vector
                "        MOV(R2, INDD(R2, 1));\n" ;now R2 is the real index
                "        ADD(R2, IMM(2));\n" ;now R2 is the index in the memory
                "        MOV(R1, FPARG(2));\n" ;the vector
                "        MOV(R0, INDD(R1, R2));\n" ;now R0 is the number represnting the element in the index
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_vector_ref:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_vector_ref));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'vector-ref fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF vector_ref BUILDING --------------- */\n")))
                
                
(define build-vector_set
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF vector_set BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_vector_set);\n"
                "    L_prim_vector_set:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R3, FPARG(4));\n" ;the scheme exper that we want to set to
                "        MOV(R2, FPARG(3));\n" ;the integer represtion of the index we need to change in the vector
                "        MOV(R2, INDD(R2, 1));\n" ;now R2 is the real index
                "        ADD(R2, IMM(2));\n" ;now R2 is the index in the memory
                "        MOV(R1, FPARG(2));\n" ;the vector
                ;doing the set
                "        MOV(INDD(R1, R2), R3);\n" ;now R0 is the number represnting the char in the index
                "        MOV(R0, IMM(SOB_VOID));\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_vector_set:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_vector_set));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'vector-set! fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF vector_set BUILDING --------------- */\n")))
                
(define build-remainder
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF remainder BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_remainder);\n"
                "    L_prim_remainder:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R2, FPARG(3));\n" ;second T_INTEGER
                "        MOV(R2, INDD(R2, 1));\n" ;second number
                "        MOV(R1, FPARG(2));\n" ;first T_INTEGER
                "        MOV(R1, INDD(R1, 1));\n" ;first number
                "        REM(R1, R2);\n" ;now R1 is number1%number2
                "        PUSH(R1);\n"
                "        CALL(MAKE_SOB_INTEGER);\n"
                "        DROP(1);\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_remainder:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_remainder));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'remainder fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF remainder BUILDING --------------- */\n")))
                
(define build-symbol_to_string
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF symbol_to_string BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_symbol_to_string);\n"
                "    L_prim_symbol_to_string:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(2));\n" ;now R1 is the T_SYMBOL
                "        MOV(R0, INDD(R1, 1));\n" ;now R1 is a pointer to the string
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_symbol_to_string:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_symbol_to_string));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'symbol->string fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF symbol_to_string BUILDING --------------- */\n")))
                
(define build-string_to_symbol
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF string_to_symbol BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_string_to_symbol);\n"
                "    L_prim_string_to_symbol:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(2));\n" ;our string
                "        MOV(R2, IMM(SYMBOL_LIST_START));\n"
                "    L_prim_string_to_symbol_loop:\n"
                "        PUSH(R1);\n"
                "        PUSH(INDD(R2, 0));\n"
                "        CALL(STRING_EQUAL);\n"
                "        DROP(2);\n"
                "        CMP(R0, IMM(SOB_TRUE));\n"
                "        JUMP_EQ(L_prim_string_to_symbol_exist);\n"
                "        CMP(INDD(R2, 1), IMM(0));\n" ;checking if we got to the end of the symbol list
                "        JUMP_EQ(L_prim_string_to_symbol_doesnt_exist);\n"
                "        MOV(R2, INDD(R2, 1));\n"
                "        JUMP(L_prim_string_to_symbol_loop);\n"
                "   L_prim_string_to_symbol_doesnt_exist:\n"
                        ;adding string to the symbol list
                "        PUSH(2);\n"
                "        CALL(MALLOC);\n"
                "        DROP(1);\n"
                "        MOV(INDD(R0, 0), R1);\n"
                "        MOV(INDD(R0, 1), IMM(0));\n"
                "        MOV(INDD(R2, 1), R0);\n"
                "        PUSH(2);\n"
                "        CALL(MALLOC);\n"
                "        DROP(1);\n"
                "        MOV(INDD(R0, 0), IMM(T_SYMBOL));\n"
                "        MOV(INDD(R0, 1), R1);\n"
                "        JUMP(L_prim_string_to_symbol_finished);\n"
                         ;creating symbol with R1 as repsrenstive string
                "   L_prim_string_to_symbol_exist:\n"
                         ;creating symbol with R2 as repsrenstive string
                "        PUSH(2);\n"
                "        CALL(MALLOC);\n"
                "        DROP(1);\n"
                "        MOV(INDD(R0, 0), IMM(T_SYMBOL));\n"
                "        MOV(INDD(R0, 1), INDD(R2, 0));\n"
                "  L_prim_string_to_symbol_finished:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_string_to_symbol:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_string_to_symbol));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'string->symbol fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF string_to_symbol BUILDING --------------- */\n")))
                
                
(define build-not
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF not BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_not);\n"
                "    L_prim_not:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(2));\n" ;now R1 is a pointer to the scheme object
                "        CMP(INDD(R1, 0), IMM(T_BOOL));\n"
                "        JUMP_NE(L_prim_not_val_is_true);\n"
                "        CMP(INDD(R1, 1), IMM(0));\n"
                "        JUMP_NE(L_prim_not_val_is_true);\n"
                "        MOV(R0, IMM(SOB_TRUE));\n" ;if we got here R1 is false
                "        JUMP(L_prim_not_end);\n"
                "    L_prim_not_val_is_true:\n"
                "        MOV(R0, IMM(SOB_FALSE));\n"
                "    L_prim_not_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_not:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_not));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'not fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF not BUILDING --------------- */\n")))
                
                
(define build-plus
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF plus BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_plus);\n"
                "    L_prim_plus:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;now R1 contains n - number of + args
                "        MOV(R2, IMM(0));\n" ;R2 - current argument
                "        MOV(R3, IMM(0));\n"
                "        MOV(R4, IMM(1));\n" ;R3/R4 will represnt the sum
                "        L_prim_plus_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_plus_end);\n"
                "            MOV(R5, FPARG(2 + R2));\n"
                "            CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "            JUMP_EQ(L_prim_plus_loop_integer);\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, INDD(R5, 2));\n"
                "            JUMP(L_prim_plus_loop_after_type_check);\n"
                "            L_prim_plus_loop_integer:\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, IMM(1));\n"
                "        L_prim_plus_loop_after_type_check:\n" ;now R6/R7 represnt the current argument to add
                "            CALL(ADD_REG_FRAC);\n" ;function the adss R6/R7 to R3/R4 and does reduction of the fraction using gcd
                "            ADD(R2, 1);\n"
                "            JUMP(L_prim_plus_loop);\n"
                "        L_prim_plus_end:\n"
                "            CMP(R4, IMM(1));\n"
                "            JUMP_EQ(L_prim_plus_end_int);\n"
                "            PUSH(3);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_FRACTION));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "            MOV(INDD(R0, 2), R4);\n"
                "            JUMP(L_prim_plus_real_end);\n"
                "            L_prim_plus_end_int:\n"
                "            PUSH(2);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_INTEGER));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "        L_prim_plus_real_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_plus:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_plus));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr '+ fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF plus BUILDING --------------- */\n")))
                
                
(define build-minus
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF minus BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_minus);\n"
                "    L_prim_minus:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;now R1 contains n - number of + args
                "        MOV(R2, IMM(1));\n" ;R2 - current argument
                "        MOV(R5, FPARG(2));\n" ;In minus we need at least one argument to start the sum from
                "        CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "        JUMP_EQ(L_prim_minus_integer);\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, INDD(R5, 2));\n"
                "        JUMP(L_prim_minus_after_type_check);\n"
                "            L_prim_minus_integer:\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, IMM(1));\n"
                "        L_prim_minus_after_type_check:\n" ;R3/R4 will represnt the sum
                "        CMP(R1, IMM(1));\n"
                "            JUMP_NE(L_prim_minus_loop);\n"
                "            MUL(R3, IMM(-1));\n" ;if we got only one argument then minus acts diffrently, it multiplies the number by -1
                "            JUMP(L_prim_minus_end);\n"
                "        L_prim_minus_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_minus_end);\n"
                "            MOV(R5, FPARG(2 + R2));\n"
                "            CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "            JUMP_EQ(L_prim_minus_loop_integer);\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, INDD(R5, 2));\n"
                "            JUMP(L_prim_minus_loop_after_type_check);\n"
                "            L_prim_minus_loop_integer:\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, IMM(1));\n"
                "        L_prim_minus_loop_after_type_check:\n" ;now R6/R7 represnt the current argument to subtract
                "            CALL(SUB_REG_FRAC);\n" ;function thhat subtracts R6/R7 from R3/R4 and does reduction of the fraction using gcd
                "            ADD(R2, 1);\n"
                "            JUMP(L_prim_minus_loop);\n"
                "        L_prim_minus_end:\n"
                "            CMP(R4, IMM(1));\n"
                "            JUMP_EQ(L_prim_minus_end_int);\n"
                "            PUSH(3);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_FRACTION));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "            MOV(INDD(R0, 2), R4);\n"
                "            JUMP(L_prim_minus_real_end);\n"
                "            L_prim_minus_end_int:\n"
                "            PUSH(2);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_INTEGER));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "        L_prim_minus_real_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_minus:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_minus));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr '- fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF minus BUILDING --------------- */\n")))
                
                
(define build-multiply
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF multiply BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_multiply);\n"
                "    L_prim_multiply:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;now R1 contains n - number of + args
                "        MOV(R2, IMM(0));\n" ;R2 - current argument
                "        MOV(R3, IMM(1));\n"
                "        MOV(R4, IMM(1));\n" ;R3/R4 will represnt the multi
                "        L_prim_multiply_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_multiply_end);\n"
                "            MOV(R5, FPARG(2 + R2));\n"
                "            CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "            JUMP_EQ(L_prim_multiply_loop_integer);\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, INDD(R5, 2));\n"
                "            JUMP(L_prim_multiply_loop_after_type_check);\n"
                "            L_prim_multiply_loop_integer:\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, IMM(1));\n"
                "        L_prim_multiply_loop_after_type_check:\n" ;now R6/R7 represnt the current argument to multiply
                "            CALL(MUL_REG_FRAC);\n" ;function the multiplys R6/R7 by R3/R4 and does reduction of the fraction using gcd
                "            ADD(R2, 1);\n"
                "            JUMP(L_prim_multiply_loop);\n"
                "        L_prim_multiply_end:\n"
                "            CMP(R4, IMM(1));\n"
                "            JUMP_EQ(L_prim_multiply_end_int);\n"
                "            PUSH(3);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_FRACTION));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "            MOV(INDD(R0, 2), R4);\n"
                "            JUMP(L_prim_multiply_real_end);\n"
                "            L_prim_multiply_end_int:\n"
                "            PUSH(2);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_INTEGER));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "        L_prim_multiply_real_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_multiply:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_multiply));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr '* fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF multiply BUILDING --------------- */\n")))
                
                
(define build-division
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF division BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_division);\n"
                "    L_prim_division:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;now R1 contains n - number of + args
                "        MOV(R2, IMM(1));\n" ;R2 - current argument
                "        MOV(R5, FPARG(2));\n" ;In division we need at least one argument to start the divi from
                "        CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "        JUMP_EQ(L_prim_division_integer);\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, INDD(R5, 2));\n"
                "        JUMP(L_prim_division_after_type_check);\n"
                "            L_prim_division_integer:\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, IMM(1));\n"
                "        L_prim_division_after_type_check:\n" ;R3/R4 will represnt the divi
                "        CMP(R1, IMM(1));\n"
                "            JUMP_NE(L_prim_division_loop);\n"
                "            MOV(R5, R3);\n" ;if we got only one argument then division acts diffrently, it does 1/number
                "            MOV(R3, R4);\n"
                "            MOV(R4, R5);\n"
                "            CMP(R4, IMM(0));\n" ;checking R4 for negative
                "            JUMP_LT(L_skip_prim_division_R4_negative);\n"
                "            JUMP(L_prim_division_end);\n"
                "            L_skip_prim_division_R4_negative:\n"
                "            MUL(R3, IMM(-1));\n" ;Putting minus in R3 instead of R4
                "            MUL(R4, IMM(-1));\n"
                "            JUMP(L_prim_division_end);\n"
                "        L_prim_division_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_division_end);\n"
                "            MOV(R5, FPARG(2 + R2));\n"
                "            CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "            JUMP_EQ(L_prim_division_loop_integer);\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, INDD(R5, 2));\n"
                "            JUMP(L_prim_division_loop_after_type_check);\n"
                "            L_prim_division_loop_integer:\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, IMM(1));\n"
                "        L_prim_division_loop_after_type_check:\n" ;now R6/R7 represnt the current argument to divied
                "            CALL(DIV_REG_FRAC);\n" ;function thhat divives R6/R7 from R3/R4 and does reduction of the fraction using gcd
                "            ADD(R2, 1);\n"
                "            JUMP(L_prim_division_loop);\n"
                "        L_prim_division_end:\n"
                "            CMP(R4, IMM(1));\n"
                "            JUMP_EQ(L_prim_division_end_int);\n"
                "            PUSH(3);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_FRACTION));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "            MOV(INDD(R0, 2), R4);\n"
                "            JUMP(L_prim_division_real_end);\n"
                "            L_prim_division_end_int:\n"
                "            PUSH(2);\n"
                "            CALL(MALLOC);\n"
                "            DROP(1);\n"
                "            MOV(INDD(R0, 0), IMM(T_INTEGER));\n"
                "            MOV(INDD(R0, 1), R3);\n"
                "        L_prim_division_real_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_division:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_division));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr '/ fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF division BUILDING --------------- */\n")))
                
(define build-sheva
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF sheva BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_sheva);\n"
                "    L_prim_sheva:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;now R1 contains number of arguments
                "        MOV(R2, IMM(1));\n" ;current argument
                "        MOV(R3, FPARG(2));\n" ;first argument = neads at least one argument
                "        L_prim_sheva_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_sheva_return_true);\n"
                "            MOV(R4, FPARG(2 + R2));\n"
                "            CMP(INDD(R3, 0), INDD(R4, 0));\n" ;checking both numbers are of same type
                "            JUMP_NE(L_prim_sheva_return_false);\n"
                "            CMP(INDD(R3, 1), INDD(R4, 1));\n" ;checking integres are same / numerator of frac is same
                "            JUMP_NE(L_prim_sheva_return_false);\n"
                "            CMP(INDD(R3, 0), IMM(T_FRACTION));\n"
                "            JUMP_NE(L_prim_sheva_loop_skip_frac_check);\n"
                "            CMP(INDD(R3, 2), INDD(R4, 2));\n" ;checking denominator of frac is same
                "            JUMP_NE(L_prim_sheva_return_false);\n"
                "        L_prim_sheva_loop_skip_frac_check:\n"
                "            ADD(R2, 1);\n"
                "            MOV(R3, R4);\n"
                "            JUMP(L_prim_sheva_loop);\n"
                "        L_prim_sheva_return_true:\n"
                "            MOV(R0, IMM(SOB_TRUE));\n"
                "            JUMP(L_prim_sheva_end);\n"
                "        L_prim_sheva_return_false:\n"
                "            MOV(R0, IMM(SOB_FALSE));\n"
                "        L_prim_sheva_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_sheva:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_sheva));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr '= fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF sheva BUILDING --------------- */\n")))
                
(define build-katan
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF katan BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_katan);\n"
                "    L_prim_katan:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;now R1 contains number of arguments
                "        MOV(R2, IMM(1));\n" ;current argument
                "        MOV(R5, FPARG(2));\n" ;first argument = neads at least one argument
                "        CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "        JUMP_EQ(L_prim_katan_integer);\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, INDD(R5, 2));\n"
                "        JUMP(L_prim_katan_after_type_check);\n"
                "        L_prim_katan_integer:\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, IMM(1));\n"
                "        L_prim_katan_after_type_check:\n" ;R3/R4 will represnt the current number
                "        L_prim_katan_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_sheva_return_true);\n"
                "            MOV(R5, FPARG(2 + R2));\n"
                "            CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "            JUMP_EQ(L_prim_katan_loop_integer);\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, INDD(R5, 2));\n"
                "            JUMP(L_prim_katan_loop_after_type_check);\n"
                "            L_prim_katan_loop_integer:\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, IMM(1));\n"
                "        L_prim_katan_loop_after_type_check:\n" ;now R6/R7 represnt the next argument to check <
                "            MOV(R8, R3);\n"
                "            MOV(R9, R6);\n"
                "            MUL(R8, R7);\n" ;now R8 = R3*R7
                "            MUL(R9, R4);\n" ;now R9 = R6*R4
                "            CMP(R8, R9);\n"
                "            JUMP_GE(L_prim_katan_return_false);\n"
                "            ADD(R2, 1);\n"
                "            MOV(R3, R6);\n"
                "            MOV(R4, R7);\n"
                "            JUMP(L_prim_katan_loop);\n"
                "        L_prim_katan_return_true:\n"
                "            MOV(R0, IMM(SOB_TRUE));\n"
                "            JUMP(L_prim_katan_end);\n"
                "        L_prim_katan_return_false:\n"
                "            MOV(R0, IMM(SOB_FALSE));\n"
                "        L_prim_katan_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_katan:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_katan));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr '< fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF katan BUILDING --------------- */\n")))
                
(define build-gadol
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF gadol BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_gadol);\n"
                "    L_prim_gadol:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;now R1 contains number of arguments
                "        MOV(R2, IMM(1));\n" ;current argument
                "        MOV(R5, FPARG(2));\n" ;first argument = neads at least one argument
                "        CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "        JUMP_EQ(L_prim_gadol_integer);\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, INDD(R5, 2));\n"
                "        JUMP(L_prim_gadol_after_type_check);\n"
                "        L_prim_gadol_integer:\n"
                "                MOV(R3, INDD(R5, 1));\n"
                "                MOV(R4, IMM(1));\n"
                "        L_prim_gadol_after_type_check:\n" ;R3/R4 will represnt the current number
                "        L_prim_gadol_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_sheva_return_true);\n"
                "            MOV(R5, FPARG(2 + R2));\n"
                "            CMP(INDD(R5, 0), IMM(T_INTEGER));\n"
                "            JUMP_EQ(L_prim_gadol_loop_integer);\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, INDD(R5, 2));\n"
                "            JUMP(L_prim_gadol_loop_after_type_check);\n"
                "            L_prim_gadol_loop_integer:\n"
                "                MOV(R6, INDD(R5, 1));\n"
                "                MOV(R7, IMM(1));\n"
                "        L_prim_gadol_loop_after_type_check:\n" ;now R6/R7 represnt the next argument to check >
                "            MOV(R8, R3);\n"
                "            MOV(R9, R6);\n"
                "            MUL(R8, R7);\n" ;now R8 = R3*R7
                "            MUL(R9, R4);\n" ;now R9 = R6*R4
                "            CMP(R8, R9);\n"
                "            JUMP_LE(L_prim_gadol_return_false);\n"
                "            ADD(R2, 1);\n"
                "            MOV(R3, R6);\n"
                "            MOV(R4, R7);\n"
                "            JUMP(L_prim_gadol_loop);\n"
                "        L_prim_gadol_return_true:\n"
                "            MOV(R0, IMM(SOB_TRUE));\n"
                "            JUMP(L_prim_gadol_end);\n"
                "        L_prim_gadol_return_false:\n"
                "            MOV(R0, IMM(SOB_FALSE));\n"
                "        L_prim_gadol_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_gadol:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_gadol));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr '> fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF gadol BUILDING --------------- */\n")))
                
                
(define build-eq
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF eq BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_eq);\n"
                "    L_prim_eq:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(2));\n" ;first argument
                "        MOV(R2, FPARG(3));\n" ;second argument
                "        CMP(INDD(R1, 0), INDD(R2, 0));\n" ;checking same type
                "        JUMP_NE(L_prim_eq_return_false);\n"
                "        CMP(INDD(R1, 0), IMM(T_INTEGER));\n"
                "        JUMP_EQ(L_prim_eq_check_one_val);\n"
                "        CMP(INDD(R1, 0), IMM(T_CHAR));\n"
                "        JUMP_EQ(L_prim_eq_check_one_val);\n"
                "        CMP(INDD(R1, 0), IMM(T_SYMBOL));\n"
                "        JUMP_EQ(L_prim_eq_check_one_val);\n"
                "        CMP(INDD(R1, 0), IMM(T_FRACTION));\n"
                "        JUMP_EQ(L_prim_eq_check_two_val);\n"
                "        JUMP(L_prim_eq_check_addr);\n"
                "        L_prim_eq_check_one_val:\n"
                "            CMP(INDD(R1, 1), INDD(R2, 1));\n" ;checking same value
                "            JUMP_EQ(L_prim_eq_return_true);\n"
                "            JUMP(L_prim_eq_return_false);\n"
                "        L_prim_eq_check_two_val:\n"
                "            CMP(INDD(R1, 1), INDD(R2, 1));\n" ;checking same value
                "            JUMP_NE(L_prim_eq_return_false);\n"
                "            CMP(INDD(R1, 2), INDD(R2, 2));\n" ;checking same value
                "            JUMP_EQ(L_prim_eq_return_true);\n"
                "            JUMP(L_prim_eq_return_false);\n"
                "        L_prim_eq_check_addr:\n"
                "            CMP(R1, R2);\n" ;checking same value
                "            JUMP_EQ(L_prim_eq_return_true);\n"
                "            JUMP(L_prim_eq_return_false);\n"
                "        L_prim_eq_return_true:\n"
                "            MOV(R0, IMM(SOB_TRUE));\n"
                "            JUMP(L_prim_eq_end);\n"
                "        L_prim_eq_return_false:\n"
                "            MOV(R0, IMM(SOB_FALSE));\n"
                "        L_prim_eq_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_eq:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_eq));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'eq? fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF eq BUILDING --------------- */\n")))
                
                
(define build-vector
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF vector BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_vector);\n"
                "    L_prim_vector:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(1));\n" ;R1 contains number of arguments
                "        MOV(R2, IMM(0));\n" ;R2 current argument index
                "        MOV(R3, R1);\n"
                "        ADD(R3, IMM(2));\n" ;R3 size of T_VECTOR
                "        PUSH(R3);\n"
                "        CALL(MALLOC);\n"
                "        DROP(1);\n"
                "        MOV(INDD(R0, 0), IMM(T_VECTOR));\n"
                "        MOV(INDD(R0, 1), R1);\n"
                "        L_prim_vector_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_vector_end);\n"
                "            MOV(R4, R2);\n"
                "            ADD(R4, IMM(2));\n"
                "            MOV(INDD(R0, R4), FPARG(R4));\n"
                "            ADD(R2, IMM(1));\n"
                "            JUMP(L_prim_vector_loop);\n"
                "       L_prim_vector_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_vector:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_vector));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'vector fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF vector BUILDING --------------- */\n")))
                
(define build-make_vector
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF make_vector BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_make_vector);\n"
                "    L_prim_make_vector:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        CMP(FPARG(1), IMM(1));\n"
                "        JUMP_EQ(L_prim_make_vector_one_arg);\n"
                "            MOV(R5, FPARG(3));\n" ;R5 contains element to create
                "            JUMP(L_prim_make_vector_cont);\n"
                "        L_prim_make_vector_one_arg:\n"
                "            PUSH(IMM(0));\n"
                "            CALL(MAKE_SOB_INTEGER);\n"
                "            DROP(1);\n"
                "            MOV(R5, R0);\n"       ;R5 contains element to create
                "        L_prim_make_vector_cont:\n"
                "            MOV(R1, FPARG(2));\n"
                "            MOV(R1, INDD(R1, 1));\n" ;R1 contatins size of vector
                "        MOV(R2, IMM(0));\n" ;R2 current argument index
                "        MOV(R3, R1);\n"
                "        ADD(R3, IMM(2));\n" ;R3 size of T_make_vector
                "        PUSH(R3);\n"
                "        CALL(MALLOC);\n"
                "        DROP(1);\n"
                "        MOV(INDD(R0, 0), IMM(T_VECTOR));\n"
                "        MOV(INDD(R0, 1), R1);\n"
                "        L_prim_make_vector_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_make_vector_end);\n"
                "            MOV(R4, R2);\n"
                "            ADD(R4, IMM(2));\n"
                "            MOV(INDD(R0, R4), R5);\n"
                "            ADD(R2, IMM(1));\n"
                "            JUMP(L_prim_make_vector_loop);\n"
                "       L_prim_make_vector_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_make_vector:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_make_vector));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'make-vector fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF make_vector BUILDING --------------- */\n")))
                
(define build-make_string
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF make_string BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_make_string);\n"
                "    L_prim_make_string:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        CMP(FPARG(1), IMM(1));\n"
                "        JUMP_EQ(L_prim_make_string_one_arg);\n"
                "            MOV(R5, FPARG(3));\n"
                "            MOV(R5, INDD(R5, 1));\n" ;R5 contains char to create
                "            JUMP(L_prim_make_string_cont);\n"
                "        L_prim_make_string_one_arg:\n"
                "            MOV(R5, IMM(0));\n"       ;R5 contains char to create
                "        L_prim_make_string_cont:\n"
                "            MOV(R1, FPARG(2));\n"
                "            MOV(R1, INDD(R1, 1));\n" ;R1 contatins size of string
                "        MOV(R2, IMM(0));\n" ;R2 current argument index
                "        MOV(R3, R1);\n"
                "        ADD(R3, IMM(2));\n" ;R3 size of T_make_string
                "        PUSH(R3);\n"
                "        CALL(MALLOC);\n"
                "        DROP(1);\n"
                "        MOV(INDD(R0, 0), IMM(T_STRING));\n"
                "        MOV(INDD(R0, 1), R1);\n"
                "        L_prim_make_string_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_make_string_end);\n"
                "            MOV(R4, R2);\n"
                "            ADD(R4, IMM(2));\n"
                "            MOV(INDD(R0, R4), R5);\n"
                "            ADD(R2, IMM(1));\n"
                "            JUMP(L_prim_make_string_loop);\n"
                "       L_prim_make_string_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_make_string:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_make_string));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'make-string fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF make_string BUILDING --------------- */\n")))
                
(define build-apply
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF apply BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_apply);\n"
                "    L_prim_apply:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        MOV(R1, FPARG(-2));\n" ;R1=old_fp
                "        MOV(R2, FPARG(-1));\n" ;R2=ret
                "        MOV(R4, FPARG(2));\n"  ;R4=procedure
                "        MOV(R10, FPARG(1));\n" ;R10= n, number of args
                "        MOV(R5, FPARG(R10 + 1));\n"  ;R5=list
                "        DROP(5);\n"            ;destory last frame
                "        MOV(R11, R10);\n"
                "        SUB(R11, IMM(2));\n"
                "        MOV(R8, R11);\n"    ;indicates number of pushed items until now
                "        L_prim_apply_loop_prepare_var:\n"
                "            CMP(R11, IMM(0));\n"
                "            JUMP_EQ(L_prim_apply_loop_prepare_var_end);\n"
                "            MOV(STARG(R11 - 1), STARG(R11 - 2));\n"
                "            SUB(R11, IMM(1));\n"
                "            JUMP(L_prim_apply_loop_prepare_var);\n"
                "        L_prim_apply_loop_prepare_var_end:\n"
                "            DROP(1);\n"
                "        L_prim_apply_loop:\n"
                "            CMP(INDD(R5, 0), IMM(T_NIL));\n"
                "            JUMP_EQ(L_prim_apply_end);\n"
                "            MOV(R6, INDD(R5, 1));\n" ;R6 = car of list
                "            MOV(R7, INDD(R5, 2));\n" ;R7 = cdr of list
                "            PUSH(R6);\n"
                "            MOV(R9, IMM(0));\n" ;current index of swap
                "            L_prim_apply_push_loop:\n"
                "                CMP(R9, R8);\n"
                "                JUMP_EQ(L_prim_apply_push_loop_end);\n"
                "                MOV(R10, STARG(R9 - 1));\n"
                "                MOV(STARG(R9 - 1), STARG(R9));\n"
                "                MOV(STARG(R9), R10);\n"
                "                ADD(R9, 1);\n"
                "                JUMP(L_prim_apply_push_loop);\n"
                "            L_prim_apply_push_loop_end:\n"
                "                MOV(R5, R7);\n"
                "                ADD(R8, 1);\n"
                "                JUMP(L_prim_apply_loop);\n"
                "        L_prim_apply_end:\n"
                "            PUSH(R8);\n" ;n = number of pushed items
                "            PUSH(INDD(R4, 1));\n" ;pushing procedure env
                "            PUSH(R2);\n" ;old return address
                "            MOV(FP, R1);\n" ;moving fp to old_fp
                "            JUMPA(INDD(R4, 2));\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_apply:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_apply));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'apply fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF apply BUILDING --------------- */\n")))
                
(define build-append
 (lambda (fvar-table)
   (string-append 
                "    /*--------------- START OF append BUILDING --------------- */\n"
                "    JUMP(L_skip_prim_append);\n"
                "    L_prim_append:\n"
                "        PUSH(FP);\n"
                "        MOV(FP, SP);\n"
                "        CMP(FPARG(1), IMM(0));\n"
                "        JUMP_NE(L_prim_append_no_zero_args);\n"
                "        MOV(R0, IMM(SOB_NIL));\n"
                "        JUMP(L_prim_append_end);\n"
                "        L_prim_append_no_zero_args:\n"
                "        CMP(FPARG(1), IMM(1));\n"
                "        JUMP_NE(L_prim_append_no_one_arg);\n"
                "        MOV(R0, FPARG(2));\n"
                "        JUMP(L_prim_append_end);\n"
                "        L_prim_append_no_one_arg:\n"
                "        MOV(R1, FPARG(1));\n" ;number of arguments
                "        MOV(R2, IMM(0));\n"    ;current index
                "        MOV(R3, IMM(SOB_NIL));\n"
                "        L_prim_append_loop:\n"
                "            CMP(R2, R1);\n"
                "            JUMP_EQ(L_prim_append_almost_end);\n"
                "            MOV(R4, IMM(FPARG(2 + R2)));\n"
                "            PUSH(R4);\n"
                "            PUSH(R3);\n"
                "            CALL(BINARY_APPEND);\n"
                "            DROP(2);\n"
                "            MOV(R3, R0);\n"
                "            ADD(R2, IMM(1));\n"
                "            JUMP(L_prim_append_loop);\n"
                "       L_prim_append_almost_end:\n"
                "            MOV(R0, R3);\n"
                "       L_prim_append_end:\n"
                "        POP(FP);\n"
                "        RETURN;\n"
                "    L_skip_prim_append:\n"
                "    PUSH(IMM(3));\n"
                "    CALL(MALLOC);\n"
                "    DROP(1);\n"
                ;create clouser
                "    MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                "    MOV(INDD(R0, 1), IMM(SOB_NIL));\n"
                "    MOV(INDD(R0, 2), LABEL(L_prim_append));\n"
                ;update fvar table
                "    MOV(R1, IMM(" (number->string (fvar->addr 'append fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                "    MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                "    /* --------------- END OF append BUILDING --------------- */\n")))
   

(define build-primitive-fvars
  (lambda (fvar-table)
    (string-append
      "/* --------------- START OF PRIMITIVE FVARS BUILDING --------------- */\n"
      (build-cons fvar-table)
      (build-car fvar-table)
      (build-cdr fvar-table)
      (fold-left string-append "" (map (lambda (e) (build-predict fvar-table e)) `((boolean? . ,T_BOOL) (char? . ,T_CHAR) (integer? . ,T_INTEGER) (pair? . ,T_PAIR) (procedure? . ,T_CLOSURE) (string? . ,T_STRING) (symbol? . ,T_SYMBOL) (vector? . ,T_VECTOR) (null? . ,T_NIL))))
      (build-char_to_integer fvar-table)
      (build-integer_to_char fvar-table)
      (build-number_and_rational-pred fvar-table)
      (build-denominator fvar-table)
      (build-numerator fvar-table)
      (build-string_and_vector_length fvar-table)
      (build-set_car fvar-table)
      (build-set_cdr fvar-table)
      (build-string_ref fvar-table)
      (build-string_set fvar-table)
      (build-vector_ref fvar-table)
      (build-vector_set fvar-table)
      (build-remainder fvar-table)
      (build-symbol_to_string fvar-table)
      (build-string_to_symbol fvar-table)
      (build-not fvar-table)
      (build-plus fvar-table)
      (build-minus fvar-table)
      (build-multiply fvar-table)
      (build-division fvar-table)
      (build-sheva fvar-table)
      (build-katan fvar-table)
      (build-gadol fvar-table)
      (build-eq fvar-table)
      (build-vector fvar-table)
      (build-make_vector fvar-table)
      (build-make_string fvar-table)
      (build-apply fvar-table)
      (build-append fvar-table)
      "/* --------------- END OF PRIMITIVE FVARS BUILDING --------------- */\n\n")))

(define scheme-primitive-fvars
   (list
       '(define list (lambda x x))
       '(define zero? (lambda (x) (= x 0)))
       '(define map
            (lambda (f . lst-of-lst)
                  (letrec ((binary-map (lambda (p lst)
                              (if (null? lst)
                                  '()
                                  (cons (p (car lst)) (binary-map p (cdr lst)))))))
                    (if (null? (car lst-of-lst))
                        '()
                        (cons (apply f (binary-map car lst-of-lst)) (apply map (cons f (binary-map cdr lst-of-lst))))))))
       '(define caar (lambda (x) (car (car x))))
       '(define cadr (lambda (x) (car (cdr x))))
       '(define cdar (lambda (x) (cdr (car x))))
       '(define cddr (lambda (x) (cdr (cdr x))))
       '(define caaar (lambda (x) (car (car (car x)))))
       '(define caadr (lambda (x) (car (car (cdr x)))))
       '(define cadar (lambda (x) (car (cdr (car x)))))
       '(define caddr (lambda (x) (car (cdr (cdr x)))))
       '(define cdaar (lambda (x) (cdr (car (car x)))))
       '(define cdadr (lambda (x) (cdr (car (cdr x)))))
       '(define cddar (lambda (x) (cdr (cdr (car x)))))
       '(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
       '(define caaaar (lambda (x) (car (car (car (car x))))))
       '(define caaadr (lambda (x) (car (car (car (cdr x))))))
       '(define caadar (lambda (x) (car (car (cdr (car x))))))
       '(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
       '(define cadaar (lambda (x) (car (cdr (car (car x))))))
       '(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
       '(define caddar (lambda (x) (car (cdr (cdr (car x))))))
       '(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
       '(define cdaaar (lambda (x) (cdr (car (car (car x))))))
       '(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
       '(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
       '(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
       '(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
       '(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
       '(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
       '(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
       ))


(define print-sob
  (lambda (level)
    (let ((dont-print-lable (L_dont_print_sob)))
       (string-append (make-spaces level) "CMP(R0, IMM(SOB_VOID));\n"
                      (make-spaces level) "JUMP_EQ(" dont-print-lable ");\n"
                      (make-spaces level) "PUSH(IMM(R0));\n" 
                      (make-spaces level) "CALL(WRITE_SOB);\n" 
                      (make-spaces level) "POP(R0);\n" 
                      (make-spaces level) "CALL(NEWLINE);\n"
                      (make-spaces level) dont-print-lable ":\n" ))))
    
(define const-codegen
  (lambda (const const-table level)
      (string-append (make-spaces level) "/* CODEGEN(" (format "~s" const) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (make-spaces level) "MOV(R0, IMM(" (number->string (const->addr (const->value const) const-table)) "));\n"
                         (make-spaces level) "/* -------------------------------- */\n")))
      
(define if3-codegen
  (lambda (if3 env-size param-size const-table fvar-table level)
    (let ((else-label (L_if3_else))
          (end-label (L_if3_end)))
      (string-append 
               (make-spaces level) "/* CODEGEN(" (format "~s" if3) ") */\n"
               (make-spaces level) "/* -------------------------------- */\n"
               (codegen (if3->test if3) env-size param-size const-table fvar-table (+ level 1))
               (make-spaces level) "CMP(R0, IMM(SOB_FALSE));\n"
               (make-spaces level) "JUMP_EQ(" else-label ");\n"
               (codegen (if3->dit if3) env-size param-size const-table fvar-table (+ level 1))
               (make-spaces level) "JUMP(" end-label ");\n"
               (make-spaces level) else-label ":\n"
               (codegen (if3->dif if3) env-size param-size const-table fvar-table (+ level 1))
               (make-spaces level) end-label ":\n"
               (make-spaces level) "/* -------------------------------- */\n" ))))
               
(define or->expr_list
  (lambda (orexpr)
      (cadr orexpr)))
               
(define or-codegen
  (lambda (or-expr env-size param-size const-table fvar-table level)
    (letrec ((end-label (L_or_exit))
             (code-loop (lambda (expr-list)
                             (string-append
                                     (codegen (car expr-list) env-size param-size const-table fvar-table (+ level 1))
                                     (if (null? (cdr expr-list))
                                         ""
                                         (string-append (make-spaces level) "CMP(R0, IMM(SOB_FALSE));\n"
                                                        (make-spaces level) "JUMP_NE(" end-label ");\n"
                                                        (code-loop (cdr expr-list))))))))
      (string-append 
               (make-spaces level) "/* CODEGEN(" (format "~s" or-expr) ") */\n"
               (make-spaces level) "/* -------------------------------- */\n"
               (code-loop (or->expr_list or-expr))
               (make-spaces level) end-label ":\n"
               (make-spaces level) "/* -------------------------------- */\n" ))))
               
(define seq->expr_list
  (lambda (seqexpr)
      (cadr seqexpr)))
               
(define seq-codegen
  (lambda (seq-expr env-size param-size const-table fvar-table level)
     (letrec ((code-loop (lambda (expr-list)
                           (if (null? expr-list)
                               ""
                               (string-append 
                                       (codegen (car expr-list) env-size param-size const-table fvar-table (+ level 1))
                                       (code-loop (cdr expr-list)))))))
         (string-append 
                 (make-spaces level) "/* CODEGEN(" (format "~s" seq-expr) ") */\n"
                 (make-spaces level) "/* -------------------------------- */\n"
                 (code-loop (seq->expr_list seq-expr))
                 (make-spaces level) "/* -------------------------------- */\n"))))
                 
(define applic-codegen
  (lambda (applic env-size param-size const-table fvar-table level)
     (letrec ((proc (applic->func applic))
              (arg-list (applic->args applic))
              (num_of_args (length (applic->args applic)))
              (label-no-error (L_applic_no_error))
              (push-args-loop (lambda (cur_args_list)
                                  (if (null? cur_args_list)
                                      ""
                                      (string-append
                                          (codegen (car cur_args_list) env-size param-size const-table fvar-table (+ level 1))
                                          (make-spaces level) "PUSH(R0);\n"
                                          (push-args-loop (cdr cur_args_list)))))))
          (string-append
                 (make-spaces level) "/* CODEGEN(" (format "~s" applic) ") */\n"
                 (make-spaces level) "/* -------------------------------- */\n"
                 (make-spaces level) "PUSH(IMM(SOB_NIL));\n" ;pushing empty-list for var and opt lambdas
                 (push-args-loop (reverse arg-list))
                 (make-spaces level) "PUSH(IMM(" (number->string num_of_args) "));\n"
                 (codegen proc env-size param-size const-table fvar-table (+ level 1))
                 (make-spaces level) "PUSH(INDD(R0, 1));\n" ;pushing procedure env
                 (make-spaces level) "CALLA(INDD(R0, 2));\n" ;calling procedure
                 (make-spaces level) "DROP(1);\n" ;removing env
                 (make-spaces level) "POP(R1);\n" ;now R1 contains "n-number of args"
                 (make-spaces level) "ADD(R1, 1);\n" ;for the empty-list we pushed
                 (make-spaces level) "DROP(R1);\n"
                 (make-spaces level) "/* -------------------------------- */\n"))))
                 
                 
(define tc-applic-codegen
  (lambda (applic env-size param-size const-table fvar-table level)
     (letrec ((proc (applic->func applic))
              (arg-list (applic->args applic))
              (num_of_args (length (applic->args applic)))
              (label-no-error (L_applic_no_error))
              (label-loop (L_tc_applic_loop))
              (label-loop-end (L_tc_applic_loop_end))
              (push-args-loop (lambda (cur_args_list)
                                  (if (null? cur_args_list)
                                      ""
                                      (string-append
                                          (codegen (car cur_args_list) env-size param-size const-table fvar-table (+ level 1))
                                          (make-spaces level) "PUSH(R0);\n"
                                          (push-args-loop (cdr cur_args_list)))))))
          (string-append
                 (make-spaces level) "/* CODEGEN(" (format "~s" applic) ") */\n"
                 (make-spaces level) "/* -------------------------------- */\n"
                 (make-spaces level) "PUSH(IMM(SOB_NIL));\n" ;pushing empty-list for var and opt lambdas
                 (push-args-loop (reverse arg-list))
                 (make-spaces level) "PUSH(IMM(" (number->string num_of_args) "));\n"
                 (codegen proc env-size param-size const-table fvar-table (+ level 1))
                 (make-spaces level) "PUSH(INDD(R0, 1));\n" ;pushing procedure env

                 (make-spaces level) "PUSH(FPARG(-1));\n" ;pushing RET
                 (make-spaces level) "MOV(R1, FPARG(-2));\n" ;saving old_fp in R1
                 
                 ;code thats put start of old_frame in R2
                 (make-spaces level) "MOV(R2, FP);\n";
                 (make-spaces level) "SUB(R2, 1);\n" ;now R2 points to the start of the old frame which is old_fp
                 (make-spaces level) "MOV(R3, FPARG(1));\n" ;now R3 contains "n" from the old_frame
                 (make-spaces level) "ADD(R3, 5);\n" ;now R3 is the number of parms we need to delete from the stack - 4 for old_fp, RET, ENV, n and 1 - for '()
                 (make-spaces level) "SUB(R2, R3);\n"
                 (make-spaces level) "ADD(R2, 1);\n" ; now R2 points to the bottom of the old_frame
                 
                 ;code thats put start of new_frame in R4
                 (make-spaces level) "MOV(R4, SP);\n";
                 (make-spaces level) "SUB(R4, 1);\n" ;now R4 points to the start of the new frame which is RET
                 (make-spaces level) "MOV(R3, " (number->string num_of_args) ");\n" ;;now R3 contains "n" from the new_frame
                 (make-spaces level) "ADD(R3, 4);\n" ;now R3 is the number of stack size for our new frame - 3 for RET, ENV, n and 1 - for '()
                 (make-spaces level) "SUB(R4, R3);\n"
                 (make-spaces level) "ADD(R4, 1);\n" ; now R4 points to the bottom of the new_frame
                 
                 ;below code overrides the old frame with our new frame
                 (make-spaces level) "    MOV(R13, IMM(0));\n"
                 (make-spaces level) "    " label-loop ":\n"
                 (make-spaces level) "        CMP(R13, R3);\n"
                 (make-spaces level) "        JUMP_GE(" label-loop-end ");\n"
                 (make-spaces level) "        STACK(R2 + R13) = STACK(R4 + R13);\n"
                 (make-spaces level) "        ADD(R13, IMM(1));\n"
                 (make-spaces level) "        JUMP(" label-loop ");\n"
                 (make-spaces level) "    " label-loop-end ":\n"
                 ;fiiting sp to our overriden frame
                 (make-spaces level) "MOV(SP, R2);\n"
                 (make-spaces level) "ADD(SP, R3);\n" ;now sp is fitted for our new frame
                 
                 (make-spaces level) "MOV(FP, R1);\n" ;moving fp to old_fp
                 (make-spaces level) "JUMPA(INDD(R0, 2));\n"
                 (make-spaces level) "/* -------------------------------- */\n"))))

                 
(define lambda-simple-codegen
  (lambda (lambda-simple env-size param-size const-table fvar-table level)
   (let ((closure-code (L_closure_code))
         (closure-exit (L_closure_exit))
         (loop-one (L_closure_loop_one))
         (loop-one-end (L_closure_loop_one_end))
         (loop-two (L_closure_loop_two))
         (loop-two-end (L_closure_loop_two_end))
         (body (lambda->body lambda-simple))
         (parms (car (lambda->list_parms lambda-simple))))
     (string-append (make-spaces level) "/* CODEGEN(" (format "~s" lambda-simple) ") */\n"
                    (make-spaces level) "/* -------------------------------- */\n"
                    (make-spaces level) "MOV(R1, FPARG(0));\n" ;R1 is a pointer to old_env
                    (if (zero? env-size)
                         (string-append (make-spaces level) "MOV(R2, IMM(SOB_NIL));\n") ;R2 is a pointer to our new_env ---> empty environment
                         (string-append (make-spaces level) "PUSH(" (number->string (+ 1 env-size)) ");\n"
                                        (make-spaces level) "CALL(MALLOC);\n"
                                        (make-spaces level) "DROP(1);\n"
                                        (make-spaces level) "MOV(R2, R0);\n") ;R2 is a pointer to our new_env
                    )
                    ;below assembly code-block copies the old env to our new env, after it finishes R2 will look like env but with a bigger size
                    (make-spaces level) "    MOV(R13, IMM(0));\n"
                    (make-spaces level) "    MOV(R14, IMM(1));\n"
                    (make-spaces level) "    " loop-one ":\n"
                    (make-spaces level) "        CMP(R13, " (number->string env-size) ");\n"
                    (make-spaces level) "        JUMP_GE(" loop-one-end ");\n"
                    (make-spaces level) "        MOV(INDD(R2, R14), INDD(R1, R13));\n"
                    (make-spaces level) "        ADD(R13, IMM(1));\n"
                    (make-spaces level) "        ADD(R14, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-one ");\n"
                    (make-spaces level) "    " loop-one-end ":\n"
                    (make-spaces level) "MOV(R3, IMM(" (number->string param-size) "));\n" ;R3 is number of parameters of the old procedure - this is very important!!!!
                    (if (zero? env-size)
                        ""
                        (if (zero? param-size)
                            (string-append (make-spaces level) "MOV(INDD(R2, 0), IMM(SOB_NIL));\n") ;update the parms to be a empty list, beacuse we dont have any parms to extend
                            (string-append
                                   (make-spaces level) "PUSH(R3);\n"
                                   (make-spaces level) "CALL(MALLOC);\n"
                                   (make-spaces level) "DROP(1);\n"
                                   (make-spaces level) "MOV(INDD(R2,0), R0);\n")) ;creating a new place for us to put the parms of the old lambda
                    )
                    ;below assembly code-block copies all the parms from the the lambda we saw before to our new environment
                    (make-spaces level) "    MOV(R4, INDD(R2, 0));\n"
                    (make-spaces level) "    MOV(R13, IMM(0));\n"
                    (make-spaces level) "    " loop-two ":\n"
                    (make-spaces level) "        CMP(R13, IMM(R3));\n"
                    (make-spaces level) "        JUMP_GE(" loop-two-end ");\n"
                    (make-spaces level) "        MOV(INDD(R4, R13), FPARG(2 + R13));\n"
                    (make-spaces level) "        ADD(R13, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-two ");\n"
                    (make-spaces level) "    " loop-two-end ":\n"
                    (make-spaces level) "PUSH(IMM(3));\n"
                    (make-spaces level) "CALL(MALLOC);\n"
                    (make-spaces level) "DROP(1);\n"
                    (make-spaces level) "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                    (make-spaces level) "MOV(INDD(R0, 1), R2);\n"
                    (make-spaces level) "MOV(INDD(R0, 2), LABEL(" closure-code "));\n"
                    (make-spaces level) "JUMP(" closure-exit ");\n"
                    (make-spaces level) closure-code ":\n"
                    (make-spaces level) "    PUSH(FP);\n"
                    (make-spaces level) "    MOV(FP, SP);\n"
                    (make-spaces level) "    " (codegen body (+ 1 env-size) (length parms) const-table fvar-table (+ level 1))
                    (make-spaces level) "    POP(FP);\n"
                    (make-spaces level) "    RETURN;\n";
                    (make-spaces level) closure-exit ":\n"
                    (make-spaces level) "/* -------------------------------- */\n"))))
                    
                    
(define lambda-var-codegen
  (lambda (lambda-var env-size param-size const-table fvar-table level)
   (let ((closure-code (L_closure_code))
         (closure-exit (L_closure_exit))
         (loop-one (L_closure_loop_one))
         (loop-one-end (L_closure_loop_one_end))
         (loop-two (L_closure_loop_two))
         (loop-two-end (L_closure_loop_two_end))
         (loop-three (L_closure_loop_three))
         (loop-three-end (L_closure_loop_three_end))
         (body (lambda->body lambda-var))
         (parms (lambda->list_parms lambda-var)))
     (string-append (make-spaces level) "/* CODEGEN(" (format "~s" lambda-var) ") */\n"
                    (make-spaces level) "/* -------------------------------- */\n"
                    (make-spaces level) "MOV(R1, FPARG(0));\n" ;R1 is a pointer to old_env
                    (if (zero? env-size)
                         (string-append (make-spaces level) "MOV(R2, IMM(SOB_NIL));\n") ;R2 is a pointer to our new_env ---> empty environment
                         (string-append (make-spaces level) "PUSH(" (number->string (+ 1 env-size)) ");\n"
                                        (make-spaces level) "CALL(MALLOC);\n"
                                        (make-spaces level) "DROP(1);\n"
                                        (make-spaces level) "MOV(R2, R0);\n") ;R2 is a pointer to our new_env
                    )
                    ;below assembly code-block copies the old env to our new env, after it finishes R2 will look like env but with a bigger size
                    (make-spaces level) "    MOV(R13, IMM(0));\n"
                    (make-spaces level) "    MOV(R14, IMM(1));\n"
                    (make-spaces level) "    " loop-one ":\n"
                    (make-spaces level) "        CMP(R13, " (number->string env-size) ");\n"
                    (make-spaces level) "        JUMP_GE(" loop-one-end ");\n"
                    (make-spaces level) "        MOV(INDD(R2, R14), INDD(R1, R13));\n"
                    (make-spaces level) "        ADD(R13, IMM(1));\n"
                    (make-spaces level) "        ADD(R14, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-one ");\n"
                    (make-spaces level) "    " loop-one-end ":\n"
                    (make-spaces level) "MOV(R3, IMM(" (number->string param-size) "));\n" ;R3 is number of parameters of the old procedure - this is very important!!!!
                    (if (zero? env-size)
                        ""
                        (if (zero? param-size)
                            (string-append (make-spaces level) "MOV(INDD(R2, 0), IMM(SOB_NIL));\n") ;update the parms to be a empty list, beacuse we dont have any parms to extend
                            (string-append
                                   (make-spaces level) "PUSH(R3);\n"
                                   (make-spaces level) "CALL(MALLOC);\n"
                                   (make-spaces level) "DROP(1);\n"
                                   (make-spaces level) "MOV(INDD(R2,0), R0);\n")) ;creating a new place for us to put the parms of the old lambda
                    )
                    ;below assembly code-block copies all the parms from the the lambda we saw before to our new environment
                    (make-spaces level) "    MOV(R4, INDD(R2, 0));\n"
                    (make-spaces level) "    MOV(R13, IMM(0));\n"
                    (make-spaces level) "    " loop-two ":\n"
                    (make-spaces level) "        CMP(R13, IMM(R3));\n"
                    (make-spaces level) "        JUMP_GE(" loop-two-end ");\n"
                    (make-spaces level) "        MOV(INDD(R4, R13), FPARG(2 + R13));\n"
                    (make-spaces level) "        ADD(R13, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-two ");\n"
                    (make-spaces level) "    " loop-two-end ":\n"
                    (make-spaces level) "PUSH(IMM(3));\n"
                    (make-spaces level) "CALL(MALLOC);\n"
                    (make-spaces level) "DROP(1);\n"
                    (make-spaces level) "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                    (make-spaces level) "MOV(INDD(R0, 1), R2);\n"
                    (make-spaces level) "MOV(INDD(R0, 2), LABEL(" closure-code "));\n"
                    (make-spaces level) "JUMP(" closure-exit ");\n"
                    (make-spaces level) closure-code ":\n"
                    (make-spaces level) "    PUSH(FP);\n"
                    (make-spaces level) "    MOV(FP, SP);\n"
                    ;special_code for lambda var - putting args inside list
                    (make-spaces level) "    MOV(R1, IMM(SOB_NIL));\n" ;R1 will be our list
                    (make-spaces level) "    MOV(R2, FPARG(1));\n" ;R2 contains n - number of args
                    ;build list loop
                    (make-spaces level) "    MOV(R13, IMM(R2));\n"
                    (make-spaces level) "    SUB(R13, IMM(1));\n"
                    (make-spaces level) "    " loop-three ":\n"
                    (make-spaces level) "        CMP(R13, IMM(0));\n"
                    (make-spaces level) "        JUMP_LT(" loop-three-end ");\n"
                    (make-spaces level) "        PUSH(R1);\n"
                    (make-spaces level) "        PUSH(FPARG(2+R13));\n"
                    (make-spaces level) "        CALL(MAKE_SOB_PAIR);\n"
                    (make-spaces level) "        DROP(2);\n"
                    (make-spaces level) "        MOV(R1, R0);\n"
                    (make-spaces level) "        SUB(R13, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-three ");\n"
                    (make-spaces level) "    " loop-three-end ":\n"
                    (make-spaces level) "    MOV(FPARG(2), R1);\n" ;overriding first argument with our list
                    ;end of speical_code
                    (make-spaces level) "    " (codegen body (+ 1 env-size) (length parms) const-table fvar-table (+ level 1))
                    (make-spaces level) "    POP(FP);\n"
                    (make-spaces level) "    RETURN;\n";
                    (make-spaces level) closure-exit ":\n"
                    (make-spaces level) "/* -------------------------------- */\n"))))
                    
                    
                    
(define lambda-opt-codegen
  (lambda (lambda-opt env-size param-size const-table fvar-table level)
   (let ((closure-code (L_closure_code))
         (closure-exit (L_closure_exit))
         (loop-one (L_closure_loop_one))
         (loop-one-end (L_closure_loop_one_end))
         (loop-two (L_closure_loop_two))
         (loop-two-end (L_closure_loop_two_end))
         (loop-three (L_closure_loop_three))
         (loop-three-end (L_closure_loop_three_end))
         (body (lambda->body lambda-opt))
         (parms (lambda->parms_good_list lambda-opt))
         (most-parms (car (lambda->list_parms lambda-opt)))
         (opt-parm (cadr (lambda->list_parms lambda-opt))))
     (string-append (make-spaces level) "/* CODEGEN(" (format "~s" lambda-opt) ") */\n"
                    (make-spaces level) "/* -------------------------------- */\n"
                    (make-spaces level) "MOV(R1, FPARG(0));\n" ;R1 is a pointer to old_env
                    (if (zero? env-size)
                         (string-append (make-spaces level) "MOV(R2, IMM(SOB_NIL));\n") ;R2 is a pointer to our new_env ---> empty environment
                         (string-append (make-spaces level) "PUSH(" (number->string (+ 1 env-size)) ");\n"
                                        (make-spaces level) "CALL(MALLOC);\n"
                                        (make-spaces level) "DROP(1);\n"
                                        (make-spaces level) "MOV(R2, R0);\n") ;R2 is a pointer to our new_env
                    )
                    ;below assembly code-block copies the old env to our new env, after it finishes R2 will look like env but with a bigger size
                    (make-spaces level) "    MOV(R13, IMM(0));\n"
                    (make-spaces level) "    MOV(R14, IMM(1));\n"
                    (make-spaces level) "    " loop-one ":\n"
                    (make-spaces level) "        CMP(R13, " (number->string env-size) ");\n"
                    (make-spaces level) "        JUMP_GE(" loop-one-end ");\n"
                    (make-spaces level) "        MOV(INDD(R2, R14), INDD(R1, R13));\n"
                    (make-spaces level) "        ADD(R13, IMM(1));\n"
                    (make-spaces level) "        ADD(R14, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-one ");\n"
                    (make-spaces level) "    " loop-one-end ":\n"
                    (make-spaces level) "MOV(R3, IMM(" (number->string param-size) "));\n" ;R3 is number of parameters of the old procedure - this is very important!!!!
                    (if (zero? env-size)
                        ""
                        (if (zero? param-size)
                            (string-append (make-spaces level) "MOV(INDD(R2, 0), IMM(SOB_NIL));\n") ;update the parms to be a empty list, beacuse we dont have any parms to extend
                            (string-append
                                   (make-spaces level) "PUSH(R3);\n"
                                   (make-spaces level) "CALL(MALLOC);\n"
                                   (make-spaces level) "DROP(1);\n"
                                   (make-spaces level) "MOV(INDD(R2,0), R0);\n")) ;creating a new place for us to put the parms of the old lambda
                    )
                    ;below assembly code-block copies all the parms from the the lambda we saw before to our new environment
                    (make-spaces level) "    MOV(R4, INDD(R2, 0));\n"
                    (make-spaces level) "    MOV(R13, IMM(0));\n"
                    (make-spaces level) "    " loop-two ":\n"
                    (make-spaces level) "        CMP(R13, IMM(R3));\n"
                    (make-spaces level) "        JUMP_GE(" loop-two-end ");\n"
                    (make-spaces level) "        MOV(INDD(R4, R13), FPARG(2 + R13));\n"
                    (make-spaces level) "        ADD(R13, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-two ");\n"
                    (make-spaces level) "    " loop-two-end ":\n"
                    (make-spaces level) "PUSH(IMM(3));\n"
                    (make-spaces level) "CALL(MALLOC);\n"
                    (make-spaces level) "DROP(1);\n"
                    (make-spaces level) "MOV(INDD(R0,0), IMM(T_CLOSURE));\n"
                    (make-spaces level) "MOV(INDD(R0, 1), R2);\n"
                    (make-spaces level) "MOV(INDD(R0, 2), LABEL(" closure-code "));\n"
                    (make-spaces level) "JUMP(" closure-exit ");\n"
                    (make-spaces level) closure-code ":\n"
                    (make-spaces level) "    PUSH(FP);\n"
                    (make-spaces level) "    MOV(FP, SP);\n"
                    ;special_code for lambda opt - putting args inside list
                    (make-spaces level) "    MOV(R3, IMM(" (number->string (length most-parms)) "));\n" ;updating the index of the opt-param
                    (make-spaces level) "    MOV(R1, IMM(SOB_NIL));\n" ;R1 will be our list
                    (make-spaces level) "    MOV(R2, FPARG(1));\n" ;R2 contains n - number of args
                    ;build list loop
                    (make-spaces level) "    MOV(R13, IMM(R2));\n"
                    (make-spaces level) "    SUB(R13, IMM(1));\n"
                    (make-spaces level) "    " loop-three ":\n"
                    (make-spaces level) "        CMP(R13, IMM(R3));\n"
                    (make-spaces level) "        JUMP_LT(" loop-three-end ");\n"
                    (make-spaces level) "        PUSH(R1);\n"
                    (make-spaces level) "        PUSH(FPARG(2+R13));\n"
                    (make-spaces level) "        CALL(MAKE_SOB_PAIR);\n"
                    (make-spaces level) "        DROP(2);\n"
                    (make-spaces level) "        MOV(R1, R0);\n"
                    (make-spaces level) "        SUB(R13, IMM(1));\n"
                    (make-spaces level) "        JUMP(" loop-three ");\n"
                    (make-spaces level) "    " loop-three-end ":\n"
                    (make-spaces level) "    MOV(FPARG(2+R3), R1);\n" ;overriding opt argument with our list
                    ;end of speical_code
                    (make-spaces level) "    " (codegen body (+ 1 env-size) (length parms) const-table fvar-table (+ level 1))
                    (make-spaces level) "    POP(FP);\n"
                    (make-spaces level) "    RETURN;\n";
                    (make-spaces level) closure-exit ":\n"
                    (make-spaces level) "/* -------------------------------- */\n"))))
     
(define pvar->minor
  (lambda (pvar)
     (caddr pvar)))
     
(define bvar->major
  (lambda (bvar)
     (caddr bvar)))
     
(define bvar->minor
  (lambda (bvar)
     (cadddr bvar)))     
  
(define pvar-codegen
  (lambda (pvar env-size param-size const-table fvar-table level)
     (string-append
         (make-spaces level) "/* CODEGEN(" (format "~s" pvar) ") */\n"
         (make-spaces level) "/* -------------------------------- */\n"
         (make-spaces level) "MOV(R0, FPARG(2 + " (number->string (pvar->minor pvar)) "));\n"
         (make-spaces level) "/* -------------------------------- */\n")))

(define bvar-codegen
  (lambda (bvar env-size param-size const-table fvar-table level)
     (string-append
         (make-spaces level) "/* CODEGEN(" (format "~s" bvar) ") */\n"
         (make-spaces level) "/* -------------------------------- */\n"
         (make-spaces level) "MOV(R0, FPARG(0));\n" ;now R0 contains env
         (make-spaces level) "MOV(R0, INDD(R0, " (number->string (bvar->major bvar)) "));\n" ;now R0 = env[major]
         (make-spaces level) "MOV(R0, INDD(R0, " (number->string (bvar->minor bvar)) "));\n" ;now R0 = env[major][minor]
         (make-spaces level) "/* -------------------------------- */\n")))
         
(define fvar->value
   (lambda (fvar)
       (cadr fvar)))
       
(define fvar-codegen
  (lambda (fvar env-size param-size const-table fvar-table level)
      (string-append (make-spaces level) "/* CODEGEN(" (format "~s" fvar) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (make-spaces level) "MOV(R0, IMM(" (number->string (fvar->addr (fvar->value fvar) fvar-table)) "));\n" ;now R0 is the address were we store the real adress of the fvar
                     (make-spaces level) "MOV(R0, IND(R0));\n" ;now R0 is the adress of our fvar
                     (make-spaces level) "/* -------------------------------- */\n")))
                         
(define def->fvar
  (lambda (def)
     (cadr def)))
     
(define def->expr
  (lambda (def)
     (caddr def)))


(define def-codegen
  (lambda (def env-size param-size const-table fvar-table level)
    (let ((fvar (def->fvar def))
         (expr (def->expr def)))
      (string-append (make-spaces level) "/* CODEGEN(" (format "~s" def) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (codegen expr env-size param-size const-table fvar-table (+ 1 level)) ;computing the expr of the def
                     (make-spaces level) "MOV(R1, IMM(" (number->string (fvar->addr (fvar->value fvar) fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                     (make-spaces level) "MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                     (make-spaces level) "MOV(R0, IMM(SOB_VOID));\n"
                     (make-spaces level) "/* -------------------------------- */\n"))))
                     
(define set->var
  (lambda (def)
     (cadr def)))
     
(define set->expr
  (lambda (def)
     (caddr def)))

(define set-codegen
  (lambda (set env-size param-size const-table fvar-table level)
    (let ((var (set->var set))
         (expr (set->expr set)))
      (if (equal? 'fvar (car var))
          (string-append (make-spaces level) "/* CODEGEN(" (format "~s" set) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (codegen expr env-size param-size const-table fvar-table (+ 1 level)) ;computing the expr of the set
                     (make-spaces level) "MOV(R1, IMM(" (number->string (fvar->addr (fvar->value var) fvar-table)) "));\n" ;now R1 is the address were we store the real adress of the fvar
                     (make-spaces level) "MOV(IND(R1), R0);\n" ;updating the adress of the fvar to our new computed expr
                     (make-spaces level) "MOV(R0, IMM(SOB_VOID));\n"
                     (make-spaces level) "/* -------------------------------- */\n")
         (if (equal? 'pvar (car var))
                     (string-append (make-spaces level) "/* CODEGEN(" (format "~s" set) ") */\n"
                                    (make-spaces level) "/* -------------------------------- */\n"
                                    (codegen expr env-size param-size const-table fvar-table (+ 1 level)) ;computing the expr of the set
                                    (make-spaces level) "MOV(R1, IMM(" (number->string (caddr var)) "));\n" ;now R1 is the minor of the pvar
                                    (make-spaces level) "ADD(R1, 2);\n" ;updating R1 for fparg
                                    (make-spaces level) "MOV(FPARG(R1), R0);\n" ;doing the set
                                    (make-spaces level) "MOV(R0, IMM(SOB_VOID));\n"
                                    (make-spaces level) "/* -------------------------------- */\n")
                     ;this next option is (set (bvar...)) this can only happend if someone wrote set whitout get (otherwise this will be (set-box...)) which is usesless... so i really don't do anything
                     (string-append   (make-spaces level) "/* CODEGEN(" (format "~s" set) ") */\n"
                                      (make-spaces level) "MOV(R0, IMM(SOB_VOID));\n"
                                      (make-spaces level) "/* -------------------------------- */\n"))))))
                     
(define box-codegen
  (lambda (box env-size param-size const-table fvar-table level)
    (let ((pvar (cadr box)))
      (string-append (make-spaces level) "/* CODEGEN(" (format "~s" box) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (make-spaces level) "PUSH(IMM(1));\n"
                     (make-spaces level) "CALL(MALLOC);\n" ;creating the memory for the box
                     (make-spaces level) "DROP(1);\n"
                     (make-spaces level) "MOV(IND(R0), FPARG(2 + " (number->string (pvar->minor pvar)) "));\n" ;boxing var
                     (make-spaces level) "/* -------------------------------- */\n"))))
                     
(define box-get-codegen
  (lambda (box-get env-size param-size const-table fvar-table level)
    (let ((var (cadr box-get)))
      (if (equal? 'pvar (car var))
          (string-append (make-spaces level) "/* CODEGEN(" (format "~s" box-get) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (make-spaces level) "MOV(R0, FPARG(2 + " (number->string (pvar->minor var)) "));\n"
                     (make-spaces level) "MOV(R0, IND(R0));\n" ;unbox
                     (make-spaces level) "/* -------------------------------- */\n")
         (string-append (make-spaces level) "/* CODEGEN(" (format "~s" box-get) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (make-spaces level) "MOV(R0, FPARG(0));\n" ;now R0 contains env
                     (make-spaces level) "MOV(R0, INDD(R0, " (number->string (bvar->major var)) "));\n" ;now R0 = env[major]
                     (make-spaces level) "MOV(R0, INDD(R0, " (number->string (bvar->minor var)) "));\n" ;now R0 = env[major][minor]
                     (make-spaces level) "MOV(R0, IND(R0));\n" ;unbox
                     (make-spaces level) "/* -------------------------------- */\n")))))
                     
(define box-set-codegen
  (lambda (box-set env-size param-size const-table fvar-table level)
    (let ((var (cadr box-set))
         (expr (caddr box-set)))
      (if (equal? 'pvar (car var))
         (string-append (make-spaces level) "/* CODEGEN(" (format "~s" box-set) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (codegen expr env-size param-size const-table fvar-table (+ 1 level)) ;computing the expr of the box-set
                     (make-spaces level) "MOV(R1, IMM(" (number->string (caddr var)) "));\n" ;now R1 is the minor of the pvar
                     (make-spaces level) "ADD(R1, 2);\n" ;updating R1 for fparg
                     (make-spaces level) "MOV(R1, FPARG(R1));\n" ;this is the box
                     (make-spaces level) "MOV(IND(R1), R0);\n" ;doing the box-set
                     (make-spaces level) "MOV(R0, IMM(SOB_VOID));\n"
                     (make-spaces level) "/* -------------------------------- */\n")
         (string-append (make-spaces level) "/* CODEGEN(" (format "~s" box-set) ") */\n"
                     (make-spaces level) "/* -------------------------------- */\n"
                     (codegen expr env-size param-size const-table fvar-table (+ 1 level)) ;computing the expr of the set
                     (make-spaces level) "MOV(R1, FPARG(0));\n" ;now R1 contains env
                     (make-spaces level) "MOV(R1, INDD(R1, " (number->string (bvar->major var)) "));\n" ;now R1 = env[major]
                     (make-spaces level) "MOV(R1, INDD(R1, " (number->string (bvar->minor var)) "));\n" ;now R1 = env[major][minor] ;this is the boxed
                     (make-spaces level) "MOV(IND(R1), R0);\n" ;doing the box-set 
                     (make-spaces level) "MOV(R0, IMM(SOB_VOID));\n"
                     (make-spaces level) "/* -------------------------------- */\n")))))
                     
(define codegen
  (lambda (pe env-size param-size const-table fvar-table level)
    (cond ((eq? (car pe) 'const) (const-codegen pe const-table level))
          ((eq? (car pe) 'if3) (if3-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'or) (or-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'seq) (seq-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'applic) (applic-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'tc-applic) (tc-applic-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'lambda-simple) (lambda-simple-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'lambda-var) (lambda-var-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'lambda-opt) (lambda-opt-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'pvar) (pvar-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'bvar) (bvar-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'fvar) (fvar-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'def) (def-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'set) (set-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'box) (box-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'box-get) (box-get-codegen pe env-size param-size const-table fvar-table level))
          ((eq? (car pe) 'box-set) (box-set-codegen pe env-size param-size const-table fvar-table level))
          ))) 
    ;TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

(define file->string
  (lambda (in-file)
    (let ([in-port (open-input-file in-file)])
      (letrec ([run (lambda ()
                      (let ([ch (read-char in-port)])
                        (if (eof-object? ch)
                            (begin (close-input-port in-port) '())
                            (cons ch (run)))))])
        (list->string (run))))))

(define global-fvars
   '(append apply < = > + / * - boolean? car cdr char->integer char? cons denominator
     eq? integer? integer->char list make-string make-vector map not null? number? numerator pair?
     procedure? rational? remainder set-car! set-cdr! string-length string-ref string-set! string->symbol
     string? symbol? symbol->string vector vector-length vector-ref vector-set! vector? zero?))

(define fvar-filter-predict
  (lambda (fvar)
       (not (member fvar global-fvars))))

(define const-table->symbol-list
  (lambda (const-table intial_addr)
     (letrec ((list-of-pointer-to-syms (map (lambda(e) (car (cdaddr e))) (filter (lambda (c-table-entry)
                                                   (= (caaddr c-table-entry) T_SYMBOL))
                                             const-table)))
;if next address is 0 we will now we are at the end of the symbol list
              (build-loop (lambda (pointer-lst symbol-lst next_sym_addr)
                              (if (null? pointer-lst)
                                   symbol-lst
                                   (build-loop (cdr pointer-lst)
                                               (append symbol-lst (list (car pointer-lst) next_sym_addr))
                                               (+ 2 next_sym_addr))))))
       (string-append "long SYMBOL_LIST[" (number->string (* 2 (length list-of-pointer-to-syms))) "] = {"  (fold-left string-append "" (map (lambda(x) (string-append (number->string x) ",")) (reverse (cdr (reverse (build-loop list-of-pointer-to-syms '() (+ 2 intial_addr))))))) "0};"))))

(define const-table->symbol-list-size
  (lambda (const-table)
     (letrec ((list-of-pointer-to-syms (map car (filter (lambda (c-table-entry)
                                                   (= (caaddr c-table-entry) T_SYMBOL))
                                             const-table))))
         (* 2 (length list-of-pointer-to-syms)))))
         

(define build-fvar-table
  (lambda (expr-list intial_addr)
      (letrec ((fvars (append global-fvars (remove-list-duplicates (filter fvar-filter-predict (expr->fvars expr-list)))))
               (build-loop (lambda (fvar-list fvar-table available_addr)
                                (if (null? fvar-list)
                                    fvar-table
                                    (build-loop (cdr fvar-list)
                                                (append fvar-table (list (list available_addr (car fvar-list) -1))) ;-1 is just an unimportant temp val
                                                (+ available_addr 1))))))
          (build-loop fvars '() intial_addr))))

(define fvar->addr
  (lambda (fvar fvar-table)
     (if (null? fvar-table)
         -1
         (if (eq? fvar (cadr (car fvar-table)))
             (car (car fvar-table))
             (fvar->addr fvar (cdr fvar-table))))))


(define fvar-table->size
  (lambda (fvar-table)
      (length fvar-table)))

(define fvar-table->array
  (lambda (fvar-table)
    (let ((intial-str (fold-left string-append "" (map (lambda (x) "-1, ") fvar-table))))
      (string-append "long FVARS["
                     (number->string (fvar-table->size fvar-table))
                      "] = "
                      "{"
                       (substring intial-str 0 (- (string-length intial-str) 2))
                      "};"))))


(define expr->fvars
  (lambda (expr)
    (cond ((null? expr) '())
          ((not (list? expr)) '())
          ((eq? (car expr) 'fvar) (list (cadr expr)))
          (else
             (letrec ((loop (lambda (lst)
                               (if (null? lst)
                                   '()
                                   (append (expr->fvars (car lst))
                                           (loop (cdr lst)))))))
                  (loop expr))))))

(define build-const-table
  (lambda (expr-list)
     (letrec ((consts (append (list (void) '() #f #t "start" 'start) (remove-list-duplicates (filter const-filter-predict (expr->consts expr-list)))))
              (build-loop (lambda (const-list const-table available_addr)
                               (if (null? const-list)
                                    const-table
                                    (let ((cisc_repr (const->cisc_repr (car const-list) const-table)))
                                       (build-loop (cdr const-list)
                                                   (append const-table (list (list available_addr (car const-list) cisc_repr)))
                                                   (+ available_addr (length cisc_repr))))))))
        (build-loop consts '() 1))))

(define flatten
  (lambda (lst)
     (fold-left append '() lst)))

(define const-table->size
 (lambda (const-table)
   (length (flatten (map (lambda (c-entry) (caddr c-entry)) const-table)))))

(define const-table->array
  (lambda (const-table)
    (let* ((array-needed-only (flatten (map (lambda (c-entry) (caddr c-entry)) const-table)))
            (intial-str (fold-left string-append ""
                                (map (lambda (s) (string-append s ", "))
                                     (map (lambda (e) (if (symbol? e) (symbol->string e) (number->string e)))    
                                          array-needed-only)))))
       (string-append "long CONSTANTS[" 
                      (number->string (length array-needed-only))
                      "] = "
                      "{"
                       (substring intial-str 0 (- (string-length intial-str) 2))
                       "};"
       ))))



(define const->addr
  (lambda (const const-table)
     (if (null? const-table)
         -1
         (if (equal? const (cadr (car const-table)))
             (car (car const-table))
             (const->addr const (cdr const-table))))))

(define T_VOID 937610)
(define T_NIL 722689)
(define T_BOOL 741553)
(define T_CHAR 181048)
(define T_INTEGER 945311)
(define T_FRACTION 5868289)
(define T_STRING 799345)
(define T_SYMBOL 368031)
(define T_PAIR 885397)
(define T_VECTOR 335728)
(define T_CLOSURE 276405)

(define const->cisc_repr
  (lambda (const old-const-table)
    (cond ((eq? const (void)) `(,T_VOID))
          ((null? const) `(,T_NIL))
          ((and (boolean? const) (not const)) `(,T_BOOL 0))
          ((and (boolean? const) const) `(,T_BOOL 1))
          ((integer? const) `(,T_INTEGER ,const))
          ((ratnum? const) `(,T_FRACTION ,(numerator const) ,(denominator const)))
          ((char? const) `(,T_CHAR ,(char->integer const)))
          ((string? const) `(,T_STRING ,(string-length const) ,@(map char->integer (string->list const))))
          ((symbol? const) `(,T_SYMBOL ,(const->addr (symbol->string const) old-const-table)))
          ((vector? const) `(,T_VECTOR ,(vector-length const) ,@(map (lambda (c) (const->addr c old-const-table)) (vector->list const))))
          ((pair? const) `(,T_PAIR ,(const->addr (car const) old-const-table) ,(const->addr (cdr const) old-const-table))))))
                                          

(define const-filter-predict
  (lambda (const)
       (and (not (null? const)) (not (boolean? const)) (not (eq? const (void))))))

(define remove-list-duplicates
  (lambda (l)
     (letrec ((loop (lambda (old_lst new_lst)
                         (if (null? old_lst)
                              new_lst
                              (if (member (car old_lst) new_lst)
                                  (loop (cdr old_lst) new_lst)
                                  (loop (cdr old_lst) (append new_lst (list (car old_lst)))))))))
       (loop l '()))))
          

(define list->all_consts
   (lambda (l)
       (if (null? l)
           (list '())
           (append (expand-const-value (car l)) (append (list->all_consts (cdr l)) (list l))))))

(define vector_list->all_consts
   (lambda (l)
     (if (null? l)
         '()
         (append (expand-const-value (car l)) (vector_list->all_consts (cdr l))))))

(define expr->consts
  (lambda (expr)
    (cond ((null? expr) '())
          ((not (list? expr)) '())
          ((eq? (car expr) 'const) (expand-const-value (const->value expr)))
          (else
             (letrec ((loop (lambda (lst)
                               (if (null? lst)
                                   '()
                                   (append (expr->consts (car lst))
                                           (loop (cdr lst)))))))
                  (loop expr))))))

(define expand-const-value
    (lambda (v)
        (cond ((list? v) (list->all_consts v))
              ((vector? v) (append (vector_list->all_consts (vector->list v)) (list v)))
              ((pair? v) (append (expand-const-value (car v)) (expand-const-value (cdr v)) (list v)))
              ((symbol? v) (append (list (symbol->string v)) (list v))) 
              (else (list v)))))

(define const->value
  (lambda (const)
     (cadr const)))


;HW3


(define var-list->alread_parsed_set
 (lambda (lst)
   (map (lambda(elm) `(set ,(car elm) ,(cadr elm))) lst)))

(define create-cons-false-list-of-size
    (lambda (n)
         (letrec ((loop (lambda (lst n)
                          (if (= n 0)
                              lst
                              (loop (append lst (list (list 'const #f))) (- n 1))))))
               (loop '() n))))

(define exprs->ls_seq (lambda (seqq)
  (if (= 1 (length seqq)) (car seqq) `(seq ,seqq))))

(define create-parsed-letrec
  (lambda (var-list body-exprs-list)
    `(applic 
       (lambda-simple
         ,(map cadr (var-list->vars var-list))
         (seq (,@(var-list->alread_parsed_set var-list) ,@body-exprs-list)))
        ,(create-cons-false-list-of-size (length (var-list->vars var-list))))))

(define lambda->list_parms
 (lambda (l)
    (if (eq? (car l) 'lambda-opt)
        (list (cadr l) (caddr l))
        (list (cadr l)))))

(define lambda->body
 (lambda (l)
    (if (eq? (car l) 'lambda-opt)
        (car (cdddr l))
        (car (cddr l)))))

(define lambda-body->define_and_exprs
 (lambda (parsed-exprs ret-defs_and_exprs)
     (if (null? parsed-exprs)
         (ret-defs_and_exprs '() '())
         (lambda-body->define_and_exprs (cdr parsed-exprs)
              (lambda (defs exprs)
                   (cond ((eq? (caar parsed-exprs) 'def)
                          (ret-defs_and_exprs (cons (car parsed-exprs) defs) exprs))
                         ((eq? (caar parsed-exprs) 'seq)
                          (lambda-body->define_and_exprs (cadar parsed-exprs)
                               (lambda (defs_1 exprs_1)
                                       (ret-defs_and_exprs (append defs_1 defs) (append exprs_1 exprs)))))
                         (else
                              (ret-defs_and_exprs defs (cons (car parsed-exprs) exprs)))))))))
        
(define lambda->no-nested-defines
   (lambda (l)
       (lambda-body->define_and_exprs (list (lambda->body l))
                                      (lambda (defs exprs)
                                          (let* ((lambda-type (car l))
                                                 (lambda-parms (lambda->list_parms l))
                                                 (defs-elimi (map eliminate-nested-defines defs))
                                                 (exprs-elimi (map eliminate-nested-defines exprs))
                                                 (letrec-var-list (map cdr defs-elimi))
                                                 (the_letrec (create-parsed-letrec letrec-var-list exprs-elimi)))
                                             (if (null? defs)
                                                 `(,lambda-type ,@lambda-parms ,(exprs->ls_seq exprs-elimi))
                                                 `(,lambda-type ,@lambda-parms ,the_letrec)))))))

(define eliminate-nested-defines
 (lambda (expr)
    (cond ((null? expr) expr)
          ((not (list? expr)) expr)
          ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
           (lambda->no-nested-defines expr))
          (else (map eliminate-nested-defines expr)))))

(define applic->func
   (lambda (applic)
      (cadr applic)))

(define applic->args
   (lambda (applic)
      (caddr applic)))

(define is-lambda-simple-with-no-parms
  (lambda (l)
      (and (list? l) (not (null? l)) (eq? (car l) 'lambda-simple) (null? (cadr l)))))

(define applic->no-lambda-nil   
  (lambda (applic)
      (let* ((func (applic->func applic))
             (args (applic->args applic))
             (func-elimi (remove-applic-lambda-nil func))
             (args-elimi (remove-applic-lambda-nil args)))
         (if (and (is-lambda-simple-with-no-parms func) (null? args))
             (lambda->body func-elimi)
             `(applic ,func-elimi ,args-elimi)))))

(define remove-applic-lambda-nil
 (lambda (expr)
    (cond ((null? expr) expr)
          ((not (list? expr)) expr)
          ((eq? (car expr) 'applic)
           (applic->no-lambda-nil expr))
          (else (map remove-applic-lambda-nil expr)))))

(define get_all_lambdas_in_expr_whithout_lambdas_in_lambdas
  (lambda (expr)
     (if (or (null? expr) (not (list? expr)))
          '()
          (if (or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
              (list expr)
              (letrec ((loop (lambda (expr)
                               (append (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas (car expr))
                                       (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas (cdr expr))))))
                   (loop expr))))))

(define get_all_lambdas_in_expr_whithout_lambdas_in_lambdas
  (lambda (expr)
     (if (or (null? expr) (not (list? expr)))
          '()
          (if (or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
              (list expr)
              (letrec ((loop (lambda (expr)
                               (append (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas (car expr))
                                       (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas (cdr expr))))))
                   (loop expr))))))

(define is-lambada-whitout-p-as-parm
  (lambda (l p)
      (and (list? l) (not (null? l)) (or (eq? (car l) 'lambda-simple) (eq? (car l) 'lambda-opt) (eq? (car l) 'lambda-var))
           (cond ((eq? (car l) 'lambda-simple)
                  (not (member p (cadr l))))
                 ((eq? (car l) 'lambda-var)
                  (not (equal? (cadr l) p)))
                 (else (and (not (member p (cadr l))) (not (equal? (caddr l) p))))))))

(define parm_accour_in_expr_which_is_not_lambda
  (lambda (expr p)
        (cond ((or (null? expr) (not (list? expr)))
               #f)
              ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
               #f)
              ((equal? expr `(var ,p))
               #t)
              (else (ormap (lambda (e) (parm_accour_in_expr_which_is_not_lambda e p)) expr)))))
            

(define param-accour-in-lambda
 (lambda (m-l p)
    (let* ((m-l-body (lambda->body m-l))
           (lambads-in-body (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas m-l-body))
           (lambads-in-body-whitout-p-as-parm (filter (lambda (e) (is-lambada-whitout-p-as-parm e p)) lambads-in-body)))
       (if (null? lambads-in-body-whitout-p-as-parm)
           (parm_accour_in_expr_which_is_not_lambda m-l-body p)
           (or (parm_accour_in_expr_which_is_not_lambda m-l-body p) (ormap (lambda (e) (param-accour-in-lambda e p)) lambads-in-body-whitout-p-as-parm))))))
           
              

(define param-bound-in-lambda
 (lambda (m-l p)
    (let* ((m-l-body (lambda->body m-l))
           (lambads-in-body (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas m-l-body))
           (lambads-in-body-whitout-p-as-parm (filter (lambda (e) (is-lambada-whitout-p-as-parm e p)) lambads-in-body)))
       (if (null? lambads-in-body-whitout-p-as-parm)
           #f
           (ormap (lambda (e) (param-accour-in-lambda e p)) lambads-in-body-whitout-p-as-parm)))))

(define parm_set_in_expr_which_is_not_lambda
  (lambda (expr p)
        (cond ((or (null? expr) (not (list? expr)))
               #f)
              ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
               #f)
              ((and (eq? (car expr) 'set) (equal? (cadr expr) `(var ,p)))
               #t)
              (else (ormap (lambda (e) (parm_set_in_expr_which_is_not_lambda e p)) expr)))))

(define param-set-in-lambda
 (lambda (m-l p)
    (let* ((m-l-body (lambda->body m-l))
           (lambads-in-body (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas m-l-body))
           (lambads-in-body-whitout-p-as-parm (filter (lambda (e) (is-lambada-whitout-p-as-parm e p)) lambads-in-body)))
       (if (null? lambads-in-body-whitout-p-as-parm)
           (parm_set_in_expr_which_is_not_lambda m-l-body p)
           (or (parm_set_in_expr_which_is_not_lambda m-l-body p) (ormap (lambda (e) (param-set-in-lambda e p)) lambads-in-body-whitout-p-as-parm))))))

(define parm_get_in_expr_which_is_not_lambda
  (lambda (expr p)
        (cond ((or (null? expr) (not (list? expr)))
               #f)
              ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
               #f)
              ((eq? (car expr) 'set)
               (parm_get_in_expr_which_is_not_lambda (caddr expr) p))
              ((equal? expr `(var ,p))
               #t)
              (else (ormap (lambda (e) (parm_get_in_expr_which_is_not_lambda e p)) expr)))))

(define param-get-in-lambda
 (lambda (m-l p)
    (let* ((m-l-body (lambda->body m-l))
           (lambads-in-body (get_all_lambdas_in_expr_whithout_lambdas_in_lambdas m-l-body))
           (lambads-in-body-whitout-p-as-parm (filter (lambda (e) (is-lambada-whitout-p-as-parm e p)) lambads-in-body)))
       (if (null? lambads-in-body-whitout-p-as-parm)
           (parm_get_in_expr_which_is_not_lambda m-l-body p)
           (or (parm_get_in_expr_which_is_not_lambda m-l-body p) (ormap (lambda (e) (param-get-in-lambda e p)) lambads-in-body-whitout-p-as-parm))))))

(define problem-parm?
 (lambda (l p)
   (and (param-bound-in-lambda l p) (param-set-in-lambda l p) (param-get-in-lambda l p))))

(define replace-get-and-set-with-box
  (lambda (expr p)
      (cond ((or (null? expr) (not (list? expr)))
             expr)
            ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
             (if (is-lambada-whitout-p-as-parm expr p)
                 (let* ((lambda-type (car expr))
                        (lambda-parms (lambda->list_parms expr))
                        (lambda-body (lambda->body expr))
                        (lambda-body-rep (replace-get-and-set-with-box lambda-body p)))
                     `(,lambda-type ,@lambda-parms ,lambda-body-rep))
                 expr))
            ((and (eq? (car expr) 'set) (equal? (cadr expr) `(var ,p)))
              `(box-set ,(cadr expr) ,(replace-get-and-set-with-box (caddr expr) p)))
           ((equal? expr `(var ,p))
               `(box-get (var ,p)))
           (else (map (lambda (e) (replace-get-and-set-with-box e p)) expr)))))

(define lambda->parms_good_list
 (lambda (l)
    (cond ((eq? (car l) 'lambda-opt)
           (append (cadr l) (list (caddr l))))
          ((eq? (car l) 'lambda-simple)
           (cadr l))
          (else (list (cadr l))))))

(define param->set-parm
  (lambda (p)
    `(set (var ,p) (box (var ,p)))))

(define box-lambda
  (lambda (l)
      (let* ((lambda-type (car l))
             (lambda-parms (lambda->list_parms l))
             (lambda-parms-in-good-list-format  (lambda->parms_good_list l))
             (lambda-body (lambda->body l))
             (lambda-body-boxed (map box-set lambda-body))
             (lambda-problem-parms (filter (lambda (p) (problem-parm? l p)) lambda-parms-in-good-list-format)))
        (if (null? lambda-problem-parms)
            `(,lambda-type ,@lambda-parms ,lambda-body-boxed)
            (letrec ((loop (lambda (p_s)
                                (if (null? p_s)
                                    (void)
                                    (begin 
                                      (set! lambda-body-boxed (map (lambda (e) (replace-get-and-set-with-box e (car p_s))) lambda-body-boxed))
                                      (loop (cdr p_s)))))))
                  (loop lambda-problem-parms)
                  (if (eq? (car lambda-body-boxed) 'seq)
                      (set! lambda-body-boxed `(seq (,@(map param->set-parm lambda-problem-parms) ,@(cadr lambda-body-boxed))))
                      (set! lambda-body-boxed `(seq (,@(map param->set-parm lambda-problem-parms) ,lambda-body-boxed))))
                  `(,lambda-type ,@lambda-parms ,lambda-body-boxed))))))
            

(define box-set
 (lambda (expr)
    (cond ((null? expr) expr)
          ((not (list? expr)) expr)
          ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
           (box-lambda expr))
          (else (map box-set expr)))))

(define get-minor-index
 (lambda (elm lst)
     (letrec ((loop (lambda (lst n)
                      (cond ((null? lst) -1)
                            ((equal? (car lst) elm) n)
                            (else (loop (cdr lst) (+ n 1)))))))
         (loop lst 0))))

(define get-major-and-minor-index
   (lambda (elm lst)
    (letrec ((loop (lambda (lst n)
                       (if (null? lst)
                           -1
                            (let ((minor (get-minor-index elm (car lst))))
                               (if (= minor -1)
                                   (loop (cdr lst) (+ n 1))
                                   (list n minor)))))))
       (loop lst 0))))

(define get-var-kind
 (lambda (var simulated_scope params)
   (cond ((member var params)
          `(pvar ,var ,(get-minor-index var params)))
         ((ormap (lambda (p_s) (member var p_s)) simulated_scope)
          `(bvar ,var ,@(get-major-and-minor-index var simulated_scope)))
         (else
           `(fvar ,var)))))

(define analyze
 (lambda (ast simulated_scope params)
    (cond ((null? ast) ast)
          ((not (list? ast)) ast)
          ((or (eq? (car ast) 'lambda-simple) (eq? (car ast) 'lambda-opt) (eq? (car ast) 'lambda-var))
           (map (lambda (child) (analyze child (append (list params) simulated_scope) (lambda->parms_good_list ast))) ast))
          ((eq? (car ast) 'var)
           (get-var-kind (cadr ast) simulated_scope params))
          (else (map (lambda (child) (analyze child simulated_scope params)) ast)))))

(define pe->lex-pe
  (lambda (expr)
     (analyze expr (list) (list))))

(define list->last
 (lambda (lst)
     (car (reverse lst))))

;(map (lambda (child) (annotate child #f)) (cdr expr))
(define list->all_but_last
 (lambda (lst)
   (reverse (cdr (reverse lst)))))

(define if3->test
  (lambda (if3)
      (cadr if3)))

(define if3->dit
  (lambda (if3)
      (caddr if3)))

(define if3->dif
  (lambda (if3)
      (cadddr if3)))

(define annotate
 (lambda (expr tp?)
    (cond ((null? expr) expr)
          ((not (list? expr)) expr)
          ((eq? (car expr) 'applic)
           (if tp?
               `(tc-applic ,@(map (lambda (child) (annotate child #f)) (cdr expr)))
               `(applic ,@(map (lambda (child) (annotate child #f)) (cdr expr)))))
          ((eq? (car expr) 'or)
           `(or (,@(map (lambda (child) (annotate child #f)) (list->all_but_last (cadr expr))) ,(annotate (list->last (cadr expr)) tp?))))
          ((eq? (car expr) 'if3)
           `(if3 ,(annotate (if3->test expr) #f) ,(annotate (if3->dit expr) tp?) ,(annotate (if3->dif expr) tp?)))
          ((eq? (car expr) 'def)
           `(def ,(cadr expr) ,(annotate (caddr expr) #f)))
          ((or (eq? (car expr) 'lambda-simple) (eq? (car expr) 'lambda-opt) (eq? (car expr) 'lambda-var))
           (map (lambda (child) (annotate child #t)) expr))
          ((eq? (car expr) 'seq)
           `(seq (,@(map (lambda (child) (annotate child #f)) (list->all_but_last (cadr expr))) ,(annotate (list->last (cadr expr)) tp?))))
          ((eq? (car expr) 'set)
           `(set ,(cadr expr) ,(annotate (caddr expr) #f)))
          ((eq? (car expr) 'box-set)
           `(box-set ,(cadr expr) ,(annotate (caddr expr) #f)))
          (else  (map (lambda (child) (annotate child tp?)) expr)))))

(define annotate-tc
  (lambda (expr)
      (annotate expr #f)))
             
          
                                                 
                         
         
         


;HW2

(define with
  (lambda (s f) 
    (apply f s)))

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define varible?
 (lambda (e)
   (and (symbol? e) (not (member e *reserved-words*)))))

(define not_in_reserved?
 (lambda (e)
  (not (member e *reserved-words*))))

(define constant? (lambda (e)
       (or (null? e) (vector? e) (boolean? e) (char? e) (number? e)
           (string? e) (eq? e (void)))))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((varible? argl) (ret-var argl))
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s)))
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
					(lambda (var) (ret-opt `(,(car argl)) var)))))))
(define exprs->begin (lambda (seqq)
  (if (= 1 (length seqq)) (car seqq) `(begin ,@seqq))))

(define are-all-unique? 
  (lambda (v)
    (if (pair? v)
        (and (not (member (car v) (cdr v)))
             (are-all-unique? (cdr v)))
        #t)))

(define are-all-unique-lambda-args?
  (lambda (argl)
       (cond
             ((null? argl) #t)
             ((varible? argl) #t)
             ((list? argl) (are-all-unique? argl))
             (else (are-all-unique? (cons argl '()))))))

(define var-list->vars
 (lambda (lst)
   (map (lambda(elm) (car elm)) lst)))

(define var-list->exprs
 (lambda (lst)
   (map (lambda(elm) (cadr elm)) lst)))

(define var-list->set
 (lambda (lst)
   (map (lambda(elm) `(set! ,(car elm) ,(cadr elm))) lst)))

(define var-list?
  (lambda (lst)
    (and (list? lst) (andmap (lambda(elm) (and (list? elm) (= 2 (length elm))
         (varible? (car elm)))) lst)) (are-all-unique? (var-list->vars lst))))

(define list_of_length_at_least_2?
  (lambda (lst)
       (and (list? lst) (>= (length lst) 2))))

(define create-false-list-of-size
    (lambda (n)
         (letrec ((loop (lambda (lst n)
                          (if (= n 0)
                              lst
                              (loop (append lst (list #f)) (- n 1))))))
               (loop '() n))))


(define parse
  (let ((run
	 (compose-patterns

	  (pattern-rule
	   (? 'c constant?)
	   (lambda (c) `(const ,c)))

          (pattern-rule
           `(quote ,(? 'q))
            (lambda (q) `(const ,q)))

	  (pattern-rule
	   (? 'v varible?)
	   (lambda (v) `(var ,v)))

          (pattern-rule
	   `(if ,(? 'test) ,(? 'then))
	   (lambda (test then)
	     `(if3 ,(parse test) ,(parse then) (const ,(void)))))

	  (pattern-rule
	   `(if ,(? 'test) ,(? 'then) ,(? 'else))
	   (lambda (test then else)
	     `(if3 ,(parse test) ,(parse then) ,(parse else))))

	  (pattern-rule
           `(or)
            (lambda() (parse '#f)))

         (pattern-rule
           `(or ,(? 'expr))
            (lambda (expr) (parse expr)))

         (pattern-rule
           `(or ,(? 'first) . ,(? 'rest))
             (lambda (first rest) 
               `(or (,(parse first) . ,(map parse rest)))))

         (pattern-rule
           `(begin ,(? 'expr))
            (lambda (expr) (parse expr)))


	 (pattern-rule
	   `(begin . ,(? 'expr))
	    (lambda (expr)
	     (letrec ((loop (lambda (cur_expr)
		                (if (null? cur_expr)
			        (list)
			       (if (and (list? (car cur_expr)) (equal? 'begin (caar cur_expr)))
			 	   (if (or (null? (cdar cur_expr)) (null? (cddar cur_expr)))
			 		`(,(parse (car cur_expr)) . ,(loop (cdr cur_expr)))
			 		(append (cadr (parse (car cur_expr))) (loop (cdr cur_expr))))
			 	   `(,(parse (car cur_expr)) . ,(loop (cdr cur_expr))))))))
		(if (null? expr) 
                    (parse (void))
		    `(seq ,(loop expr))))))

          (pattern-rule
           `(lambda ,(? 'argl are-all-unique-lambda-args?) ,(? 'expr) . ,(? 'exprs list?))
            (lambda (argl expr exprs) (identify-lambda argl
                                    (lambda(s) `(lambda-simple ,s ,(parse (exprs->begin (cons expr exprs)))))
                                      (lambda(s opt) `(lambda-opt ,s ,opt ,(parse (exprs->begin (cons expr exprs)))))
                                      (lambda(s) `(lambda-var ,s ,(parse (exprs->begin (cons expr exprs)))))
                                  )))

	  (pattern-rule
	   `(define ,(? 'var varible?) ,(? 'expr))
	   (lambda (var expr)
	     `(def ,(parse var) ,(parse expr))))

	  (pattern-rule
	   `(define ,(? 'var varible?) ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (var expr exprs)
	     `(def ,(parse var) ,(parse (exprs->begin (cons expr exprs))))))

	  (pattern-rule
	   `(define (,(? 'var varible?) . ,(? 'argl)) ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (var argl expr exprs)
	     (parse `(define ,var (lambda ,argl ,@(cons expr exprs))))))

         (pattern-rule
           `(,(? 'first-expr not_in_reserved?) . ,(? 'rest-exprs))
            (lambda (first-expr rest-exprs)
              `(applic ,(parse first-expr) ,(map parse rest-exprs))))

         (pattern-rule
          `(let () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
               (parse `((lambda () ,@(cons expr exprs))))))

         (pattern-rule
          `(let ,(? 'var-list var-list?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (var-list expr exprs)
            (parse `((lambda ,(var-list->vars var-list) ,@(cons expr exprs)) ,@(var-list->exprs var-list)))))

         (pattern-rule
          `(let* () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
               (parse `((lambda () ,@(cons expr exprs))))))

         (pattern-rule
          `(let* ((,(? 'key) ,(? 'value))) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (key value expr exprs)
               (parse `((lambda ,(list key) ,@(cons expr exprs)) ,value))))

         (pattern-rule
          `(let* ((,(? 'key) ,(? 'value)) . ,(? 'other-bindings)) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (key value other-bindings expr exprs)
               (parse `(let ((,key ,value)) ,(list 'let* other-bindings (exprs->begin (cons expr exprs)))))))


         (pattern-rule
           `(and)
            (lambda() (parse '#t)))

         (pattern-rule
           `(and ,(? 'expr))
            (lambda (expr) (parse expr)))

         (pattern-rule
           `(and ,(? 'first) . ,(? 'rest))
            (lambda (first rest) (parse `(if ,first (and ,@rest) ,#f))))

         (pattern-rule
           `(cond (else ,(? 'expr)))
            (lambda(expr) (parse expr)))

         (pattern-rule
           `(cond (else ,(? 'first) . ,(? 'rest)))
            (lambda(first rest) (parse (exprs->begin (cons first rest)))))

         (pattern-rule
           `(cond ,(? 'first list_of_length_at_least_2?) . ,(? 'rest))
            (lambda (first rest) (parse `(if ,(car first) ,(exprs->begin (cdr first)) ,(if (null? rest) (void) `(cond ,@rest))))))

          (pattern-rule
           (list 'quasiquote (? 'expr))
           (lambda (expr) (parse (expand-qq expr))))

          (pattern-rule
           `(set! ,(? 'expr1) ,(? 'expr2))
             (lambda (expr1 expr2) `(set ,(parse expr1) ,(parse expr2))))

         (pattern-rule
          `(letrec () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
               (parse `((lambda () ((lambda () ,@(cons expr exprs))))))))

         (pattern-rule
          `(letrec ,(? 'var-list var-list?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (var-list expr exprs)
            (parse `((lambda ,(var-list->vars var-list)
                    (begin ,@(var-list->set var-list) ((lambda () ,@(cons expr exprs))))) ,@(create-false-list-of-size (length (var-list->vars var-list)))))))



        )))
    (lambda (sexpr)
      (run sexpr
	   (lambda ()
	     (error 'parse
		    (format "The value ~s is not an sexpr!!!" sexpr)))))))

;HW1

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

(define <sexpr-comment-infix>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <comment-infix>
  (disj <line-comment>
	<sexpr-comment-infix>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define <skip-infix>
  (disj <comment-infix>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))
(define ^<skipped-infix*> (^^<wrapped> (star <skip-infix>)))



(define <Boolean>
  (new (*parser (char #\#))
       (*parser (char-ci #\f))
       (*pack (lambda (bool) #f))
       (*parser (char-ci #\t))
       (*pack (lambda (bool) #t))
       (*disj 2)
       (*caten 2)
       (*pack-with (lambda (hash bool) bool))
       done))

(define <CharPrefix>
  (new (*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
       done))

(define <VisibleSimpleChar>
  (new (*parser <any-char>)
       (*guard (lambda (c) (> (char->integer c) 32)))
       done))

(define <NamedChar>
  (new (*parser (word-ci "lambda"))
       (*pack (lambda (c) (integer->char 955)))
       (*parser (word-ci "newline"))
       (*pack (lambda (c) #\newline))
       (*parser (word-ci "nul"))
       (*pack (lambda (c) #\nul))
       (*parser (word-ci "page"))
       (*pack (lambda (c) #\page))
       (*parser (word-ci "return"))
       (*pack (lambda (c) #\return))
       (*parser (word-ci "space"))
       (*pack (lambda (c) #\space))
       (*parser (word-ci "tab"))
       (*pack (lambda (c) #\tab))
       (*disj 7)
       done))

(define hexCharsToDec
 (lambda (lst_hex_chars)
  (letrec ((rev-lst (reverse lst_hex_chars))
           (convertor (lambda (lst power res)
                         (if (null? lst)
                             res
                             (convertor (cdr lst) (+ power 1) (+ res (* (car lst) (expt 16 power)))))
                       )))
           (convertor rev-lst 0 0)
    )))

(define <HexChar>
 (new (*parser (range #\0 #\9))
      (*pack (lambda (ch) (- (char->integer ch) (char->integer #\0))))
      (*parser (range #\a #\f))
      (*pack (lambda (ch) (+ 10 (- (char->integer ch) (char->integer #\a)))))
      (*parser (range #\A #\F))
      (*pack (lambda (ch) (+ 10 (- (char->integer ch) (char->integer #\A)))))
      (*disj 3)
      done))

(define <HexUnicodeChar>
 (new (*parser (char-ci #\x))
      (*parser <HexChar>) *plus
      (*guard (lambda (lst_hex_chars) (< (hexCharsToDec lst_hex_chars) 1114112)))
      (*pack (lambda (lst_hex_chars) (integer->char (hexCharsToDec lst_hex_chars))))
      (*caten 2)
      (*pack-with (lambda (x c) c))
      done))

(define <Char>
  (new (*parser <CharPrefix>)


       (*parser <NamedChar>)

       (*parser <HexUnicodeChar>)

       (*parser <VisibleSimpleChar>)
       (*parser <HexChar>) *plus
       *not-followed-by


       (*disj 3)
       (*caten 2)
       (*pack-with (lambda (hash c) c))
       done))

(define <Natural>
(new (*parser (range #\0 #\9)) *plus
     (*pack (lambda (digits) (string->number (list->string digits))))
     done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with (lambda (plus_sign n) n))
       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with (lambda (minus_sign n) (- n)))
       (*parser <Natural>)
       (*disj 3)
       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (natural) (not (zero? natural))))
       (*caten 3)
       (*pack-with (lambda (first_num slash last_num) (/ first_num last_num)))
       done))

(define <Number>
 (new (*parser <Fraction>)
      (*parser <Integer>)
      (*disj 2)
      done))


(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*guard (lambda (c) (not (char=? c #\\))))
       done))

(define <StringMetaChar>
    (new (*parser (word "\\\\"))
	 (*pack (lambda (_) #\\))

         (*parser (word "\\\""))
	 (*pack (lambda (_) #\"));"

         (*parser (word "\\t"))
	 (*pack (lambda (_) #\tab))

         (*parser (word "\\f"))
	 (*pack (lambda (_) #\page))

         (*parser (word "\\n"))
	 (*pack (lambda (_) #\newline))

         (*parser (word "\\r"))
	 (*pack (lambda (_) #\return))

         (*disj 6)
         done))

(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser (char-ci #\x))
       (*parser <HexChar>) *plus
       (*guard (lambda (lst_hex_chars) (< (hexCharsToDec lst_hex_chars) 1114112)))
       (*pack (lambda (lst_hex_chars) (integer->char (hexCharsToDec lst_hex_chars))))
       (*parser (char (integer->char 59)))
       (*caten 4)
       (*pack-with (lambda (slash x c sign) c))
       done))

(define <StringChar>
  (new (*parser <StringMetaChar>)
       (*parser <StringHexChar>)
       (*parser <StringLiteralChar>)

       (*parser (char #\"));"

       *diff

       (*disj 3)
       done))

(define <String>
  (new (*parser (char #\"));"
       (*parser <StringChar>) *star
       (*parser (char #\"));"
       (*caten 3)
       (*pack-with (lambda (aps1 chars aps2) (list->string chars)))
       done))

(define <SymbolChar>
 (new (*parser (range #\0 #\9))
      (*parser (range #\a #\z))
      (*parser (range #\A #\Z))
      (*pack (lambda (c) (integer->char (+ (char->integer c) 32))))
      (*parser (char #\!))
      (*parser (char #\$))
      (*parser (char #\^))
      (*parser (char #\*))
      (*parser (char #\-))
      (*parser (char #\_))
      (*parser (char #\=))
      (*parser (char #\+))
      (*parser (char #\<))
      (*parser (char #\>))
      (*parser (char #\?))
      (*parser (char #\/))
      (*disj 15)
      done))

(define <Symbol>
 (new (*parser <SymbolChar>) *plus
      (*guard (lambda (syms) (not (string->number (list->string syms)))))
      (*pack (lambda(syms) (string->symbol (list->string syms))))
      done))


(define <ProperList>
 (new (*parser (char #\())
      (*delayed (lambda () <sexpr>)) *star
      (*parser (char #\)))
      (*caten 3)
      (*pack-with (lambda (open expr close) expr))

      done))


(define <ImproperList>
 (new (*parser (char #\())
      (*delayed (lambda () <sexpr>)) *plus
      (*parser (char #\.))
      (*delayed (lambda () <sexpr>))
      (*parser (char #\)))
      (*caten 5)
      (*pack-with (lambda (open expr_lst dot expr close) `(,@expr_lst . ,expr)))

      done))

(define <Vector>
 (new (*parser (char #\#))
      (*parser <ProperList>)
      (*caten 2)
      (*pack-with (lambda (hash lst) (list->vector lst)))
      done))

(define <Quoted>
 (new (*parser (char #\'))
      (*delayed (lambda () <sexpr>))
      (*caten 2)
      (*pack-with (lambda (hash expr) (list 'quote expr)))
      done))

(define <QuasiQuoted>
 (new (*parser (char #\`))
      (*delayed (lambda () <sexpr>))
      (*caten 2)
      (*pack-with (lambda (hash expr) (list 'quasiquote expr)))
      done))

(define <Unquoted>
 (new (*parser (char #\,))
      (*delayed (lambda () <sexpr>))
      (*caten 2)
      (*pack-with (lambda (hash expr) (list 'unquote expr)))
      done))

(define <UnquoteAndSpliced>
 (new (*parser (char #\,))
      (*parser (char #\@))
      (*delayed (lambda () <sexpr>))
      (*caten 3)
      (*pack-with (lambda (hash at-sign expr) (list 'unquote-splicing expr)))
      done))


(define <PowerSymbol>
 (new (*parser (char #\^))
      (*parser (char #\*))
      (*parser (char #\*))
      (*caten 2)
      (*disj 2)
      done))

(define <InfixSymbolChar>
(new (*parser <SymbolChar>)

      ;Except any of the following:
      (*parser (char #\+))

      (*parser (char #\-))

      (*parser (char #\*))

      (*parser (char #\*))
      (*parser (char #\*))
      (*caten 2)

      (*parser (char #\^))

      (*parser (char #\/))
      
      (*disj 6)

      *diff
      done))


(define <InfixSymbol>
 (new (*parser <InfixSymbolChar>) *plus
      (*guard (lambda (syms) (not (string->number (list->string syms)))))
      (*pack (lambda(syms) (string->symbol (list->string syms))))
      done))



(define build_exp_from_infix_list 
  (lambda (infix_lst acc)
     (if (null? infix_lst)
         acc
         (let ((op (caar infix_lst)))
           (if (equal? op 'create-func-op)
               (let ((args (cadar infix_lst)))
                  (build_exp_from_infix_list (cdr infix_lst) `(,acc ,@args)))
               (let ((expr (cadar infix_lst)))
                  (build_exp_from_infix_list (cdr infix_lst) `(,op ,acc ,expr))))))))

(define build_exp_from_infix_list_right_assoc
  (lambda (infix_lst acc)
     (if (null? infix_lst)
         acc
         (let ((op (caar infix_lst))
               (expr (cadar infix_lst)))
            `(,op ,acc ,(build_exp_from_infix_list_right_assoc (cdr infix_lst) expr))))))




(define <InfixLevel6>
 (^<skipped-infix*> (new 
       ;<InfixSymbol>
       (*parser <InfixSymbol>)

       ;<Number>
       (*parser <Number>)
       
       ;<InfixParen>
       (*parser (char #\())
       (*delayed (lambda () <InfixLevel1>))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (open expr close) expr))

       ;<InfixSexprEscape>
       (*delayed (lambda () <InfixPrefixExtensionPrefix>))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (prefix expr) expr))

       (*disj 4)
       done)))

(define <InfixArgList>
 (new
      (*delayed (lambda () <InfixLevel1>))

      (*parser (char #\,))
      (*delayed (lambda () <InfixLevel1>))
      (*caten 2)
      (*pack-with (lambda (par expr) expr))
      *star

      (*caten 2)
      (*pack-with (lambda (expr expr_lst) `(,expr ,@expr_lst)))


      (*parser (^<skipped-infix*> <epsilon>))

      (*disj 2)
      done))

(define <InfixLevel5>
 (^<skipped-infix*> (new 
       
       (*parser <InfixLevel6>)

       ;<InfixArrayGet>
       (*parser (char #\[))
       (*delayed (lambda () <InfixLevel1>))
       (*parser (char #\]))
       (*caten 3)
       (*pack-with (lambda (open infix close) `(vector-ref ,infix)))


       ;<InfixFuncal>
       (*parser (char #\())
       (*delayed (lambda () <InfixArgList>))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (open infix close) (list 'create-func-op infix)))

       (*disj 2)
       *star

       (*caten 2)
       (*pack-with (lambda (first_infix rest_infix_lst) (build_exp_from_infix_list rest_infix_lst first_infix)))

       done)))

(define <InfixLevel4>
 (^<skipped-infix*> (new

      (*parser <InfixLevel5>)
   
      ;<InfixNeg>
      (*parser (char #\-))
      (*parser <InfixLevel5>)
      (*caten 2)
      (*pack-with (lambda (sminus expr) `(- ,expr)))

      (*disj 2)
      done)))

(define <InfixLevel3>
 (^<skipped-infix*> (new (*parser <InfixLevel4>)
      
      (*parser <PowerSymbol>)
      (*pack (lambda(c) 'expt))
      (*parser <InfixLevel4>)
      (*caten 2)
      *star


      (*caten 2)
      (*pack-with (lambda (first_infix rest_infix_lst) (build_exp_from_infix_list_right_assoc rest_infix_lst first_infix)))
      done)))


(define <InfixLevel2>
 (^<skipped-infix*> (new (*parser <InfixLevel3>)
      
      (*parser (char #\*))
      (*pack (lambda(c) '*))
      (*parser (char #\/))
      (*pack (lambda(c) '/))
      (*disj 2)
      (*parser <InfixLevel3>)
      (*caten 2)
      *star

      (*caten 2)
      (*pack-with (lambda (first_infix rest_infix_lst) (build_exp_from_infix_list rest_infix_lst first_infix)))

      done)))
        

(define <InfixLevel1>
 (^<skipped-infix*> (new (*parser <InfixLevel2>)
      
      (*parser (char #\+))
      (*pack (lambda(c) '+))
      (*parser (char #\-))
      (*pack (lambda(c) '-))
      (*disj 2)
      (*parser <InfixLevel2>)
      (*caten 2)
      *star


      (*caten 2)
      (*pack-with (lambda (first_infix rest_infix_lst) (build_exp_from_infix_list rest_infix_lst first_infix)))
      done)))

(define <InfixExpression> <InfixLevel1>)


(define <InfixPrefixExtensionPrefix>
 (new (*parser (char #\#))

      (*parser (char #\#))
      (*parser (char #\%))
      (*disj 2)
      
      (*caten 2)
      done))

(define <InfixExtension>
 (new (*parser <InfixPrefixExtensionPrefix>)
      (*parser <InfixExpression>)
      (*caten 2)

      (*pack-with (lambda (prefix expr) expr))
      done))




(define <sexpr>
(^<skipped*> (new 

                (*parser <Boolean>)
                (*parser <Char>)
                (*parser <Symbol>)
                (*parser <Number>)
                (*parser <String>)
                (*parser <ProperList>)
                (*parser <ImproperList>)
                (*parser <Vector>)
                (*parser <Quoted>)
                (*parser <QuasiQuoted>)
                (*parser <Unquoted>)
                (*parser <UnquoteAndSpliced>)
                (*parser <InfixExtension>)
                (*disj 13)
                done)))

(define <Sexpr> <sexpr>)


;;; qq.scm
;;; A naive, one-level quasiquote implementation + optimizations
;;;
;;; Programmer: Mayer Goldberg, 2016

;;;

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))




      






