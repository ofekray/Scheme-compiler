/* binary_append (append l m == append R1 R2)
(define (append l m)
 (if (null? l) 
      m
      (cons (car l) (append (cdr l) m))))
----------------------------------------------------------------- */
BINARY_APPEND:
    PUSH(FP);
    MOV(FP, SP);
    PUSH(R1);
    PUSH(R2);
    MOV(R1, FPARG(0)); /* l */
    MOV(R2, FPARG(1)); /* m */
    CMP(INDD(R1, 0), IMM(T_NIL));
    JUMP_EQ(BINARY_APPEND_RETURN_M);
    /* do recuresion */
        PUSH(R2);
        PUSH(INDD(R1, 2)); /* pushing (cdr l) */
        CALL(BINARY_APPEND);
        DROP(2);
        /* doing cons */
            PUSH(R0);
            PUSH(INDD(R1, 1)); /* pushing (car l) */
            CALL(MAKE_SOB_PAIR);
            DROP(2);
    JUMP(BINARY_APPEND_END);
BINARY_APPEND_RETURN_M:
    MOV(R0, R2);
BINARY_APPEND_END:
    POP(R2);
    POP(R1);
    POP(FP);
    RETURN;
