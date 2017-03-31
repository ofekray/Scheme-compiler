/* GCD_REG (GCD(ABS(R3), ABS(R4))
Ofek Bashan, 2017
-------------- */
GCD_REG:
    PUSH(R3);
    CALL(ABS);
    DROP(1);
    MOV(R8, R0); /* now R8 = ABS(R3) */
    PUSH(R4);
    CALL(ABS);
    DROP(1);
    MOV(R9, R0); /* now R9 = ABS(R4) */
GCD_REG_LOOP:
    MOV(R10, R8);
    REM(R10, R9);
    CMP(R10, IMM(0));
    JUMP_EQ(GCD_REG_EXIT);
    MOV(R8, R9);
    MOV(R9, R10);
    JUMP(GCD_REG_LOOP);
GCD_REG_EXIT:
    MOV(R0, R9);
    RETURN;
