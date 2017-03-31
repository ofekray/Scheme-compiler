/* DIV_REG_FRAC (R3/R4 divi... R6/R7 new val)
Ofek Bashan, 2017
------------------- */
DIV_REG_FRAC:
    MUL(R3, R7); /* now new_R3 is R3*R7 */
    MUL(R4, R6); /* now new_R4 is R4*R6 */
                 /* now new_R3/new_R4 is actually  R3*R7/R4*R6 = (R3/R4)/(R6/R7) */
    CALL(GCD_REG); /* function the computes (GCD(ABS(R3), ABS(R4)) and puts result in R0 */
    DIV(R3, R0);
    DIV(R4, R0);
/* checking R4 for negative */
    CMP(R4, IMM(0));
    JUMP_LT(DIV_REG_FRAC_IS_NEGATIVE);
    RETURN; /* Not negative */
DIV_REG_FRAC_IS_NEGATIVE:
    MUL(R3, IMM(-1)); /* Putting minus in R3 instead of R4 */
    MUL(R4, IMM(-1));
    RETURN;
