/* MUL_REG_FRAC (R3/R4 multi... R6/R7 new val)
Ofek Bashan, 2017
------------------- */
MUL_REG_FRAC:
    MUL(R3, R6); /* now new_R3 is R3*R6 */
    MUL(R4, R7); /* now new_R4 is R4*R7 */
                 /* now new_R3/new_R4 is actually  R3*R6/R4*R7 */
    CALL(GCD_REG); /* function the computes (GCD(ABS(R3), ABS(R4)) and puts result in R0 */
    DIV(R3, R0);
    DIV(R4, R0);
    RETURN;
