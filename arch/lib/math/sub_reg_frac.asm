/* SUB_REG_FRAC (R3/R4 sum... R6/R7 new val)
Ofek Bashan, 2017
------------------- */
SUB_REG_FRAC:
    MUL(R3, R7); /* now new_R3 is R3*R7 */
    MUL(R6, R4); /* now new_R6 is R6*R4 */
    SUB(R3, R6); /* now new_R3 is (R3*R7 - R6*R4) */
    MUL(R4, R7); /* now new_R4 is R4*R7 */
                 /* now new_R3/new_R4 is actually  (R3*R7 - R6*R4)/R4*R7 = R3/R4 - R6/R7 */
    CALL(GCD_REG); /* function the computes (GCD(ABS(R3), ABS(R4)) and puts result in R0 */
    DIV(R3, R0);
    DIV(R4, R0);
    RETURN;
