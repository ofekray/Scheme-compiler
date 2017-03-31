 STRING_EQUAL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  MOV(R1, FPARG(0)); /* first string */
  MOV(R2, FPARG(1)); /* second string */
  MOV(R3, INDD(R1, 1)); /* length of first string */
  CMP(R3, INDD(R2, 1)); /* compare string length */
  JUMP_EQ(STRING_EQUAL_SAME_LENGTH);
  JUMP(STRING_EQUAL_FALSE);
 STRING_EQUAL_SAME_LENGTH:
  CMP(R3, IMM(0)); /* if str length is 0 then we are finished */
  JUMP_EQ(STRING_EQUAL_TRUE);
  MOV(R4, R3);
  ADD(R4, 1); /* now R4 points to the index of the R3'th letter in the string */
  CMP(INDD(R1, R4), INDD(R2, R4));
  JUMP_NE(STRING_EQUAL_FALSE);
  SUB(R3, 1);
  JUMP(STRING_EQUAL_SAME_LENGTH);
 STRING_EQUAL_TRUE:
  MOV(R0, IMM(SOB_TRUE));
  JUMP(STRING_EQUAL_EXIT);
 STRING_EQUAL_FALSE:
  MOV(R0, IMM(SOB_FALSE));
 STRING_EQUAL_EXIT:
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
