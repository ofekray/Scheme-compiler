/* scheme/write_sob_fraction.asm
 * Take a pointer to a Scheme fraction object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Ofek Bashan, 2016
 */

 WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R3, INDD(R0, 2));
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH('/');
  CALL(PUTCHAR);
  DROP(1);
  PUSH(R3);
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;