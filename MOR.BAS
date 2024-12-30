CLS
INPUT "HOW MANY NUMBERS YOU HAVE "; t
DIM numb(t)
DIM num(t)
FOR s = 1 TO t
 INPUT numb(s)
NEXT s
FOR s = 1 TO t
num(s) = numb(s)
NEXT s

FOR s = 1 TO t
  FOR s2 = s + 1 TO t
  IF num(s) > num(s2) THEN SWAP num(s), num(s2)
  NEXT s2
NEXT s

FOR s = 1 TO t
PRINT s, num(s)
IF s MOD 20 = 0 THEN a$ = INPUT$(1)
NEXT s
END

