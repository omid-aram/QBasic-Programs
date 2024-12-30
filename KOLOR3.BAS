CLS : SCREEN 12
x = 0: y = 0: a0 = 40: b0 = 30: n = 1

1 FOR k0 = 0 TO 15
k00 = k0
12 FOR y = 0 + b TO b0 + b STEP 2
PSET (x, y), k0
PSET (x, y + 1), k2
NEXT y
'SLEEP
IF n = 1 THEN IF x < a0 + a THEN x = x + 1: GOTO 12
IF n = 2 THEN IF x < a0 + a THEN x = x + 1: SWAP k2, k0: GOTO 12
IF k0 <> k00 THEN SWAP k0, k2

k2 = k2 + 1
b = b + b0
x = a
'LOCATE 20, 20: PRINT k0, k2, a, (k0 + 1) * a0: SLEEP
IF b < 16 * b0 THEN IF b < (k0 + 1) * b0 THEN n = 1: GOTO 12
IF b < 16 * b0 THEN IF b >= (k0 + 1) * b0 THEN n = 2: GOTO 12
n = 1
b = 0
a = a + a0
x = a
k2 = 0
'LOCATE 20, 20: PRINT k0, k2, y + b, b: SLEEP
NEXT k0
SLEEP
END

