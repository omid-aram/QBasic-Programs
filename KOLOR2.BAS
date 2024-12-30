CLS : SCREEN 12
x = 0: y = 0: a0 = 40: b0 = 30: n = 1

1 FOR k0 = 0 TO 15
k00 = k0
12 FOR x = 0 + a TO a0 + a STEP 2
PSET (x, y), k0
PSET (x + 1, y), k2
NEXT x
'SLEEP
IF n = 1 THEN IF y < b0 + b THEN y = y + 1: GOTO 12
IF n = 2 THEN IF y < b0 + b THEN y = y + 1: SWAP k2, k0: GOTO 12
IF k0 <> k00 THEN SWAP k0, k2

k2 = k2 + 1
a = a + a0
y = b
'LOCATE 20, 20: PRINT k0, k2, a, (k0 + 1) * a0: SLEEP
IF a < 16 * a0 THEN IF a < (k0 + 1) * a0 THEN n = 1: GOTO 12
IF a < 16 * a0 THEN IF a >= (k0 + 1) * a0 THEN n = 2: GOTO 12
n = 1
a = 0
b = b + b0
y = b
k2 = 0
'LOCATE 20, 20: PRINT k0, k2, y + b, b: SLEEP
NEXT k0
SLEEP
END

