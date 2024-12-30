1 CLS : SCREEN 12: WINDOW (-320, -240)-(320, 240): k = 40
INPUT x
INPUT y
xn = x: yn = y
2 CLS : x = x * k
y = y * k
GOSUB 657
'
f2 = (z / 2) - f
hx = -1 * (z / 2) + (z1 / 2) * ((z1 / 2) / (z / 2))
hy = (z1 / 2) * ((f2) / (z / 2) * -1)
k1 = 0: k11 = 0: k111 = 0: k1111 = 0: k2 = 2: k22 = 2: k222 = 2: k2222 = 2
WHILE ak$ <> CHR$(27)
ak$ = INKEY$
GOSUB 10: GOSUB 20: GOSUB 30: GOSUB 40
IF ak$ = CHR$(45) THEN k = k / 1.1: x = xn: y = yn: GOTO 2
IF ak$ = CHR$(43) THEN k = k * 1.1: x = xn: y = yn: GOTO 2
IF ak$ = CHR$(50) THEN k1 = 4: k2 = 0
IF ak$ = CHR$(52) THEN k11 = 4: k22 = 0
IF ak$ = CHR$(54) THEN k111 = 4: k222 = 0
IF ak$ = CHR$(56) THEN k1111 = 4: k2222 = 0
IF ak$ = CHR$(48) THEN GOSUB 131
WEND
LOCATE 30, 1: PRINT "DO YOU WANT TO TRY AGAIN ?": ak$ = INPUT$(1): IF ak$ = CHR$(121) THEN GOTO 1 ELSE END
''LINE ((z / 2) - (f), (z1 / 2))-(-1 * (z / 2) + (f), -1 * (z1 / 2)), 4
10 LINE (-1 * (z / 2), 0)-(hx, hy), k1
LINE -((z / 2) - (f), (z1 / 2)), k1
LINE ((z / 2) - (f), (z1 / 2))-(-1 * (z / 2), (z1 / 2)), k2: LINE -(-1 * (z / 2), 0), k2
RETURN
20 LINE ((z / 2), 0)-(-1 * hx, -1 * hy), k11
LINE -((z / 2) - (f), (z1 / 2)), k11
LINE ((z / 2), 0)-((z / 2), (z1 / 2)), k22: LINE -((z / 2) - (f), (z1 / 2)), k22
RETURN
30 LINE (-1 * (z / 2), 0)-(hx, hy), k111
LINE -(-1 * (z / 2) + (f), -1 * (z1 / 2)), k111
LINE (-1 * (z / 2), 0)-(-1 * (z / 2), -1 * (z1 / 2)), k222: LINE -(-1 * (z / 2) + (f), -1 * (z1 / 2)), k222
RETURN
40 LINE ((z / 2), 0)-(-1 * hx, -1 * hy), k1111
LINE -(-1 * (z / 2) + (f), -1 * (z1 / 2)), k1111
LINE (-1 * (z / 2) + (f), -1 * (z1 / 2))-((z / 2), -1 * (z1 / 2)), k2222: LINE -((z / 2), 0), k2222
RETURN

131 WHILE ak$ = CHR$(48)
ak$ = INPUT$(1)
GOSUB 10: GOSUB 20: GOSUB 30: GOSUB 40
IF ak$ = CHR$(56) THEN k1 = 0: k2 = 2
IF ak$ = CHR$(54) THEN k11 = 0: k22 = 2
IF ak$ = CHR$(52) THEN k111 = 0: k222 = 2
IF ak$ = CHR$(50) THEN k1111 = 0: k2222 = 2
WEND
RETURN

657 s = (x * y) * 2
z = ((x ^ 2) + (y ^ 2)) ^ .5
z1 = s / z
f = ((x ^ 2) - ((z1 / 2) ^ 2)) ^ .5
f1 = z - f
PRINT z / k, z1 / k, f / k, f1 / k
LINE ((z / 2), (z1 / 2))-(-1 * (z / 2), -1 * (z1 / 2)), 2, B
LINE (-1 * (z / 2), 0)-(-1 * (z / 2) + (f), -1 * (z1 / 2)), 3
LINE -((z / 2), 0), 3
LINE -((z / 2) - (f), (z1 / 2)), 3
LINE -(-1 * (z / 2), 0), 3
RETURN

