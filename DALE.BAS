r0 = 100: x00 = 0
1 CLS : SCREEN 12
PI = 4 * ATN(1)
x0 = -270 - x00
k0 = 12:
'x0 = 316: y0 = 233: a0 = 1
CIRCLE (320, 240), r0 - 5, 13

y1 = -3 - r0: r1 = SQR(1 + (y1 ^ 2)): x1 = ATN((y1 / r1) / SQR(1 - (y1 / r1) ^ 2)) * (180 / PI)
y2 = -1 - r0: r2 = SQR((3 ^ 2) + (y2 ^ 2)): x2 = ATN((y2 / r2) / SQR(1 - (y2 / r2) ^ 2)) * (180 / PI)
y3 = -r0: r3 = SQR(9 + y3 ^ 2): x3 = ATN((y3 / r3) / SQR(1 - (y3 / r3) ^ 2)) * (180 / PI)
y4 = 1 - r0: r4 = SQR(4 + y4 ^ 2): x4 = ATN((y4 / r4) / SQR(1 - (y4 / r4) ^ 2)) * (180 / PI)
y5 = 1 - r0: r5 = SQR(1 + y5 ^ 2): x5 = ATN((y5 / r5) / SQR(1 - (y5 / r5) ^ 2)) * (180 / PI)

'10 LINE (0 + x0, 0 * a0 + y0)-(8 + x0, 14 * a0 + y0), 0, BF:
LINE (320 + r1 * COS((x1 + x0) / (180 / PI)), 240 + r1 * SIN((x1 + x0) / (180 / PI)))-(320 + r2 * COS((x2 + x0) / (180 / PI)), 240 + r2 * SIN((x2 + x0) / (180 / PI))), k0:
LINE -(320 + r3 * COS((x3 + x0) / (180 / PI)), 240 + r3 * SIN((x3 + x0) / (180 / PI))), k0:
LINE (320 + r4 * COS((x4 + x0) / (180 / PI)), 240 + r4 * SIN((x4 + x0) / (180 / PI)))-(319 + r5 * COS((x5 + x0) / (180 / PI)), 240 + r5 * SIN((x5 + x0) / (180 / PI))), k0:
': n = 1:  x0 = x0 - 8:
















'y1 = 2 - r0: r1 = SQR(1 + (y1 ^ 2)): x1 = ATN((y1 / r1) / SQR(1 - (y1 / r1) ^ 2)) * (180 / PI)
'y2 = -2 - r0: r2 = y2: x2 = 90:  x2 = ATN((y2 / r2) / SQR(1 - (y2 / r2) ^ 2)) * (180 / PI)
'y3 = 2 - r0: r3 = y3: x3 = 90: x3 = ATN((y3 / r3) / SQR(1 - (y3 / r3) ^ 2)) * (180 / PI)
'y4 = -1 - r0: r4 = SQR(1 + (y4) ^ 2): x4 = ATN((y4 / r4) / SQR(1 - (y4 / r4) ^ 2)) * (180 / PI)
'y5 = 2 - r0: r5 = SQR(1 + (y5) ^ 2): x5 = ATN((y5 / r5) / SQR(1 - (y5 / r5) ^ 2)) * (180 / PI)
'y6 = -r0: r6 = SQR(4 + (y6) ^ 2): x6 = ATN((y6 / r6) / SQR(1 - (y6 / r6) ^ 2)) * (180 / PI)
'y7 = 1 - r0: r7 = SQR(4 + (y7) ^ 2): x7 = ATN((y7 / r7) / SQR(1 - (y7 / r7) ^ 2)) * (180 / PI)
'
'PSET (320 + r1 * COS((x1 + x0) / (180 / PI)), 240 + r1 * SIN((x1 + x0) / (180 / PI))), 14
'PSET (320 + r2 * COS((x2 + x0) / (180 / PI)), 240 + r2 * SIN((x2 + x0) / (180 / PI))), 14
'PSET (320 + r3 * COS((x3 + x0) / (180 / PI)), 240 + r3 * SIN((x3 + x0) / (180 / PI))), 14
'PSET (320 + r4 * COS((x4 + x0) / (180 / PI)), 240 + r4 * SIN((x4 + x0) / (180 / PI))), 14
'PSET (320 + r5 * COS((x5 + x0) / (180 / PI)), 240 + r5 * SIN((x5 + x0) / (180 / PI))), 14
'PSET (320 + r6 * COS((x6 + x0) / (180 / PI)), 240 + r6 * SIN((x6 + x0) / (180 / PI))), 14
'PSET (320 + r7 * COS((x7 + x0) / (180 / PI)), 240 + r7 * SIN((x7 + x0) / (180 / PI))), 14
'
WHILE ak$ <> CHR$(27)
ak$ = INPUT$(1)
IF ak$ = CHR$(43) THEN r0 = r0 + 10: GOTO 1
IF ak$ = CHR$(45) THEN r0 = r0 - 10: GOTO 1
IF ak$ = CHR$(52) THEN x00 = x00 + 10: GOTO 1
IF ak$ = CHR$(54) THEN x00 = x00 - 10: GOTO 1
WEND
 

