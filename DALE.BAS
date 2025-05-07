r0 = 100: x00 = 0
1 Cls: Screen 12
print "Use -/+ to Zoom and Num4/Num6 to Scroll the pointer!"
PI = 4 * Atn(1)
x0 = -270 - x00
k0 = 12:
'x0 = 316: y0 = 233: a0 = 1
Circle (320, 240), r0 - 5, 13

y1 = -3 - r0: r1 = Sqr(1 + (y1 ^ 2)): x1 = Atn((y1 / r1) / Sqr(1 - (y1 / r1) ^ 2)) * (180 / PI)
y2 = -1 - r0: r2 = Sqr((3 ^ 2) + (y2 ^ 2)): x2 = Atn((y2 / r2) / Sqr(1 - (y2 / r2) ^ 2)) * (180 / PI)
y3 = -r0: r3 = Sqr(9 + y3 ^ 2): x3 = Atn((y3 / r3) / Sqr(1 - (y3 / r3) ^ 2)) * (180 / PI)
y4 = 1 - r0: r4 = Sqr(4 + y4 ^ 2): x4 = Atn((y4 / r4) / Sqr(1 - (y4 / r4) ^ 2)) * (180 / PI)
y5 = 1 - r0: r5 = Sqr(1 + y5 ^ 2): x5 = Atn((y5 / r5) / Sqr(1 - (y5 / r5) ^ 2)) * (180 / PI)

'10 LINE (0 + x0, 0 * a0 + y0)-(8 + x0, 14 * a0 + y0), 0, BF:
Line (320 + r1 * Cos((x1 + x0) / (180 / PI)), 240 + r1 * Sin((x1 + x0) / (180 / PI)))-(320 + r2 * Cos((x2 + x0) / (180 / PI)), 240 + r2 * Sin((x2 + x0) / (180 / PI))), k0:
Line -(320 + r3 * Cos((x3 + x0) / (180 / PI)), 240 + r3 * Sin((x3 + x0) / (180 / PI))), k0:
Line (320 + r4 * Cos((x4 + x0) / (180 / PI)), 240 + r4 * Sin((x4 + x0) / (180 / PI)))-(319 + r5 * Cos((x5 + x0) / (180 / PI)), 240 + r5 * Sin((x5 + x0) / (180 / PI))), k0:
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
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(43) Then r0 = r0 + 10: GoTo 1
    If ak$ = Chr$(45) Then r0 = r0 - 10: GoTo 1
    If ak$ = Chr$(52) Then x00 = x00 + 10: GoTo 1
    If ak$ = Chr$(54) Then x00 = x00 - 10: GoTo 1
Wend
 

