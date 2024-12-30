CLS : SCREEN 12: WINDOW (-319, -239)-(320, 240)
f0 = 250
DEF fnf (x) = x00 / 640 * x
DEF fng (x) = y00 / 480 * x
GOSUB 10

'x0 = 100
'y0 = -129
'x1 = 200
'y1 = -50
''PRINT x0, fnf(x0)
'
'LINE (x0, y0)-(x1, y1), 12, B
'LINE (fnf(x0), fng(y0))-(fnf(x1), fng(y1)), 12, B
'LINE (x0, y0)-(fnf(x0), fng(y0)), 12
'LINE (x1, y1)-(fnf(x1), fng(y1)), 12
'LINE (x0, y1)-(fnf(x0), fng(y1)), 12
'LINE (x1, y0)-(fnf(x1), fng(y0)), 12
''''''''''''''''''''''''''''''''''''''''
'FOR x0 = -319 TO 320 STEP 10
'LINE (x0, -239)-(fnf(x0), fng(-239)), 6
'NEXT x0
'FOR f0 = 0 TO 250 STEP 4
'GOSUB 10
'LINE (fnf(-319), fng(-240))-(fnf(320), fng(-240)), 6
'NEXT f0: f0 = 250
'FOR f0 = 0 TO 250 STEP 10
'GOSUB 10
'LINE (fnf(-319), fng(-240))-(fnf(-319), fng(240)), 7
'NEXT f0: f0 = 250
'FOR y0 = -239 TO 240 STEP 4
'LINE (-319, y0)-(fnf(-319), fng(y0)), 7
'NEXT y0
'FOR f0 = 0 TO 250 STEP 10
'GOSUB 10
'LINE (fnf(319), fng(-240))-(fnf(319), fng(240)), 7
'NEXT f0: f0 = 250
'FOR y0 = -239 TO 240 STEP 4
'LINE (319, y0)-(fnf(319), fng(y0)), 7
'NEXT y0
'FOR x0 = -319 TO 320 STEP 20
'LINE (x0, 239)-(fnf(x0), fng(239)), 7
'NEXT x0
'FOR f0 = 0 TO 250 STEP 38.4
'GOSUB 10
'LINE (fnf(-319), fng(240))-(fnf(320), fng(240)), 7
'NEXT f0: f0 = 250
'''''''''''''''''''''''''''''''''''''''
PI = 4 * ATN(1)
'WHILE 1
'f0 = 250: GOSUB 10
'q0 = .07: r0 = 100: k9 = 12
'FOR f0 = 0 TO 800 STEP 20
'GOSUB 10: k9 = 12
'11 FOR l = q0 TO 1 STEP q0
'CIRCLE (fnf(100), fng(-40)), fnf(r0), k9, , , 1 / l
'NEXT l
'FOR l1 = 0 TO 1 STEP q0
'CIRCLE (fnf(100), fng(-40)), fnf(r0), k9, , , l1
'NEXT l1
'IF k9 = 12 THEN k9 = 0: GOTO 11
'NEXT f0
'WEND
'
'y0 = -50
'FOR x0 = -50 TO 50
'f0 = SQR(2500 - x0 ^ 2)
'GOSUB 10
'PSET (fnf(x0), fng(y0)), 12
'f0 = -SQR(2500 - x0 ^ 2)
'GOSUB 10
'PSET (fnf(x0), fng(y0)), 12
'NEXT x0
'y0 = -150
'FOR x0 = -50 TO 50 STEP .1
'f0 = SQR(2500 - x0 ^ 2)
'GOSUB 10
'PSET (fnf(x0), fng(y0)), 12
'f0 = -SQR(2500 - x0 ^ 2)
'GOSUB 10
'PSET (fnf(x0), fng(y0)), 12
'NEXT x0
'PAINT (0, -150), 4, 12
'FOR x0 = -50 TO 50 STEP 2
'f0 = SQR(2500 - x0 ^ 2)
'GOSUB 10
'LINE (fnf(x0), fng(-150))-(fnf(x0), fng(-50)), 12
'f0 = -SQR(2500 - x0 ^ 2)
'GOSUB 10
'LINE (fnf(x0), fng(-150))-(fnf(x0), fng(-50)), 12
'NEXT x0
'
y0 = -0: r0 = 90
FOR y0 = -90 TO 90
12 FOR x0 = -r0 TO r0
'LOCATE 1, 1: PRINT x0
f0 = SQR(8100 - x0 ^ 2)

GOSUB 10
PSET (fnf(x0), fng(y0)), 12
NEXT x0
r0 = 90 * COS(y0 * PI / 180)
'LOCATE 1, 1: PRINT r0, y0, COS(y0 * (PI / 180))
NEXT y0
SLEEP

END
10 f1 = 400 - f0
x00 = 2 * (f1 ^ 2) * (1 + COS(1.287)): x00 = SQR(x00)
y00 = 2 * (f1 ^ 2) * (1 - COS(1.287)): y00 = SQR(y00)
RETURN

