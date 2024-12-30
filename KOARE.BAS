CLS : SCREEN 12
PI = 4 * ATN(1)
r0 = 200: y0 = .07
'FOR l0 = 0 TO r0 STEP r0 / (1 / y0)
'FOR z = 90 TO 450 STEP .9
'PSET (320 + l0 * COS(z * (PI / 180)), 240 + r0 * SIN(z * (PI / 180))), 12
'NEXT z
'NEXT l0
LINE (320, 40)-(320, 440), 12
FOR l = y0 TO 1 STEP y0
CIRCLE (320, 240), r0, 12, , , 1 / l
NEXT l
'SLEEP
FOR l1 = 0 TO 1 STEP y0
CIRCLE (320, 240), r0, 12, , , l1
NEXT l1


