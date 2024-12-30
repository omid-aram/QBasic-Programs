CLS : SCREEN 12: x0 = 320: f = 2:  k = 14

FOR r = 1 TO 200 STEP f * 2
x1 = x0 + f
CIRCLE (x0, 240), r, k, 0, 3.14
'SLEEP 1
CIRCLE (x1, 240), r + f, k, 3.14, 6.28
NEXT r

