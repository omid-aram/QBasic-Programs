CLS : SCREEN 12: WINDOW (-100, -75)-(100, 75)
k00 = 12
DEF fny (x) = ABS(x) ^ .5
'DEF fny (x) = 25 * SIN(x / 25) + 26
'DEF fnz (a) = (-1 / 2) * a
'DEF fnz (x) = 25 * SIN(x / 25) + 26
'DEF fnz (x) = SIN(200 / x) + 15
DEF fnz (x) = x ^ 3 + 5 * x ^ 2 + 8 * x + 9000000
x1 = -100
x2 = 100
b = x1
c = x2
a = b + 1
5 a = a + 1
IF a = 0 THEN a = a + .0001
IF a > c THEN GOTO 100
y0 = fny(a)
z0 = fnz(a)
'PRINT y0, z0, a
IF z0 = 0 THEN GOTO 7
CLS : GOSUB 10
7 b = a
'SLEEP

GOTO 5

100 END

10
PI = 4 * ATN(1)
y00 = .07
'LINE (320, 40)-(320, 440), 12
FOR l = y00 TO 1 STEP y00
PRINT a, y0, z0
CIRCLE (a, y0), z0, k00, , , 1 / l
NEXT l
FOR l1 = 0 TO 1 STEP y00
CIRCLE (a, y0), z0, k00, , , l1
NEXT l1
RETURN
'CIRCLE (a, y0), z0, 14
'PAINT (a, y0), 14, 14
'RETURN

