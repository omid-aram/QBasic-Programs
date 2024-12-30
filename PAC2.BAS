1 CLS : SCREEN 12: WINDOW (-320, -240)-(320, 240):  k = 40
INPUT x
INPUT y
IF y > x THEN SWAP y, x
xn = x
yn = y
2 CLS : x = x * k
y = y * k
v = x / 2
v1 = y / 2
f = SQR((v ^ 2) - (v1 ^ 2))
z = SQR((v - f) ^ 2 + (-v1) ^ 2)
z1 = (x * y) / z / 2
g = v - f
g2 = x - g
PRINT CLNG(100 * z / k) / 100, CLNG(100 * z1 / k) / 100, CLNG(100 * g / k) / 100, CLNG(100 * g2 / k) / 100
LINE (-v, v1)-(v, -v1), 2, B
LINE (-f, -v1)-(-v, 0), 4
LINE -(f, v1), 4
LINE -(v, 0), 4
LINE -(-f, -v1), 4
WHILE ak$ <> CHR$(27)
ak$ = INKEY$
IF ak$ = CHR$(45) THEN k = k / 1.1: x = xn: y = yn: GOTO 2
IF ak$ = CHR$(43) THEN k = k * 1.1: x = xn: y = yn: GOTO 2
WEND

