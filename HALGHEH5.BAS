CLS : SCREEN 12
PI = 4 * ATN(1)
a = 1: b = 100: c = b - 1: d = a - 1: a0 = a: b0 = b
DIM nogh(4 * (b - d))

LINE (0, 0)-(100, 100), 14, BF
LINE (0, 0)-(50, 50), 12, BF
LINE (0, 100)-(100, 50), 14, BF
LINE (100, 0)-(50, 50), 13, BF
CIRCLE (50, 50), 25, 4
PAINT (50, 50), 2, 4


WHILE s2 < (b + a) ^ 2 - 5
FOR x = a TO b
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(x, d): PSET (x, d), 0
NEXT x
FOR y = a TO b
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(b, y): PSET (b, y), 0
NEXT y
FOR x0 = c TO d STEP -1
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(x0, b): PSET (x0, b), 0
NEXT x0
FOR y0 = c TO d STEP -1
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(d, y0): PSET (d, y0), 0
NEXT y0

p0 = 4 * (b - d)
'xa = (PI * (b - d + 1)) / (2 * SQR(PI) * (b - d))
xa = 1
r0 = xa * (p0 / (2 * PI))
r1 = p0 / (2 * PI) '+ 50
'r0 = SQR(((b + 1) ^ 2) / PI)
z0 = -135
'z0 = ATN((b1 / b)) / (PI / 180) + 180
'z0 = 270 - ((((b - d) * 180) / (b - d + b - d)) / 2)
FOR sh = 1 TO s1
z0 = z0 + (360 / p0)
PSET (320 + (r0 * COS(z0 * (PI / 180))), 240 + (r0 * SIN(z0 * (PI / 180)))), nogh(sh)
'PSET (500 + (r1 * COS(z0 * (PI / 180))), 240 + (r1 * SIN(z0 * (PI / 180)))), nogh(sh)
NEXT sh
s1 = 0

a = a + 1
d = d + 1
c = c - 1
b = b - 1
WEND
e = POINT(b0 / 2, b0 / 2): PSET (b0 / 2, b0 / 2), 0
PSET (320, 240), e


