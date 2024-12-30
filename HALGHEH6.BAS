CLS : SCREEN 12
PI = 4 * ATN(1)
a = 1: b = 125: b1 = 57: c = b - 1: c1 = b1 - 1: d = a - 1: a0 = a: b0 = b
DIM nogh(2 * (b + b1))

p0 = 2 * (b - d + b1 - d)
IF b1 - d <> 0 THEN ne = (b - d) / (b1 - d)
r0 = p0 / (PI * (ne + 1)): r1 = ((p0 / PI) - r0)
IF b > b1 THEN r0 = r1
x000 = 320: y000 = 340
WHILE ak$ <> CHR$(13)
ak$ = INKEY$
LINE (0, 0)-(b, b1), 14, B
GOSUB cir
IF ak$ = CHR$(54) THEN k0 = 0: GOSUB cir: x000 = x000 + 10: GOSUB cir
IF ak$ = CHR$(52) THEN k0 = 0: GOSUB cir: x000 = x000 - 10: GOSUB cir
IF ak$ = CHR$(56) THEN k0 = 0: GOSUB cir: y000 = y000 - 10: GOSUB cir
IF ak$ = CHR$(50) THEN k0 = 0: GOSUB cir: y000 = y000 + 10: GOSUB cir
WEND
CLS
LINE (0, 0)-(b, b1), 4, BF
'LINE (0, 0)-(50, 50), 11, BF
'LINE (100, 0)-(50, 50), 6, BF

'FOR g = 10 TO b STEP 20
'o = o + 1
'FOR f = 10 TO b1 STEP 20
'CIRCLE (g, f), 10, o
'PAINT (g, f), o, o
'NEXT f
'NEXT g
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
k000 = 11
LINE (1, 17)-(4, 20), k000, BF
LINE (1, 21)-(4, 24), k000, BF
LINE (1, 25)-(4, 28), k000, BF
LINE (1, 29)-(4, 32), k000, BF
LINE (1, 33)-(4, 36), k000, BF
LINE (1, 37)-(4, 40), k000, BF
LINE (5, 13)-(8, 16), k000, BF
LINE (5, 17)-(8, 20), k000, BF
LINE (5, 21)-(8, 24), k000, BF
LINE (5, 25)-(8, 28), k000, BF
LINE (5, 29)-(8, 32), k000, BF
LINE (5, 33)-(8, 36), k000, BF
LINE (5, 37)-(8, 40), k000, BF
LINE (5, 41)-(8, 44), k000, BF
LINE (9, 9)-(12, 12), k000, BF
LINE (9, 13)-(12, 16), k000, BF
LINE (9, 41)-(12, 44), k000, BF
LINE (9, 45)-(12, 48), k000, BF
LINE (13, 9)-(16, 12), k000, BF
LINE (13, 45)-(16, 48), k000, BF
LINE (17, 9)-(20, 12), k000, BF
LINE (17, 13)-(20, 16), k000, BF
LINE (17, 41)-(20, 44), k000, BF
LINE (17, 45)-(20, 48), k000, BF
LINE (21, 13)-(24, 16), k000, BF
LINE (21, 17)-(24, 20), k000, BF
LINE (21, 21)-(24, 24), k000, BF
LINE (21, 25)-(24, 28), k000, BF
LINE (21, 29)-(24, 32), k000, BF
LINE (21, 33)-(24, 36), k000, BF
LINE (21, 37)-(24, 40), k000, BF
LINE (21, 41)-(24, 44), k000, BF
LINE (25, 17)-(28, 20), k000, BF
LINE (25, 21)-(28, 24), k000, BF
LINE (25, 25)-(28, 28), k000, BF
LINE (25, 29)-(28, 32), k000, BF
LINE (25, 33)-(28, 36), k000, BF
LINE (25, 37)-(28, 40), k000, BF
LINE (33, 9)-(36, 12), k000, BF
LINE (33, 13)-(36, 16), k000, BF
LINE (33, 17)-(36, 20), k000, BF
LINE (33, 21)-(36, 24), k000, BF
LINE (33, 25)-(36, 28), k000, BF
LINE (33, 29)-(36, 32), k000, BF
LINE (33, 33)-(36, 36), k000, BF
LINE (33, 37)-(36, 40), k000, BF
LINE (33, 41)-(36, 44), k000, BF
LINE (33, 45)-(36, 48), k000, BF
LINE (37, 9)-(40, 12), k000, BF
LINE (37, 13)-(40, 16), k000, BF
LINE (37, 17)-(40, 20), k000, BF
LINE (37, 21)-(40, 24), k000, BF
LINE (37, 25)-(40, 28), k000, BF
LINE (37, 29)-(40, 32), k000, BF
LINE (37, 33)-(40, 36), k000, BF
LINE (37, 37)-(40, 40), k000, BF
LINE (37, 41)-(40, 44), k000, BF
LINE (37, 45)-(40, 48), k000, BF
LINE (41, 13)-(44, 16), k000, BF
LINE (41, 17)-(44, 20), k000, BF
LINE (41, 21)-(44, 24), k000, BF
LINE (45, 17)-(48, 20), k000, BF
LINE (45, 21)-(48, 24), k000, BF
LINE (45, 25)-(48, 28), k000, BF
LINE (49, 13)-(52, 16), k000, BF
LINE (49, 17)-(52, 20), k000, BF
LINE (49, 21)-(52, 24), k000, BF
LINE (53, 9)-(56, 12), k000, BF
LINE (53, 13)-(56, 16), k000, BF
LINE (53, 17)-(56, 20), k000, BF
LINE (53, 21)-(56, 24), k000, BF
LINE (53, 25)-(56, 28), k000, BF
LINE (53, 29)-(56, 32), k000, BF
LINE (53, 33)-(56, 36), k000, BF
LINE (53, 37)-(56, 40), k000, BF
LINE (53, 41)-(56, 44), k000, BF
LINE (53, 45)-(56, 48), k000, BF
LINE (57, 9)-(60, 12), k000, BF
LINE (57, 13)-(60, 16), k000, BF
LINE (57, 17)-(60, 20), k000, BF
LINE (57, 21)-(60, 24), k000, BF
LINE (57, 25)-(60, 28), k000, BF
LINE (57, 29)-(60, 32), k000, BF
LINE (57, 33)-(60, 36), k000, BF
LINE (57, 37)-(60, 40), k000, BF
LINE (57, 41)-(60, 44), k000, BF
LINE (57, 45)-(60, 48), k000, BF
LINE (73, 9)-(76, 12), k000, BF
LINE (73, 45)-(76, 48), k000, BF
LINE (77, 9)-(80, 12), k000, BF
LINE (77, 13)-(80, 16), k000, BF
LINE (77, 17)-(80, 20), k000, BF
LINE (77, 21)-(80, 24), k000, BF
LINE (77, 25)-(80, 28), k000, BF
LINE (77, 29)-(80, 32), k000, BF
LINE (77, 33)-(80, 36), k000, BF
LINE (77, 37)-(80, 40), k000, BF
LINE (77, 41)-(80, 44), k000, BF
LINE (77, 45)-(80, 48), k000, BF
LINE (81, 9)-(84, 12), k000, BF
LINE (81, 13)-(84, 16), k000, BF
LINE (81, 17)-(84, 20), k000, BF
LINE (81, 21)-(84, 24), k000, BF
LINE (81, 25)-(84, 28), k000, BF
LINE (81, 29)-(84, 32), k000, BF
LINE (81, 33)-(84, 36), k000, BF
LINE (81, 37)-(84, 40), k000, BF
LINE (81, 41)-(84, 44), k000, BF
LINE (81, 45)-(84, 48), k000, BF
LINE (85, 9)-(88, 12), k000, BF
LINE (85, 45)-(88, 48), k000, BF
LINE (97, 9)-(100, 12), k000, BF
LINE (97, 45)-(100, 48), k000, BF
LINE (101, 9)-(104, 12), k000, BF
LINE (101, 13)-(104, 16), k000, BF
LINE (101, 17)-(104, 20), k000, BF
LINE (101, 21)-(104, 24), k000, BF
LINE (101, 25)-(104, 28), k000, BF
LINE (101, 29)-(104, 32), k000, BF
LINE (101, 33)-(104, 36), k000, BF
LINE (101, 37)-(104, 40), k000, BF
LINE (101, 41)-(104, 44), k000, BF
LINE (101, 45)-(104, 48), k000, BF
LINE (105, 9)-(108, 12), k000, BF
LINE (105, 13)-(108, 16), k000, BF
LINE (105, 17)-(108, 20), k000, BF
LINE (105, 21)-(108, 24), k000, BF
LINE (105, 25)-(108, 28), k000, BF
LINE (105, 29)-(108, 32), k000, BF
LINE (105, 33)-(108, 36), k000, BF
LINE (105, 37)-(108, 40), k000, BF
LINE (105, 41)-(108, 44), k000, BF
LINE (105, 45)-(108, 48), k000, BF
LINE (109, 9)-(112, 12), k000, BF
LINE (109, 45)-(112, 48), k000, BF
LINE (113, 9)-(116, 12), k000, BF
LINE (113, 13)-(116, 16), k000, BF
LINE (113, 41)-(116, 44), k000, BF
LINE (113, 45)-(116, 48), k000, BF
LINE (117, 13)-(120, 16), k000, BF
LINE (117, 17)-(120, 20), k000, BF
LINE (117, 21)-(120, 24), k000, BF
LINE (117, 25)-(120, 28), k000, BF
LINE (117, 29)-(120, 32), k000, BF
LINE (117, 33)-(120, 36), k000, BF
LINE (117, 37)-(120, 40), k000, BF
LINE (117, 41)-(120, 44), k000, BF
LINE (121, 17)-(124, 20), k000, BF
LINE (121, 21)-(124, 24), k000, BF
LINE (121, 25)-(124, 28), k000, BF
LINE (121, 29)-(124, 32), k000, BF
LINE (121, 33)-(124, 36), k000, BF
LINE (121, 37)-(124, 40), k000, BF

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
WHILE s2 < (b + a) * (b1 + a) - 5
FOR x = a TO b
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(x, d): PSET (x, d), 0
NEXT x
FOR y = a TO b1
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(b, y): PSET (b, y), 0
NEXT y
FOR x0 = c TO d STEP -1
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(x0, b1): PSET (x0, b1), 0
NEXT x0
FOR y0 = c1 TO d STEP -1
s1 = s1 + 1
s2 = s2 + 1
nogh(s1) = POINT(d, y0): PSET (d, y0), 0
NEXT y0

p0 = 2 * (b - d + b1 - d)
IF b1 - d <> 0 THEN ne = (b - d) / (b1 - d)
'r0 = p0 / (2 * PI): r1 = r0
r0 = p0 / (PI * (ne + 1)): r1 = ((p0 / PI) - r0)
z0 = 270 - ((((b - d) * 180) / (b - d + b1 - d)) / 2)
'LOCATE 24, 2: PRINT z0
FOR sh = 1 TO s1
z0 = z0 + (360 / p0)
PSET (x000 + (r1 * COS(z0 * (PI / 180))), y000 + (r0 * SIN(z0 * (PI / 180)))), nogh(sh)
NEXT sh
s1 = 0

a = a + 1
d = d + 1
c = c - 1
c1 = c1 - 1
b = b - 1
b1 = b1 - 1
WEND
e = POINT(b0 / 2, b0 / 2): PSET (b0 / 2, b0 / 2), 0
PSET (320, 240), e
SLEEP
END
cir: CIRCLE (x000, y000), r0, k0, , , b1 / b: k0 = 14: RETURN

