CLS : SCREEN 12
PI = 4 * ATN(1)
rb = 100: rk = 10
y0 = .1: k0 = 12: z1 = 120: z2 = z1: z3 = 180 - z2: r1 = ((z1 * (rb - rk)) / 180) + rk: r2 = ((z3 * (rb - rk)) / 180) + rk: s1 = 2
c1 = 100: c2 = 300: m1 = 320: m2 = 0
WHILE ak$ <> CHR$(27)
ak$ = INKEY$
FOR r0 = r1 TO r2 STEP -s1
x1 = m1 + c1 * COS(z1 * (PI / 180))
y1 = m2 + c2 * SIN(z1 * (PI / 180))
'PRINT z1, r0: SLEEP 1

k0 = 12
10 LINE (m1, m2)-(x1, y1), k0
LINE (x1, y1 - r0)-(x1, y1 + r0), k0
FOR l = y0 TO 1 STEP y0
'PRINT l
CIRCLE (x1, y1), r0, k0, , , 1 / l
NEXT l
FOR l1 = 0 TO 1 STEP y0
CIRCLE (x1, y1), r0, k0, , , l1
NEXT l1
IF k0 = 12 THEN k0 = 0: GOTO 10
z1 = z1 - (z2 - z3) / ((r1 - r2) / s1)
NEXT r0

FOR r0 = r2 TO r1 STEP s1

z1 = z1 + (z2 - z3) / ((r1 - r2) / s1)
x1 = m1 + c1 * COS(z1 * (PI / 180))
y1 = m2 + c2 * SIN(z1 * (PI / 180))
'PRINT z1, r0: SLEEP 1

k0 = 12
11 LINE (m1, m2)-(x1, y1), k0
LINE (x1, y1 - r0)-(x1, y1 + r0), k0
FOR l = y0 TO 1 STEP y0
CIRCLE (x1, y1), r0, k0, , , 1 / l
NEXT l
FOR l1 = 0 TO 1 STEP y0
CIRCLE (x1, y1), r0, k0, , , l1
NEXT l1
IF k0 = 12 THEN k0 = 0: GOTO 11

NEXT r0
WEND

