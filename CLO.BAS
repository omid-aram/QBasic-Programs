g = 1: g1 = 0: g2 = 0: x = 1: y = 1
1 CLS : SCREEN 12: k = 9: k1 = 4: k2 = 2: k12 = 13: n5 = 10: n = 0
CONST PI = 3.141592654#
'LOCATE 1, 40: PRINT "12": LOCATE 15, 69: PRINT "3": LOCATE 28, 40: PRINT "6": LOCATE 15, 11: PRINT "9"
IF g = 0 THEN g = g + .25
WHILE n < 360
LINE ((318 + g1) + (200 / g) * COS(n * (PI / 180)), (231 + g2) + (200 / g) * SIN(n * (PI / 180)))-((318 + g1) + ((205 + n5) / g) * COS(n * (PI / 180)), (231 + g2) + ((205 + n5) / g) * SIN(n * (PI / 180))), k12:
n = n + 6: n5 = 0: k12 = 5
n3 = n3 + 1: IF n3 = 5 THEN n5 = 10: n3 = 0: k12 = 13
WEND
GOSUB 40
WHILE 1
    IF g = 1 AND g1 = 0 AND g2 = 0 THEN GOSUB 60
    LOCATE y, x: PRINT TIME$: LOCATE y, x + 8: PRINT USING ".##"; TIMER - FIX(TIMER): LOCATE 1, 12: PRINT "    "
    GOSUB 24: GOSUB 30: GOSUB 10: GOSUB 20
    a$ = INKEY$
    IF a$ = CHR$(27) THEN END
    IF a$ = CHR$(121) THEN a$ = INPUT$(1)
    IF a$ = CHR$(43) THEN g = g - .25: GOTO 1
    IF a$ = CHR$(45) THEN g = g + .25: GOTO 1
    IF a$ = CHR$(50) THEN g2 = g2 + 10: GOTO 1
    IF a$ = CHR$(52) THEN g1 = g1 - 10: GOTO 1
    IF a$ = CHR$(54) THEN g1 = g1 + 10: GOTO 1
    IF a$ = CHR$(56) THEN g2 = g2 - 10: GOTO 1
    IF a$ = CHR$(51) THEN x = x + 1: GOTO 1
    IF a$ = CHR$(49) THEN x = x - 1: GOTO 1
    IF a$ = CHR$(53) THEN y = y - 1: GOTO 1
    IF a$ = CHR$(46) THEN y = y + 1: GOTO 1
    s8 = c2
    s6 = c
    m6 = c1
    h6 = a
    GOSUB 40
    GOSUB 50
    s9 = c2 - s8
    IF c2 <> s8 THEN k3 = 0: c2 = c2 - s9: GOSUB 24: c2 = c2 + s9
    s7 = c - s6
    IF c <> s6 THEN k1 = 0: c = c - s7: GOSUB 20: c = c + s7
    m7 = c1 - m6
    IF c1 <> m6 THEN k2 = 0: c1 = c1 - m7: GOSUB 30: c1 = c1 + m7
    h7 = a - h6
    IF a <> h6 THEN k = 0: a = a - h7: GOSUB 10: a = a + h7
    WEND
END
10 LINE ((318 + g1), (231 + g2))-((318 + g1) + (100 / g) * COS(a * (PI / 180)), (231 + g2) + (100 / g) * SIN(a * (PI / 180))), k: k = 9: RETURN
24 CIRCLE (318 + g1, 231 + g2), 50 / g, k3, 0, c2: k3 = 14: RETURN
20 LINE ((318 + g1), (231 + g2))-((318 + g1) + (198 / g) * COS(c * (PI / 180)), (231 + g2) + (198 / g) * SIN(c * (PI / 180))), k1: LINE ((318 + g1), (231 + g2))-((318 + g1) + (28 / g) * -COS(c * (PI / 180)), (231 + g2) + (28 / g) * -SIN(c * (PI / 180))), k1: k1 = 4: RETURN
30 LINE ((318 + g1), (231 + g2))-((318 + g1) + (150 / g) * COS(c1 * (PI / 180)), (231 + g2) + (150 / g) * SIN(c1 * (PI / 180))), k2: k2 = 2: RETURN
40 tim = TIMER: t = tim \ 1: h = t \ 3600: m2 = t MOD 3600: m = m2 \ 60: s = m2 MOD 60: c = (s - 15) * 6: c1 = (m - 15) * 6: a11 = (h - 15) * 30: a12 = m \ 12: a13 = a12 * 6: a = a11 + a13:  RETURN
50 tim = TIMER: t1 = tim \ 1: t12 = tim - t1: c2 = t12 * 12.56637061#: RETURN
60 LOCATE 8, 32: PRINT "OMID KHOSHNIAT ARAM": RETURN

