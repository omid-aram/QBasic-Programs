0 g = 1: g1 = 0: g2 = 0: x = 1: y = 1: t100 = 0
1 CLS : SCREEN 12: k = 9: k1 = 4: k2 = 2: k12 = 13: n5 = 10: n = 0
GOSUB 1000
CONST PI = 3.141592654#
IF g = 0 THEN g = g + .25
WHILE n < 360
LINE ((318 + g1) + (200 / g) * COS(n * (PI / 180)), (231 + g2) + (200 / g) * SIN(n * (PI / 180)))-((318 + g1) + ((205 + n5) / g) * COS(n * (PI / 180)), (231 + g2) + ((205 + n5) / g) * SIN(n * (PI / 180))), k12:
n = n + 6: n5 = 0: k12 = 5
n3 = n3 + 1: IF n3 = 5 THEN n5 = 10: n3 = 0: k12 = 13
WEND
GOSUB 40
GOSUB 400
18  LOCATE 4, 70: PRINT "U": k0 = 15: y0 = 48: x0 = 632: GOSUB usa
    LOCATE 5, 70: PRINT "I": k0 = 15: y0 = 64: x0 = 632: GOSUB niran
    LOCATE 6, 70: PRINT "E": k0 = 15: y0 = 80: x0 = 632: GOSUB eng: k0 = 7: y0 = 96: x0 = 624: GOSUB isl: k0 = 7: y0 = 112: x0 = 624: GOSUB prt
    LOCATE 9, 70: PRINT "F": k0 = 15: y0 = 128: x0 = 632: GOSUB fra: k0 = 7: y0 = 144: x0 = 624: GOSUB sow: k0 = 7: y0 = 160: x0 = 624: GOSUB ita: k0 = 7: y0 = 176: x0 = 624: GOSUB grm: k0 = 7: y0 = 192: x0 = 624: GOSUB otr: k0 = 7: y0 = 208: x0 = 624: GOSUB hld: k0 = 7: y0 = 224: x0 = 624: GOSUB esp
    LOCATE 16, 70: PRINT "A": k0 = 15: y0 = 240: x0 = 632: GOSUB ara: k0 = 7: y0 = 256: x0 = 624: GOSUB qtr: k0 = 7: y0 = 272: x0 = 624: GOSUB trk
    LOCATE 19, 70: PRINT "J": k0 = 15: y0 = 288: x0 = 632: GOSUB jpa: k0 = 7: y0 = 304: x0 = 624: GOSUB krj
    LOCATE 21, 70: PRINT "T": k0 = 15: y0 = 320: x0 = 632: GOSUB ast
    LOCATE 22, 70: PRINT "R": k0 = 15: y0 = 336: x0 = 632: GOSUB arj: k0 = 7: y0 = 352: x0 = 624: GOSUB brz
WHILE 1
    LOCATE y, x: PRINT TIME$
    GOSUB iran
    a$ = INKEY$
    IF a$ = CHR$(27) THEN END
    IF a$ = CHR$(85) THEN t100 = -41400: COLOR 3: LOCATE 4, 70: PRINT "U": k0 = 14: y0 = 48: x0 = 632: GOSUB usa: GOSUB 100
    IF a$ = CHR$(73) THEN t100 = 0:  COLOR 3: LOCATE 5, 70: PRINT "I": k0 = 14: y0 = 64: x0 = 632: GOSUB niran: GOSUB 100
    IF a$ = CHR$(69) THEN t100 = -12600: COLOR 3: LOCATE 6, 70: PRINT "E": k0 = 14: y0 = 80: x0 = 632: GOSUB eng: k0 = 6: y0 = 96: x0 = 624: GOSUB isl: k0 = 6: y0 = 112: x0 = 624: GOSUB prt: GOSUB 100
    IF a$ = CHR$(70) THEN t100 = -9000: COLOR 3: LOCATE 9, 70: PRINT "F": k0 = 14: y0 = 128: x0 = 632: GOSUB fra: k0 = 6: y0 = 144: x0 = 624: GOSUB sow: k0 = 6: y0 = 160: x0 = 624: GOSUB ita: k0 = 6: y0 = 176: x0 = 624: GOSUB grm: k0 = 6: y0 = 192: x0 = 624: GOSUB otr: k0 = 6: y0 = 208: x0 = 624: GOSUB hld: k0 = 6: y0 = 224: x0 = 624: GOSUB esp: GOSUB 100
    IF a$ = CHR$(65) THEN t100 = -1800: COLOR 3: LOCATE 16, 70: PRINT "A": k0 = 14: y0 = 240: x0 = 632: GOSUB ara: k0 = 6: y0 = 256: x0 = 624: GOSUB qtr: k0 = 6: y0 = 272: x0 = 624: GOSUB trk: GOSUB 100
    IF a$ = CHR$(74) THEN t100 = 19800: COLOR 3: LOCATE 19, 70: PRINT "J": k0 = 14: y0 = 288: x0 = 632: GOSUB jpa: k0 = 6: y0 = 304: x0 = 624: GOSUB krj: GOSUB 100
    IF a$ = CHR$(84) THEN t100 = 23400: COLOR 3: LOCATE 21, 70: PRINT "T": k0 = 14: y0 = 320: x0 = 632: GOSUB ast: GOSUB 100
    IF a$ = CHR$(82) THEN t100 = -23400: COLOR 3: LOCATE 22, 70: PRINT "R": k0 = 14: y0 = 336: x0 = 632: GOSUB arj: k0 = 6: y0 = 352: x0 = 624: GOSUB brz: GOSUB 100
WEND
END
100 WHILE 1
    a$ = INKEY$
    IF a$ = CHR$(27) THEN COLOR 15: GOTO 0
    LOCATE y, x: PRINT TIME$
    GOSUB 30: GOSUB 10: GOSUB 20
    s6 = c
    m6 = c1
    h6 = a
    GOSUB 40
    COLOR 15: LOCATE 3, 1: PRINT ; h; ":"; m; ":"; s; "    "
    s7 = c - s6
    IF c <> s6 THEN k1 = 0: c = c - s7: GOSUB 20: c = c + s7
    m7 = c1 - m6
    IF c1 <> m6 THEN k2 = 0: c1 = c1 - m7: GOSUB 30: c1 = c1 + m7
    h7 = a - h6
    IF a <> h6 THEN k = 0: a = a - h7: GOSUB 10: a = a + h7
    GOSUB iran
WEND
RETURN
10 LINE ((318 + g1), (231 + g2))-((318 + g1) + (100 / g) * COS(a * (PI / 180)), (231 + g2) + (100 / g) * SIN(a * (PI / 180))), k: k = 9: RETURN
20 LINE ((318 + g1), (231 + g2))-((318 + g1) + (198 / g) * COS(c * (PI / 180)), (231 + g2) + (198 / g) * SIN(c * (PI / 180))), k1: k1 = 4: RETURN
30 LINE ((318 + g1), (231 + g2))-((318 + g1) + (150 / g) * COS(c1 * (PI / 180)), (231 + g2) + (150 / g) * SIN(c1 * (PI / 180))), k2: k2 = 2: RETURN
40 tim = TIMER: t10 = tim \ 1: t = t10 + t100: GOSUB dh: h = t \ 3600: m2 = t MOD 3600: m = m2 \ 60: s = m2 MOD 60: c = (s - 15) * 6: c1 = (m - 15) * 6: a11 = (h - 15) * 30: a12 = m \ 12: a13 = a12 * 6: a = a11 + a13: RETURN
101 LINE ((318 + g10), (231 + g20))-((318 + g10) + (100 / g70) * COS(a1 * (PI / 180)), (231 + g20) + (100 / g70) * SIN(a1 * (PI / 180))), k9: k9 = 9: RETURN
200 LINE ((318 + g10), (231 + g20))-((318 + g10) + (198 / g70) * COS(c11 * (PI / 180)), (231 + g20) + (198 / g70) * SIN(c11 * (PI / 180))), k91: k91 = 4: RETURN
300 LINE ((318 + g10), (231 + g20))-((318 + g10) + (150 / g70) * COS(c111 * (PI / 180)), (231 + g20) + (150 / g70) * SIN(c111 * (PI / 180))), k92: k92 = 2: RETURN
400 tim = TIMER: t10 = tim \ 1: t1 = t10: h1 = t1 \ 3600: m22 = t1 MOD 3600: m1 = m22 \ 60: s1 = m22 MOD 60: c11 = (s1 - 15) * 6: c111 = (m1 - 15) * 6: a111 = (h1 - 15) * 30: a122 = m1 \ 12: a133 = a122 * 6: a1 = a111 + a133: RETURN
END
1000 g70 = 3: g10 = -250: g20 = 160: k120 = 5: n50 = 10: n70 = 0
WHILE n70 < 360
LINE ((318 + g10) + (200 / g70) * COS(n70 * (PI / 180)), (231 + g20) + (200 / g70) * SIN(n70 * (PI / 180)))-((318 + g10) + ((205 + n50) / g70) * COS(n70 * (PI / 180)), (231 + g20) + ((205 + n50) / g70) * SIN(n70 * (PI / 180))), k120:
n70 = n70 + 6: n50 = 0: k120 = 13
n30 = n30 + 1: IF n30 = 5 THEN n50 = 10: n30 = 0: k120 = 5
WEND
RETURN
iran: GOSUB 300: GOSUB 101: GOSUB 200
    s60 = c11
    m60 = c111
    h60 = a1
    GOSUB 400
    s70 = c11 - s60
    IF c11 <> s60 THEN k91 = 0: c11 = c11 - s70: GOSUB 200: c11 = c11 + s70
    m70 = c111 - m60
    IF c111 <> m60 THEN k92 = 0: c111 = c111 - m70: GOSUB 300: c111 = c111 + m70
    h70 = a1 - h60
    IF a1 <> h60 THEN k9 = 0: a1 = a1 - h70: GOSUB 101: a1 = a1 + h70
RETURN
dh: IF t < 0 THEN t = t + 86400: RETURN
IF t > 86400 THEN t = t - 86400: RETURN
RETURN


niran: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
32 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
12 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
1111 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
2929 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 6 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: PSET (4 + x0, 6 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN
usa: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (4 + x0, 4 + y0)-(4 + x0, 9 + y0), k0: LINE (2 + x0, 4 + y0)-(2 + x0, 2 + y0), k0: LINE -(6 + x0, 2 + y0), k0: LINE -(6 + x0, 1 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 8 + y0)-(7 + x0, 8 + y0), k0: LINE -(7 + x0, 6 + y0), k0: LINE (6 + x0, 5 + y0)-(6 + x0, 5 + y0), k0: LINE (5 + x0, 6 + y0)-(5 + x0, 7 + y0), k0: LINE (4 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 17:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (1 + x0, 8 + y0)-(7 + x0, 8 + y0), k0: LINE (7 + x0, 7 + y0)-(5 + x0, 4 + y0), k0: LINE (5 + x0, 3 + y0)-(7 + x0, 0 + y0), k0: : n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

eng: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: n = 18:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (1 + x0, 8 + y0)-(7 + x0, 8 + y0), k0: LINE (7 + x0, 7 + y0)-(5 + x0, 4 + y0), k0: LINE (5 + x0, 3 + y0)-(7 + x0, 0 + y0), k0: : LINE (4 + x0, 1 + y0)-(3 + x0, 2 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 16:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (5 + x0, 8 + y0)-(5 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: : n = 7:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 6 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: PSET (4 + x0, 6 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

fra: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 5 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: PSET (6 + x0, 4 + y0), k0: PSET (5 + x0, 5 + y0), k0: LINE (6 + x0, 6 + y0)-(5 + x0, 6 + y0), k0: : PSET (5 + x0, 2 + y0), k0: n = 2:  x0 = x0 - 8
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: n = 18:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (5 + x0, 8 + y0)-(5 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: : n = 7:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 8 + y0)-(6 + x0, 8 + y0), k0: LINE (5 + x0, 7 + y0)-(5 + x0, 4 + y0), k0: LINE -(3 + x0, 6 + y0), k0: LINE -(3 + x0, 8 + y0), k0: LINE -(4 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

ara: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: LINE (4 + x0, 5 + y0)-(5 + x0, 5 + y0), k0: n = 11:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (3 + x0, 11 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (5 + x0, 8 + y0)-(5 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: : n = 7:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 6 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: PSET (4 + x0, 6 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

jpa: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : PSET (6 + x0, 3 + y0), k0: PSET (4 + x0, 3 + y0), k0: PSET (5 + x0, 2 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: PSET (3 + x0, 12 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 6 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: PSET (4 + x0, 6 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

ast: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (5 + x0, 8 + y0)-(5 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: : n = 7:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 16:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

sow: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (5 + x0, 8 + y0)-(5 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: : n = 7:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 8 + y0)-(5 + x0, 8 + y0), k0: LINE -(5 + x0, 6 + y0), k0: PSET (6 + x0, 5 + y0), k0: LINE (7 + x0, 6 + y0)-(7 + x0, 9 + y0), k0: LINE -(5 + x0, 11 + y0), k0: LINE (4 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : LINE (5 + x0, 2 + y0)-(3 + x0, 4 + y0), k0: LINE (4 + x0, 3 + y0)-(3 + x0, 2 + y0), k0: LINE -(4 + x0, 1 + y0), k0: n = 22:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (5 + x0, 4 + y0)-(7 + x0, 6 + y0), k0: LINE -(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(4 + x0, 8 + y0), k0: : n = 1:  x0 = x0 - 8:
RETURN

isl: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (5 + x0, 8 + y0)-(5 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: : n = 7:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 16:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: n = 18:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (5 + x0, 4 + y0)-(7 + x0, 6 + y0), k0: LINE -(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(4 + x0, 8 + y0), k0: : n = 1:  x0 = x0 - 8:
RETURN

prt: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: PSET (3 + x0, 12 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (6 + x0, 7 + y0)-(4 + x0, 7 + y0), k0: LINE (6 + x0, 6 + y0)-(3 + x0, 6 + y0), k0: PSET (5 + x0, 3 + y0), k0: n = 14:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

ita: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 16:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

grm: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (4 + x0, 4 + y0)-(4 + x0, 9 + y0), k0: LINE (2 + x0, 4 + y0)-(2 + x0, 2 + y0), k0: LINE -(6 + x0, 2 + y0), k0: LINE -(6 + x0, 1 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 16:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 8 + y0)-(7 + x0, 8 + y0), k0: LINE -(7 + x0, 6 + y0), k0: LINE (6 + x0, 5 + y0)-(6 + x0, 5 + y0), k0: LINE (5 + x0, 6 + y0)-(5 + x0, 7 + y0), k0: LINE (4 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 17:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 6 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: PSET (4 + x0, 6 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

otr: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(5 + x0, 8 + y0), k0: LINE (4 + x0, 7 + y0)-(4 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(2 + x0, 8 + y0), k0: LINE (1 + x0, 6 + y0)-(1 + x0, 10 + y0), k0: : PSET (6 + x0, 3 + y0), k0: PSET (4 + x0, 3 + y0), k0: PSET (5 + x0, 2 + y0), k0: x0 = x0 - 8: n = 1:  :
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (8 + x0, 11 + y0)-(4 + x0, 11 + y0), k0: LINE (3 + x0, 10 + y0)-(3 + x0, 7 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

hld: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 8 + y0)-(0 + x0, 8 + y0), k0: LINE (7 + x0, 7 + y0)-(7 + x0, 6 + y0), k0: LINE (6 + x0, 5 + y0)-(3 + x0, 5 + y0), k0: LINE -(3 + x0, 4 + y0), k0: LINE (2 + x0, 6 + y0)-(2 + x0, 8 + y0), k0: LINE (4 + x0, 5 + y0)-(4 + x0, 7 + y0), k0: n = 19:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: n = 16:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: n = 18:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (5 + x0, 4 + y0)-(7 + x0, 6 + y0), k0: LINE -(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(4 + x0, 8 + y0), k0: : n = 1:  x0 = x0 - 8:
RETURN

esp: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (5 + x0, 8 + y0)-(5 + x0, 6 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 6 + y0), k0: : n = 7:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: PSET (3 + x0, 12 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: n = 18:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (6 + x0, 1 + y0)-(6 + x0, 7 + y0), k0: LINE (7 + x0, 8 + y0)-(8 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

qtr: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 5 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: PSET (6 + x0, 4 + y0), k0: PSET (5 + x0, 5 + y0), k0: LINE (6 + x0, 6 + y0)-(5 + x0, 6 + y0), k0: : PSET (5 + x0, 2 + y0), k0: PSET (7 + x0, 2 + y0), k0: n = 15:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: LINE (7 + x0, 7 + y0)-(7 + x0, 6 + y0), k0: LINE (6 + x0, 5 + y0)-(4 + x0, 5 + y0), k0: LINE (3 + x0, 8 + y0)-(3 + x0, 1 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
RETURN

krj: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (1 + x0, 8 + y0)-(7 + x0, 8 + y0), k0: LINE (7 + x0, 7 + y0)-(5 + x0, 4 + y0), k0: LINE (5 + x0, 3 + y0)-(7 + x0, 0 + y0), k0: : n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 8 + y0)-(3 + x0, 8 + y0), k0: LINE (6 + x0, 7 + y0)-(6 + x0, 6 + y0), k0: LINE -(4 + x0, 4 + y0), k0: LINE (3 + x0, 7 + y0)-(5 + x0, 5 + y0), k0: n = 1:  x0 = x0 - 8: x0 = x0 - 8
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (3 + x0, 6 + y0)-(4 + x0, 5 + y0), k0: LINE (5 + x0, 5 + y0)-(7 + x0, 8 + y0), k0: LINE -(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: n = 3:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: n = 18:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 8 + y0)-(5 + x0, 8 + y0), k0: LINE -(5 + x0, 6 + y0), k0: PSET (6 + x0, 5 + y0), k0: LINE (7 + x0, 6 + y0)-(7 + x0, 9 + y0), k0: LINE -(5 + x0, 11 + y0), k0: LINE (4 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (3 + x0, 11 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 8 + y0)-(5 + x0, 8 + y0), k0: PSET (4 + x0, 9 + y0), k0: LINE (5 + x0, 10 + y0)-(7 + x0, 10 + y0), k0: LINE -(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 9 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

arj: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (4 + x0, 4 + y0)-(4 + x0, 9 + y0), k0: LINE (2 + x0, 4 + y0)-(2 + x0, 2 + y0), k0: LINE -(6 + x0, 2 + y0), k0: LINE -(6 + x0, 1 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : PSET (6 + x0, 3 + y0), k0: PSET (4 + x0, 3 + y0), k0: PSET (5 + x0, 2 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (5 + x0, 1 + y0)-(5 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: n = 18:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 6 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: PSET (4 + x0, 6 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

brz: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (3 + x0, 11 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : PSET (6 + x0, 3 + y0), k0: n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 1 + y0)-(7 + x0, 11 + y0), k0: LINE (6 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: LINE (2 + x0, 11 + y0)-(2 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

trk: LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 3 + y0), k0: PSET (2 + x0, 3 + y0), k0: n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (7 + x0, 7 + y0)-(7 + x0, 10 + y0), k0: PSET (6 + x0, 11 + y0), k0: LINE (5 + x0, 12 + y0)-(3 + x0, 12 + y0), k0: : n = 1:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF:  LINE (1 + x0, 8 + y0)-(7 + x0, 8 + y0), k0: LINE (7 + x0, 7 + y0)-(5 + x0, 4 + y0), k0: LINE (5 + x0, 3 + y0)-(7 + x0, 0 + y0), k0: : n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 5 + y0)-(7 + x0, 7 + y0), k0: LINE (6 + x0, 8 + y0)-(1 + x0, 8 + y0), k0: : PSET (4 + x0, 11 + y0), k0: PSET (2 + x0, 11 + y0), k0: n = 21:  x0 = x0 - 8:
 LINE (0 + x0, 0 + y0)-(8 + x0, 14 + y0), 0, BF: LINE (7 + x0, 8 + y0)-(6 + x0, 8 + y0), k0: LINE (5 + x0, 7 + y0)-(5 + x0, 4 + y0), k0: LINE -(3 + x0, 6 + y0), k0: LINE -(3 + x0, 8 + y0), k0: LINE -(4 + x0, 8 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN

