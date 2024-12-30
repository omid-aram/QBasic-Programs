n5 = 143: n6 = 80: n7 = 120: g = 4: p = 0: l = 0: d = .01: b = 150: r = .01: m2 = -2: so = 137
21 SCREEN 1: CLS : COLOR 0, 0: RANDOMIZE TIMER: OPEN "names" FOR INPUT AS #2: LINE INPUT #2, n$: LOCATE 19, 2: PRINT n$: CLOSE : OPEN "score" FOR INPUT AS #1: INPUT #1, y1: LOCATE 21, 1: PRINT y1: CLOSE
IF g = -1 THEN y = p + (s * 1000) + (l * 10000): GOSUB 22: GOSUB 23: GOSUB 24: GOSUB 25: GOSUB 38: LOCATE 1, 1: PRINT "GAME OVER": GOSUB 108: END
LOCATE 4, 34: IF m2 < 0 THEN PRINT CHR$(14) ELSE GOSUB pake
LOCATE 13, 38: PRINT "6": LOCATE 15, 36: PRINT "Right": LINE (296, 108)-(310, 107), , BF: LINE -(306, 105): LINE (310, 108)-(306, 110): LOCATE 13, 31: PRINT "4": LOCATE 15, 29: PRINT "Left": LINE (248, 108)-(233, 107), , BF: LINE -(238, 105): LINE (234, 108)-(238, 110): LOCATE 7, 34: PRINT "8": LOCATE 10, 32: PRINT "Turbo": LINE (266, 70)-(267, 60), , BF: LINE -(269, 62): LINE (266, 60)-(264, 62): LOCATE 20, 30: PRINT "p => pause": LOCATE 2, 30: PRINT "m => music": LOCATE 1, 1: PRINT "hearth ="; g: LOCATE 3, 1: PRINT "point ="; p: LOCATE 5, 1: PRINT "speed ="; s: LOCATE 7, 1: PRINT "level ="; l: LINE (100, 0)-(102, 200), 3, BF: LINE (218, 0)-(220, 200), 3, BF: a = 143: c = 2: GOSUB 10: u = -80: c = 1: GOSUB 111: u1 = u - 80: c1 = 1: GOSUB 112: u2 = u1 - 80: c2 = 1: GOSUB 113: ak$ = INPUT$(1): IF ak$ = CHR$(111) THEN ak$ = INPUT$(1): IF ak$ = CHR$(77) THEN ak$ = INPUT$(1): IF ak$ = CHR$(105) THEN ak$ = INPUT$(1): IF ak$ = CHR$(68) THEN n5 = 0: n6 = 0: n7 = 0: CIRCLE (40, 100), 20, 2: CIRCLE (40, 100), 19, 2: CIRCLE (40, 100), 18, 2: CIRCLE (33, 94), 3, 2: CIRCLE (47, 94), 3, 2: CIRCLE (40, 100), 10, 2, 3.2, 6.22: CIRCLE (40, 100), 9, 2, 3.2, 6.22
IF ak$ = CHR$(72) THEN ak$ = INPUT$(1): IF ak$ = CHR$(104) THEN GOSUB h
WHILE ak$ <> CHR$(27): IF ak$ = CHR$(112) THEN c6 = 1: GOSUB 5555: ak$ = INPUT$(1): c6 = 0: GOSUB 5555: IF ak$ = CHR$(71) THEN ak$ = INPUT$(1): IF ak$ = CHR$(103) THEN GOSUB 6666
IF p > 989 THEN s = s + 1: r = r - .0009999999#: GOSUB 51: i% = 4: GOSUB 57: GOSUB 23: p = 0: GOSUB 22
IF s = 10 THEN l = l + 1: GOSUB 24: s = 0: r = .01: GOSUB 23: GOSUB 51: GOSUB 58: b = b - 40: g = g + 1: GOTO 21
IF l = 4 THEN END
IF u > 120 THEN u = u2 - 80: GOSUB 111
IF u1 > 120 THEN u1 = u - 80: GOSUB 112
IF u2 > 120 THEN u2 = u1 - 80: GOSUB 113
'PRINT ASC("m")
ak$ = INKEY$: IF ak$ = CHR$(56) THEN d = d - 1200 ELSE d = r
start = TIMER: WHILE TIMER < start + d: WEND: c = 0: GOSUB 11: u = u + 10: GOSUB 11: c1 = 0: GOSUB 12: u1 = u1 + 10: GOSUB 12: c2 = 0: GOSUB 13: u2 = u2 + 10: GOSUB 13
IF a > 107 THEN IF ak$ = CHR$(52) THEN c = 0: GOSUB 10: a = a - 36: GOSUB 10
IF a < 179 THEN IF ak$ = CHR$(54) THEN c = 0: GOSUB 10: a = a + 36: GOSUB 10
IF ak$ = CHR$(109) THEN m2 = -1 * m2: IF so = 137 THEN so = 0 ELSE so = 137
LOCATE 4, 34: IF ak$ = CHR$(109) THEN IF m2 < 0 THEN PRINT CHR$(14) ELSE GOSUB pake
IF 40 + b = 80 + u OR 40 + b = 80 + u1 OR 40 + b = 80 + u2 THEN p = p + 10: GOSUB 22
FOR f = b TO 39 + b STEP 10
IF a = n5 + w AND f = n6 + u OR a = n5 + w AND f = n7 + u THEN SOUND so, 2: d10 = 8.61D-16: GOSUB 29: ak$ = INPUT$(1): g = g - 1: GOTO 21
IF a = n5 + w1 AND f = n6 + u1 OR a = n5 + w1 AND f = n7 + u1 THEN SOUND so, 2: d10 = 8.61D-16: GOSUB 29: ak$ = INPUT$(1): g = g - 1: GOTO 21
IF a = n5 + w2 AND f = n6 + u2 OR a = n5 + w2 AND f = n7 + u2 THEN SOUND so, 2: d10 = 8.61D-16: GOSUB 29: ak$ = INPUT$(1): g = g - 1: GOTO 21
NEXT f: WEND: END
10 LINE (7 + a, 10 + b)-(27 + a, 40 + b), c, BF: LINE (5 + a, 11 + b)-(a, 18 + b), c, BF: LINE (29 + a, 38 + b)-(34 + a, 31 + b), c, BF: LINE (29 + a, 11 + b)-(34 + a, 18 + b), c, BF: LINE (5 + a, 38 + b)-(a, 31 + b), c, BF: LINE (12 + a, 8 + b)-(22 + a, b), c, BF: LINE (12 + a, 40 + b)-(22 + a, 37 + b), 0, BF: c = 2: RETURN
111 k = INT(3 * RND) + 1: ON k GOSUB 1, 2, 3
11 LINE (150 + w, 90 + u)-(170 + w, 120 + u), c, BF: LINE (148 + w, 91 + u)-(143 + w, 98 + u), c, BF: LINE (172 + w, 118 + u)-(177 + w, 111 + u), c, BF: LINE (172 + w, 91 + u)-(177 + w, 98 + u), c, BF: LINE (148 + w, 118 + u)-(143 + w, 111 + u), c, BF: LINE (155 + w, 88 + u)-(165 + w, 80 + u), c, BF: LINE (155 + w, 120 + u)-(165 + w, 117 + u), 0, BF: c = 1: RETURN
112 k1 = INT(3 * RND) + 1: ON k1 GOSUB 4, 5, 6
12 LINE (150 + w1, 90 + u1)-(170 + w1, 120 + u1), c1, BF: LINE (148 + w1, 91 + u1)-(143 + w1, 98 + u1), c1, BF: LINE (172 + w1, 118 + u1)-(177 + w1, 111 + u1), c1, BF: LINE (172 + w1, 91 + u1)-(177 + w1, 98 + u1), c1, BF: LINE (148 + w1, 118 + u1)-(143 + w1, 111 + u1), c1, BF: LINE (155 + w1, 88 + u1)-(165 + w1, 80 + u1), c1, BF: LINE (155 + w1, 120 + u1)-(165 + w1, 117 + u1), 0, BF: c1 = 1: RETURN
113 k2 = INT(3 * RND) + 1: ON k2 GOSUB 7, 8, 9
13 LINE (150 + w2, 90 + u2)-(170 + w2, 120 + u2), c2, BF: LINE (148 + w2, 91 + u2)-(143 + w2, 98 + u2), c2, BF: LINE (172 + w2, 118 + u2)-(177 + w2, 111 + u2), c2, BF: LINE (172 + w2, 91 + u2)-(177 + w2, 98 + u2), c2, BF: LINE (148 + w2, 118 + u2)-(143 + w2, 111 + u2), c2, BF: LINE (155 + w2, 88 + u2)-(165 + w2, 80 + u2), c2, BF: LINE (155 + w2, 120 + u2)-(165 + w2, 117 + u2), 0, BF: c2 = 1: RETURN
1 w = -36: RETURN
2 w = 0: RETURN
3 w = 36: RETURN
4 w1 = -36: RETURN
5 w1 = 0: RETURN
6 w1 = 36: RETURN
7 w2 = -36: RETURN
8 w2 = 0: RETURN
9 w2 = 36: RETURN
22 LOCATE 3, 1: PRINT "point ="; p: RETURN
23 LOCATE 5, 1: PRINT "speed ="; s: RETURN
24 LOCATE 7, 1: PRINT "level ="; l: RETURN
25 LOCATE 11, 1: PRINT "Your Score ="; y: RETURN
29 a1 = 200: WHILE a1 > -1: LINE (103, a1)-(217, a1), 2: start = TIMER: WHILE TIMER < start + d10: WEND: a1 = a1 - 2: WEND: b1 = 1: WHILE b1 < 201: LINE (103, b1)-(217, b1), 1: start = TIMER: WHILE TIMER < start + d10: WEND: b1 = b1 + 2: WEND: RETURN
51 IF m2 = 2 THEN RETURN
scale$ = "CDEFGAB": PLAY "L16": RETURN
58 IF m2 = 2 THEN RETURN
FOR i% = 0 TO 6: GOSUB 57: NEXT i%: RETURN
57 IF m2 = 2 THEN RETURN
PLAY "O" + STR$(i%): PLAY "X" + VARPTR$(scale$): RETURN
38 IF m2 = 2 THEN RETURN
Music$ = "MBT180o2P2P8L8GGGL2E-P24P8L8FFFL2D": PLAY Music$: WHILE PLAY(0) > 5: WEND: RETURN
h: CLS : PRINT "Enter oMiD while the game will be start": PRINT "After prees p then enter Gg": ak$ = INPUT$(1): GOSUB 21
108 OPEN "score" FOR OUTPUT AS #1: IF y > y1 THEN PRINT #1, y ELSE PRINT #1, y1
IF y > y1 THEN OPEN "names" FOR OUTPUT AS #2: LOCATE 15, 1: LINE INPUT "Your Name ?"; n$: PRINT #2, n$
RETURN
6666 INPUT g1: g = g + g1: IF g > 999 THEN g = 999
GOSUB 21
pake: LINE (264, 24)-(271, 31), 0, BF: RETURN
5555 CIRCLE (266, 180), 10, c6, , , .2: CIRCLE (266, 180), 10, c6, 3.14, 6.28, 1: CIRCLE (280, 182), 3, c6: CIRCLE (266, 190), 20, c6, 2, 1.2, .2: PAINT (266, 183), c6, c6: RETURN
LINE (49 + w, 49 + u)-(55 + w, 27 + u), 2: LINE (55 + w, 27 + u)-(62 + w, 49 + u), 2: LINE (62 + w, 49 + u)-(44 + w, 34 + u), 2: LINE (44 + w, 34 + u)-(67 + w, 34 + u), 2: LINE (67 + w, 34 + u)-(48 + w, 50 + u), 2: RETURN

