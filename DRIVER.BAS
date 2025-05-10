n5 = 143: n6 = 80: n7 = 120: g = 4: p = 0: l = 0: d = .01: b = 150: r = .01: m2 = -2: so = 137
21 Screen 1: Cls: Color 0, 0: Randomize Timer: Open "DRIVER_names.txt" For Input As #2: Line Input #2, n$: Locate 19, 2: Print n$: Close: Open "DRIVER_score.txt" For Input As #1: Input #1, y1: Locate 21, 1: Print y1: Close
If g = -1 Then y = p + (s * 1000) + (l * 10000): GoSub 22: GoSub 23: GoSub 24: GoSub 25: GoSub 38: Locate 1, 1: Print "GAME OVER": GoSub 108: End
Locate 4, 34: If m2 < 0 Then Print Chr$(14) Else GoSub pake
Locate 13, 38: Print "6": Locate 15, 36: Print "Right": Line (296, 108)-(310, 107), , BF: Line -(306, 105): Line (310, 108)-(306, 110): Locate 13, 31: Print "4": Locate 15, 29: Print "Left": Line (248, 108)-(233, 107), , BF: Line -(238, 105): Line (234, 108)-(238, 110): Locate 7, 34: Print "8": Locate 10, 32: Print "Turbo": Line (266, 70)-(267, 60), , BF: Line -(269, 62): Line (266, 60)-(264, 62): Locate 20, 30: Print "p => pause": Locate 2, 30: Print "m => music": Locate 1, 1: Print "hearth ="; g: Locate 3, 1: Print "point ="; p: Locate 5, 1: Print "speed ="; s: Locate 7, 1: Print "level ="; l: Line (100, 0)-(102, 200), 3, BF: Line (218, 0)-(220, 200), 3, BF: a = 143: c = 2: GoSub 10: u = -80: c = 1: GoSub 111: u1 = u - 80: c1 = 1: GoSub 112: u2 = u1 - 80: c2 = 1: GoSub 113: ak$ = Input$(1): If ak$ = Chr$(111) Then ak$ = Input$(1): If ak$ = Chr$(77) Then ak$ = Input$(1): If ak$ = Chr$(105) Then ak$ = Input$(1): If ak$ = Chr$(68) Then n5 = 0: n6 = 0: n7 = 0: Circle (40, 100), 20, 2: Circle (40, 100), 19, 2: Circle (40, 100), 18, 2: Circle (33, 94), 3, 2: Circle (47, 94), 3, 2: Circle (40, 100), 10, 2, 3.2, 6.22: Circle (40, 100), 9, 2, 3.2, 6.22
If ak$ = Chr$(72) Then ak$ = Input$(1): If ak$ = Chr$(104) Then GoSub h
While ak$ <> Chr$(27): If ak$ = Chr$(112) Then c6 = 1: GoSub 5555: ak$ = Input$(1): c6 = 0: GoSub 5555: If ak$ = Chr$(71) Then ak$ = Input$(1): If ak$ = Chr$(103) Then GoSub 6666
    If p > 989 Then s = s + 1: r = r - .0009999999#: GoSub 51: i% = 4: GoSub 57: GoSub 23: p = 0: GoSub 22
    If s = 10 Then l = l + 1: GoSub 24: s = 0: r = .01: GoSub 23: GoSub 51: GoSub 58: b = b - 40: g = g + 1: GoTo 21
    If l = 4 Then End
    If u > 120 Then u = u2 - 80: GoSub 111
    If u1 > 120 Then u1 = u - 80: GoSub 112
    If u2 > 120 Then u2 = u1 - 80: GoSub 113
    'PRINT ASC("m")
    ak$ = InKey$: If ak$ = Chr$(56) Then d = d - 1200 Else d = r
    start = Timer: While Timer < start + d: Wend: c = 0: GoSub 11: u = u + 10: GoSub 11: c1 = 0: GoSub 12: u1 = u1 + 10: GoSub 12: c2 = 0: GoSub 13: u2 = u2 + 10: GoSub 13
    If a > 107 Then If ak$ = Chr$(52) Then c = 0: GoSub 10: a = a - 36: GoSub 10
    If a < 179 Then If ak$ = Chr$(54) Then c = 0: GoSub 10: a = a + 36: GoSub 10
    If ak$ = Chr$(109) Then m2 = -1 * m2: If so = 137 Then so = 0 Else so = 137
    Locate 4, 34: If ak$ = Chr$(109) Then If m2 < 0 Then Print Chr$(14) Else GoSub pake
    If 40 + b = 80 + u Or 40 + b = 80 + u1 Or 40 + b = 80 + u2 Then p = p + 10: GoSub 22
    For f = b To 39 + b Step 10
        If a = n5 + w And f = n6 + u Or a = n5 + w And f = n7 + u Then Sound so, 2: d10 = 8.61D-16: GoSub 29: ak$ = Input$(1): g = g - 1: GoTo 21
        If a = n5 + w1 And f = n6 + u1 Or a = n5 + w1 And f = n7 + u1 Then Sound so, 2: d10 = 8.61D-16: GoSub 29: ak$ = Input$(1): g = g - 1: GoTo 21
        If a = n5 + w2 And f = n6 + u2 Or a = n5 + w2 And f = n7 + u2 Then Sound so, 2: d10 = 8.61D-16: GoSub 29: ak$ = Input$(1): g = g - 1: GoTo 21
Next f: Wend: End
10 Line (7 + a, 10 + b)-(27 + a, 40 + b), c, BF: Line (5 + a, 11 + b)-(a, 18 + b), c, BF: Line (29 + a, 38 + b)-(34 + a, 31 + b), c, BF: Line (29 + a, 11 + b)-(34 + a, 18 + b), c, BF: Line (5 + a, 38 + b)-(a, 31 + b), c, BF: Line (12 + a, 8 + b)-(22 + a, b), c, BF: Line (12 + a, 40 + b)-(22 + a, 37 + b), 0, BF: c = 2: Return
111 k = Int(3 * Rnd) + 1: On k GOSUB 1, 2, 3
11 Line (150 + w, 90 + u)-(170 + w, 120 + u), c, BF: Line (148 + w, 91 + u)-(143 + w, 98 + u), c, BF: Line (172 + w, 118 + u)-(177 + w, 111 + u), c, BF: Line (172 + w, 91 + u)-(177 + w, 98 + u), c, BF: Line (148 + w, 118 + u)-(143 + w, 111 + u), c, BF: Line (155 + w, 88 + u)-(165 + w, 80 + u), c, BF: Line (155 + w, 120 + u)-(165 + w, 117 + u), 0, BF: c = 1: Return
112 k1 = Int(3 * Rnd) + 1: On k1 GOSUB 4, 5, 6
12 Line (150 + w1, 90 + u1)-(170 + w1, 120 + u1), c1, BF: Line (148 + w1, 91 + u1)-(143 + w1, 98 + u1), c1, BF: Line (172 + w1, 118 + u1)-(177 + w1, 111 + u1), c1, BF: Line (172 + w1, 91 + u1)-(177 + w1, 98 + u1), c1, BF: Line (148 + w1, 118 + u1)-(143 + w1, 111 + u1), c1, BF: Line (155 + w1, 88 + u1)-(165 + w1, 80 + u1), c1, BF: Line (155 + w1, 120 + u1)-(165 + w1, 117 + u1), 0, BF: c1 = 1: Return
113 k2 = Int(3 * Rnd) + 1: On k2 GOSUB 7, 8, 9
13 Line (150 + w2, 90 + u2)-(170 + w2, 120 + u2), c2, BF: Line (148 + w2, 91 + u2)-(143 + w2, 98 + u2), c2, BF: Line (172 + w2, 118 + u2)-(177 + w2, 111 + u2), c2, BF: Line (172 + w2, 91 + u2)-(177 + w2, 98 + u2), c2, BF: Line (148 + w2, 118 + u2)-(143 + w2, 111 + u2), c2, BF: Line (155 + w2, 88 + u2)-(165 + w2, 80 + u2), c2, BF: Line (155 + w2, 120 + u2)-(165 + w2, 117 + u2), 0, BF: c2 = 1: Return
1 w = -36: Return
2 w = 0: Return
3 w = 36: Return
4 w1 = -36: Return
5 w1 = 0: Return
6 w1 = 36: Return
7 w2 = -36: Return
8 w2 = 0: Return
9 w2 = 36: Return
22 Locate 3, 1: Print "point ="; p: Return
23 Locate 5, 1: Print "speed ="; s: Return
24 Locate 7, 1: Print "level ="; l: Return
25 Locate 11, 1: Print "Your Score ="; y: Return
29 a1 = 200: While a1 > -1: Line (103, a1)-(217, a1), 2: start = Timer: While Timer < start + d10: Wend: a1 = a1 - 2: Wend: b1 = 1: While b1 < 201: Line (103, b1)-(217, b1), 1: start = Timer: While Timer < start + d10: Wend: b1 = b1 + 2: Wend: Return
51 If m2 = 2 Then Return
scale$ = "CDEFGAB": Play "L16": Return
58 If m2 = 2 Then Return
For i% = 0 To 6: GoSub 57: Next i%: Return
57 If m2 = 2 Then Return
Play "O" + Str$(i%): 'Play "X" + VarPtr$(scale$) ': Return
Play "CDEFGAB": Return
38 If m2 = 2 Then Return
Music$ = "MBT180o2P2P8L8GGGL2E-P24P8L8FFFL2D": Play Music$: While Play(0) > 5: Wend: Return
h: Cls: Print "You pressed Hh to see this cheatsheet!": Print "Enter oMiD while the game will be start": Print "After prees p then enter Gg": ak$ = Input$(1): GoSub 21
108 Open "DRIVER_score.txt" For Output As #1: If y > y1 Then Print #1, y Else Print #1, y1
If y > y1 Then Open "DRIVER_names.txt" For Output As #2: Locate 15, 1: Line Input "Your Name ?"; n$: Print #2, n$
Return
6666 Input g1: g = g + g1: If g > 999 Then g = 999
GoSub 21
pake: Line (264, 24)-(271, 31), 0, BF: Return
5555 Circle (266, 180), 10, c6, , , .2: Circle (266, 180), 10, c6, 3.14, 6.28, 1: Circle (280, 182), 3, c6: Circle (266, 190), 20, c6, 2, 1.2, .2: Paint (266, 183), c6, c6: Return
Line (49 + w, 49 + u)-(55 + w, 27 + u), 2: Line (55 + w, 27 + u)-(62 + w, 49 + u), 2: Line (62 + w, 49 + u)-(44 + w, 34 + u), 2: Line (44 + w, 34 + u)-(67 + w, 34 + u), 2: Line (67 + w, 34 + u)-(48 + w, 50 + u), 2: Return

