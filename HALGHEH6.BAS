Cls: Screen 12
PI = 4 * Atn(1)
a = 1: b = 125: b1 = 57: c = b - 1: c1 = b1 - 1: d = a - 1: a0 = a: b0 = b
Dim nogh(2 * (b + b1))

p0 = 2 * (b - d + b1 - d)
If b1 - d <> 0 Then ne = (b - d) / (b1 - d)
r0 = p0 / (PI * (ne + 1)): r1 = ((p0 / PI) - r0)
If b > b1 Then r0 = r1
x000 = 320: y000 = 340
While ak$ <> Chr$(13)
    ak$ = InKey$
    Line (0, 0)-(b, b1), 14, B
    GoSub cir
    If ak$ = Chr$(54) Then k0 = 0: GoSub cir: x000 = x000 + 10: GoSub cir
    If ak$ = Chr$(52) Then k0 = 0: GoSub cir: x000 = x000 - 10: GoSub cir
    If ak$ = Chr$(56) Then k0 = 0: GoSub cir: y000 = y000 - 10: GoSub cir
    If ak$ = Chr$(50) Then k0 = 0: GoSub cir: y000 = y000 + 10: GoSub cir
Wend
Cls
Line (0, 0)-(b, b1), 4, BF
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
Line (1, 17)-(4, 20), k000, BF
Line (1, 21)-(4, 24), k000, BF
Line (1, 25)-(4, 28), k000, BF
Line (1, 29)-(4, 32), k000, BF
Line (1, 33)-(4, 36), k000, BF
Line (1, 37)-(4, 40), k000, BF
Line (5, 13)-(8, 16), k000, BF
Line (5, 17)-(8, 20), k000, BF
Line (5, 21)-(8, 24), k000, BF
Line (5, 25)-(8, 28), k000, BF
Line (5, 29)-(8, 32), k000, BF
Line (5, 33)-(8, 36), k000, BF
Line (5, 37)-(8, 40), k000, BF
Line (5, 41)-(8, 44), k000, BF
Line (9, 9)-(12, 12), k000, BF
Line (9, 13)-(12, 16), k000, BF
Line (9, 41)-(12, 44), k000, BF
Line (9, 45)-(12, 48), k000, BF
Line (13, 9)-(16, 12), k000, BF
Line (13, 45)-(16, 48), k000, BF
Line (17, 9)-(20, 12), k000, BF
Line (17, 13)-(20, 16), k000, BF
Line (17, 41)-(20, 44), k000, BF
Line (17, 45)-(20, 48), k000, BF
Line (21, 13)-(24, 16), k000, BF
Line (21, 17)-(24, 20), k000, BF
Line (21, 21)-(24, 24), k000, BF
Line (21, 25)-(24, 28), k000, BF
Line (21, 29)-(24, 32), k000, BF
Line (21, 33)-(24, 36), k000, BF
Line (21, 37)-(24, 40), k000, BF
Line (21, 41)-(24, 44), k000, BF
Line (25, 17)-(28, 20), k000, BF
Line (25, 21)-(28, 24), k000, BF
Line (25, 25)-(28, 28), k000, BF
Line (25, 29)-(28, 32), k000, BF
Line (25, 33)-(28, 36), k000, BF
Line (25, 37)-(28, 40), k000, BF
Line (33, 9)-(36, 12), k000, BF
Line (33, 13)-(36, 16), k000, BF
Line (33, 17)-(36, 20), k000, BF
Line (33, 21)-(36, 24), k000, BF
Line (33, 25)-(36, 28), k000, BF
Line (33, 29)-(36, 32), k000, BF
Line (33, 33)-(36, 36), k000, BF
Line (33, 37)-(36, 40), k000, BF
Line (33, 41)-(36, 44), k000, BF
Line (33, 45)-(36, 48), k000, BF
Line (37, 9)-(40, 12), k000, BF
Line (37, 13)-(40, 16), k000, BF
Line (37, 17)-(40, 20), k000, BF
Line (37, 21)-(40, 24), k000, BF
Line (37, 25)-(40, 28), k000, BF
Line (37, 29)-(40, 32), k000, BF
Line (37, 33)-(40, 36), k000, BF
Line (37, 37)-(40, 40), k000, BF
Line (37, 41)-(40, 44), k000, BF
Line (37, 45)-(40, 48), k000, BF
Line (41, 13)-(44, 16), k000, BF
Line (41, 17)-(44, 20), k000, BF
Line (41, 21)-(44, 24), k000, BF
Line (45, 17)-(48, 20), k000, BF
Line (45, 21)-(48, 24), k000, BF
Line (45, 25)-(48, 28), k000, BF
Line (49, 13)-(52, 16), k000, BF
Line (49, 17)-(52, 20), k000, BF
Line (49, 21)-(52, 24), k000, BF
Line (53, 9)-(56, 12), k000, BF
Line (53, 13)-(56, 16), k000, BF
Line (53, 17)-(56, 20), k000, BF
Line (53, 21)-(56, 24), k000, BF
Line (53, 25)-(56, 28), k000, BF
Line (53, 29)-(56, 32), k000, BF
Line (53, 33)-(56, 36), k000, BF
Line (53, 37)-(56, 40), k000, BF
Line (53, 41)-(56, 44), k000, BF
Line (53, 45)-(56, 48), k000, BF
Line (57, 9)-(60, 12), k000, BF
Line (57, 13)-(60, 16), k000, BF
Line (57, 17)-(60, 20), k000, BF
Line (57, 21)-(60, 24), k000, BF
Line (57, 25)-(60, 28), k000, BF
Line (57, 29)-(60, 32), k000, BF
Line (57, 33)-(60, 36), k000, BF
Line (57, 37)-(60, 40), k000, BF
Line (57, 41)-(60, 44), k000, BF
Line (57, 45)-(60, 48), k000, BF
Line (73, 9)-(76, 12), k000, BF
Line (73, 45)-(76, 48), k000, BF
Line (77, 9)-(80, 12), k000, BF
Line (77, 13)-(80, 16), k000, BF
Line (77, 17)-(80, 20), k000, BF
Line (77, 21)-(80, 24), k000, BF
Line (77, 25)-(80, 28), k000, BF
Line (77, 29)-(80, 32), k000, BF
Line (77, 33)-(80, 36), k000, BF
Line (77, 37)-(80, 40), k000, BF
Line (77, 41)-(80, 44), k000, BF
Line (77, 45)-(80, 48), k000, BF
Line (81, 9)-(84, 12), k000, BF
Line (81, 13)-(84, 16), k000, BF
Line (81, 17)-(84, 20), k000, BF
Line (81, 21)-(84, 24), k000, BF
Line (81, 25)-(84, 28), k000, BF
Line (81, 29)-(84, 32), k000, BF
Line (81, 33)-(84, 36), k000, BF
Line (81, 37)-(84, 40), k000, BF
Line (81, 41)-(84, 44), k000, BF
Line (81, 45)-(84, 48), k000, BF
Line (85, 9)-(88, 12), k000, BF
Line (85, 45)-(88, 48), k000, BF
Line (97, 9)-(100, 12), k000, BF
Line (97, 45)-(100, 48), k000, BF
Line (101, 9)-(104, 12), k000, BF
Line (101, 13)-(104, 16), k000, BF
Line (101, 17)-(104, 20), k000, BF
Line (101, 21)-(104, 24), k000, BF
Line (101, 25)-(104, 28), k000, BF
Line (101, 29)-(104, 32), k000, BF
Line (101, 33)-(104, 36), k000, BF
Line (101, 37)-(104, 40), k000, BF
Line (101, 41)-(104, 44), k000, BF
Line (101, 45)-(104, 48), k000, BF
Line (105, 9)-(108, 12), k000, BF
Line (105, 13)-(108, 16), k000, BF
Line (105, 17)-(108, 20), k000, BF
Line (105, 21)-(108, 24), k000, BF
Line (105, 25)-(108, 28), k000, BF
Line (105, 29)-(108, 32), k000, BF
Line (105, 33)-(108, 36), k000, BF
Line (105, 37)-(108, 40), k000, BF
Line (105, 41)-(108, 44), k000, BF
Line (105, 45)-(108, 48), k000, BF
Line (109, 9)-(112, 12), k000, BF
Line (109, 45)-(112, 48), k000, BF
Line (113, 9)-(116, 12), k000, BF
Line (113, 13)-(116, 16), k000, BF
Line (113, 41)-(116, 44), k000, BF
Line (113, 45)-(116, 48), k000, BF
Line (117, 13)-(120, 16), k000, BF
Line (117, 17)-(120, 20), k000, BF
Line (117, 21)-(120, 24), k000, BF
Line (117, 25)-(120, 28), k000, BF
Line (117, 29)-(120, 32), k000, BF
Line (117, 33)-(120, 36), k000, BF
Line (117, 37)-(120, 40), k000, BF
Line (117, 41)-(120, 44), k000, BF
Line (121, 17)-(124, 20), k000, BF
Line (121, 21)-(124, 24), k000, BF
Line (121, 25)-(124, 28), k000, BF
Line (121, 29)-(124, 32), k000, BF
Line (121, 33)-(124, 36), k000, BF
Line (121, 37)-(124, 40), k000, BF

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
While s2 < (b + a) * (b1 + a) - 5
    Sleep 1
    For x = a To b
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(x, d) ': PSet (x, d), 0
    Next x
    For y = a To b1
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(b, y) ': PSet (b, y), 0
    Next y
    For x0 = c To d Step -1
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(x0, b1) ': PSet (x0, b1), 0
    Next x0
    For y0 = c1 To d Step -1
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(d, y0) ': PSet (d, y0), 0
    Next y0

    p0 = 2 * (b - d + b1 - d)
    If b1 - d <> 0 Then ne = (b - d) / (b1 - d)
    'r0 = p0 / (2 * PI): r1 = r0
    r0 = p0 / (PI * (ne + 1)): r1 = ((p0 / PI) - r0)
    z0 = 270 - ((((b - d) * 180) / (b - d + b1 - d)) / 2)
    'LOCATE 24, 2: PRINT z0
    For sh = 1 To s1
        z0 = z0 + (360 / p0)
        PSet (x000 + (r1 * Cos(z0 * (PI / 180))), y000 + (r0 * Sin(z0 * (PI / 180)))), nogh(sh)
    Next sh
    s1 = 0

    a = a + 1
    d = d + 1
    c = c - 1
    c1 = c1 - 1
    b = b - 1
    b1 = b1 - 1
Wend
e = Point(b0 / 2, b0 / 2) ': PSet (b0 / 2, b0 / 2), 0
PSet (320, 240), e
Sleep
End
cir: Circle (x000, y000), r0, k0, , , b1 / b: k0 = 14: Return

