Cls: Screen 12
PI = 4 * Atn(1)
a = 1: b = 100: c = b - 1: d = a - 1: a0 = a: b0 = b
Dim nogh(4 * (b - d))

Line (0, 0)-(100, 100), 14, BF
Line (0, 0)-(50, 50), 12, BF
Line (0, 100)-(100, 50), 14, BF
Line (100, 0)-(50, 50), 13, BF
Circle (50, 50), 25, 4
Paint (50, 50), 2, 4


While s2 < (b + a) ^ 2 - 5
    Sleep 1
    For x = a To b
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(x, d) ': PSet (x, d), 0
    Next x
    For y = a To b
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(b, y) ': PSet (b, y), 0
    Next y
    For x0 = c To d Step -1
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(x0, b) ': PSet (x0, b), 0
    Next x0
    For y0 = c To d Step -1
        s1 = s1 + 1
        s2 = s2 + 1
        nogh(s1) = Point(d, y0) ': PSet (d, y0), 0
    Next y0

    p0 = 4 * (b - d)
    'xa = (PI * (b - d + 1)) / (2 * SQR(PI) * (b - d))
    xa = 1
    r0 = xa * (p0 / (2 * PI))
    r1 = p0 / (2 * PI) '+ 50
    'r0 = SQR(((b + 1) ^ 2) / PI)
    z0 = -135
    'z0 = ATN((b1 / b)) / (PI / 180) + 180
    'z0 = 270 - ((((b - d) * 180) / (b - d + b - d)) / 2)
    For sh = 1 To s1
        z0 = z0 + (360 / p0)
        PSet (320 + (r0 * Cos(z0 * (PI / 180))), 240 + (r0 * Sin(z0 * (PI / 180)))), nogh(sh)
        'PSET (500 + (r1 * COS(z0 * (PI / 180))), 240 + (r1 * SIN(z0 * (PI / 180)))), nogh(sh)
    Next sh
    s1 = 0

    a = a + 1
    d = d + 1
    c = c - 1
    b = b - 1
Wend
e = Point(b0 / 2, b0 / 2)': PSet (b0 / 2, b0 / 2), 0
PSet (320, 240), e


