Cls: Screen 12
PI = 4 * Atn(1)
rb = 100: rk = 10
y0 = .1: k0 = 12: z1 = 120: z2 = z1: z3 = 180 - z2: r1 = ((z1 * (rb - rk)) / 180) + rk: r2 = ((z3 * (rb - rk)) / 180) + rk: s1 = 2
c1 = 100: c2 = 300: m1 = 320: m2 = 0
While ak$ <> Chr$(27)
    ak$ = InKey$
    For r0 = r1 To r2 Step -s1
        x1 = m1 + c1 * Cos(z1 * (PI / 180))
        y1 = m2 + c2 * Sin(z1 * (PI / 180))
        'PRINT z1, r0: SLEEP 1

        k0 = 12
        10 Line (m1, m2)-(x1, y1), k0
        Line (x1, y1 - r0)-(x1, y1 + r0), k0
        For l = y0 To 1 Step y0
            'PRINT l
            Circle (x1, y1), r0, k0, , , 1 / l
        Next l

        Sleep (1)

        For l1 = 0 To 1 Step y0
            Circle (x1, y1), r0, k0, , , l1
        Next l1
        If k0 = 12 Then k0 = 0: GoTo 10
        z1 = z1 - (z2 - z3) / ((r1 - r2) / s1)
    Next r0

    For r0 = r2 To r1 Step s1

        z1 = z1 + (z2 - z3) / ((r1 - r2) / s1)
        x1 = m1 + c1 * Cos(z1 * (PI / 180))
        y1 = m2 + c2 * Sin(z1 * (PI / 180))
        'PRINT z1, r0: SLEEP 1

        k0 = 12
        11 Line (m1, m2)-(x1, y1), k0
        Line (x1, y1 - r0)-(x1, y1 + r0), k0
        For l = y0 To 1 Step y0
            Circle (x1, y1), r0, k0, , , 1 / l
        Next l

        Sleep (1)

        For l1 = 0 To 1 Step y0
            Circle (x1, y1), r0, k0, , , l1
        Next l1
        If k0 = 12 Then k0 = 0: GoTo 11

    Next r0
Wend

