1 Cls: Screen 12: Window (-320, -240)-(320, 240): k = 40
Print "Input Width of Envelope: ": Input x
Print "Input Height of Envelope: ": Input y
xn = x: yn = y
2 Cls: x = x * k
y = y * k
GoSub 657
'
f2 = (z / 2) - f
hx = -1 * (z / 2) + (z1 / 2) * ((z1 / 2) / (z / 2))
hy = (z1 / 2) * ((f2) / (z / 2) * -1)
k1 = 0: k11 = 0: k111 = 0: k1111 = 0: k2 = 2: k22 = 2: k222 = 2: k2222 = 2
While ak$ <> Chr$(27)
    ak$ = InKey$
    GoSub 10: GoSub 20: GoSub 30: GoSub 40
    If ak$ = Chr$(45) Then k = k / 1.1: x = xn: y = yn: GoTo 2
    If ak$ = Chr$(43) Then k = k * 1.1: x = xn: y = yn: GoTo 2
    If ak$ = Chr$(50) Then k1 = 4: k2 = 0
    If ak$ = Chr$(52) Then k11 = 4: k22 = 0
    If ak$ = Chr$(54) Then k111 = 4: k222 = 0
    If ak$ = Chr$(56) Then k1111 = 4: k2222 = 0
    If ak$ = Chr$(48) Then GoSub 131
Wend
Locate 30, 1: Print "DO YOU WANT TO TRY AGAIN ?": ak$ = Input$(1): If ak$ = Chr$(121) Then GoTo 1 Else End
''LINE ((z / 2) - (f), (z1 / 2))-(-1 * (z / 2) + (f), -1 * (z1 / 2)), 4
10 Line (-1 * (z / 2), 0)-(hx, hy), k1
Line -((z / 2) - (f), (z1 / 2)), k1
Line ((z / 2) - (f), (z1 / 2))-(-1 * (z / 2), (z1 / 2)), k2: Line -(-1 * (z / 2), 0), k2
Return
20 Line ((z / 2), 0)-(-1 * hx, -1 * hy), k11
Line -((z / 2) - (f), (z1 / 2)), k11
Line ((z / 2), 0)-((z / 2), (z1 / 2)), k22: Line -((z / 2) - (f), (z1 / 2)), k22
Return
30 Line (-1 * (z / 2), 0)-(hx, hy), k111
Line -(-1 * (z / 2) + (f), -1 * (z1 / 2)), k111
Line (-1 * (z / 2), 0)-(-1 * (z / 2), -1 * (z1 / 2)), k222: Line -(-1 * (z / 2) + (f), -1 * (z1 / 2)), k222
Return
40 Line ((z / 2), 0)-(-1 * hx, -1 * hy), k1111
Line -(-1 * (z / 2) + (f), -1 * (z1 / 2)), k1111
Line (-1 * (z / 2) + (f), -1 * (z1 / 2))-((z / 2), -1 * (z1 / 2)), k2222: Line -((z / 2), 0), k2222
Return

131 While ak$ = Chr$(48)
    ak$ = Input$(1)
    GoSub 10: GoSub 20: GoSub 30: GoSub 40
    If ak$ = Chr$(56) Then k1 = 0: k2 = 2
    If ak$ = Chr$(54) Then k11 = 0: k22 = 2
    If ak$ = Chr$(52) Then k111 = 0: k222 = 2
    If ak$ = Chr$(50) Then k1111 = 0: k2222 = 2
Wend
Return

657 s = (x * y) * 2
z = ((x ^ 2) + (y ^ 2)) ^ .5
z1 = s / z
f = ((x ^ 2) - ((z1 / 2) ^ 2)) ^ .5
f1 = z - f
Print "This is your required paper dimension: "
Print z / k, z1 / k, f / k, f1 / k
Print: Print "Use NumArrows(2,4,6,8,+,-) to seal the envelope!"
Line ((z / 2), (z1 / 2))-(-1 * (z / 2), -1 * (z1 / 2)), 2, B
Line (-1 * (z / 2), 0)-(-1 * (z / 2) + (f), -1 * (z1 / 2)), 3
Line -((z / 2), 0), 3
Line -((z / 2) - (f), (z1 / 2)), 3
Line -(-1 * (z / 2), 0), 3
Return

