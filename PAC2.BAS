1 CLS : SCREEN 12: WINDOW (-320, -240)-(320, 240):  k = 40
Input x
Input y
If y > x Then Swap y, x
xn = x
yn = y
2 Cls: x = x * k
y = y * k
v = x / 2
v1 = y / 2
f = Sqr((v ^ 2) - (v1 ^ 2))
z = Sqr((v - f) ^ 2 + (-v1) ^ 2)
z1 = (x * y) / z / 2
g = v - f
g2 = x - g
Print CLng(100 * z / k) / 100, CLng(100 * z1 / k) / 100, CLng(100 * g / k) / 100, CLng(100 * g2 / k) / 100
Line (-v, v1)-(v, -v1), 2, B
Line (-f, -v1)-(-v, 0), 4
Line -(f, v1), 4
Line -(v, 0), 4
Line -(-f, -v1), 4
While ak$ <> Chr$(27)
    ak$ = InKey$
    If ak$ = Chr$(45) Then k = k / 1.1: x = xn: y = yn: GoTo 2
    If ak$ = Chr$(43) Then k = k * 1.1: x = xn: y = yn: GoTo 2
Wend

