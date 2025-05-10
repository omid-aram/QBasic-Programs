Cls
PI = 4 * Atn(1)
Input "n = ", n0
Dim n1(n0)
Dim name$(n0)
Dim z0(n0)
Dim z1(n0)
For n = 1 To n0
    Input "name,number = "; name$(n), n1(n)
    s0 = s0 + n1(n)
Next n
Screen 12: Window (0, 0)-(639, 479)
ch0 = 1
Paint (1, 1), 7
For n = 1 To n0
    f0 = f0 + n1(n)
    z1(n) = f0 * 360 / s0
    z0(n) = f0 * 2 * PI / s0
    Circle (320, 240), 200, 0, 0, -z0(n), ch0
Next n
For n = 1 To n0
    t1 = (z0(n) - z0(n - 1)) / 2 + z0(n - 1)
    GoSub 10
    Paint (320 + 100 * Cos(t1), 240 + (100 * ch0) * Sin(t1)), t0$, 0
Next n
End
10 t0$ = ""
Randomize Timer
For i0 = 1 To 8
    i00 = Int(255 * Rnd)
    t0$ = t0$ + Chr$(i00)
Next i0
Return

