CLS
PI = 4 * ATN(1)
INPUT "n = ", n0
DIM n1(n0)
DIM name$(n0)
DIM z0(n0)
DIM z1(n0)
FOR n = 1 TO n0
INPUT "name,number = "; name$(n), n1(n)
s0 = s0 + n1(n)
NEXT n
SCREEN 12: WINDOW (0, 0)-(639, 479)
ch0 = 1
PAINT (1, 1), 7
FOR n = 1 TO n0
f0 = f0 + n1(n)
z1(n) = f0 * 360 / s0
z0(n) = f0 * 2 * PI / s0
CIRCLE (320, 240), 200, 0, 0, -z0(n), ch0
NEXT n
FOR n = 1 TO n0
t1 = (z0(n) - z0(n - 1)) / 2 + z0(n - 1)
GOSUB 10
PAINT (320 + 100 * COS(t1), 240 + (100 * ch0) * SIN(t1)), t0$, 0
NEXT n
END
10 t0$ = ""
RANDOMIZE TIMER
FOR i0 = 1 TO 8
i00 = INT(255 * RND)
t0$ = t0$ + CHR$(i00)
NEXT i0
RETURN

