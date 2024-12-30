1 CLS : SCREEN 12: n = 0: k = 0: z1 = 0
CONST PI = 3.141592654#
INPUT a
IF FIX(a / 2) <> a / 2 THEN n = -90
z = 360 / a
GOSUB 20
WHILE n < 360
IF k = 7 THEN k = 0
n = n + z1
z1 = z
k = k + 1
GOSUB 10
LINE (x, y)-(x1, y1), k
GOSUB 20
WEND
LOCATE 30, 1: PRINT "DO YOU WANT TO TRY AGAIN ?": ak$ = INPUT$(1): IF ak$ = CHR$(121) THEN GOTO 1 ELSE END
10 x = 318 + 200 * COS(n * (PI / 180))
y = 231 + 200 * SIN(n * (PI / 180)): RETURN
20 x1 = 318 + 200 * COS(n * (PI / 180))
y1 = 231 + 200 * SIN(n * (PI / 180)): RETURN

