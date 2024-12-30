CLS : SCREEN 12: WINDOW (-319, -239)-(320, 240)
DEF fnf (x) = x / 640 * pn
DEF fng (x) = x / 480 * pq

PI = 4 * ATN(1)
f0 = 414: k0 = 12: a = 50: B = 50: GOSUB 10
WHILE ak$ <> CHR$(13)
ak$ = INPUT$(1)
IF a < 88 THEN IF ak$ = CHR$(52) THEN k0 = 0: GOSUB 10: a = a + 2: GOSUB 10
IF a > 2 THEN IF ak$ = CHR$(54) THEN k0 = 0: GOSUB 10: a = a - 2: GOSUB 10
IF B > 2 THEN IF ak$ = CHR$(50) THEN k0 = 0: GOSUB 10: B = B - 2: GOSUB 10
IF B < 88 THEN IF ak$ = CHR$(56) THEN k0 = 0: GOSUB 10: B = B + 2: GOSUB 10
IF ak$ = CHR$(43) THEN k0 = 0: GOSUB 10: f0 = f0 + 10: GOSUB 10
IF ak$ = CHR$(45) THEN k0 = 0: GOSUB 10: f0 = f0 - 10: GOSUB 10
WEND
t00 = t00 + 1

yw = -(yl + 239)
yw1 = 240 - yl
CLS : WINDOW (-319, yw)-(320, yw1)

f0 = 414
GOSUB 10
'LINE (fnf(-319), fng(yw))-(fnf(320), fng(yw1)), , B
x0 = 100
y0 = -129: y0 = y0 - yl
x1 = 200
y1 = -50: y1 = y1 - yl
'PRINT x0, fnf(x0)

LINE (x0, y0)-(x1, y1), 12, B
LINE (fnf(x0), fng(y0))-(fnf(x1), fng(y1)), 12, B
LINE (x0, y0)-(fnf(x0), fng(y0)), 12
LINE (x1, y1)-(fnf(x1), fng(y1)), 12
LINE (x0, y1)-(fnf(x0), fng(y1)), 12
LINE (x1, y0)-(fnf(x1), fng(y0)), 12

SLEEP

END
10 a1 = a: b1 = B
DEF fnm (x) = TAN((a + 90) * (PI / 180)) * x + (320 / TAN(a * (PI / 180))) - 240
DEF fnx (x) = TAN((90 - B) * (PI / 180)) * x - (320 / TAN(B * (PI / 180))) + 240
DEF fnm1 (x) = TAN((a1 + 90) * (PI / 180)) * -x + (320 / TAN(a1 * (PI / 180))) - 240
DEF fnx1 (x) = TAN((90 - b1) * (PI / 180)) * -x - (320 / TAN(b1 * (PI / 180))) + 240

v = -TAN((a + 90) * (PI / 180)): v1 = -TAN((90 - B) * (PI / 180)): c = (320 / TAN(a * (PI / 180))) - 240: c1 = -(320 / TAN(B * (PI / 180))) + 240: u = 1: u1 = 1
sv = -TAN((a1 + 90) * (PI / 180)): sv1 = -TAN((90 - b1) * (PI / 180)): sc = -(320 / TAN(a1 * (PI / 180))) + 240: sc1 = (320 / TAN(b1 * (PI / 180))) - 240: su = 1: su1 = 1

xl = (u * c1 - u1 * c) / (v1 * u - v * u1)
yl = fnm(xl)
sxl = (su * sc1 - su1 * sc) / (sv1 * su - sv * su1)
syl = fnm1(sxl)
oh = SQR((320 - xl) ^ 2 + (240 - yl) ^ 2)
og = SQR((320 - xl) ^ 2 + (-239 - yl) ^ 2)
IF og > oh THEN f1 = og - f0 ELSE f1 = oh - f0
IF og > oh THEN f2 = f1 * oh / og ELSE f2 = f1 * og / oh
IF og > oh THEN f00 = oh - f2 ELSE f00 = og - f2

c = 180 - (a + B)
pq = f1 ^ 2 + f2 ^ 2 - 2 * f1 * f2 * COS(c * PI / 180): pq = SQR(pq)
IF og > oh THEN oh1 = f1 * SIN(a * PI / 180) ELSE oh1 = f1 * SIN(B * PI / 180)
pn = 2 * oh1 + xl - sxl
IF t00 < 1 THEN
LOCATE 1, 1: PRINT a, B, og, oh

LINE (sxl, syl)-(xl, yl), k0
LINE (sxl, syl)-(-320, -240), k0
LINE (xl, yl)-(320, 240), k0
LINE (sxl, syl)-(-320, 240), k0
LINE (xl, yl)-(320, -240), k0
'IF og > oh THEN LINE (pn / 2, -239 + f0 * COS(a * PI / 180))-(-pn / 2, -239 + f0 * COS(a * PI / 180) + pq), k0, B ELSE LINE (pn / 2, -239 + f00 * COS(a * PI / 180))-(-pn / 2, -239 + f00 * COS(a * PI / 180) + pq), k0, B
'IF og > oh THEN LINE (pn / 2 - 1, -239 + f0 * COS(a * PI / 180) + 1)-(-pn / 2 + 1, -239 + f0 * COS(a * PI / 180) + pq - 1), 0, BF ELSE LINE (pn / 2 - 1, -239 + f00 * COS(a * PI / 180) + 1)-(-pn / 2 + 1, -239 + f00 * COS(a * PI / 180) + pq - 1), 0, BF

'LINE (pn / 2 - 1, -239 + f0 * COS(a * PI / 180) + 1)-(-pn / 2 + 1, -239 + f0 * COS(a * PI / 180) + pq - 1), 0, BF
END IF
k0 = 12
RETURN

