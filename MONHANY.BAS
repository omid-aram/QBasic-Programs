CLS : SCREEN 12: k = 1: k2 = 1
CONST PI = 3.141592654#
INPUT t
DIM num(t)
FOR s = 1 TO t
INPUT "n"; num(s)
m = m + num(s)
NEXT s
CIRCLE (318, 231), 150, 1
FOR s = 1 TO t
k = k + 1
GOSUB 20
GOSUB ba
k2 = k2 + 1
x = 318 + 100 * COS(b * (PI / 180))': PRINT x
y = 231 + 100 * SIN(b * (PI / 180))': PRINT y
'PSET (x, y), 15
'a$ = INPUT$(1)
'PAINT (x, y), k2, 1
'k1 = k1 + 1
NEXT s
END
20 n = (num(s) * 360) / m: n1 = n1 + n: GOSUB 10: RETURN
10 LINE (318, 231)-(318 + 149 * COS(n1 * (PI / 180)), 231 + 149 * SIN(n1 * (PI / 180))), k: RETURN
ba: ' PRINT num(s): PRINT num(s + 1)
WHILE s < t
b = (num(s) + num(s + 1)) / 2: RETURN
WEND

