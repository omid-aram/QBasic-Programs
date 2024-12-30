CLS : SCREEN 12: t = 0: y = 240: f = 2: f1 = 2: k = 14

FOR l = 320 - f TO 200 STEP -f
y = y - f1
t = t + (f * 2)
t1 = t1 + (f1 * 2)
LINE (l, y)-(l + t, y), k
LINE -(l + t, y + t1), k
LINE -(l - f, y + t1), k
LINE -(l - f, y - f1), k
NEXT l

