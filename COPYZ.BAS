CLS : SCREEN 12:
INPUT cpy$
INPUT "Z "; a000
FOR l = 1 TO 15
LOCATE l, 35: PRINT l
LINE (320, (16 * l) - 16)-(336, 16 * l), l, BF
NEXT l
INPUT "KOLOR "; k000
b000 = a000 - 1
cpy$ = cpy$ + ".bas"
CLS
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

k0 = 7: x0 = 21: y0 = 0: a0 = 1
1111  LINE (5 + x0, a0 * 1 + y0)-(5 + x0, a0 * 8 + y0), k0: n = 1:  x0 = x0 - 7:
28  LINE (6 + x0, 8 * a0 + y0)-(7 + x0, 8 * a0 + y0), k0: LINE -(7 + x0, 6 * a0 + y0), k0: LINE (6 + x0, 5 * a0 + y0)-(6 + x0, 5 * a0 + y0), k0: LINE (5 + x0, 6 * a0 + y0)-(5 + x0, 7 * a0 + y0), k0: LINE (4 + x0, 8 * a0 + y0)-(1 + x0, 8 * a0 + y0), k0: n = 17:  x0 = x0 - 7:
32  LINE (7 + x0, 5 * a0 + y0)-(7 + x0, 7 * a0 + y0), k0: LINE (6 + x0, 8 * a0 + y0)-(1 + x0, 8 * a0 + y0), k0: : PSET (4 + x0, 11 * a0 + y0), k0: PSET (2 + x0, 11 * a0 + y0), k0: n = 21:  x0 = x0 - 7:
10 LINE (5 + x0, 4 * a0 + y0)-(7 + x0, 6 * a0 + y0), k0: LINE -(7 + x0, 7 * a0 + y0), k0: LINE (6 + x0, 8 * a0 + y0)-(4 + x0, 8 * a0 + y0), k0: : n = 1:  x0 = x0 - 7:

y0 = 20: x0 = 21
1 LINE (4 + x0, a0 * 4 + y0)-(4 + x0, a0 * 9 + y0), k0: LINE (2 + x0, a0 * 4 + y0)-(2 + x0, a0 * 2 + y0), k0: LINE -(6 + x0, a0 * 2 + y0), k0: LINE -(6 + x0, a0 * 1 + y0), k0: n = 1:   x0 = x0 - 7:
12  LINE (7 + x0, 7 * a0 + y0)-(7 + x0, 10 * a0 + y0), k0: PSET (6 + x0, 11 * a0 + y0), k0: LINE (5 + x0, 12 * a0 + y0)-(3 + x0, 12 * a0 + y0), k0: : n = 1:  x0 = x0 - 7:
 LINE (5 + x0, a0 * 1 + y0)-(5 + x0, a0 * 8 + y0), k0: n = 1:  x0 = x0 - 7:
2828 LINE (6 + x0, 8 * a0 + y0)-(7 + x0, 8 * a0 + y0), k0: LINE -(7 + x0, 6 * a0 + y0), k0: LINE (6 + x0, 5 * a0 + y0)-(6 + x0, 5 * a0 + y0), k0: LINE (5 + x0, 6 * a0 + y0)-(5 + x0, 7 * a0 + y0), k0: LINE (4 + x0, 8 * a0 + y0)-(4 + x0, 8 * a0 + y0), k0: LINE (3 + x0, 9 * a0 + y0)-(3 + x0, 13 * a0 + y0), k0: n = 1:  x0 = x0 - 7:


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
OPEN cpy$ FOR OUTPUT AS #1
PRINT #1, "cls:screen 12:k000 ="; k000

FOR x000 = 0 TO 100
FOR y000 = 0 TO 100
a = POINT(x000, y000)
'IF a > 0 THEN GOSUB 1010
IF a > 0 THEN GOSUB 1011
NEXT y000
NEXT x000
PRINT #1, "''xm ="; (a000 * (xm + 1)); ": ym = "; (a000 * (ym + 1))
PRINT #1, "''xm ="; (a000 * (xm + 2)); ": ym = "; (a000 * (ym + 2))

CLOSE

END

1010 PRINT #1, "line("; (a000 * (x000 + 1)) - b000; ","; (a000 * (y000 + 1)) - b000; ")-("; (a000 * (x000 + 1)); ","; a000 * (y000 + 1); "),"; a; ",BF": RETURN
1011 PRINT #1, "line("; (a000 * (x000 + 1)) - b000; "+ x0000 ,"; (a000 * (y000 + 1)) - b000; "+ y0000)-("; (a000 * (x000 + 1)); "+ x0000 ,"; a000 * (y000 + 1); "+ y0000), k000 ,BF":
IF x000 > xm THEN xm = x000
IF y000 > ym THEN ym = y000
RETURN


