CLS
SCREEN 12

OPEN "alefba.bas" FOR OUTPUT AS #1
PRINT #1, "cls: screen 12: k0 = 15: z0 = 1: z1 = z0 - 1: x00 = 0: y00 = 0"

WHILE ak$ <> CHR$(27)
LOCATE 25, 40: INPUT x0, y0, x1, y1
PRINT #1, "x0="; x0, ":y0="; y0, ":x1="; x1, ":y1="; y1, ":gosub 11"

GOSUB 11
ak$ = INPUT$(1)

WEND

PRINT #1, "end"
PRINT #1, "11 LINE ((z0 * x0 - z1) + x00, (z0 * y0 - z1) + y00)-((z0 * x1) + x00, (z0 * y1) + y00), k0, BF: RETURN "
CLOSE
LINE INPUT harf$
harf$ = harf$ + ".bas"
NAME "alefba" AS harf$

END
11 LINE (x0, y0)-(x1 + x00, y1 + y00): RETURN

