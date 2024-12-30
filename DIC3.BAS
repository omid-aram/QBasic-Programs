CLS
READ a$, b$, c$, d$, e$, f$, g$, h$, i$, j$, k$, l$, m$, n$, o$, p$, q$, r$, s$, t$, u$, v$, w$, x$, y$, z$
INPUT s00$: s00$ = LCASE$(s00$)
DO WHILE ss < LEN(s00$)
ss = ss + 1
s0$ = MID$(s00$, ss, 1)
IF s0$ = " " THEN sn$ = sn$ + "900"
s1$ = s0$ + "$"
SELECT CASE s1$
CASE "a$"
sn$ = sn$ + a$
CASE "b$"
sn$ = sn$ + b$
CASE "c$"
sn$ = sn$ + c$
CASE "d$"
sn$ = sn$ + d$
CASE "e$"
sn$ = sn$ + e$
CASE "f$"
sn$ = sn$ + f$
CASE "g$"
sn$ = sn$ + g$
CASE "h$"
sn$ = sn$ + h$
CASE "i$"
sn$ = sn$ + i$
CASE "j$"
sn$ = sn$ + j$
CASE "k$"
sn$ = sn$ + k$
CASE "l$"
sn$ = sn$ + l$
CASE "m$"
sn$ = sn$ + m$
CASE "n$"
sn$ = sn$ + n$
CASE "o$"
sn$ = sn$ + o$
CASE "p$"
sn$ = sn$ + p$
CASE "q$"
sn$ = sn$ + q$
CASE "r$"
sn$ = sn$ + r$
CASE "s$"
sn$ = sn$ + s$
CASE "t$"
sn$ = sn$ + t$
CASE "u$"
sn$ = sn$ + u$
CASE "v$"
sn$ = sn$ + v$
CASE "w$"
sn$ = sn$ + w$
CASE "x$"
sn$ = sn$ + x$
CASE "y$"
sn$ = sn$ + y$
CASE "z$"
sn$ = sn$ + z$
END SELECT
LOOP
sn# = VAL(sn$)
PRINT sn$, sn#

DATA 1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800
END

