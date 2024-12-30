10 CLS : za = 1: mot = 0: dade1 = 0: taj = 0: nes = 0: sad = 0: tkh = 0: nes1 = 0: h0 = 0: radious = 200: bs = .1: bs1 = .1: bez = 1: SCREEN 12: WINDOW (0, 0)-(640, 480)
3 LOCATE 1, 1: PRINT "1 = NEW  ,  2 = LOAD": ak$ = INPUT$(1)
IF ak$ = CHR$(49) THEN CLS : GOTO 142
IF ak$ = CHR$(50) THEN CLS : GOTO 152
IF ak$ = CHR$(27) THEN END ELSE GOTO 3
142 LINE INPUT " ? "; nam$: nam$ = nam$ + ".bes"
CLS : k0 = 15: x0 = 80: y0 = -0: GOSUB kol: GOSUB dadeha: LOCATE 1, 12: INPUT t
k0 = 15: x0 = 72: y0 = -16: GOSUB tul: GOSUB dasteh: LOCATE 2, 12: INPUT tu
k0 = 15: x0 = 72: y0 = -32: GOSUB ebteda: GOSUB dasteh: LOCATE 3, 12: INPUT ed
2 LOCATE 28, 1: PRINT "ARE YOU SURE ?": ak$ = INPUT$(1)
IF ak$ = CHR$(110) OR ak$ = CHR$(78) THEN CLS : GOTO 142
IF ak$ = CHR$(121) OR ak$ = CHR$(89) THEN GOTO 143
IF ak$ = CHR$(27) THEN END ELSE GOTO 2
143 OPEN nam$ FOR OUTPUT AS #1
PRINT #1, t
PRINT #1, tu
PRINT #1, ed
60 DIM dad(t)
CLS
70 FOR dade = 1 TO t
dade1 = dade1 + 1
IF dade1 > 27 THEN dade1 = 1: GOSUB okoy: CLS
80 LOCATE dade1, 1: PRINT dade; "                          ": LOCATE dade1, 5: INPUT dad(dade)
86 NEXT dade
GOSUB okoy
FOR dade = 1 TO t
PRINT #1, dad(dade)
NEXT dade
CLOSE #1
GOTO 89
152 SHELL "dir.bes/p"
LINE INPUT " ? "; loa$: loa$ = loa$ + ".bes"
CLS
OPEN loa$ FOR INPUT AS #1
INPUT #1, t
CLOSE #1
DIM dad(t)
lo = 0
DIM dades(t + 4)
OPEN loa$ FOR INPUT AS #1
DO
lo = lo + 1
INPUT #1, dades(lo)
LOOP UNTIL (EOF(1))
CLOSE #1
t = dades(1)
tu = dades(2)
ed = dades(3)
FOR loa = 4 TO t + 3
dad(loa - 3) = dades(loa)
NEXT loa
GOTO 89

89 ebd = ed: kd = 1D+17: DIM ted(t): DIM ted1(t): DIM numbe(t): DIM num(t)
FOR dade = 1 TO t
90 ld = dad(dade)
100 IF ld > bd THEN bd = ld
110 IF ld < kd THEN kd = ld
120 NEXT dade
130 dt = bd - kd + 1
140 td = FIX(dt / tu)' + 1
150 IF (td * tu) + ed < bd THEN td = td + 1: GOTO 150
160 DIM far(td)
180 WHILE ed <= (tu * td) + ebd
190 oed = ed
200 ed = ed + tu
210 mot = mot + 1
220 FOR dade = 1 TO t
230 IF dad(dade) <= ed AND dad(dade) > oed THEN GOSUB 656
240 NEXT dade
250 WEND
tt = 0
FOR mot = 1 TO td
tt = tt + far(mot)
NEXT mot
DIM fara(td)
DIM farav(td)
DIM dsf(td)
DIM nemod(td)
260 mot = 0
270 WHILE mot < td
280 mot = mot + 1
taj = taj + 1
nes = nes + 1
sad = sad + 1
nes1 = nes1 + 1
fara(taj) = far(mot) + fara(taj - 1)
farav(nes) = far(mot) / tt
dsf(sad) = farav(nes) * 100
nemod(nes1) = fara(taj) / tt
300 WEND
nes11 = nes1
123 CLS : SCREEN 12: WINDOW (0, 0)-(640, 480)
PRINT : tkh = 0: fid = 0: ebd1 = ebd: mot = 0: taj = 0: nes = 0: sad = 0
WHILE taj < td
tkh = tkh + 1
fid = ebd1 + tu
ebd1 = ebd1 + tu
mot = mot + 1
taj = taj + 1
nes = nes + 1
sad = sad + 1
290 PRINT ebd1 - tu; "-"; fid, far(mot), fara(taj), CLNG((1000 * za) * farav(nes)) / (1000 * za), CLNG(100 * dsf(sad)) / 100
IF tkh = 26 THEN GOSUB 432: ak$ = INPUT$(1): tkh = 0
WEND
GOSUB 432
IF h0 <> 1 THEN a$ = INPUT$(1): LOCATE 28, 1: PRINT "PRESS 'H' FOR VIEW HELP": h0 = 1
WHILE ak$ <> CHR$(27)
ak$ = INPUT$(1)
IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
IF ak$ = CHR$(50) THEN GOSUB nemodar
IF ak$ = CHR$(49) THEN GOSUB stony1
IF ak$ = CHR$(52) THEN GOSUB stony
IF ak$ = CHR$(51) THEN GOSUB miley1
IF ak$ = CHR$(54) THEN GOSUB miley
IF ak$ = CHR$(55) THEN GOSUB chnbr1
IF ak$ = CHR$(56) THEN GOSUB chnbr
IF ak$ = CHR$(53) THEN GOTO 123
IF ak$ = CHR$(57) THEN GOSUB sagheh
IF za < 100000 THEN IF ak$ = CHR$(48) THEN za = za * 10: GOTO 123
IF za > .1 THEN IF ak$ = CHR$(46) THEN za = za / 10: GOTO 123
WEND
310 END
656 far(mot) = far(mot) + 1: RETURN
faravany: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 5 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: PSET (6 + x0, 480 - 4 + y0), k0: PSET (5 + x0, 480 - 5 + y0), k0:  _
LINE (6 + x0, 480 - 6 + y0)-(5 + x0, 480 - 6 + y0), k0: : PSET (5 + x0, 480 - 2 + y0), k0: n = 2: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSET (6 + x0, 480 - 11 + y0), k0: LINE (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: PSET (4 + x0, 480 - 9 + y0), k0: LINE (5 + x0, 480 - 10 + y0)-(7 + x0, 480 - 10 + y0), k0: LINE -(7 + x0, 480 - 11 + y0), k0: LINE ( _
6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 9 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
dadeha: x0 = x0 - 8
 LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (5 + x0, 480 - 8 + y0)-(3 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 7 + y0)-(6 + x0, 480 - 6 + y0), k0: LINE -(4 + x0, 480 - 4 + y0), k0: LINE (3 + x0, 480 - 7 + y0)-(5 + x0, 480 - 5 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (6 + x0, 480 - 8 + y0)-(0 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(3 + x0, 480 - 5 + y0), k0: LINE -(3 + x0, 480 - 4 + y0), k0: LINE (2 + x0, 480 - 6 + y0)-(2 + x0, 480 - 8 + _
 y0), k0: LINE (4 + x0, 480 - 5 + y0)-(4 + x0, 480 - 7 + y0), k0: n = 19: x0 = x0 - 8:
 LINE (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: LINE (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
kol: LINE (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF:  LINE (1 + x0, 472 + y0)-(7 + x0, 472 + y0), k0: LINE (7 + x0, 473 + y0)-(5 + x0, 476 + y0), k0: LINE (5 + x0, 477 + y0)-(7 + x0, 480 + y0), k0: : n = 2:  x0 = x0 - 8:
 LINE (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: LINE (7 + x0, 479 + y0)-(7 + x0, 469 + y0), k0: LINE (6 + x0, 468 + y0)-(3 + x0, 468 + y0), k0: LINE (2 + x0, 469 + y0)-(2 + x0, 472 + y0), k0: n = 1:  x0 = x0 - 8:
RETURN
jadval: LINE (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (3 + x0, 480 - 6 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 5 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: n = 3 _
: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: LINE (7 + x0, 479 + y0)-(7 + x0, 469 + y0), k0: LINE (6 + x0, 468 + y0)-(3 + x0, 468 + y0), k0: LINE (2 + x0, 469 + y0)-(2 + x0, 472 + y0), k0: n = 1:  x0 = x0 - 8: : x0 = x0 - 7
RETURN
motlagh: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - _
 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: n = 17: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (3 + x0, 480 -  _
8 + y0)-(3 + x0, 480 - 1 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 1 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: n = 16: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(2 + x0, 480 - 12 + y0), k0: LINE (1 + x0, 480 - 11 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (6 + x0,  _
480 - 5 + y0)-(5 + x0, 480 - 5 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE (4 + x0, 480 - 7 + y0)-(4 + x0, 480 - 6 + y0), k0: PSET (4 + x0, 480 - 2 + y0), k0: PSET (6 + x0, 480 - 2 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
namayesh:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
 LINE (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: LINE (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: PSET (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE (4 + x0, 480 - 7 + y0)-(4 + x0, 480 - 6 + y0), k0: LINE (3 + x0, 480 - 8 + y0)-(2 + x0, 480 - 8 + y0), k0: LINE (1 + x0, 480 - 6 +  _
y0)-(1 + x0, 480 - 10 + y0), k0: : PSET (6 + x0, 480 - 3 + y0), k0: PSET (4 + x0, 480 - 3 + y0), k0: PSET (5 + x0, 480 - 2 + y0), k0: x0 = x0 - 8: LINE (8 + x0, 480 - 11 + y0)-(4 + x0, 480 - 11 + y0), k0: LINE (3 + x0, 480 - 10 + y0)-(3 + x0, 480 -  _
7 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
tul: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (3 + x0,  _
480 - 8 + y0)-(3 + x0, 480 - 1 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 1 + y0)-(7 + x0, 480 - 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - _
 8:
RETURN
dasteh: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(5 + x0, 480 - 6 + y0), k0: LINE (3 + x0, 480 -  _
8 + y0)-(3 + x0, 480 - 6 + y0), k0: : n = 7: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: PSET (2 + x0, 480 - 3 + y0), k0: n = 2:  _
x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: LINE -(3 + x0, 480 - 6 + y0), k0: LINE -(3 + x0, 480 - 8 + y0), k0: LINE -(4 _
 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
jabehee: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (3 + x0, 480 - 6 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 5 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: n = 3 _
: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 7 + y0)-(4 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 6 + y0)-(3 + x0, 480 - 6 + y0), k0: n = 12: x0 = x0 - 8 _
:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (3 + x0, 480 - 11 + y0), k0: n = 2: x0 = x0 - 8:
 LINE (7 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: LINE -(3 + x0, 480 - 6 + y0), k0: LINE -(3 + x0, 480 - 8 + y0), k0: LINE -(4 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 6 + y0)-(6 + x0, 480 - 6 + y0), k0: LINE -(5 + x0, 480 - 7 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 9 + y0)-(7 + x0, 480 - _
 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
ebteda: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (3 + x0, 480 - 11 + y0), k0: n = 2: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: PSET (2 + x0, 480 - 3 + y0), k0: n = 2:  _
x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
hodud: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (3 + x0, 480 - 6 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 5 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(1 + x0, 480 - 8 + y0), k0: : n = 5: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
RETURN
tajamoei: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: PSET (2 + x0, 480 - 3 + y0), k0: n = 2:  _
x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (3 + x0, 480 - 6 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 5 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: n = 3 _
: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - _
 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: n = 17: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 7 + y0)-(4 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 6 + y0)-(3 + x0, 480 - 6 + y0), k0: n = 12: x0 = x0 - 8 _
:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: PSET (4 + x0, 480 - 9 + y0), k0: LINE (5 + x0, 480 - 10 + y0)-(7 + x0, 480 - 10 + y0), k0: LINE -(7 + x0, 480 - 11 + y0), k0: LINE ( _
6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 9 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
nesby: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(5 + x0, 480 - 6 + y0), k0: LINE (3 + x0, 480 -  _
8 + y0)-(3 + x0, 480 - 6 + y0), k0: : n = 7: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (3 + x0, 480 - 11 + y0), k0: n = 2: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: PSET (4 + x0, 480 - 9 + y0), k0: LINE (5 + x0, 480 - 10 + y0)-(7 + x0, 480 - 10 + y0), k0: LINE -(7 + x0, 480 - 11 + y0), k0: LINE ( _
6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 9 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
darsad: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSET (6 + x0, 480 - 11 + y0), k0: LINE (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(5 + x0, 480 - 5 + y0), k0: LINE (4 + x0, 480 -  _
8 + y0)-(4 + x0, 480 - 6 + y0), k0: LINE (2 + x0, 480 - 8 + y0)-(2 + x0, 480 - 7 + y0), k0: : n = 9: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
RETURN
mianeh:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: PSET (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
 LINE (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: LINE (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (7 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: LINE -(3 + x0, 480 - 6 + y0), k0: LINE -(3 + x0, 480 - 8 + y0), k0: LINE -(4 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
mood:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
 LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
RETURN
miangin:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: PSET (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
 LINE (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: LINE (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (1 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: LINE (5 + x0, 480 - 3 + y0)-(7 + x0, 480 - 0 + y0), k0: : LINE (4 + x0, 480 - 1 + y0)-(3 + x0, 480 - 2 + y0), k0: n = 2: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: PSET (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
 LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: PSET (4 + x0, 480 - 6 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
nemodare: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - _
 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: n = 17: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSET (6 + x0, 480 - 11 + y0), k0: LINE (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: : n = 1: x0 = x0 - 8:
RETURN
daerehei: LINE (0 + x0, 480 - 0 * a0 + y0)-(8 + x0, 14 * a0 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE -(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: PSET (2 + x0, 480 - 11 + y0), k0: n = 21 _
: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSET (6 + x0, 480 - 11 + y0), k0: LINE (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 8 + y0)-(3 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 7 + y0)-(6 + x0, 480 - 6 + y0), k0: LINE -(4 + x0, 480 - 4 + y0), k0: LINE (3 + x0, 480 - 7 + y0)-(5 + x0, 480 - _
 5 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 6 + y0)-(6 + x0, 480 - 6 + y0), k0: LINE -(5 + x0, 480 - 7 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 9 + y0)-(7 + x0, 480 - _
 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
RETURN
432 LINE (0, 480 - 0)-(640, 480 - 15), 0, BF
ert = (td + 1) * 16
FOR dam = 112 TO 448 STEP 112
LINE (dam, 480 - 0)-(dam, 480 - ert), 10
NEXT dam
LINE (0, 480 - 15)-(600, 480 - 15), 10
x0 = 85: y0 = 0: k0 = 14: GOSUB hodud: GOSUB dasteh
x0 = 210: GOSUB faravany: GOSUB motlagh
x0 = 322: GOSUB faravany: GOSUB tajamoei
x0 = 434: GOSUB faravany: GOSUB nesby
x0 = 546: GOSUB darsad: GOSUB faravany
RETURN

okoy:
1 LOCATE 28, 1: PRINT "ARA THESE RIGHT ? (Y for YES  and  N for NO)                                ": ak$ = INPUT$(1)
IF ak$ = CHR$(110) OR ak$ = CHR$(78) THEN LOCATE 28, 1: INPUT "PLEASE TYPY LIKE THIS : number of WRONG , RIGHT of this"; wrong, right: dad(wrong) = right
IF ak$ = CHR$(121) OR ak$ = CHR$(89) THEN RETURN
IF ak$ = CHR$(27) THEN END
GOTO 1
RETURN

nemodar: nes1 = 0
CLS : SCREEN 12: WINDOW (0, 0)-(640, 480): FOR nes1 = 1 TO nes11
CIRCLE (320, 240), radious, 3, 0, nemod(nes1) * -6.28, bez
NEXT nes1
IF radious = 200 AND bez = 1 THEN CIRCLE (320, 240), 230, 4, 0, 1: LINE (455, 400)-(445, 433), 4: LINE -(475, 430), 4
 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF radious < 320 THEN IF ak$ = CHR$(42) THEN radious = radious + 15: GOSUB nemodar
 IF radious > 15 THEN IF ak$ = CHR$(47) THEN radious = radious - 15: GOSUB nemodar
 IF ak$ = CHR$(43) THEN bez = bez - .1: GOSUB nemodar
 IF ak$ = CHR$(45) THEN bez = bez + .1: GOSUB nemodar
WEND
RETURN

stony:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
FOR mot = 1 TO td
IF BF < far(mot) THEN BF = far(mot)
NEXT mot
CLS : WINDOW (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
LINE (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
LINE (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
FOR kha = 1 TO BF
LINE (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
NEXT kha
FOR mot = 1 TO td
kl = kl + 1
IF kl > 14 THEN kl = kl - 14
LINE (0 + hf, 0)-(hf + tu, far(mot)), kl, BF
hf = hf + tu
LINE (-bs1 * td * tu / 4, far(mot))-(-3 * bs1 * td * tu / 4, far(mot)), 13
NEXT mot
 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF ak$ = CHR$(48) THEN SWAP ki, ki1: GOSUB khafm
 IF ak$ = CHR$(42) THEN bs = bs / 1.5: bs1 = bs1 / 1.5: GOSUB stony
 IF ak$ = CHR$(47) THEN bs = bs * 1.5: bs1 = bs1 * 1.5: GOSUB stony
 IF jah = 1 THEN IF ak$ = CHR$(45) THEN bs = bs * 1.5: GOSUB stony
 IF jah = 1 THEN IF ak$ = CHR$(43) THEN bs = bs / 1.5: GOSUB stony
 IF jah = 2 THEN IF ak$ = CHR$(45) THEN bs1 = bs1 * 1.5: GOSUB stony
 IF jah = 2 THEN IF ak$ = CHR$(43) THEN bs1 = bs1 / 1.5: GOSUB stony
 IF ak$ = CHR$(46) THEN IF jah = 2 THEN jah = 1: LOCATE 2, 1: PRINT " "; CHR$(18); " " ELSE jah = 2: LOCATE 2, 1: PRINT "® ¯"
WEND
RETURN

stony1:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
FOR taj = 1 TO td
IF BF < fara(taj) THEN BF = fara(taj)
NEXT taj
CLS : WINDOW (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
LINE (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
LINE (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
FOR kha = 1 TO BF
LINE (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
NEXT kha
FOR taj = 1 TO td
kl = kl + 1
IF kl > 14 THEN kl = kl - 14
LINE (0 + hf, 0)-(hf + tu, fara(taj)), kl, BF
hf = hf + tu
LINE (-bs1 * td * tu / 4, fara(taj))-(-3 * bs1 * td * tu / 4, fara(taj)), 13
NEXT taj
 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF ak$ = CHR$(48) THEN SWAP ki, ki1: GOSUB khaft
 IF ak$ = CHR$(42) THEN bs = bs / 1.5: bs1 = bs1 / 1.5: GOSUB stony1
 IF ak$ = CHR$(47) THEN bs = bs * 1.5: bs1 = bs1 * 1.5: GOSUB stony1
 IF jah = 1 THEN IF ak$ = CHR$(45) THEN bs = bs * 1.5: GOSUB stony1
 IF jah = 1 THEN IF ak$ = CHR$(43) THEN bs = bs / 1.5: GOSUB stony1
 IF jah = 2 THEN IF ak$ = CHR$(45) THEN bs1 = bs1 * 1.5: GOSUB stony1
 IF jah = 2 THEN IF ak$ = CHR$(43) THEN bs1 = bs1 / 1.5: GOSUB stony1
 IF ak$ = CHR$(46) THEN IF jah = 2 THEN jah = 1: LOCATE 2, 1: PRINT " "; CHR$(18); " " ELSE jah = 2: LOCATE 2, 1: PRINT "® ¯"
WEND
RETURN

miley:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
FOR mot = 1 TO td
IF BF < far(mot) THEN BF = far(mot)
NEXT mot
CLS : SCREEN 12: WINDOW (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
LINE (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
LINE (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
FOR kha = 1 TO BF
LINE (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
NEXT kha
FOR mot = 1 TO td
kl = kl + 1
IF kl > 14 THEN kl = kl - 14
LINE (hf + (tu / 2), 0)-(hf + (tu / 2), far(mot)), kl
hf = hf + tu
LINE (-bs1 * td * tu / 4, far(mot))-(-3 * bs1 * td * tu / 4, far(mot)), 13
NEXT mot
 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF ak$ = CHR$(48) THEN SWAP ki, ki1: GOSUB khafm
 IF ak$ = CHR$(42) THEN bs = bs / 1.5: bs1 = bs1 / 1.5: GOSUB miley
 IF ak$ = CHR$(47) THEN bs = bs * 1.5: bs1 = bs1 * 1.5: GOSUB miley
 IF jah = 1 THEN IF ak$ = CHR$(45) THEN bs = bs * 1.5: GOSUB miley
 IF jah = 1 THEN IF ak$ = CHR$(43) THEN bs = bs / 1.5: GOSUB miley
 IF jah = 2 THEN IF ak$ = CHR$(45) THEN bs1 = bs1 * 1.5: GOSUB miley
 IF jah = 2 THEN IF ak$ = CHR$(43) THEN bs1 = bs1 / 1.5: GOSUB miley
 IF ak$ = CHR$(46) THEN IF jah = 2 THEN jah = 1: LOCATE 2, 1: PRINT " "; CHR$(18); " " ELSE jah = 2: LOCATE 2, 1: PRINT "® ¯"
WEND
RETURN

helf:
CLS : SCREEN 12: WINDOW (0, 0)-(639, 480)
LINE (25, 60)-(45, 51), 2, B: LINE -(65, 80), 2, B: LINE (65, 51)-(85, 90), 2, B: LINE (85, 51)-(105, 95), 2, B: LINE (105, 51)-(125, 120), 2, B: LINE (20, 159)-(20, 50), 4: LINE -(159, 50), 4: x0 = 151: y0 = -437: k0 = 3: GOSUB faravany: GOSUB  _
tajamoei: LOCATE 28, 4: PRINT " 1 :"
LINE (25, 220)-(45, 211), 2, B: LINE -(65, 250), 2, B: LINE (65, 211)-(85, 230), 2, B: LINE (85, 211)-(105, 285), 2, B: LINE (105, 211)-(125, 240), 2, B: LINE (20, 319)-(20, 210), 4: LINE -(159, 210), 4: x0 = 151: y0 = -277: k0 = 3: GOSUB faravany:  _
GOSUB motlagh: LOCATE 18, 4: PRINT " 4 :"
LINE (35, 381)-(55, 400), 2: LINE -(75, 410), 2: LINE -(95, 415), 2: LINE -(115, 440), 2: : LINE (20, 479)-(20, 370), 4: LINE -(159, 370), 4: x0 = 151: y0 = -117: k0 = 3: GOSUB faravany: GOSUB tajamoei: LOCATE 8, 4: PRINT " 7 :"
LINE (245, 380)-(265, 410), 2: LINE -(285, 390), 2: LINE -(305, 465), 2: LINE -(325, 400), 2: : LINE (230, 479)-(230, 370), 4: LINE -(369, 370), 4: x0 = 361: y0 = -117: k0 = 3: GOSUB faravany: GOSUB motlagh: LOCATE 8, 30: PRINT " 8 :"
LINE (220, 300)-(400, 300), 4: LINE (255, 317)-(255, 210), 4: LINE (295, 317)-(295, 210), 4: LINE (335, 317)-(335, 210), 4: LINE (375, 317)-(375, 210), 4: x0 = 361: y0 = -277: k0 = 3: GOSUB jadval: GOSUB faravany: LOCATE 18, 30: PRINT " 5 :"
LINE (320, 105)-(374, 105), 2: LINE (320, 105)-(366, 132), 2: LINE (320, 105)-(274, 132), 2: LINE (320, 105)-(301, 54), 2: CIRCLE (320, 105), 54, 4: x0 = 370: y0 = -437: k0 = 3: GOSUB nemodare: GOSUB daerehei: LOCATE 28, 30: PRINT " 2 :"
LINE (455, 60)-(455, 51), 2: LINE (475, 51)-(475, 80), 2: LINE (495, 51)-(495, 90), 2: LINE (515, 51)-(515, 95), 2: LINE (535, 51)-(535, 120), 2: LINE (440, 159)-(440, 50), 4: LINE -(579, 50), 4: x0 = 571: y0 = -437: k0 = 3: GOSUB faravany: GOSUB  _
tajamoei: LOCATE 28, 56: PRINT " 3 :"
LINE (455, 220)-(455, 211), 2: LINE (475, 211)-(475, 250), 2: LINE (495, 211)-(495, 230), 2: LINE (515, 211)-(515, 285), 2: LINE (535, 211)-(535, 240), 2: : LINE (440, 319)-(440, 210), 4: LINE -(579, 210), 4: x0 = 571: y0 = -277: k0 = 3: GOSUB  _
faravany: GOSUB motlagh: LOCATE 18, 56: PRINT " 6 :"
x0 = 571: y0 = -117: k0 = 3: GOSUB namayesh: GOSUB dadeha: LOCATE 8, 56: PRINT " 9 :"
x0 = 560: y0 = -37: k0 = 11: GOSUB mood
x0 = 560: y0 = -53: k0 = 11: GOSUB mianeh
x0 = 560: y0 = -69: k0 = 11: GOSUB miangin
x0 = 560: y0 = -85: k0 = 11: GOSUB nemodare: GOSUB jabehee

 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
WEND
RETURN

miley1:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
FOR taj = 1 TO td
IF BF < fara(taj) THEN BF = fara(taj)
NEXT taj
CLS : SCREEN 12: WINDOW (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
LINE (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
LINE (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
FOR kha = 1 TO BF
LINE (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
NEXT kha
FOR taj = 1 TO td
kl = kl + 1
IF kl > 14 THEN kl = kl - 14
LINE (hf + (tu / 2), 0)-(hf + (tu / 2), fara(taj)), kl
hf = hf + tu
LINE (-bs1 * td * tu / 4, fara(taj))-(-3 * bs1 * td * tu / 4, fara(taj)), 13
NEXT taj
 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF ak$ = CHR$(48) THEN SWAP ki, ki1: GOSUB khaft
 IF ak$ = CHR$(42) THEN bs = bs / 1.5: bs1 = bs1 / 1.5: GOSUB miley1
 IF ak$ = CHR$(47) THEN bs = bs * 1.5: bs1 = bs1 * 1.5: GOSUB miley1
 IF jah = 1 THEN IF ak$ = CHR$(45) THEN bs = bs * 1.5: GOSUB miley1
 IF jah = 1 THEN IF ak$ = CHR$(43) THEN bs = bs / 1.5: GOSUB miley1
 IF jah = 2 THEN IF ak$ = CHR$(45) THEN bs1 = bs1 * 1.5: GOSUB miley1
 IF jah = 2 THEN IF ak$ = CHR$(43) THEN bs1 = bs1 / 1.5: GOSUB miley1
 IF ak$ = CHR$(46) THEN IF jah = 2 THEN jah = 1: LOCATE 2, 1: PRINT " "; CHR$(18); " " ELSE jah = 2: LOCATE 2, 1: PRINT "® ¯"
WEND
RETURN

chnbr:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
FOR mot = 1 TO td
IF BF < far(mot) THEN BF = far(mot)
NEXT mot
CLS : SCREEN 12: WINDOW (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
LINE (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
LINE (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
FOR kha = 1 TO BF
LINE (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
NEXT kha
FOR mot = 1 TO td - 1
kl = kl + 1
IF kl > 14 THEN kl = kl - 14
LINE (hf + (tu / 2), far(mot))-(tu + hf + (tu / 2), far(mot + 1)), kl
hf = hf + tu
LINE (-bs1 * td * tu / 4, far(mot))-(-3 * bs1 * td * tu / 4, far(mot)), 13
NEXT mot
 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF ak$ = CHR$(48) THEN SWAP ki, ki1: GOSUB khafm
 IF ak$ = CHR$(42) THEN bs = bs / 1.5: bs1 = bs1 / 1.5: GOSUB chnbr
 IF ak$ = CHR$(47) THEN bs = bs * 1.5: bs1 = bs1 * 1.5: GOSUB chnbr
 IF jah = 1 THEN IF ak$ = CHR$(45) THEN bs = bs * 1.5: GOSUB chnbr
 IF jah = 1 THEN IF ak$ = CHR$(43) THEN bs = bs / 1.5: GOSUB chnbr
 IF jah = 2 THEN IF ak$ = CHR$(45) THEN bs1 = bs1 * 1.5: GOSUB chnbr
 IF jah = 2 THEN IF ak$ = CHR$(43) THEN bs1 = bs1 / 1.5: GOSUB chnbr
 IF ak$ = CHR$(46) THEN IF jah = 2 THEN jah = 1: LOCATE 2, 1: PRINT " "; CHR$(18); " " ELSE jah = 2: LOCATE 2, 1: PRINT "® ¯"
WEND
RETURN

chnbr1:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
FOR taj = 1 TO td
IF BF < fara(taj) THEN BF = fara(taj)
NEXT taj
CLS : SCREEN 12: WINDOW (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
LINE (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
LINE (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
FOR kha = 1 TO BF
LINE (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
NEXT kha
FOR taj = 1 TO td - 1
kl = kl + 1
IF kl > 14 THEN kl = kl - 14
LINE (hf + (tu / 2), fara(taj))-(tu + hf + (tu / 2), fara(taj + 1)), kl
hf = hf + tu
LINE (-bs1 * td * tu / 4, fara(taj))-(-3 * bs1 * td * tu / 4, fara(taj)), 13
NEXT taj
 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF ak$ = CHR$(48) THEN SWAP ki, ki1: GOSUB khaft
 IF ak$ = CHR$(42) THEN bs = bs / 1.5: bs1 = bs1 / 1.5: GOSUB chnbr1
 IF ak$ = CHR$(47) THEN bs = bs * 1.5: bs1 = bs1 * 1.5: GOSUB chnbr1
 IF jah = 1 THEN IF ak$ = CHR$(45) THEN bs = bs * 1.5: GOSUB chnbr1
 IF jah = 1 THEN IF ak$ = CHR$(43) THEN bs = bs / 1.5: GOSUB chnbr1
 IF jah = 2 THEN IF ak$ = CHR$(45) THEN bs1 = bs1 * 1.5: GOSUB chnbr1
 IF jah = 2 THEN IF ak$ = CHR$(43) THEN bs1 = bs1 / 1.5: GOSUB chnbr1
 IF ak$ = CHR$(46) THEN IF jah = 2 THEN jah = 1: LOCATE 2, 1: PRINT " "; CHR$(18); " " ELSE jah = 2: LOCATE 2, 1: PRINT "® ¯"
WEND
RETURN

khaft:
FOR taj = 1 TO td
LINE (-bs1 * td * tu, fara(taj))-(td * tu + bs1 * td * tu, fara(taj)), ki
NEXT taj
RETURN

khafm:
FOR mot = 1 TO td
LINE (-bs1 * td * tu, far(mot))-(td * tu + bs1 * td * tu, far(mot)), ki
NEXT mot
RETURN

sagheh: sh = 1
400 CLS : WINDOW (0, 0)-(640, 480)
y0 = 0: LINE (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 360: GOSUB namayesh: GOSUB dadeha: PRINT
FOR dade = 1 TO t
num(dade) = dad(dade)
NEXT dade
FOR s = 1 TO t
  FOR s2 = s + 1 TO t
  IF num(s) > num(s2) THEN SWAP num(s), num(s2)
  NEXT s2
IF s MOD 26 = 0 THEN LOCATE 26, 1: PRINT CHR$(25): ak$ = INPUT$(1): LOCATE 26, 1: COLOR 0: PRINT CHR$(25): COLOR 15
PRINT , , s, num(s)
NEXT s
GOTO 321

401 CLS : s7 = 0: s18 = 0: bt = 0: i = 0: WINDOW (0, 0)-(640, 480)
FOR s5 = 1 TO t
 l = 0
 FOR s6 = 1 TO t
  IF num(s5) = num(s6) THEN l = l + 1
 NEXT s6
 ted(s5) = l
NEXT s5
y0 = 0: LINE (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 320: GOSUB mood: PRINT
WHILE s7 < t
 s18 = s18 + 1
 s7 = s7 + ted(s7 + 1)
 numbe(s18) = num(s7)
 ted1(s18) = ted(s7)
 IF (s18 MOD 26) = 0 THEN LOCATE 26, 1: PRINT CHR$(25): ak$ = INPUT$(1): LOCATE 26, 1: COLOR 0: PRINT CHR$(25): COLOR 15
 PRINT , , ted(s7); "*"; numbe(s18)
 IF ted(s7) > bt THEN bt = ted(s7): bt1 = s18
WEND
s185 = s18
i1 = (s185 MOD 26)
COLOR 14
FOR s18 = 1 TO s185
 IF (i1 + i) MOD 26 = 0 THEN LOCATE 27, 1: PRINT CHR$(25): ak$ = INPUT$(1): LOCATE 27, 1: COLOR 0: PRINT CHR$(25): COLOR 14: i1 = 0
 IF bt = ted1(s18) THEN PRINT , , , numbe(s18): i = i + 1
NEXT s18
COLOR 15
GOTO 321

402 CLS : WINDOW (0, 0)-(640, 480)
y0 = 0: LINE (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 340: GOSUB miangin: PRINT
miangin = (bd + kd) / 2
PRINT , , miangin
y0 = -48: LINE (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 332: GOSUB mianeh: PRINT : PRINT
IF t MOD 2 = 0 THEN miane = (num(t / 2) + num((t / 2) + 1)) / 2: t2 = (t + 1) / 2: t3 = t2 - .5: t4 = t2 + .5
IF t MOD 2 <> 0 THEN miane = num((t \ 2) + 1): t2 = (t \ 2) + 1: t3 = t \ 2: t4 = (t \ 2) + 2
IF t3 MOD 2 = 0 THEN charak1 = (num(t3 / 2) + num((t3 / 2) + 1)) / 2: charak3 = (num((t3 / 2) + t3) + num((t3 / 2) + t3 + 1)) / 2
IF t3 MOD 2 <> 0 THEN charak1 = num((t3 \ 2) + 1): charak3 = num((t3 \ 2) + t4)

PRINT , , miane, charak1, charak3
GOTO 321

403 CLS : WINDOW (-bs * (bd - kd), -10)-(bs * (bd - kd) + (bd - kd), 10)
LINE (0, 0)-(bd - kd, 0)
LINE (charak1 - kd, -1)-(charak3 - kd, 1), 13, BF
LINE (miane - kd, 1)-(miane - kd, -4.6), 8: LINE -(-bs * (bd - kd), -4.6), 8: LOCATE 15 + 7, 1: PRINT miane
CIRCLE (0, 0), bs * bs * (bd - kd)
CIRCLE (bd - kd, 0), bs * bs * (bd - kd)
LINE (0, 0)-(0, -2), 8: LINE -(-bs * (bd - kd), -2), 8: LOCATE 15 + 3, 1: PRINT kd
LINE (charak1 - kd, 0)-(charak1 - kd, -3.3), 8: LINE -(-bs * (bd - kd), -3.3), 8: LOCATE 15 + 5, 1: PRINT charak1
LINE (charak3 - kd, 0)-(charak3 - kd, -6), 8: LINE -(-bs * (bd - kd), -6), 8: LOCATE 15 + 9, 1: PRINT charak3
LINE (bd - kd, 0)-(bd - kd, -7.3), 8: LINE -(-bs * (bd - kd), -7.3), 8: LOCATE 15 + 11, 1: PRINT bd

321 WHILE ak$ <> CHR$(27)
 ak$ = INPUT$(1)
 IF ak$ = CHR$(104) OR ak$ = CHR$(72) THEN GOSUB helf
 IF ak$ = CHR$(50) THEN GOSUB nemodar
 IF ak$ = CHR$(49) THEN GOSUB stony1
 IF ak$ = CHR$(52) THEN GOSUB stony
 IF ak$ = CHR$(51) THEN GOSUB miley1
 IF ak$ = CHR$(54) THEN GOSUB miley
 IF ak$ = CHR$(55) THEN GOSUB chnbr1
 IF ak$ = CHR$(56) THEN GOSUB chnbr
 IF ak$ = CHR$(53) THEN GOTO 123
 IF ak$ = CHR$(57) THEN GOSUB sagheh
 IF sh < 4 THEN IF ak$ = CHR$(43) THEN sh = sh + 1: ON sh GOTO 400, 401, 402, 403
 IF sh > 1 THEN IF ak$ = CHR$(45) THEN sh = sh - 1: ON sh GOTO 400, 401, 402, 403
WEND
RETURN

