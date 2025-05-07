10 Cls: za = 1: mot = 0: dade1 = 0: taj = 0: nes = 0: sad = 0: tkh = 0: nes1 = 0: h0 = 0: radious = 200: bs = .1: bs1 = .1: bez = 1: Screen 12: Window (0, 0)-(640, 480)
3 Locate 1, 1: Print "1 = NEW  ,  2 = LOAD": ak$ = Input$(1)
If ak$ = Chr$(49) Then Cls: GoTo 142
If ak$ = Chr$(50) Then Cls: GoTo 152
If ak$ = Chr$(27) Then End Else GoTo 3
142 Line Input "Filename? "; nam$: nam$ = nam$ + ".bes"
Cls: k0 = 15: x0 = 80: y0 = -0: GoSub kol: GoSub dadeha: Locate 1, 12: Input t
k0 = 15: x0 = 72: y0 = -16: GoSub tul: GoSub dasteh: Locate 2, 12: Input tu
k0 = 15: x0 = 72: y0 = -32: GoSub ebteda: GoSub dasteh: Locate 3, 12: Input ed
2 Locate 28, 1: Print "ARE YOU SURE ?": ak$ = Input$(1)
If ak$ = Chr$(110) Or ak$ = Chr$(78) Then Cls: GoTo 142
If ak$ = Chr$(121) Or ak$ = Chr$(89) Then GoTo 143
If ak$ = Chr$(27) Then End Else GoTo 2
143 Open nam$ For Output As #1
Print #1, t
Print #1, tu
Print #1, ed
60 Dim dad(t)
Cls
70 For dade = 1 To t
    dade1 = dade1 + 1
    If dade1 > 27 Then dade1 = 1: GoSub okoy: Cls
    80 Locate dade1, 1: Print dade; "                          ": Locate dade1, 5: Input dad(dade)
86 Next dade
GoSub okoy
For dade = 1 To t
    Print #1, dad(dade)
Next dade
Close #1
GoTo 89
152 Shell "dir.bes/p"
Line Input "Enter the .bes Filename... (e.g. AMAR02) : "; loa$: loa$ = loa$ + ".bes"
Cls
Open loa$ For Input As #1
Input #1, t
Close #1
Dim dad(t)
lo = 0
Dim dades(t + 4)
Open loa$ For Input As #1
Do
    lo = lo + 1
    Input #1, dades(lo)
Loop Until (EOF(1))
Close #1
t = dades(1)
tu = dades(2)
ed = dades(3)
For loa = 4 To t + 3
    dad(loa - 3) = dades(loa)
Next loa
GoTo 89

89 ebd = ed: kd = 1D+17: Dim ted(t): Dim ted1(t): Dim numbe(t): Dim num(t)
For dade = 1 To t
    90 ld = dad(dade)
    100 If ld > bd Then bd = ld
    110 If ld < kd Then kd = ld
120 Next dade
130 dt = bd - kd + 1
140 td = Fix(dt / tu) ' + 1
150 If (td * tu) + ed < bd Then td = td + 1: GoTo 150
160 Dim far(td)
180 While ed <= (tu * td) + ebd
    190 oed = ed
    200 ed = ed + tu
    210 mot = mot + 1
    220 For dade = 1 To t
        230 If dad(dade) <= ed And dad(dade) > oed Then GoSub 656
    240 Next dade
250 Wend
tt = 0
For mot = 1 To td
    tt = tt + far(mot)
Next mot
Dim fara(td)
Dim farav(td)
Dim dsf(td)
Dim nemod(td)
260 mot = 0
270 While mot < td
    280 mot = mot + 1
    taj = taj + 1
    nes = nes + 1
    sad = sad + 1
    nes1 = nes1 + 1
    fara(taj) = far(mot) + fara(taj - 1)
    farav(nes) = far(mot) / tt
    dsf(sad) = farav(nes) * 100
    nemod(nes1) = fara(taj) / tt
300 Wend
nes11 = nes1
123 Cls: Screen 12: Window (0, 0)-(640, 480)
Print: tkh = 0: fid = 0: ebd1 = ebd: mot = 0: taj = 0: nes = 0: sad = 0
While taj < td
    tkh = tkh + 1
    fid = ebd1 + tu
    ebd1 = ebd1 + tu
    mot = mot + 1
    taj = taj + 1
    nes = nes + 1
    sad = sad + 1
    290 Print ebd1 - tu; "-"; fid, far(mot), fara(taj), CLng((1000 * za) * farav(nes)) / (1000 * za), CLng(100 * dsf(sad)) / 100
    If tkh = 26 Then GoSub 432: ak$ = Input$(1): tkh = 0
Wend
GoSub 432
If h0 <> 1 Then a$ = Input$(1): Locate 28, 1: Print "PRESS 'H' FOR VIEW HELP (Also use '/', '*', '+', '-', '.', '0' to zoom in/out)": h0 = 1
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If za < 100000 Then If ak$ = Chr$(48) Then za = za * 10: GoTo 123
    If za > .1 Then If ak$ = Chr$(46) Then za = za / 10: GoTo 123
Wend
310 End
656 far(mot) = far(mot) + 1: Return
faravany: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 5 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: PSET (6 + x0, 480 - 4 + y0), k0: PSET (5 + x0, 480 - 5 + y0), k0:  _
LINE (6 + x0, 480 - 6 + y0)-(5 + x0, 480 - 6 + y0), k0: : PSET (5 + x0, 480 - 2 + y0), k0: n = 2: x0 = x0 - 8
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSet (6 + x0, 480 - 11 + y0), k0: Line (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0:: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: PSET (4 + x0, 480 - 9 + y0), k0: LINE (5 + x0, 480 - 10 + y0)-(7 + x0, 480 - 10 + y0), k0: LINE -(7 + x0, 480 - 11 + y0), k0: LINE ( _
6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 9 + y0), k0: n = 1: x0 = x0 - 8:
Return
dadeha: x0 = x0 - 8
Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Line (5 + x0, 480 - 8 + y0)-(3 + x0, 480 - 8 + y0), k0: Line (6 + x0, 480 - 7 + y0)-(6 + x0, 480 - 6 + y0), k0: Line -(4 + x0, 480 - 4 + y0), k0: Line (3 + x0, 480 - 7 + y0)-(5 + x0, 480 - 5 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (6 + x0, 480 - 8 + y0)-(0 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(3 + x0, 480 - 5 + y0), k0: LINE -(3 + x0, 480 - 4 + y0), k0: LINE (2 + x0, 480 - 6 + y0)-(2 + x0, 480 - 8 + _
 y0), k0: LINE (4 + x0, 480 - 5 + y0)-(4 + x0, 480 - 7 + y0), k0: n = 19: x0 = x0 - 8:
Line (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: Line (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Return
kol: Line (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: x0 = x0 - 8
Line (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: Line (1 + x0, 472 + y0)-(7 + x0, 472 + y0), k0: Line (7 + x0, 473 + y0)-(5 + x0, 476 + y0), k0: Line (5 + x0, 477 + y0)-(7 + x0, 480 + y0), k0:: n = 2: x0 = x0 - 8:
Line (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: Line (7 + x0, 479 + y0)-(7 + x0, 469 + y0), k0: Line (6 + x0, 468 + y0)-(3 + x0, 468 + y0), k0: Line (2 + x0, 469 + y0)-(2 + x0, 472 + y0), k0: n = 1: x0 = x0 - 8:
Return
jadval: Line (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (3 + x0, 480 - 6 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 5 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: n = 3 _
: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 + y0)-(8 + x0, 466 + y0), 0, BF: Line (7 + x0, 479 + y0)-(7 + x0, 469 + y0), k0: Line (6 + x0, 468 + y0)-(3 + x0, 468 + y0), k0: Line (2 + x0, 469 + y0)-(2 + x0, 472 + y0), k0: n = 1: x0 = x0 - 8:: x0 = x0 - 7
Return
motlagh: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - _
 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: n = 17: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (3 + x0, 480 -  _
8 + y0)-(3 + x0, 480 - 1 + y0), k0: : n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 1 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: n = 16: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(2 + x0, 480 - 12 + y0), k0: LINE (1 + x0, 480 - 11 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (6 + x0,  _
480 - 5 + y0)-(5 + x0, 480 - 5 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE (4 + x0, 480 - 7 + y0)-(4 + x0, 480 - 6 + y0), k0: PSET (4 + x0, 480 - 2 + y0), k0: PSET (6 + x0, 480 - 2 + y0), k0: n = 1: x0 = x0 - 8:
Return
namayesh:
Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
Line (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: Line (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 11 + y0), k0: PSet (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
 LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE (4 + x0, 480 - 7 + y0)-(4 + x0, 480 - 6 + y0), k0: LINE (3 + x0, 480 - 8 + y0)-(2 + x0, 480 - 8 + y0), k0: LINE (1 + x0, 480 - 6 +  _
y0)-(1 + x0, 480 - 10 + y0), k0: : PSET (6 + x0, 480 - 3 + y0), k0: PSET (4 + x0, 480 - 3 + y0), k0: PSET (5 + x0, 480 - 2 + y0), k0: x0 = x0 - 8: LINE (8 + x0, 480 - 11 + y0)-(4 + x0, 480 - 11 + y0), k0: LINE (3 + x0, 480 - 10 + y0)-(3 + x0, 480 -  _
7 + y0), k0: n = 1: x0 = x0 - 8:
Return
tul: LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (3 + x0,  _
480 - 8 + y0)-(3 + x0, 480 - 1 + y0), k0: : n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 1 + y0)-(7 + x0, 480 - 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - _
 8:
Return
dasteh: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(5 + x0, 480 - 6 + y0), k0: LINE (3 + x0, 480 -  _
8 + y0)-(3 + x0, 480 - 6 + y0), k0: : n = 7: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: PSET (2 + x0, 480 - 3 + y0), k0: n = 2:  _
x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: LINE -(3 + x0, 480 - 6 + y0), k0: LINE -(3 + x0, 480 - 8 + y0), k0: LINE -(4 _
 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Return
jabehee: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (3 + x0, 480 - 6 + y0)-(4 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 5 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: n = 3 _
: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 7 + y0)-(4 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 6 + y0)-(3 + x0, 480 - 6 + y0), k0: n = 12: x0 = x0 - 8 _
:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (3 + x0, 480 - 11 + y0), k0: n = 2: x0 = x0 - 8:
Line (7 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: Line (5 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: Line -(3 + x0, 480 - 6 + y0), k0: Line -(3 + x0, 480 - 8 + y0), k0: Line -(4 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 6 + y0)-(6 + x0, 480 - 6 + y0), k0: LINE -(5 + x0, 480 - 7 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 9 + y0)-(7 + x0, 480 - _
 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Return
ebteda: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (3 + x0, 480 - 11 + y0), k0: n = 2: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 3 + y0), k0: PSET (2 + x0, 480 - 3 + y0), k0: n = 2:  _
x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Return
hodud: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (3 + x0, 480 - 6 + y0)-(4 + x0, 480 - 5 + y0), k0: Line (5 + x0, 480 - 5 + y0)-(7 + x0, 480 - 8 + y0), k0: Line -(1 + x0, 480 - 8 + y0), k0:: n = 5: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Return
tajamoei: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
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
Return
nesby: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(5 + x0, 480 - 6 + y0), k0: LINE (3 + x0, 480 -  _
8 + y0)-(3 + x0, 480 - 6 + y0), k0: : n = 7: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (3 + x0, 480 - 11 + y0), k0: n = 2: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: PSET (4 + x0, 480 - 9 + y0), k0: LINE (5 + x0, 480 - 10 + y0)-(7 + x0, 480 - 10 + y0), k0: LINE -(7 + x0, 480 - 11 + y0), k0: LINE ( _
6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 9 + y0), k0: n = 1: x0 = x0 - 8:
Return
darsad: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSet (6 + x0, 480 - 11 + y0), k0: Line (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0:: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(5 + x0, 480 - 5 + y0), k0: LINE (4 + x0, 480 -  _
8 + y0)-(4 + x0, 480 - 6 + y0), k0: LINE (2 + x0, 480 - 8 + y0)-(2 + x0, 480 - 7 + y0), k0: : n = 9: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: x0 = x0 - 8
Return
mianeh:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 11 + y0), k0: PSet (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
Line (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: Line (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
Line (7 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: Line (5 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: Line -(3 + x0, 480 - 6 + y0), k0: Line -(3 + x0, 480 - 8 + y0), k0: Line -(4 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Return
mood:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Return
miangin:
 LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + _
 y0), k0: n = 17: x0 = x0 - 8:
Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 11 + y0), k0: PSet (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
Line (6 + x0, 480 - 1 + y0)-(6 + x0, 480 - 7 + y0), k0: Line (7 + x0, 480 - 8 + y0)-(8 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
Line (1 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: Line (7 + x0, 480 - 7 + y0)-(5 + x0, 480 - 4 + y0), k0: Line (5 + x0, 480 - 3 + y0)-(7 + x0, 480 - 0 + y0), k0:: Line (4 + x0, 480 - 1 + y0)-(3 + x0, 480 - 2 + y0), k0: n = 2: x0 = x0 - 8:
Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 11 + y0), k0: PSet (2 + x0, 480 - 11 + y0), k0: n = 21: x0 = x0 - 8:
Line (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 11 + y0), k0: Line (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: Line (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: PSet (4 + x0, 480 - 6 + y0), k0: n = 1: x0 = x0 - 8:
Return
nemodare: Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0:: PSet (4 + x0, 480 - 3 + y0), k0: n = 18: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (6 + x0, 480 - 8 + y0)-(7 + x0, 480 - 8 + y0), k0: LINE -(7 + x0, 480 - 6 + y0), k0: LINE (6 + x0, 480 - 5 + y0)-(6 + x0, 480 - 5 + y0), k0: LINE (5 + x0, 480 - 6 + y0)-(5 + x0, 480 - _
 7 + y0), k0: LINE (4 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: n = 17: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 8 + y0)-(5 + x0, 480 - 8 + y0), k0: LINE -(5 + x0, 480 - 6 + y0), k0: PSET (6 + x0, 480 - 5 + y0), k0: LINE (7 + x0, 480 - 6 + y0)-(7 + x0, 480 - 9 + y0), k0: LINE -(5  _
+ x0, 480 - 11 + y0), k0: LINE (4 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSet (6 + x0, 480 - 11 + y0), k0: Line (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0:: n = 1: x0 = x0 - 8:
Return
daerehei: Line (0 + x0, 480 - 0 * a0 + y0)-(8 + x0, 14 * a0 + y0), 0, BF: x0 = x0 - 8
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 4 + y0)-(7 + x0, 480 - 6 + y0), k0: Line -(7 + x0, 480 - 7 + y0), k0: Line (6 + x0, 480 - 8 + y0)-(4 + x0, 480 - 8 + y0), k0:: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 5 + y0)-(7 + x0, 480 - 7 + y0), k0: LINE (6 + x0, 480 - 8 + y0)-(1 + x0, 480 - 8 + y0), k0: : PSET (4 + x0, 480 - 11 + y0), k0: PSET (2 + x0, 480 - 11 + y0), k0: n = 21 _
: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (7 + x0, 480 - 7 + y0)-(7 + x0, 480 - 10 + y0), k0: PSet (6 + x0, 480 - 11 + y0), k0: Line (5 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0:: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (5 + x0, 480 - 8 + y0)-(3 + x0, 480 - 8 + y0), k0: LINE (6 + x0, 480 - 7 + y0)-(6 + x0, 480 - 6 + y0), k0: LINE -(4 + x0, 480 - 4 + y0), k0: LINE (3 + x0, 480 - 7 + y0)-(5 + x0, 480 - _
 5 + y0), k0: n = 1: x0 = x0 - 8:
Line (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: Line (5 + x0, 480 - 1 + y0)-(5 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
 LINE (0 + x0, 480 - 0 + y0)-(8 + x0, 480 - 14 + y0), 0, BF: LINE (7 + x0, 480 - 6 + y0)-(6 + x0, 480 - 6 + y0), k0: LINE -(5 + x0, 480 - 7 + y0), k0: LINE (5 + x0, 480 - 8 + y0)-(6 + x0, 480 - 8 + y0), k0: LINE (7 + x0, 480 - 9 + y0)-(7 + x0, 480 - _
 11 + y0), k0: LINE (6 + x0, 480 - 12 + y0)-(3 + x0, 480 - 12 + y0), k0: LINE (2 + x0, 480 - 11 + y0)-(2 + x0, 480 - 8 + y0), k0: n = 1: x0 = x0 - 8:
Return
432 Line (0, 480 - 0)-(640, 480 - 15), 0, BF
ert = (td + 1) * 16
For dam = 112 To 448 Step 112
    Line (dam, 480 - 0)-(dam, 480 - ert), 10
Next dam
Line (0, 480 - 15)-(600, 480 - 15), 10
x0 = 85: y0 = 0: k0 = 14: GoSub hodud: GoSub dasteh
x0 = 210: GoSub faravany: GoSub motlagh
x0 = 322: GoSub faravany: GoSub tajamoei
x0 = 434: GoSub faravany: GoSub nesby
x0 = 546: GoSub darsad: GoSub faravany
Return

okoy:
1 Locate 28, 1: Print "ARA THESE RIGHT ? (Y for YES  and  N for NO)                                ": ak$ = Input$(1)
If ak$ = Chr$(110) Or ak$ = Chr$(78) Then Locate 28, 1: Input "PLEASE TYPY LIKE THIS : number of WRONG , RIGHT of this"; wrong, right: dad(wrong) = right
If ak$ = Chr$(121) Or ak$ = Chr$(89) Then Return
If ak$ = Chr$(27) Then End
GoTo 1
Return

nemodar: nes1 = 0
Cls: Screen 12: Window (0, 0)-(640, 480): For nes1 = 1 To nes11
    Circle (320, 240), radious, 3, 0, nemod(nes1) * -6.28, bez
Next nes1
If radious = 200 And bez = 1 Then Circle (320, 240), 230, 4, 0, 1: Line (455, 400)-(445, 433), 4: Line -(475, 430), 4
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If radious < 320 Then If ak$ = Chr$(42) Then radious = radious + 15: GoSub nemodar
    If radious > 15 Then If ak$ = Chr$(47) Then radious = radious - 15: GoSub nemodar
    If ak$ = Chr$(43) Then bez = bez - .1: GoSub nemodar
    If ak$ = Chr$(45) Then bez = bez + .1: GoSub nemodar
Wend
Return

stony:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
For mot = 1 To td
    If BF < far(mot) Then BF = far(mot)
Next mot
Cls: Window (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
Line (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
Line (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
For kha = 1 To BF
    Line (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
Next kha
For mot = 1 To td
    kl = kl + 1
    If kl > 14 Then kl = kl - 14
    Line (0 + hf, 0)-(hf + tu, far(mot)), kl, BF
    hf = hf + tu
    Line (-bs1 * td * tu / 4, far(mot))-(-3 * bs1 * td * tu / 4, far(mot)), 13
Next mot
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If ak$ = Chr$(48) Then Swap ki, ki1: GoSub khafm
    If ak$ = Chr$(42) Then bs = bs / 1.5: bs1 = bs1 / 1.5: GoSub stony
    If ak$ = Chr$(47) Then bs = bs * 1.5: bs1 = bs1 * 1.5: GoSub stony
    If jah = 1 Then If ak$ = Chr$(45) Then bs = bs * 1.5: GoSub stony
    If jah = 1 Then If ak$ = Chr$(43) Then bs = bs / 1.5: GoSub stony
    If jah = 2 Then If ak$ = Chr$(45) Then bs1 = bs1 * 1.5: GoSub stony
    If jah = 2 Then If ak$ = Chr$(43) Then bs1 = bs1 / 1.5: GoSub stony
    If ak$ = Chr$(46) Then If jah = 2 Then jah = 1: Locate 2, 1: Print " "; Chr$(18); " " Else jah = 2: Locate 2, 1: Print "® ¯"
Wend
Return

stony1:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
For taj = 1 To td
    If BF < fara(taj) Then BF = fara(taj)
Next taj
Cls: Window (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
Line (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
Line (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
For kha = 1 To BF
    Line (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
Next kha
For taj = 1 To td
    kl = kl + 1
    If kl > 14 Then kl = kl - 14
    Line (0 + hf, 0)-(hf + tu, fara(taj)), kl, BF
    hf = hf + tu
    Line (-bs1 * td * tu / 4, fara(taj))-(-3 * bs1 * td * tu / 4, fara(taj)), 13
Next taj
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If ak$ = Chr$(48) Then Swap ki, ki1: GoSub khaft
    If ak$ = Chr$(42) Then bs = bs / 1.5: bs1 = bs1 / 1.5: GoSub stony1
    If ak$ = Chr$(47) Then bs = bs * 1.5: bs1 = bs1 * 1.5: GoSub stony1
    If jah = 1 Then If ak$ = Chr$(45) Then bs = bs * 1.5: GoSub stony1
    If jah = 1 Then If ak$ = Chr$(43) Then bs = bs / 1.5: GoSub stony1
    If jah = 2 Then If ak$ = Chr$(45) Then bs1 = bs1 * 1.5: GoSub stony1
    If jah = 2 Then If ak$ = Chr$(43) Then bs1 = bs1 / 1.5: GoSub stony1
    If ak$ = Chr$(46) Then If jah = 2 Then jah = 1: Locate 2, 1: Print " "; Chr$(18); " " Else jah = 2: Locate 2, 1: Print "® ¯"
Wend
Return

miley:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
For mot = 1 To td
    If BF < far(mot) Then BF = far(mot)
Next mot
Cls: Screen 12: Window (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
Line (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
Line (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
For kha = 1 To BF
    Line (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
Next kha
For mot = 1 To td
    kl = kl + 1
    If kl > 14 Then kl = kl - 14
    Line (hf + (tu / 2), 0)-(hf + (tu / 2), far(mot)), kl
    hf = hf + tu
    Line (-bs1 * td * tu / 4, far(mot))-(-3 * bs1 * td * tu / 4, far(mot)), 13
Next mot
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If ak$ = Chr$(48) Then Swap ki, ki1: GoSub khafm
    If ak$ = Chr$(42) Then bs = bs / 1.5: bs1 = bs1 / 1.5: GoSub miley
    If ak$ = Chr$(47) Then bs = bs * 1.5: bs1 = bs1 * 1.5: GoSub miley
    If jah = 1 Then If ak$ = Chr$(45) Then bs = bs * 1.5: GoSub miley
    If jah = 1 Then If ak$ = Chr$(43) Then bs = bs / 1.5: GoSub miley
    If jah = 2 Then If ak$ = Chr$(45) Then bs1 = bs1 * 1.5: GoSub miley
    If jah = 2 Then If ak$ = Chr$(43) Then bs1 = bs1 / 1.5: GoSub miley
    If ak$ = Chr$(46) Then If jah = 2 Then jah = 1: Locate 2, 1: Print " "; Chr$(18); " " Else jah = 2: Locate 2, 1: Print "® ¯"
Wend
Return

helf:
Cls: Screen 12: Window (0, 0)-(639, 480)
LINE (25, 60)-(45, 51), 2, B: LINE -(65, 80), 2, B: LINE (65, 51)-(85, 90), 2, B: LINE (85, 51)-(105, 95), 2, B: LINE (105, 51)-(125, 120), 2, B: LINE (20, 159)-(20, 50), 4: LINE -(159, 50), 4: x0 = 151: y0 = -437: k0 = 3: GOSUB faravany: GOSUB  _
tajamoei: LOCATE 28, 4: PRINT " 1 :"
LINE (25, 220)-(45, 211), 2, B: LINE -(65, 250), 2, B: LINE (65, 211)-(85, 230), 2, B: LINE (85, 211)-(105, 285), 2, B: LINE (105, 211)-(125, 240), 2, B: LINE (20, 319)-(20, 210), 4: LINE -(159, 210), 4: x0 = 151: y0 = -277: k0 = 3: GOSUB faravany:  _
GOSUB motlagh: LOCATE 18, 4: PRINT " 4 :"
Line (35, 381)-(55, 400), 2: Line -(75, 410), 2: Line -(95, 415), 2: Line -(115, 440), 2:: Line (20, 479)-(20, 370), 4: Line -(159, 370), 4: x0 = 151: y0 = -117: k0 = 3: GoSub faravany: GoSub tajamoei: Locate 8, 4: Print " 7 :"
Line (245, 380)-(265, 410), 2: Line -(285, 390), 2: Line -(305, 465), 2: Line -(325, 400), 2:: Line (230, 479)-(230, 370), 4: Line -(369, 370), 4: x0 = 361: y0 = -117: k0 = 3: GoSub faravany: GoSub motlagh: Locate 8, 30: Print " 8 :"
Line (220, 300)-(400, 300), 4: Line (255, 317)-(255, 210), 4: Line (295, 317)-(295, 210), 4: Line (335, 317)-(335, 210), 4: Line (375, 317)-(375, 210), 4: x0 = 361: y0 = -277: k0 = 3: GoSub jadval: GoSub faravany: Locate 18, 30: Print " 5 :"
Line (320, 105)-(374, 105), 2: Line (320, 105)-(366, 132), 2: Line (320, 105)-(274, 132), 2: Line (320, 105)-(301, 54), 2: Circle (320, 105), 54, 4: x0 = 370: y0 = -437: k0 = 3: GoSub nemodare: GoSub daerehei: Locate 28, 30: Print " 2 :"
LINE (455, 60)-(455, 51), 2: LINE (475, 51)-(475, 80), 2: LINE (495, 51)-(495, 90), 2: LINE (515, 51)-(515, 95), 2: LINE (535, 51)-(535, 120), 2: LINE (440, 159)-(440, 50), 4: LINE -(579, 50), 4: x0 = 571: y0 = -437: k0 = 3: GOSUB faravany: GOSUB  _
tajamoei: LOCATE 28, 56: PRINT " 3 :"
LINE (455, 220)-(455, 211), 2: LINE (475, 211)-(475, 250), 2: LINE (495, 211)-(495, 230), 2: LINE (515, 211)-(515, 285), 2: LINE (535, 211)-(535, 240), 2: : LINE (440, 319)-(440, 210), 4: LINE -(579, 210), 4: x0 = 571: y0 = -277: k0 = 3: GOSUB  _
faravany: GOSUB motlagh: LOCATE 18, 56: PRINT " 6 :"
x0 = 571: y0 = -117: k0 = 3: GoSub namayesh: GoSub dadeha: Locate 8, 56: Print " 9 :"
x0 = 560: y0 = -37: k0 = 11: GoSub mood
x0 = 560: y0 = -53: k0 = 11: GoSub mianeh
x0 = 560: y0 = -69: k0 = 11: GoSub miangin
x0 = 560: y0 = -85: k0 = 11: GoSub nemodare: GoSub jabehee

While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
Wend
Return

miley1:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
For taj = 1 To td
    If BF < fara(taj) Then BF = fara(taj)
Next taj
Cls: Screen 12: Window (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
Line (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
Line (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
For kha = 1 To BF
    Line (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
Next kha
For taj = 1 To td
    kl = kl + 1
    If kl > 14 Then kl = kl - 14
    Line (hf + (tu / 2), 0)-(hf + (tu / 2), fara(taj)), kl
    hf = hf + tu
    Line (-bs1 * td * tu / 4, fara(taj))-(-3 * bs1 * td * tu / 4, fara(taj)), 13
Next taj
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If ak$ = Chr$(48) Then Swap ki, ki1: GoSub khaft
    If ak$ = Chr$(42) Then bs = bs / 1.5: bs1 = bs1 / 1.5: GoSub miley1
    If ak$ = Chr$(47) Then bs = bs * 1.5: bs1 = bs1 * 1.5: GoSub miley1
    If jah = 1 Then If ak$ = Chr$(45) Then bs = bs * 1.5: GoSub miley1
    If jah = 1 Then If ak$ = Chr$(43) Then bs = bs / 1.5: GoSub miley1
    If jah = 2 Then If ak$ = Chr$(45) Then bs1 = bs1 * 1.5: GoSub miley1
    If jah = 2 Then If ak$ = Chr$(43) Then bs1 = bs1 / 1.5: GoSub miley1
    If ak$ = Chr$(46) Then If jah = 2 Then jah = 1: Locate 2, 1: Print " "; Chr$(18); " " Else jah = 2: Locate 2, 1: Print "® ¯"
Wend
Return

chnbr:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
For mot = 1 To td
    If BF < far(mot) Then BF = far(mot)
Next mot
Cls: Screen 12: Window (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
Line (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
Line (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
For kha = 1 To BF
    Line (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
Next kha
For mot = 1 To td - 1
    kl = kl + 1
    If kl > 14 Then kl = kl - 14
    Line (hf + (tu / 2), far(mot))-(tu + hf + (tu / 2), far(mot + 1)), kl
    hf = hf + tu
    Line (-bs1 * td * tu / 4, far(mot))-(-3 * bs1 * td * tu / 4, far(mot)), 13
Next mot
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If ak$ = Chr$(48) Then Swap ki, ki1: GoSub khafm
    If ak$ = Chr$(42) Then bs = bs / 1.5: bs1 = bs1 / 1.5: GoSub chnbr
    If ak$ = Chr$(47) Then bs = bs * 1.5: bs1 = bs1 * 1.5: GoSub chnbr
    If jah = 1 Then If ak$ = Chr$(45) Then bs = bs * 1.5: GoSub chnbr
    If jah = 1 Then If ak$ = Chr$(43) Then bs = bs / 1.5: GoSub chnbr
    If jah = 2 Then If ak$ = Chr$(45) Then bs1 = bs1 * 1.5: GoSub chnbr
    If jah = 2 Then If ak$ = Chr$(43) Then bs1 = bs1 / 1.5: GoSub chnbr
    If ak$ = Chr$(46) Then If jah = 2 Then jah = 1: Locate 2, 1: Print " "; Chr$(18); " " Else jah = 2: Locate 2, 1: Print "® ¯"
Wend
Return

chnbr1:
hf = 0: kl = 0: BF = 0: ki = 8: ki1 = 0
For taj = 1 To td
    If BF < fara(taj) Then BF = fara(taj)
Next taj
Cls: Screen 12: Window (-bs1 * td * tu, -bs * BF)-(td * tu + bs1 * td * tu, BF + bs * BF)
Line (-bs1 * td * tu / 2, 0)-(-bs1 * td * tu / 2, BF + bs * BF), 15
Line (-bs1 * td * tu / 2, 0)-(td * tu + bs1 * td * tu, 0), 15
For kha = 1 To BF
    Line (-bs1 * td * tu / 4, kha)-(-3 * bs1 * td * tu / 4, kha), 14
Next kha
For taj = 1 To td - 1
    kl = kl + 1
    If kl > 14 Then kl = kl - 14
    Line (hf + (tu / 2), fara(taj))-(tu + hf + (tu / 2), fara(taj + 1)), kl
    hf = hf + tu
    Line (-bs1 * td * tu / 4, fara(taj))-(-3 * bs1 * td * tu / 4, fara(taj)), 13
Next taj
While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If ak$ = Chr$(48) Then Swap ki, ki1: GoSub khaft
    If ak$ = Chr$(42) Then bs = bs / 1.5: bs1 = bs1 / 1.5: GoSub chnbr1
    If ak$ = Chr$(47) Then bs = bs * 1.5: bs1 = bs1 * 1.5: GoSub chnbr1
    If jah = 1 Then If ak$ = Chr$(45) Then bs = bs * 1.5: GoSub chnbr1
    If jah = 1 Then If ak$ = Chr$(43) Then bs = bs / 1.5: GoSub chnbr1
    If jah = 2 Then If ak$ = Chr$(45) Then bs1 = bs1 * 1.5: GoSub chnbr1
    If jah = 2 Then If ak$ = Chr$(43) Then bs1 = bs1 / 1.5: GoSub chnbr1
    If ak$ = Chr$(46) Then If jah = 2 Then jah = 1: Locate 2, 1: Print " "; Chr$(18); " " Else jah = 2: Locate 2, 1: Print "® ¯"
Wend
Return

khaft:
For taj = 1 To td
    Line (-bs1 * td * tu, fara(taj))-(td * tu + bs1 * td * tu, fara(taj)), ki
Next taj
Return

khafm:
For mot = 1 To td
    Line (-bs1 * td * tu, far(mot))-(td * tu + bs1 * td * tu, far(mot)), ki
Next mot
Return

sagheh: sh = 1
400 Cls: Window (0, 0)-(640, 480)
y0 = 0: Line (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 360: GoSub namayesh: GoSub dadeha: Print
For dade = 1 To t
    num(dade) = dad(dade)
Next dade
For s = 1 To t
    For s2 = s + 1 To t
        If num(s) > num(s2) Then Swap num(s), num(s2)
    Next s2
    If s Mod 26 = 0 Then Locate 26, 1: Print Chr$(25): ak$ = Input$(1): Locate 26, 1: Color 0: Print Chr$(25): Color 15
    Print , , s, num(s)
Next s
GoTo 321

401 Cls: s7 = 0: s18 = 0: bt = 0: i = 0: Window (0, 0)-(640, 480)
For s5 = 1 To t
    l = 0
    For s6 = 1 To t
        If num(s5) = num(s6) Then l = l + 1
    Next s6
    ted(s5) = l
Next s5
y0 = 0: Line (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 320: GoSub mood: Print
While s7 < t
    s18 = s18 + 1
    s7 = s7 + ted(s7 + 1)
    numbe(s18) = num(s7)
    ted1(s18) = ted(s7)
    If (s18 Mod 26) = 0 Then Locate 26, 1: Print Chr$(25): ak$ = Input$(1): Locate 26, 1: Color 0: Print Chr$(25): Color 15
    Print , , ted(s7); "*"; numbe(s18)
    If ted(s7) > bt Then bt = ted(s7): bt1 = s18
Wend
s185 = s18
i1 = (s185 Mod 26)
Color 14
For s18 = 1 To s185
    If (i1 + i) Mod 26 = 0 Then Locate 27, 1: Print Chr$(25): ak$ = Input$(1): Locate 27, 1: Color 0: Print Chr$(25): Color 14: i1 = 0
    If bt = ted1(s18) Then Print , , , numbe(s18): i = i + 1
Next s18
Color 15
GoTo 321

402 Cls: Window (0, 0)-(640, 480)
y0 = 0: Line (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 340: GoSub miangin: Print
miangin = (bd + kd) / 2
Print , , miangin
y0 = -48: Line (0, 480 + y0)-(640, 480 + y0 - 15), 8, BF
k0 = 15: x0 = 332: GoSub mianeh: Print: Print
If t Mod 2 = 0 Then miane = (num(t / 2) + num((t / 2) + 1)) / 2: t2 = (t + 1) / 2: t3 = t2 - .5: t4 = t2 + .5
If t Mod 2 <> 0 Then miane = num((t \ 2) + 1): t2 = (t \ 2) + 1: t3 = t \ 2: t4 = (t \ 2) + 2
If t3 Mod 2 = 0 Then charak1 = (num(t3 / 2) + num((t3 / 2) + 1)) / 2: charak3 = (num((t3 / 2) + t3) + num((t3 / 2) + t3 + 1)) / 2
If t3 Mod 2 <> 0 Then charak1 = num((t3 \ 2) + 1): charak3 = num((t3 \ 2) + t4)

Print , , miane, charak1, charak3
GoTo 321

403 Cls: Window (-bs * (bd - kd), -10)-(bs * (bd - kd) + (bd - kd), 10)
Line (0, 0)-(bd - kd, 0)
Line (charak1 - kd, -1)-(charak3 - kd, 1), 13, BF
Line (miane - kd, 1)-(miane - kd, -4.6), 8: Line -(-bs * (bd - kd), -4.6), 8: Locate 15 + 7, 1: Print miane
Circle (0, 0), bs * bs * (bd - kd)
Circle (bd - kd, 0), bs * bs * (bd - kd)
Line (0, 0)-(0, -2), 8: Line -(-bs * (bd - kd), -2), 8: Locate 15 + 3, 1: Print kd
Line (charak1 - kd, 0)-(charak1 - kd, -3.3), 8: Line -(-bs * (bd - kd), -3.3), 8: Locate 15 + 5, 1: Print charak1
Line (charak3 - kd, 0)-(charak3 - kd, -6), 8: Line -(-bs * (bd - kd), -6), 8: Locate 15 + 9, 1: Print charak3
Line (bd - kd, 0)-(bd - kd, -7.3), 8: Line -(-bs * (bd - kd), -7.3), 8: Locate 15 + 11, 1: Print bd

321 While ak$ <> Chr$(27)
    ak$ = Input$(1)
    If ak$ = Chr$(104) Or ak$ = Chr$(72) Then GoSub helf
    If ak$ = Chr$(50) Then GoSub nemodar
    If ak$ = Chr$(49) Then GoSub stony1
    If ak$ = Chr$(52) Then GoSub stony
    If ak$ = Chr$(51) Then GoSub miley1
    If ak$ = Chr$(54) Then GoSub miley
    If ak$ = Chr$(55) Then GoSub chnbr1
    If ak$ = Chr$(56) Then GoSub chnbr
    If ak$ = Chr$(53) Then GoTo 123
    If ak$ = Chr$(57) Then GoSub sagheh
    If sh < 4 Then If ak$ = Chr$(43) Then sh = sh + 1: On sh GOTO 400, 401, 402, 403
    If sh > 1 Then If ak$ = Chr$(45) Then sh = sh - 1: On sh GOTO 400, 401, 402, 403
Wend
Return

