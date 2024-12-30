DECLARE SUB rlin ()
DECLARE SUB mehvar ()
DECLARE SUB xcop ()
DECLARE SUB Dfg ()
DECLARE SUB hesab ()
DECLARE SUB rasm ()
COMMON SHARED x, a, a1, pix, Df, Dg, x001, x002, y001, y002, Xm, Ym, j, Xm1, zx, ste, PI, lin
 CLS : SCREEN 12
 pix = 1: j = 1: Xm1 = 1: zx = 0: zx1 = 1: ste = j: dot1 = 1
 INPUT "The graph should start from "; aa
 INPUT "The graph should stop on "; aa1
 IF aa > aa1 THEN SWAP aa, aa1
 a = aa: a1 = aa1
DEF fnf (x) = ABS(SIN(x))
DEF fng (x) = SIN(x) ^ 2
''''''''''
70 CALL hesab
''''''''''
80 CALL rasm
''''''''''
 WHILE ak$ <> CHR$(27)
 ak$ = INKEY$: GOSUB 1000
 IF ak$ = CHR$(43) THEN pix = pix + 1: GOTO 80
 IF ak$ = CHR$(45) THEN
  pix = pix - 1: IF pix = 0 THEN pix = 1
  GOTO 70
 END IF
 IF ak$ = CHR$(46) THEN pix = 1: GOTO 70
 IF ak$ = CHR$(48) THEN j = 1: a = aa: a1 = aa1: pix = 1: GOTO 70
 IF ak$ = CHR$(42) THEN a1 = a1 / 2: a = a / 2: GOTO 70
 IF ak$ = CHR$(47) THEN a1 = a1 * 2: a = a * 2: GOTO 70
 IF ak$ = CHR$(119) OR ak$ = CHR$(87) THEN ste = j * (Xm1 / 320): GOTO 70
 IF ak$ = CHR$(27) THEN END
 IF ak$ = CHR$(99) OR ak$ = CHR$(67) THEN SWAP a, a1: SWAP x001, x002: j = j * -1: GOTO 80
 IF ak$ = CHR$(90) OR ak$ = CHR$(122) THEN SWAP zx, zx1: GOTO 70
 IF ak$ = CHR$(126) OR ak$ = CHR$(96) THEN SWAP dot, dot1: k00 = 0: GOSUB 1100: x00 = CINT(x00): y00 = CINT(y00): x001 = x00: y001 = y00: GOSUB 1000
 IF dot = 1 THEN IF ak$ = CHR$(50) THEN GOSUB 1100: GOSUB 1201: y00 = y00 - (Xm1 / 320): k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1201
 IF dot = 1 THEN IF ak$ = CHR$(52) THEN GOSUB 1100: GOSUB 1201: x00 = x00 - (Xm1 / 320): k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1201
 IF dot = 1 THEN IF ak$ = CHR$(54) THEN GOSUB 1100: GOSUB 1201: x00 = x00 + (Xm1 / 320): k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1201
 IF dot = 1 THEN IF ak$ = CHR$(56) THEN GOSUB 1100: GOSUB 1201: y00 = y00 + (Xm1 / 320): k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1201
 IF dot = 0 THEN IF ak$ = CHR$(50) THEN GOSUB 1100: k01 = 8: GOSUB 1200: y00 = y00 - 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF dot = 0 THEN IF ak$ = CHR$(52) THEN GOSUB 1100: k01 = 8: GOSUB 1200: x00 = x00 - 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF dot = 0 THEN IF ak$ = CHR$(54) THEN GOSUB 1100: k01 = 8: GOSUB 1200: x00 = x00 + 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF dot = 0 THEN IF ak$ = CHR$(56) THEN GOSUB 1100: k01 = 8: GOSUB 1200: y00 = y00 + 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF ak$ = CHR$(53) THEN
  n000 = n000 + 1
  IF n000 MOD 2 <> 0 THEN x001 = x00: y001 = y00
  IF n000 MOD 2 = 0 THEN x002 = x00: y002 = y00: IF x001 <> x002 OR y001 <> y002 THEN CALL xcop
 END IF
 x010 = CLNG(100 * x00): y010 = (CLNG(100 * y00)): LOCATE 1, 1: PRINT "("; x010 / 100; ","; y010 / 100; ")  ",
 IF ak$ = "x" OR ak$ = "X" THEN ak$ = INPUT$(1): IF ak$ = "=" OR ak$ = "+" THEN lin = 1: CALL rlin
 IF ak$ = "y" OR ak$ = "Y" THEN ak$ = INPUT$(1): IF ak$ = "=" OR ak$ = "+" THEN lin = 2: CALL rlin
WEND
END
1000 PSET (x00, y00), 14: RETURN
1100 PSET (x00, y00), k00: RETURN
1200 LINE (x001, y001)-(x00, y00), k01, B: k01 = 10: LINE (-Xm, 0)-(Xm, 0): LINE (0, -Ym)-(0, Ym): RETURN
1201 LINE (-Xm, 0)-(Xm, 0): LINE (0, -Ym)-(0, Ym): RETURN

SUB Dfg
IR = x = x
IZ = x = INT(x)
IN = x = INT(x) AND x > 0
IW = x = INT(x) AND x >= 0
ISqr = x >= 0
PI = 4 * ATN(1)
  Df = IR AND x <> 0

END SUB

SUB hesab

 Ym = 0
 FOR x0 = a TO a1 STEP ste
 x = x0
 CALL Dfg
 IF Df = -1 THEN
  y = ABS(fnf(x))
  IF y > Ym THEN Ym = y
 END IF
 NEXT x0
 Xm = 320 * Ym / 240
 IF ABS(a) >= ABS(a1) THEN a2 = ABS(a) ELSE a2 = ABS(a1)
 IF a2 > Xm THEN Xm = a2: Ym = 240 * Xm / 320
 IF Xm1 <> 0 THEN CLS : SCREEN 12: WINDOW (-Xm, -Ym)-(Xm, Ym)
 CALL mehvar
 Xm1 = Xm
 ste = j
END SUB

SUB mehvar
 FOR l = 0 TO Ym
 LINE (-Xm, l)-(Xm, l), 8: LINE (-Xm, -l)-(Xm, -l), 8
 NEXT l
 FOR l = 0 TO Xm
 LINE (l, -Ym)-(l, Ym), 8: LINE (-l, -Ym)-(-l, Ym), 8
 NEXT l
 LINE (-Xm, 0)-(Xm, 0): LINE (0, -Ym)-(0, Ym)

END SUB

SUB rasm
 FOR x0 = a TO a1 STEP j * (Xm1 / 320) / pix
  x = x0
  CALL Dfg
  IF Df = -1 THEN PSET (x, fnf(x)), 12: GOTO 52
 NEXT x0
52 FOR x0 = a TO a1 STEP j * (Xm1 / 320) / pix
  x = x0
  CALL Dfg
  IF zx = 0 THEN IF Df = -1 THEN PSET (x, fnf(x)), 12: PSET (x, fng(x)), 14
  IF zx = 1 THEN IF Df = -1 THEN LINE -(x, fnf(x)), 12: LINE -(x, fng(x)), 14
 NEXT x0

END SUB

SUB rlin
IF lin = 1 THEN
 INPUT "x = "; o$
 IF LCASE$(RIGHT$(o$, 1)) = "n" THEN k = 0 ELSE k = 3
 LINE (VAL(o$), -Ym)-(VAL(o$), Ym), k
END IF
IF lin = 2 THEN
 INPUT "y = "; h$
 IF LCASE$(RIGHT$(h$, 1)) = "n" THEN k = 0 ELSE k = 3
 LINE (-Xm, VAL(h$))-(Xm, VAL(h$)), k
END IF
END SUB

SUB xcop
IF x001 > x002 THEN SWAP x001, x002
IF y001 > y002 THEN SWAP y001, y002
IF j = -1 THEN SWAP x001, x002
x000 = x002 - x001
y000 = y002 - y001
IF x000 >= (4 / 3) * y000 THEN
 yn00 = x000 * (3 / 4)
 yk00 = (yn00 - y000) / 2
 CLS : SCREEN 12: WINDOW (x001, y001 - yk00)-(x002, y002 + yk00)
END IF
IF x000 < (4 / 3) * y000 THEN
 xn00 = y000 * (4 / 3)
 xk00 = (xn00 - x000) / 2
 CLS : SCREEN 12: WINDOW (x001 - xk00, y001)-(x002 + xk00, y002)
END IF
 a = x001
 a1 = x002
 Xm1 = 0
CALL hesab
 LINE (x001, y001)-(x002, y002), 10, B
 Xm1 = x002 - x001 + (2 * xk00)
CALL rasm

END SUB

