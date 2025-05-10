common flag
ON ERROR GOTO ert
 pix = 1: j = 1: Xm1 = 1: zx = 0: zx1 = 1:ste=j:cls
INPUT"The graph should start from "; aa
INPUT "The graph should stop on "; aa1
 IF aa > aa1 THEN SWAP aa, aa1
 a = aa: a1 = aa1
 DEF fnf (x) = sin(x)
''''''''''
70 gosub hesab
''''''''''
80 gosub rasm
''''''''''
 WHILE ak$ <> CHR$(27)
 ak$ = INKEY$
 IF ak$ = CHR$(43) THEN pix = pix + 1: GOTO 80
 IF ak$ = CHR$(45) THEN
  pix = pix - 1: IF pix = 0 THEN pix = 1
  GOTO 70
 END IF
 IF ak$ = CHR$(46) THEN pix = 1: GOTO 70
 IF ak$ = CHR$(48) THEN j = 1: a = aa: a1 = aa1: pix = 1: GOTO 70
 IF ak$ = CHR$(42) THEN a1 = a1 / 2: a = a / 2: GOTO 70
 IF ak$ = CHR$(47) THEN a1 = a1 * 2: a = a * 2: GOTO 70
 IF ak$ = CHR$(99) OR ak$ = CHR$(67) THEN SWAP a, a1: SWAP x001, x002: j = j * -1: GOTO 80
 IF ak$ = CHR$(90) OR ak$ = CHR$(122) THEN SWAP zx, zx1: GOTO 70
 IF ak$ = CHR$(50) THEN GOSUB 1100: k01 = 8: GOSUB 1200: y00 = y00 - 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF ak$ = CHR$(119) OR ak$ = CHR$(87) THEN ste = j * (Xm1 / 320): GOTO 70
 IF ak$ = CHR$(52) THEN GOSUB 1100: k01 = 8: GOSUB 1200: x00 = x00 - 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF ak$ = CHR$(54) THEN GOSUB 1100: k01 = 8: GOSUB 1200: x00 = x00 + 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF ak$ = CHR$(56) THEN GOSUB 1100: k01 = 8: GOSUB 1200: y00 = y00 + 1: k00 = POINT(x00, y00): GOSUB 1000: IF n000 MOD 2 <> 0 THEN GOSUB 1200
 IF ak$ = CHR$(53) THEN
  n000 = n000 + 1
  IF n000 MOD 2 <> 0 THEN x001 = x00: y001 = y00
  IF n000 MOD 2 = 0 THEN x002 = x00: y002 = y00: IF x001 <> x002 OR y001 <> y002 THEN gosub xcop
 END IF
 LOCATE 1, 1: PRINT "("; x00; ","; y00; ") "
WEND
print"Do you want to try again ?(Press <N> to exit)" : ak$=input$(1):if ucase$(ak$)=chr$(78) then flag=1 else flag=0
chain"rtab0.bas"
end
1000 PSET (x00, y00), 14: RETURN
1100 PSET (x00, y00), k00: RETURN
1200 LINE (x001, y001)-(x00, y00), k01, B: k01 = 10: LINE (-Xm, 0)-(Xm, 0): LINE (0, -Ym)-(0, Ym): RETURN
Dfg:IR = x = x: IZ = x = INT(x): IN = x = INT(x) AND x > 0: IW = x = INT(x) AND x >= 0: ISqr = x >= 0: Pi = 4 * ATN(1): Df =IR :return
hesab: Ym = 0: FOR x0 = a TO a1 STEP ste: x = x0: GOSUB Dfg
 IF Df = -1 THEN
  y = ABS(fnf(x))
  IF y > Ym THEN Ym = y
 END IF: NEXT x0: Xm = 320 * Ym / 240
 IF ABS(a) >= ABS(a1) THEN a2 = ABS(a) ELSE a2 = ABS(a1)
 IF a2 > Xm THEN Xm = a2: Ym = 240 * Xm / 320
 IF Xm1 <> 0 THEN CLS : SCREEN 12: WINDOW (-Xm, -Ym)-(Xm, Ym)
 GOSUB mehvar: Xm1 = Xm:ste=j:return
mehvar: FOR l = 0 TO Ym: LINE (-Xm, l)-(Xm, l), 8: LINE (-Xm, -l)-(Xm, -l), 8: NEXT l: FOR l = 0 TO Xm: LINE (l, -Ym)-(l, Ym), 8: LINE (-l, -Ym)-(-l, Ym), 8: NEXT l: LINE (-Xm, 0)-(Xm, 0): LINE (0, -Ym)-(0, Ym):return
rasm:FOR x0 = a TO a1 STEP j * (Xm1 / 320) / pix: x = x0: GOSUB Dfg: IF Df = -1 THEN PSET (x, fnf(x)), 12: GOTO 52
 NEXT x0
52 FOR x0 = a TO a1 STEP j * (Xm1 / 320) / pix: x = x0: GOSUB Dfg
 if zx=0 then IF Df = -1 THEN PSET (x, fnf(x)), 12
 IF zx = 1 THEN IF Df = -1 THEN LINE -(x, fnf(x)), 12
 NEXT x0: RETURN
xcop: IF x001 > x002 THEN SWAP x001, x002
IF y001 > y002 THEN SWAP y001, y002
IF j = -1 THEN SWAP x001, x002
x000 = x002 - x001: y000 = y002 - y001
IF x000 >= (4 / 3) * y000 THEN
 yn00 = x000 * (3 / 4): yk00 = (yn00 - y000) / 2: CLS : SCREEN 12: WINDOW (x001, y001 - yk00)-(x002, y002 + yk00)
END IF
IF x000 < (4 / 3) * y000 THEN
 xn00 = y000 * (4 / 3): xk00 = (xn00 - x000) / 2: CLS : SCREEN 12: WINDOW (x001 - xk00, y001)-(x002 + xk00, y002)
END IF
 a = x001: a1 = x002: Xm1 = 0
GOSUB hesab: LINE (x001, y001)-(x002, y002), 10, B: Xm1 = x002 - x001 + (2 * xk00): GOSUB rasm: RETURN
ert: PRINT"ERROR"ERR :print "TRY AGAIN,PLEASE"flag=2:chain"rtab0.bas"
function ASN(x)
cx = SQR(1 - (x) ^ 2)
IF cx = 0 THEN ASN=1.57: goto 300
tx = x / cx: ASN = ATN(tx)
300 end function
function ACS(x)
PI=4*atn(1):if x=0 then ACS=1.57:goto 400
sx = SQR(1 - (x) ^ 2):tx = sx / x:
if x<0 then ACS = ATN(tx)+PI else acs=atn(tx)
400 end function
