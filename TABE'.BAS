CLS : pd = 1
INPUT " SIN = 1 ''  COS = 2 ''  TAN = 3 ''  COT = 4"; mehvarm
CLS
ON mehvarm GOSUB nsi, sco, nta, nco
INPUT "a"; a: INPUT "x1"; x1: INPUT "y1"; y1
INPUT "k * (-2ã , 2ã)"; k
10 CLS : SCREEN 12: sx = 360 * k: WINDOW (-sx * a * pd, pd * (-2 * k * a) + y1)-(sx * a * pd, pd * (2 * a * k) + y1)
PI = 4 * ATN(1)
LINE (-sx * a * pd, 0)-(sx * a * pd, 0), 8
LINE (0, pd * (-2 * a * k) + y1)-(0, (2 * k * a) * pd + y1), 8
'  ON mehvarm GOSUB nsin, ncos, ntan, ncot
FOR x = -sx TO sx STEP 5
  ON mehvarm GOSUB nsin0, ncos0, ntan0, ncot0
  PSET (x, ten), 1
NEXT x
FOR x = -sx TO sx STEP 5
  ON mehvarm GOSUB nsin1, ncos1, ntan1, ncot1
  PSET (x, ten), 2
NEXT x
FOR x = -sx TO sx STEP 5
  ON mehvarm GOSUB nsin2, ncos2, ntan2, ncot2
  PSET (x, ten), 3
NEXT x
FOR x = -sx TO sx STEP .1
  ON mehvarm GOSUB nsin, ncos, ntan, ncot
  PSET (x, ten), 4
NEXT x

FOR psy = -2 * k * ABS(a) + y1 TO 2 * ABS(a) * k + y1
 PSET (0, psy), 15:
NEXT psy
FOR ps = -sx * ABS(a) TO sx * ABS(a) STEP 90
 PSET (ps, 0), 15
 PSET (ps, y1), 15
NEXT ps
PRINT "finish"
WHILE ak$ <> CHR$(27)
ak$ = INPUT$(1)
IF ak$ = CHR$(43) THEN pd = pd / 1.1: GOTO 10
IF ak$ = CHR$(45) THEN pd = pd * 1.1: GOTO 10
IF ak$ = CHR$(42) THEN INPUT pd0: pd = pd / pd0: GOTO 10
WEND
END
nsi: PRINT " y = a * SIN(x + x1) + y1": RETURN
sco: PRINT " y = a * COS(x + x1) + y1": RETURN
nta: PRINT " y = a * TAN(x + x1) + y1": RETURN
nco: PRINT " y = a * COT(x + x1) + y1": RETURN
nsin0: ten = SIN(x * (PI / 180)): RETURN
nsin1: ten = SIN((x + x1) * (PI / 180)): RETURN
nsin2: ten = (a * SIN((x + x1) * (PI / 180))): RETURN
nsin: ten = (a * SIN((x + x1) * (PI / 180))) + y1: RETURN
ncos0: ten = COS(x * (PI / 180)): RETURN
ncos1: ten = COS((x + x1) * (PI / 180)): RETURN
ncos2: ten = (a * COS((x + x1) * (PI / 180))): RETURN
ncos: ten = (a * COS((x + x1) * (PI / 180))) + y1: RETURN
ntan0: ten = TAN(x * (PI / 180)): RETURN
ntan1: ten = TAN((x + x1) * (PI / 180)): RETURN
ntan2: ten = (a * TAN((x + x1) * (PI / 180))): RETURN
ntan: ten = (a * TAN((x + x1) * (PI / 180))) + y1: RETURN
ncot0: ten = 1 / TAN(x * (PI / 180)): RETURN
ncot1: ten = 1 / TAN((x + x1) * (PI / 180)): RETURN
ncot2: ten = (a * 1 / TAN((x + x1) * (PI / 180))): RETURN
ncot: ten = (a * 1 / TAN((x + x1) * (PI / 180))) + y1: RETURN

