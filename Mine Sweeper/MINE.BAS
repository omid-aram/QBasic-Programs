DECLARE SUB NMB (c%, ai%, aj%, mine%)
DECLARE SUB Ques (x%, y%)
DECLARE SUB FLAG (x%, y%)
DECLARE SUB NAGHB (ai%, aj%, mine%, bom%)
DECLARE SUB AFRAID (bom%, x%, y%)
DECLARE SUB NOTAFRAID (bom%)
DECLARE SUB TNT (ai%, aj%, mine%)
DECLARE SUB STRACTUR ()
DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseStatus (Lb%, Rb%, xMouse%, yMouse%)
DECLARE SUB MouseRange (X1%, Y1%, x2%, Y2%)
DECLARE SUB MousePut (x%, y%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseDriver (ax%, bx%, cx%, dx%)
DECLARE SUB MouseShow ()
DEFINT A-Z: DEF SEG = &HA000:
DIM SHARED mouse$: mouse$ = SPACE$(57)
DIM xx(20): DIM yy(20): DIM flg(8, 8)
CLS
SCREEN 12
FOR i% = 1 TO 57: READ a$:  H$ = CHR$(VAL("&H" + a$))
MID$(mouse$, i%, 1) = H$: NEXT i%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00
RESTORE
ms% = MouseInit%
IF NOT ms% THEN
  PRINT "          Mouse not found"
  PRINT "Please Run Qbasic from WIN 9X/ME "
 END
END IF
DEF FNA (x)
x = x - 200: IF x / 20 = x \ 20 THEN FNA = 0 ELSE FNA = x \ 20
END DEF
DEF FNB (x)
x = x - 130: IF x / 20 = x \ 20 THEN FNB = 0 ELSE FNB = x \ 20
END DEF
REM ==============================   MINE PUT   =========================================
mine = 10
1 CLS : STRACTUR
miine = mine: bom = 0: ending = 0
MouseShow
COLOR 12
RANDOMIZE TIMER
FOR i = 1 TO mine
2 x(i) = INT(RND * 8) + 1: y(i) = INT(RND * 8) + 1
   FOR j = 1 TO i - 1
  IF x(j) = x(i) AND y(j) = y(i) THEN GOTO 2
   NEXT j
'PRINT x(i), y(i)
NEXT
REM ==============================  START   =================================================================
DO
30 MouseStatus Lb, Rb, x, y
IF Lb = -1 AND y <= 76 AND y >= 66 THEN meno = x: btime# = INT(TIMER): GOTO 20
IF Lb = -1 AND FNA(x) > 0 AND FNA(x) <= 8 AND FNB(y) > 0 AND FNB(y) <= 8 THEN
 p = 1
 MouseStatus Lb, Rb, x, y
 END IF
 IF p = 1 AND Lb = 0 THEN btime# = INT(TIMER): p = 0: GOTO 3
LOOP
REM ------------------------------------------------------------------------------
DO
meno = 0
  'Cheking for end of the Game
3 MouseStatus Lb, Rb, x, y
ending = 0
FOR i = 1 TO 8
   FOR j = 1 TO 8
     IF flg(i, j) = 0 THEN ending = 3
   NEXT j
NEXT i
IF ending = 0 AND miine = 0 THEN SOUND 50, 2

LOCATE 8, 45: PRINT USING "###"; miine
atime# = INT(TIMER): time = atime# - btime#
IF bom = 0 THEN
 LOCATE 8, 29
  IF time = 1000 THEN time = 0
  PRINT USING "###"; time
END IF
'LOCATE 1, 1: PRINT Lb, Rb, x, y
'LOCATE 2, 1: PRINT FNA(x), FNB(y)
IF Rb = -1 THEN GOTO 6
IF Lb = -1 THEN AFRAID bom, x, y: GOTO 5
REM ----------------------------------------------------------------------------------     
20                                     SELECT CASE meno
             CASE 224 TO 246      'New
         MouseDriver 2, 0, 0, 0
         meno = 0
         GOTO 1
             CASE 279 TO 311        'Mine set
         COLOR 14
         meno = 0
         LOCATE 10, 1:
         PRINT " Mines:"
         PRINT "1 TO 10"
4        INPUT mmine
         IF mmine > 10 THEN
         LOCATE 12, 1: PRINT "            ": LOCATE 12, 1: GOTO 4
         END IF
         IF mmine = 0 THEN GOTO 7
         mine = mmine
7         MouseDriver 2, 0, 0, 0
         LINE (0, 140)-(100, 240), 0, BF
         MouseDriver 1, 0, 0, 0
        GOTO 1
        CASE 344 TO 375              'Exit
        MouseDriver 0, 0, 0, 0
        FOR Ex = 0 TO 20
        LOCATE 30, 1: PRINT
        NEXT
        END
                             END SELECT
IF meno <> 0 THEN GOTO 30
IF suub = 1 THEN
suub = 0
FOR i = 1 TO mine
IF x(i) = ai AND y(i) = aj AND flg(ai, aj) = 0 THEN bom = 1: TNT ai, aj, mine
NEXT
NAGHB ai, aj, mine, bom
END IF
g$ = INKEY$
IF g$ = CHR$(27) THEN END
LOOP
REM ====================================ATTECTS========================================================
5 DO
MouseStatus Lb, Rb, x, y
IF Lb = 0 THEN
NOTAFRAID bom
  IF y >= 67 AND y <= 75 THEN meno = x
  IF bom = 0 AND FNA(x) >= 1 AND FNA(x) <= 8 AND FNB(y) >= 1 AND FNB(y) <= 8 THEN
  suub = 1
  ai = FNA(x): aj = FNB(y)
  END IF
GOTO 3
END IF
LOOP

REM ============================ ANTI MINE ===========================================================
6  DO
MouseStatus Lb, Rb, x, y
IF Rb = 0 THEN
IF FNA(x) >= 1 AND FNA(x) <= 8 AND FNB(y) >= 1 AND FNB(y) <= 8 AND bom = 0 THEN
FLAG x, y
END IF
mn = 0
  FOR i = 1 TO 8
    FOR j = 1 TO 8
   IF flg(i, j) = 1 THEN mn = mn + 1
    NEXT j
  NEXT i
miine = mine - mn
GOTO 3
END IF
LOOP

SUB AFRAID (bom, x, y)
ai = FNA(x): aj = FNB(y)
IF ai > 8 OR ai < 1 OR aj > 8 OR aj < 1 THEN GOTO 202
IF bom = 1 THEN GOTO 202
CIRCLE (298, 120), 8, 10, 3.5, 5.9
CIRCLE (298, 125), 3, 0
202
END SUB

SUB FLAG (x, y)
SHARED flg()
MouseDriver 2, 0, 0, 0
ai = FNA(x): aj = FNB(y)
 IF flg(ai, aj) = 0 THEN
  flg(ai, aj) = 1
  LINE (208 + 20 * ai, 134 + 20 * aj)-(213 + 20 * ai, 138 + 20 * aj), 4, BF
  LINE (213 + 20 * ai, 134 + 20 * aj)-(213 + 20 * ai, 143 + 20 * aj), 4
  LINE (211 + 20 * ai, 143 + 20 * aj)-(215 + 20 * ai, 143 + 20 * aj), 4
  GOTO 205
 END IF
  IF flg(ai, aj) = 1 THEN
 flg(ai, aj) = 0
 LINE (202 + 20 * ai, 132 + 20 * aj)-(218 + 20 * ai, 147 + 20 * aj), 8, BF
  END IF
205 MouseDriver 1, 0, 0, 0
END SUB

DEFLNG A-Z
SUB MouseDriver (ax%, bx%, cx%, dx%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(ax%, bx%, cx%, dx%, mouse%)
END SUB

SUB MouseHide
 ax% = 2
 MouseDriver ax%, 0, 0, 0
END SUB

FUNCTION MouseInit%
  ax% = 0
  MouseDriver ax%, 0, 0, 0
  MouseInit% = ax%
END FUNCTION

SUB MousePut (x%, y%)
  ax% = 4
  cx% = x%
  dx% = y%
  MouseDriver ax%, 0, cx%, dx%
END SUB

SUB MouseRange (X1%, Y1%, x2%, Y2%)
  ax% = 7
  cx% = X1%
  dx% = x2%
MouseDriver ax%, 0, cx%, dx%
  ax% = 8
  cx% = Y1%
  dx% = Y2%
  MouseDriver ax%, 0, cx%, dx%
END SUB

SUB MouseShow
  ax% = 1
  MouseDriver ax%, 0, 0, 0
END SUB

SUB MouseStatus (Lb%, Rb%, xMouse%, yMouse%)
  ax% = 3
  MouseDriver ax%, bx%, cx%, dx%
  Lb% = ((bx% AND 1) <> 0)
  Rb% = ((bx% AND 2) <> 0)
  xMouse% = cx%
  yMouse% = dx%
END SUB

DEFINT A-Z
SUB NAGHB (ai, aj, mine, bom)
SHARED x(), y()
IF bom = 1 THEN GOTO 201
FOR t = -1 TO 1
  FOR r = -1 TO 1
    FOR i = 1 TO mine
    IF ai + t = x(i) AND aj + r = y(i) THEN c = c + 1
    NEXT i
  NEXT r
NEXT t
NMB c, ai, aj, mine
201
END SUB

SUB NMB (c, ai, aj, mine)
SHARED flg(), x(), y()
x = 20 * ai: y = 20 * aj
IF flg(ai, aj) = 1 THEN GOTO 204
flg(ai, aj) = 2
MouseDriver 2, 0, 0, 0
LINE (202 + x, 132 + y)-(219 + x, 149 + y), 7, BF
IF c = 4 OR c = 5 OR c = 6 OR c = 7 OR c = 8 THEN
LINE (208 + x, 135 + y)-(208 + x, 140 + y), 0
END IF
IF c = 1 OR c = 2 OR c = 4 OR c = 7 OR c = 8 OR c = 3 THEN
LINE (212 + x, 135 + y)-(212 + x, 140 + y), 0
END IF
IF c = 1 OR c = 3 OR c = 4 OR c = 5 OR c = 6 OR c = 8 THEN
LINE (212 + x, 145 + y)-(212 + x, 140 + y), 0
END IF
IF c = 2 OR c = 6 OR c = 8 THEN
LINE (208 + x, 145 + y)-(208 + x, 140 + y), 0
END IF
IF c = 2 OR c = 3 OR c = 4 OR c = 5 OR c = 6 OR c = 8 THEN
LINE (208 + x, 140 + y)-(212 + x, 140 + y), 0
END IF
IF c = 2 OR c = 3 OR c = 5 OR c = 6 OR c = 7 OR c = 8 THEN
LINE (208 + x, 135 + y)-(212 + x, 135 + y), 0
END IF
IF c = 2 OR c = 3 OR c = 5 OR c = 6 OR c = 8 THEN
LINE (208 + x, 145 + y)-(212 + x, 145 + y), 0
END IF
REM IF c = 0 THEN
REM END IF

MouseDriver 1, 0, 0, 0
204
END SUB

SUB NOTAFRAID (bom)
IF bom = 1 THEN GOTO 203
CIRCLE (298, 125), 3, 10
CIRCLE (298, 120), 8, 0, 3.5, 5.9
203
END SUB

SUB STRACTUR
SHARED flg()
COLOR 3
move = 0
LINE (200 + move, 50 + move)-(400 + move, 330 + move), 8, BF
FOR i = 0 TO 8 * 20 STEP 20
LINE (220 + move, 150 + i + move)-(380 + move, 151 + i + move), 1, BF
LINE (220 + i + move, 150 + move)-(221 + i + move, 310 + move), 1, BF
NEXT
FOR i = 0 TO 2
LINE (213 + i, 60 + i)-(386 - i, 82 - i), 14, B
LINE (220 + i, 109 + i)-(250 - i, 130 - i), 14, B
LINE (349 + i, 109 + i)-(378 - i, 130 - i), 14, B
LINE (221 - i, 151 - i)-(380 + i, 310 + i), 4, B
CIRCLE (298, 120), 10, 10
PAINT (298, 120), 10
CIRCLE (298, 120), 8, 0, 3.5, 5.9
CIRCLE (301, 116), 1 + i / 2, 0: CIRCLE (295, 116), 1 + i / 2, 0
NEXT
LOCATE 5, 28: PRINT " New    Mine    Exit "
COLOR 12
LOCATE 8, 29: PRINT USING "00#"; time
LOCATE 8, 45: PRINT USING "00#"; mine
FOR i = 1 TO 8
FOR j = 1 TO 8
  flg(i, j) = 0
 NEXT j
NEXT i




END SUB

SUB TNT (ai, aj, mine)
SHARED x(), y(), flg()
MouseDriver 2, 0, 0, 0
FOR i = 1 TO mine
IF flg(x(i), y(i)) = 0 THEN
LINE (202 + 20 * x(i), 132 + 20 * y(i))-(219 + 20 * x(i), 149 + 20 * y(i)), 7, BF
  FOR k = 0 TO 4
 CIRCLE (210 + 20 * x(i), 140 + 20 * y(i)), k, 0
  NEXT k
 END IF
NEXT i
SOUND 50, 1
MouseDriver 1, 0, 0, 0
CIRCLE (298, 120), 8, 10, 3.5, 5.9
CIRCLE (298, 120), 8, 10, 3.5, 5.9
CIRCLE (298, 130), 8, 0, .6, 2.5
FOR i = 1 TO 8
 FOR j = 1 TO 8
  IF flg(i, j) = 1 THEN
    flg(i, j) = 0
    FOR p = 1 TO mine
     IF i = x(p) AND j = y(p) THEN GOTO 100
    NEXT p
LINE (205 + 20 * i, 135 + 20 * j)-(215 + 20 * i, 145 + 20 * j), 0
LINE (215 + 20 * i, 135 + 20 * j)-(205 + 20 * i, 145 + 20 * j), 0
 
  END IF
100
 NEXT j
NEXT i

END SUB


