DECLARE SUB mousehide ()
DECLARE SUB MouseShow ()
DECLARE SUB MouseStatus (lb%, rb%, xmouse%, ymouse%)
DECLARE SUB mousedriver (ax%, bx%, cx%, dx%)
DECLARE FUNCTION MouseInit% ()
DIM SHARED mouse$: mouse$ = SPACE$(57)
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
MouseShow
LINE (0, 0)-(200, 200), 14, BF
'CALL mouserange(0, 0, 200, 200)
DO
  ak$ = INKEY$
  IF ak$ = CHR$(27) THEN END
  MouseStatus lb%, rb%, xmouse%, ymouse%
  LOCATE 1, 1: PRINT xmouse%, ymouse%
  IF lb% = -1 THEN mousedriver 2, 0, 0, 0: GOTO 10
'    DO
'      IF lb% = 0 THEN
'    LOOP
'  END IF
  IF rb% = -1 THEN mousedriver 1, 0, 0, 0: GOTO 10
'    DO
'      IF rb% = 0 THEN
'    LOOP
'  END IF
10 LOOP

DEFLNG A-Z
SUB mousedriver (ax%, bx%, cx%, dx%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(ax%, bx%, cx%, dx%, mouse%)
END SUB

SUB mousehide
 ax% = 2
 mousedriver ax%, 0, 0, 0
END SUB

FUNCTION MouseInit%
  ax% = 0
  mousedriver ax%, 0, 0, 0
  MouseInit% = ax%
END FUNCTION

SUB mouseput (x%, y%)
  ax% = 4
  cx% = x%
  dx% = y%
  mousedriver ax%, 0, cx%, dx%
END SUB

SUB mouserange (X1%, Y1%, x2%, Y2%)
  ax% = 7
  cx% = X1%
  dx% = x2%
mousedriver ax%, 0, cx%, dx%
  ax% = 8
  cx% = Y1%
  dx% = Y2%
  mousedriver ax%, 0, cx%, dx%
END SUB

SUB MouseShow
  ax% = 1
  mousedriver ax%, 0, 0, 0
END SUB

SUB MouseStatus (lb%, rb%, xmouse%, ymouse%)
  ax% = 3
  mousedriver ax%, bx%, cx%, dx%
  lb% = ((bx% AND 1) <> 0)
  rb% = ((bx% AND 2) <> 0)
  xmouse% = cx%
  ymouse% = dx%
END SUB

