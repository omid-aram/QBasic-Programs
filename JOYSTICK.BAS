    CLS : SCREEN 12, 3: WINDOW SCREEN (10, 0)-(225, 255)
    Temp% = STICK(1): k0 = 14
    WHILE INKEY$ = ""
    
    LOCATE 1, 1: PRINT PMAP(STICK(0), 0), PMAP(STICK(1), 1)
    LINE (PMAP(320, 2), PMAP(0, 3))-(PMAP(320, 2), PMAP(480, 3))
    LINE (PMAP(0, 2), PMAP(240, 3))-(PMAP(640, 2), PMAP(240, 3))
    x = STICK(0)
    y = STICK(1)
    GOSUB 10: k0 = 0: GOSUB 10
    IF STRIG(5) THEN k0 = 10: GOSUB 11
    IF STRIG(0) THEN k0 = 12: GOSUB 11

WEND
    END
10
'PSET (x, y), k0
CIRCLE (x, y), 10, k0:
PAINT (x, y), k0, k0:
SLEEP
k0 = 14: RETURN
11
'PSET (x, y), k0
CIRCLE (x, y), 10, k0:
PAINT (x, y), k0, k0:
RETURN
      

