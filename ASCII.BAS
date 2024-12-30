CLS
DO
f$ = ""
f$ = INKEY$
IF LEN(f$) > 1 THEN g$ = RIGHT$(f$, 1) ELSE g$ = f$
LOCATE 1, 1: PRINT g$
LOOP

