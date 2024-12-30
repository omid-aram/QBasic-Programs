1 CLS : v = 0: v1 = 0: k = 1: k1 = 0
PRINT "ENTER FIRST NUMBER AND FINALY NUMBER "
PRINT : INPUT a#
INPUT b#
FOR n# = a# TO b#
IF n# < 2 THEN GOTO 11
GOSUB 10
IF v = 100 THEN PRINT "PRESS ANY KEY TO CONTINUE": a$ = INPUT$(1): v = 0
11 NEXT n#
COLOR 15: PRINT : PRINT : PRINT v1; : COLOR 7: PRINT "prime numbers are between "; : COLOR 15: PRINT a#; "to"; b#: COLOR 7
PRINT "DO YOU WANT TO TRY AGAIN ?": PRINT "( press ESC for NO and OTHER KEYS for YES )": a$ = INPUT$(1)
IF a$ = CHR$(27) THEN GOTO 2 ELSE GOTO 1
2 CLS : PRINT "THANKS": PRINT : PRINT ".....OMID.....": END
10 c# = n# ^ .5
FOR P# = 2 TO c#
d# = n# / P#
IF FIX(d#) = d# THEN RETURN
NEXT P#
COLOR k
PRINT n#,
COLOR 7: v = v + 1: v1 = v1 + 1: k1 = k1 + 1
IF k1 = 5 THEN k = k + 1: k1 = 0
IF k = 9 THEN k = 1
RETURN

