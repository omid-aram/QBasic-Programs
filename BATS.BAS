CLS
INPUT "Name of Qbasic file "; a$
a$ = "c:\d\bases\" + a$
IF LEN(a$) = 0 THEN END
OPEN "azdc.bat" FOR OUTPUT AS #1
PRINT #1, "@echo off"
PRINT #1, "qbasic/run", a$
CLOSE
INPUT "What's new name "; b$
b$ = b$ + ".bat"
NAME "azdc.bat" AS b$

