DECLARE SUB help ()
CLS
PRINT "PRESS [Esc] TO EXIT": y$ = INPUT$(1): IF y$ = CHR$(27) THEN CLS : END
CLS : CALL help
'END
LINE INPUT "YOUR ACCOUNT ? "; a$
OPEN "cal.cal" FOR OUTPUT AS #1
PRINT #1, "ON ERROR GOTO ert"
PRINT #1, "PI = 4 * ATN(1)"
PRINT #1, "function FAC(n)"
PRINT #1, "n0 = 1"
PRINT #1, "FOR v = 1 TO n"
PRINT #1, "n0 = n0 * v"
PRINT #1, "NEXT v"
PRINT #1, "FAC = n0"
PRINT #1, "end function"
PRINT #1, "function ASN(x)"
PRINT #1, "PI=4*atn(1): cx = SQR(1 - (x) ^ 2)"
PRINT #1, "IF cx = 0 THEN ASN=90: goto 30"
PRINT #1, "tx = x / cx: ASN = ATN(tx) * (180 / PI)"
PRINT #1, "30 end function"
PRINT #1, "function ACS(x)"
PRINT #1, "if x=0 then ACS=90:goto 40"
PRINT #1, " PI = 4 * ATN(1):sx = SQR(1 - (x) ^ 2):tx = sx / x:ACS = ATN(tx) * (180 / PI)"
PRINT #1, "40 end function"
PRINT #1, "print:print " + CHR$(34) + "ANSWER = " + CHR$(34)
PRINT #1, "answer# ="; a$
PRINT #1, "locate csrlin-1,10:print answer#"
PRINT #1, "10 ak$=input$(1)"
PRINT #1, "run " + CHR$(34) + "calculat.bas" + CHR$(34)
PRINT #1, "ert: locate csrlin-1,10:PRINT " + CHR$(34) + "î" + CHR$(34) + ": GOTO 10"
CLOSE #1
RUN "cal.cal"

SUB help
PRINT CHR$(34) + "+" + CHR$(34); " = +        " + CHR$(34) + "-" + CHR$(34); " = -        " + CHR$(34) + "*" + CHR$(34); " = x        " + CHR$(34) + "/" + CHR$(34); " = ö        " + CHR$(34) + "Enter" + CHR$(34); " ¯ =        "
PRINT "-------------------------------------------------------------------------------"
PRINT CHR$(34) + "SQR(x)" + CHR$(34); " = ûx       " + CHR$(34) + "x^n" + CHR$(34); " = xü        " + CHR$(34) + "FAC(n)" + CHR$(34); " =  n!        " + CHR$(34) + "PI" + CHR$(34); " = ã           "
PRINT "-------------------------------------------------------------------------------"
PRINT CHR$(34) + "SIN(x)" + CHR$(34); " = Sin(x)         " + CHR$(34) + "COS(x)" + CHR$(34); " = Cos(x)         " + CHR$(34) + "TAN(x)" + CHR$(34); " = Tan(x)         "
PRINT "-------------------------------------------------------------------------------"
PRINT CHR$(34) + "ASN(x)" + CHR$(34); " = Arcsin(x)      " + CHR$(34) + "ACS(x)" + CHR$(34); " = Arccos(x)      " + CHR$(34) + "ATN(x)" + CHR$(34); " = Arctan(x)      "
PRINT "-------------------------------------------------------------------------------"
PRINT CHR$(34) + "ABS(x)" + CHR$(34); " = ³x³            " + CHR$(34) + "INT(x)" + CHR$(34); " = [x]            " + CHR$(34) + "SGN(x)" + CHR$(34); " = Sign(x)        "
PRINT "-------------------------------------------------------------------------------"
END SUB

