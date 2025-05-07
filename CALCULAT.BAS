DECLARE SUB help ()
Cls
Print "PRESS any key to load the Calculator or [Esc] TO EXIT": y$ = Input$(1): If y$ = Chr$(27) Then Cls: End
Cls: Call help
'END
Line Input "YOUR ACCOUNT ? "; a$
Open "cal.cal" For Output As #1
Print #1, "ON ERROR GOTO ert"
Print #1, "PI = 4 * ATN(1)"
Print #1, "function FAC(n)"
Print #1, "n0 = 1"
Print #1, "FOR v = 1 TO n"
Print #1, "n0 = n0 * v"
Print #1, "NEXT v"
Print #1, "FAC = n0"
Print #1, "end function"
Print #1, "function ASN(x)"
Print #1, "PI=4*atn(1): cx = SQR(1 - (x) ^ 2)"
Print #1, "IF cx = 0 THEN ASN=90: goto 30"
Print #1, "tx = x / cx: ASN = ATN(tx) * (180 / PI)"
Print #1, "30 end function"
Print #1, "function ACS(x)"
Print #1, "if x=0 then ACS=90:goto 40"
Print #1, " PI = 4 * ATN(1):sx = SQR(1 - (x) ^ 2):tx = sx / x:ACS = ATN(tx) * (180 / PI)"
Print #1, "40 end function"
Print #1, "print:print " + Chr$(34) + "ANSWER = " + Chr$(34)
Print #1, "answer# ="; a$
Print #1, "locate csrlin-1,10:print answer#"
Print #1, "10 ak$=input$(1)"
Print #1, "run " + Chr$(34) + "calculat.bas" + Chr$(34)
Print #1, "ert: locate csrlin-1,10:PRINT " + Chr$(34) + "î" + Chr$(34) + ": GOTO 10"
Close #1
Run "cal.cal"

Sub help
    Print Chr$(34) + "+" + Chr$(34); " = +        " + Chr$(34) + "-" + Chr$(34); " = -        " + Chr$(34) + "*" + Chr$(34); " = x        " + Chr$(34) + "/" + Chr$(34); " = ö        " + Chr$(34) + "Enter" + Chr$(34); " ¯ =        "
    Print "-------------------------------------------------------------------------------"
    Print Chr$(34) + "SQR(x)" + Chr$(34); " = ûx       " + Chr$(34) + "x^n" + Chr$(34); " = xü        " + Chr$(34) + "FAC(n)" + Chr$(34); " =  n!        " + Chr$(34) + "PI" + Chr$(34); " = ã           "
    Print "-------------------------------------------------------------------------------"
    Print Chr$(34) + "SIN(x)" + Chr$(34); " = Sin(x)         " + Chr$(34) + "COS(x)" + Chr$(34); " = Cos(x)         " + Chr$(34) + "TAN(x)" + Chr$(34); " = Tan(x)         "
    Print "-------------------------------------------------------------------------------"
    Print Chr$(34) + "ASN(x)" + Chr$(34); " = Arcsin(x)      " + Chr$(34) + "ACS(x)" + Chr$(34); " = Arccos(x)      " + Chr$(34) + "ATN(x)" + Chr$(34); " = Arctan(x)      "
    Print "-------------------------------------------------------------------------------"
    Print Chr$(34) + "ABS(x)" + Chr$(34); " = ³x³            " + Chr$(34) + "INT(x)" + Chr$(34); " = [x]            " + Chr$(34) + "SGN(x)" + Chr$(34); " = Sign(x)        "
    Print "-------------------------------------------------------------------------------"
End Sub

