DECLARE SUB setaxes (x!(), y!(), ylow!, yhigh!)
DECLARE SUB scaleextrema (lo!, hi!)
COMMON flag%, x(), y()
CONST mode = 12
CONST maxX = 639
CONST maxY = 479
CONST overflow = 3.402822E+38
'CONST overflow = 3
CONST true = -1
CONST false = 0
IF flag% = 0 THEN
  CLS
  DIM x(0 TO maxX + 1)
  DIM y(0 TO maxX + 1)
  INPUT "Input function: y = ", function$
  INPUT "Graphing should begin with x = ", x(0)
  INPUT "Graphing should end with x = ", x(maxX)
  increment = (x(maxX) - x(0)) / (maxX + 1)
  FOR i% = 1 TO maxX - 1
   x(i%) = x(i% - 1) + increment
  NEXT i%
'  SLEEP
  OPEN "EVALUATE.BAS" FOR OUTPUT AS #1
  PRINT #1, "COMMON flag%, x(), y()"
  PRINT #1, "on error goto errhandler"
  PRINT #1, "for i%=0 to " + STR$(maxX)
  PRINT #1, " x=x(i%)"
  PRINT #1, " y(i%) =" + function$
  PRINT #1, "next i%"
  PRINT #1, "chain " + CHR$(34) + "GRAPH" + CHR$(34)
  PRINT #1, "errhandler:"
  PRINT #1, "if err then"
 
  'PRINT #1, "print i%"

  PRINT #1, "  y(i%)= " + STR$(overflow)
  'PRINT #1, "  y(i%)= 0"
  PRINT #1, "  resume next"
  PRINT #1, "end if"
 
  'PRINT #1, "print chr$(52)"
 
  CLOSE #1
  flag% = -1
  PRINT : PRINT "evaluating function please wait..."
  CHAIN "EVALUATE"
 ELSE
  ON ERROR GOTO skippoint
  CALL setaxes(x(), y(), ylow, yhigh)
  i% = 0
  pointok% = false
  DO WHILE i% <= maxX
   DO WHILE NOT pointok% AND i% <= maxX
    IF y(i%) <> overflow AND y(i%) >= ylow AND y(i%) <= yhigh THEN
      'LOCATE 1, 1: PRINT x(i%): SLEEP
      PSET (x(i%), y(i%)), 14
      pointok% = true
    END IF
    i% = i% + 1
   LOOP
   DO WHILE pointok% AND i% <= maxX
    IF y(i%) <> overflow AND y(i%) >= ylow AND y(i%) <= yhigh THEN
      LINE -(x(i%), y(i%)), 14
      pointok% = false
    END IF
    i% = i% + 1
   LOOP
  LOOP
END IF
END

skippoint:
pointok% = false
RESUME NEXT

SUB scaleextrema (lo, hi)
 IF lo >= 0 THEN lo = -hi / 20
 IF hi <= 0 THEN hi = -lo / 20
END SUB

SUB setaxes (x(), y(), ylow, yhigh)
 xlow = x(0)
 xhigh = x(maxX)
 CALL scaleextrema(xlow, xhigh)
 PRINT : PRINT "Do you wish to give lower and upper bounds ";
 PRINT "for the graph (Y/N)"
 PRINT " (if not, the minimum and maximum value of of the function"
 PRINT "  on the given interval will be computed and used )"
 answer$ = UCASE$(INPUT$(1))
 IF answer$ = "Y" THEN
   INPUT "What is the lower bound"; ylow
   INPUT "What is the upper bound"; yhigh
  ELSE
   ylow = y(0)
   yhigh = y(0)
   FOR i% = 1 TO maxX
    SELECT CASE y(i%)
     CASE overflow
     CASE IS < ylow: ylow = y(i%)
     CASE IS > yhigh: yhigh = y(i%)
     CASE ELSE
    END SELECT
   NEXT i%
   CALL scaleextrema(ylow, yhigh)
 END IF
 SCREEN mode
 VIEW (0, 0)-(maxX * .75 * .9, maxY * .9)
 WINDOW (xlow, ylow)-(xhigh, yhigh)
 LINE (0, ylow)-(0, yhigh)
 LINE (xlow, 0)-(xhigh, 0)

END SUB

