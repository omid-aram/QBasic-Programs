CLS
PI = 4 * ATN(1)
INPUT x0
PRINT SIN(x0 / (180 / PI))
INPUT sinx
cosx = SQR(1 - (sinx) ^ 2)
IF cosx = 0 THEN PRINT "90": END
tanx = sinx / cosx
x = ATN(tanx) * (180 / PI)
PRINT x

