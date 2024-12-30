CLS : SCREEN 12
FOR k = 0 TO 15
LINE (250, y)-(300, y + 12), k, BF
y = y + 12
NEXT k

LINE (50, 50)-(150, 150), , B
't$ = CHR$(195) + CHR$(24) + CHR$(36) + CHR$(195)
't$ = CHR$(0) + CHR$(21) + CHR$(4) + CHR$(16) + CHR$(64) + CHR$(16) + CHR$(4) + CHR$(21) + CHR$(0)
't$ = CHR$(4) + CHR$(8) + CHR$(16) + CHR$(32) + CHR$(0) + CHR$(0) + CHR$(4)
't$ = CHR$(85) + CHR$(170) + CHR$(85) + CHR$(0) + CHR$(170) + CHR$(85) '+ CHR$(170) + CHR$(0)
't$ = CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(24) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(36) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(66) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(126) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(66) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(66) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(66) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0)
't$ = CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(54) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(73) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(65) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(65) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(34) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(20) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(8) + CHR$(0) + CHR$(0) + CHR$(0)'+ CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0)
t$ = CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(54) + CHR$(54) + CHR$(0) + CHR$(0) + CHR$(73) + CHR$(73) + CHR$(0) + CHR$(0) + CHR$(65) + CHR$(65) + CHR$(0) + CHR$(0) + CHR$(65) + CHR$(65) + CHR$(0) + CHR$(0) + CHR$(34) + CHR$(34) + CHR$(0) + CHR$(0) + CHR$(20) + CHR$(20) + CHR$(0) + CHR$(0) + CHR$(8) + CHR$(8) + CHR$(0) + CHR$(0)'+ CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0)
't$ = CHR$(170) + CHR$(85) + CHR$(85) + CHR$(85) '+ CHR$(170) + CHR$(0) + CHR$(0) + CHR$(170)
't$ = "a + b + c + d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z"
PAINT (100, 100), t$

