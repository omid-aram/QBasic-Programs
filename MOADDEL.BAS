CLS : g = 0
INPUT "T"; t
DIM nom(t)
DIM va(t)
FOR n = 1 TO t
INPUT "N"; nom(n)
NEXT n
INPUT "DO YOU WANT TO ENTER VAHED"; v$: IF v$ = CHR$(121) THEN GOTO 2 ELSE FOR g = 1 TO t: va(g) = 1: NEXT g: GOTO 1
2 FOR g = 1 TO t
LOCATE g + 1, 10: INPUT "V"; va(g)
NEXT g
1 n = 0
FOR g = 1 TO t
n = n + 1
moa = va(g) * nom(n)
min = min + moa
to1 = to1 + va(g)
NEXT g
PRINT : PRINT min / to1: PRINT USING "##.##"; min / to1

