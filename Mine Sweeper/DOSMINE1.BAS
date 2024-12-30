DECLARE SUB numbers (x000!, y000!, m!)
DECLARE SUB pooch (bomb!(), r, c, fl, flag(), x100, y100)
DECLARE SUB bombs (room!(), bomb!())
DECLARE SUB scene (x100, y100, x200, y200, p0, p1)
start:
  CLEAR , , 4096
10 CLS : SCREEN 12: PALETTE 12, 63: PALETTE 4, 31
20 RANDOMIZE TIMER
30 DIM room(8, 8)
35 DIM bomb(8, 8)
40 DIM r(20)
50 DIM c(20)
55 DIM flag(8, 8)
60 nm = 10: min = nm: fl = 0: s = 0: n = 0
70 DO
80  n = n + 1
90  r(n) = INT(RND * 8) + 1: c(n) = INT(RND * 8) + 1
100  FOR t = 1 TO n - 1
110   IF r(n) = r(t) AND c(n) = c(t) THEN nm = nm + 1: GOTO 140
120  NEXT t
130  room(r(n), c(n)) = 1
140 LOOP UNTIL n >= nm
150 CALL scene(x100, y100, x200, y200, p0, p1)
160 CALL bombs(room(), bomb())
170 time = TIMER: x00 = x100: y00 = y100: r = 1: c = 1: COLOR 12
nm = min
GOSUB same
180 DO
190 f$ = ""
200 f$ = INKEY$
210 IF LEN(f$) = 2 THEN g$ = RIGHT$(f$, 1) ELSE g$ = f$
SELECT CASE g$
  CASE "M"
   GOSUB same1: x00 = x00 + 5: GOSUB same
  CASE "K"
   GOSUB same1: x00 = x00 - 5: GOSUB same
  CASE "H"
   GOSUB same1: y00 = y00 - 5: GOSUB same
  CASE "P"
   GOSUB same1: y00 = y00 + 5: GOSUB same
  CASE CHR$(13)
   IF r > 0 AND r < 9 THEN
    IF c > 0 AND c < 9 THEN
     IF s = 0 THEN IF flag(r, c) <> 1 AND flag(r, c) <> 3 THEN IF bomb(r, c) = 9 THEN s = 1: GOSUB 1100
     IF s = 0 THEN IF flag(r, c) <> 1 AND flag(r, c) <> 3 THEN IF bomb(r, c) = 0 THEN LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE -(15 + x000, 0 + y000), 0, , &H5555: CALL pooch(bomb(), r, c, fl, flag(), x100, y100)
     IF s = 0 THEN IF flag(r, c) <> 1 AND flag(r, c) <> 3 THEN IF bomb(r, c) <> 0 THEN flag(r, c) = 3: fl = fl + 1: m = bomb(r, c): CALL numbers(x000, y000, m)
    END IF
   END IF
   IF x00 >= 66 + x200 AND x00 <= 90 + x200 THEN IF y00 >= 56 + y200 AND y00 <= 80 + y200 THEN ERASE flag, room, bomb, r, c: GOTO start
   IF x00 >= 137 + x200 AND x00 <= 152 + x200 THEN IF y00 >= 5 + y200 AND y00 <= 18 + y200 THEN END
  CASE CHR$(32)
   IF r > 0 AND r < 9 THEN
    IF c > 0 AND c < 9 THEN
     IF s = 0 THEN IF flag(r, c) = 0 THEN flag(r, c) = 1: min = min - 1: GOSUB 1200
     IF s = 0 THEN IF flag(r, c) = 1 THEN flag(r, c) = 2: min = min + 1: GOSUB 1300
     IF s = 0 THEN IF flag(r, c) = 2 THEN flag(r, c) = 0: GOSUB 1400
    END IF
   END IF
   IF x00 >= 66 + x200 AND x00 <= 90 + x200 THEN IF y00 >= 56 + y200 AND y00 <= 80 + y200 THEN ERASE flag, room, bomb, r, c: GOTO start
   IF x00 >= 137 + x200 AND x00 <= 152 + x200 THEN IF y00 >= 5 + y200 AND y00 <= 18 + y200 THEN END
   CASE CHR$(60)
    ERASE flag, room, bomb, r, c
    GOTO start
  CASE CHR$(27)
    END
410 END SELECT
   r = INT(((y00 - y100) / 16) + 1): c = INT(((x00 - x100) / 16) + 1)
   x000 = (c - 1) * 16 + x100: y000 = (r - 1) * 16 + y100
IF INT(TIMER - time) > 999 THEN GOSUB 1100
420 IF s = 0 THEN GOSUB 425
415 IF s = 0 THEN IF fl = 64 - nm THEN GOSUB 1110: s = 1
430 LOOP
440 END
425 LOCATE 5 + p0, 5 + p1: PRINT USING "###"; min
LOCATE 5 + p0, 15 + p1: PRINT USING "###"; INT(TIMER - time): RETURN
1100 FOR x = 1 TO 8
FOR y = 1 TO 8
IF room(x, y) = 1 AND flag(x, y) <> 1 THEN y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100: GOSUB bomp
IF room(x, y) <> 1 AND flag(x, y) = 1 THEN y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100: GOSUB cross
NEXT y
NEXT x
LINE (74 + x200, 64 + y200)-(76 + x200, 66 + y200), 0, BF: CIRCLE (75 + x200, 65 + y200), 1, 14: LINE (80 + x200, 64 + y200)-(82 + x200, 66 + y200), 0, BF: CIRCLE (81 + x200, 65 + y200), 1, 14: LINE (74 + x200, 70 + y200)-(82 + x200, 72 + y200), 14, BF: LINE (74 + x200, 72 + y200)-(76 + x200, 70 + y200), 0: LINE -(80 + x200, 70 + y200), 0: LINE -(82 + x200, 72 + y200), 0
RETURN
1110 FOR x = 1 TO 8
FOR y = 1 TO 8
IF room(x, y) = 1 AND flag(x, y) <> 1 THEN
y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100
LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE -(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: LINE -(15 + x000, 0 + y000), 0: LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE -(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
LINE (4 + x000, 3 + y000)-(8 + x000, 6 + y000), 4, BF: LINE (4 + x000, 6 + y000)-(7 + x000, 3 + y000), 12: LINE (4 + x000, 5 + y000)-(6 + x000, 3 + y000), 12: LINE (4 + x000, 3 + y000)-(5 + x000, 4 + y000), 12, B: LINE (8 + x000, 7 + y000)-(8 + x000, 10 + y000), 0: LINE (7 + x000, 11 + y000)-(9 + x000, 11 + y000), 0: LINE (5 + x000, 12 + y000)-(11 + x000, 12 + y000), 0
END IF
NEXT y
NEXT x
LINE (70 + x200, 69 + y200)-(74 + x200, 65 + y200), 0: LINE -(77 + x200, 67 + y200), 0, BF: PSET (78 + x200, 65 + y200), 0: LINE (86 + x200, 69 + y200)-(82 + x200, 65 + y200), 0: LINE -(79 + x200, 67 + y200), 0, BF: LINE (74 + x200, 68 + y200)-(82 + x200, 68 + y200), 0, , &H6300
min = 0: GOSUB 425
RETURN
1200 LINE (4 + x000, 3 + y000)-(8 + x000, 6 + y000), 4, BF: LINE (4 + x000, 6 + y000)-(7 + x000, 3 + y000), 12: LINE (4 + x000, 5 + y000)-(6 + x000, 3 + y000), 12: LINE (4 + x000, 3 + y000)-(5 + x000, 4 + y000), 12, B: LINE (8 + x000, 7 + y000)-(8 + x000, 10 + y000), 0: LINE (7 + x000, 11 + y000)-(9 + x000, 11 + y000), 0: LINE (5 + x000, 12 + y000)-(11 + x000, 12 + y000), 0
GOTO 420

1300 LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE -(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: LINE -(15 + x000, 0 + y000), 0: LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE -(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
 LINE (4 + x000, 4 + y000)-(5 + x000, 5 + y000), 1, B: LINE (5 + x000, 3 + y000)-(8 + x000, 4 + y000), 1, B: LINE -(9 + x000, 7 + y000), 1, B: LINE (10 + x000, 5 + y000)-(10 + x000, 6 + y000), 1: LINE (8 + x000, 7 + y000)-(6 + x000, 9 + y000), 1, BF: PSET (6 + x000, 7 + y000), 7: LINE (6 + x000, 11 + y000)-(8 + x000, 12 + y000), 1, B
GOTO 420
1400 LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE -(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: LINE -(15 + x000, 0 + y000), 0: LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE -(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
GOTO 420
bomp:
LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE -(15 + x000, 0 + y000), 0, , &H5555
LINE (5 + x000, 5 + y000)-(9 + x000, 9 + y000), 0, BF: PSET (6 + x000, 6 + y000), 15
LINE (4 + x000, 4 + y000)-(10 + x000, 10 + y000), 8, B, &H5555
LINE (4 + x000, 7 + y000)-(2 + x000, 7 + y000), 0: LINE (7 + x000, 10 + y000)-(7 + x000, 12 + y000), 0: LINE (10 + x000, 7 + y000)-(12 + x000, 7 + y000), 0: LINE (7 + x000, 4 + y000)-(7 + x000, 2 + y000), 0
RETURN
cross:
LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE -(15 + x000, 0 + y000), 0, , &H5555
LINE (3 + x000, 3 + y000)-(12 + x000, 12 + y000), 12: LINE (4 + x000, 3 + y000)-(12 + x000, 11 + y000), 12: LINE (3 + x000, 4 + y000)-(11 + x000, 12 + y000), 12: LINE (3 + x000, 11 + y000)-(11 + x000, 3 + y000), 12: LINE (3 + x000, 12 + y000)-(12 + x000, 3 + y000), 12: LINE (4 + x000, 12 + y000)-(12 + x000, 4 + y000), 12
RETURN

same: k08 = POINT(x00, y00): PSET (x00, y00), 12: RETURN
same1: PSET (x00, y00), k08: RETURN

SUB bombs (room(), bomb())
FOR x = 1 TO 8
 x1 = x
 FOR y = 1 TO 8
  y1 = y
  IF room(x, y) <> 1 THEN
   x0 = x1: y0 = y1: x0 = x0 - 1: y0 = y0 - 1: IF x0 > 0 AND x0 < 9 THEN IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
   x0 = x1: y0 = y1: y0 = y0 - 1: IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
   x0 = x1: y0 = y1: x0 = x0 + 1: y0 = y0 - 1: IF x0 > 0 AND x0 < 9 THEN IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
   x0 = x1: y0 = y1: x0 = x0 + 1: IF x0 > 0 AND x0 < 9 THEN IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
   x0 = x1: y0 = y1: x0 = x0 + 1: y0 = y0 + 1: IF x0 > 0 AND x0 < 9 THEN IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
   x0 = x1: y0 = y1: y0 = y0 + 1: IF x0 > 0 AND x0 < 9 THEN IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
   x0 = x1: y0 = y1: x0 = x0 - 1: y0 = y0 + 1: IF x0 > 0 AND x0 < 9 THEN IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
   x0 = x1: y0 = y1: x0 = x0 - 1: IF x0 > 0 AND x0 < 9 THEN IF y0 > 0 AND y0 < 9 THEN IF room(x0, y0) = 1 THEN n = n + 1
  ELSE
   n = 9
  END IF
  bomb(x, y) = n: n = 0
 NEXT y
NEXT x
END SUB

SUB numbers (x000, y000, m)
LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE -(15 + x000, 0 + y000), 0, , &H5555
SELECT CASE m
 CASE 1
  k = 1
  LINE (7 + x000, 3 + y000)-(8 + x000, 10 + y000), k, BF: LINE (5 + x000, 11 + y000)-(10 + x000, 11 + y000), k: LINE (5 + x000, 5 + y000)-(6 + x000, 5 + y000), k: PSET (6 + x000, 5 + y000), k
 CASE 2
  k = 5
  LINE (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: LINE (10 + x000, 4 + y000)-(4 + x000, 10 + y000), k: LINE (10 + x000, 5 + y000)-(4 + x000, 11 + y000), k: LINE -(10 + x000, 11 + y000), k: PSET (4 + x000, 4 + y000), k: PSET (9 + x000, 4 + y000), k
 CASE 3
  k = 4
  LINE (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: LINE (9 + x000, 4 + y000)-(10 + x000, 5 + y000), k, B: LINE (5 + x000, 6 + y000)-(9 + x000, 6 + y000), k: LINE (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: LINE (4 + x000, 11 + y000)-(9 + x000, 11 + y000), k
 CASE 4
  k = 9
  LINE (4 + x000, 3 + y000)-(5 + x000, 7 + y000), k, B: LINE (6 + x000, 7 + y000)-(8 + x000, 7 + y000), k: LINE (9 + x000, 3 + y000)-(10 + x000, 11 + y000), k, B
 CASE 5
  k = 6
  LINE (4 + x000, 3 + y000)-(5 + x000, 6 + y000), k, B: LINE (6 + x000, 3 + y000)-(10 + x000, 3 + y000), k: LINE (6 + x000, 6 + y000)-(9 + x000, 6 + y000), k: LINE (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: LINE (4 + x000, 10 + y000)-(5 + x000, 10 + y000), k: LINE (4 + x000, 11 + y000)-(9 + x000, 11 + y000), k
 CASE 6
  k = 11
  LINE (4 + x000, 4 + y000)-(5 + x000, 10 + y000), k, B: LINE (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: LINE (9 + x000, 4 + y000)-(10 + x000, 4 + y000), k: LINE (6 + x000, 6 + y000)-(9 + x000, 6 + y000), k: LINE (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: LINE (5 + x000, 11 + y000)-(9 + x000, 11 + y000), k
 CASE 7
  k = 13
  LINE (4 + x000, 3 + y000)-(5 + x000, 4 + y000), k, B: LINE (6 + x000, 3 + y000)-(10 + x000, 3 + y000), k: LINE (6 + x000, 10 + y000)-(9 + x000, 4 + y000), k: LINE (7 + x000, 10 + y000)-(10 + x000, 4 + y000), k: LINE (6 + x000, 11 + y000)-(7 + x000, 11 + y000), k
 CASE 8
  k = 0
  LINE (5 + x000, 3 + y000)-(9 + x000, 11 + y000), k, B: LINE (4 + x000, 3 + y000)-(4 + x000, 11 + y000), k, , &H6F00: LINE (10 + x000, 3 + y000)-(10 + x000, 11 + y000), k, , &H6F00: LINE (5 + x000, 6 + y000)-(9 + x000, 6 + y000), k
END SELECT

END SUB

SUB pooch (bomb(), r, c, fl, flag(), x100, y100)
  n = 0: n2 = 0: j = 0: n0 = 0
  DIM a(64)
  DIM B(64)
  DIM a0(64)
  DIM b0(64)
  DIM n10(64)
  DIM g0(64)
  flag(r + B(n2), c + a(n2)) = 3: fl = fl + 1: n2 = n2 + 1: n = n2
  g = 1
  a(n2) = 0
  B(n2) = 0
  IF bomb(r + B(n), c + a(n)) = 0 THEN n0 = n0 + 1: a0(n0) = a(n2): b0(n0) = B(n2): g0(n0) = g: n10(n0) = n2
901 flag(r + B(n2), c + a(n2)) = 3: fl = fl + 1: n2 = n2 + 1: n = n2
1004
 IF g = 1 THEN a(n2) = a(n - 1) - 1: B(n2) = B(n - 1) - 1: GOSUB 1005
 IF d = 0 AND g = 2 THEN a(n2) = a(n - 1) + 1: B(n2) = B(n - 1): GOSUB 1005
 IF d = 1 AND g = 2 THEN a(n2) = a0(n0): B(n2) = b0(n0) - 1: GOSUB 1005
 IF d = 0 AND g = 3 THEN a(n2) = a(n - 1) + 1: B(n2) = B(n - 1): GOSUB 1005
 IF d = 1 AND g = 3 THEN a(n2) = a0(n0) + 1: B(n2) = b0(n0) - 1: GOSUB 1005
 IF d = 0 AND g = 4 THEN a(n2) = a(n - 1): B(n2) = B(n - 1) + 1: GOSUB 1005
 IF d = 1 AND g = 4 THEN a(n2) = a0(n0) + 1: B(n2) = b0(n0): GOSUB 1005
 IF d = 0 AND g = 5 THEN a(n2) = a(n - 1): B(n2) = B(n - 1) + 1: GOSUB 1005
 IF d = 1 AND g = 5 THEN a(n2) = a0(n0) + 1: B(n2) = b0(n0) + 1: GOSUB 1005
 IF d = 0 AND g = 6 THEN a(n2) = a(n - 1) - 1: B(n2) = B(n - 1): GOSUB 1005
 IF d = 1 AND g = 6 THEN a(n2) = a0(n0): B(n2) = b0(n0) + 1: GOSUB 1005
 IF d = 0 AND g = 7 THEN a(n2) = a(n - 1) - 1: B(n2) = B(n - 1): GOSUB 1005
 IF d = 1 AND g = 7 THEN a(n2) = a0(n0) - 1: B(n2) = b0(n0) + 1: GOSUB 1005
 IF d = 0 AND g = 8 THEN a(n2) = a(n - 1): B(n2) = B(n - 1) - 1: GOSUB 1005
 IF d = 1 AND g = 8 THEN a(n2) = a0(n0) - 1: B(n2) = b0(n0): GOSUB 1005
 IF g = 9 THEN j = j + 1: j1 = j: n0 = n0 - 1: IF n0 = -1 THEN GOTO 1008 ELSE n = n10(n0) + 1: g = g0(n0 + 1) + 1: d = 1: GOTO 1004
1008 fl = fl - 1
EXIT SUB
1005
  IF r + B(n2) > 0 AND r + B(n2) < 9 THEN
   IF c + a(n2) > 0 AND c + a(n2) < 9 THEN
    IF flag(r + B(n2), c + a(n2)) = 3 OR flag(r + B(n2), c + a(n2)) = 1 THEN g = g + 1: d = 1: GOTO 1004
    IF bomb(r + B(n2), c + a(n2)) = 0 THEN n0 = n0 + j1 + 1: j1 = 0: a0(n0) = a(n2): b0(n0) = B(n2): g0(n0) = g: n10(n0) = n2
    IF r + B(n2) > 0 AND r + B(n2) < 9 THEN
     IF c + a(n2) > 0 AND c + a(n2) < 9 THEN
       m = bomb(r + B(n2), c + a(n2))
       x000 = (c + a(n2) - 1) * 16 + x100: y000 = (r + B(n2) - 1) * 16 + y100
      IF m = 0 THEN
       LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE -(15 + x000, 0 + y000), 0, , &H5555
       g = 1
       GOTO 901
      ELSE
       CALL numbers(x000, y000, m)
       g = g + 1
       GOTO 901
      END IF
     END IF
    END IF
    g = g + 1: d = 1
   
   END IF
  END IF
  g = g + 1: d = 1: GOTO 1004
RETURN

END SUB

SUB scene (x100, y100, x200, y200, p0, p1)
p0 = 10: p1 = 20
x200 = 7 + (8 * p1): y200 = 3 + (16 * p0)
PAINT (0, 0), 3
LINE (0 + x200, 0 + y200)-(157 + x200, 238 + y200), 0, B
LINE (1 + x200, 1 + y200)-(156 + x200, 237 + y200), 8, B
LINE (156 + x200, 0 + y200)-(0 + x200, 0 + y200), 7: LINE -(0 + x200, 237 + y200), 7
LINE (155 + x200, 1 + y200)-(1 + x200, 1 + y200), 15: LINE -(1 + x200, 236 + y200), 15
LINE (2 + x200, 2 + y200)-(155 + x200, 236 + y200), 7, BF
LINE (3 + x200, 41 + y200)-(154 + x200, 235 + y200), 15, BF
LINE (154 + x200, 41 + y200)-(152 + x200, 235 + y200), 8, BF: LINE -(3 + x200, 233 + y200), 8, BF: LINE (152 + x200, 42 + y200)-(153 + x200, 41 + y200), 15: PSET (152 + x200, 41 + y200), 15: LINE (4 + x200, 233 + y200)-(3 + x200, 234 + y200), 15: PSET (3 + x200, 233 + y200), 15
LINE (6 + x200, 44 + y200)-(151 + x200, 232 + y200), 7, BF
LINE (12 + x200, 93 + y200)-(145 + x200, 226 + y200), 8, BF
LINE (145 + x200, 93 + y200)-(143 + x200, 226 + y200), 15, BF: LINE -(12 + x200, 223 + y200), 15, BF: LINE (143 + x200, 94 + y200)-(144 + x200, 93 + y200), 8: PSET (143 + x200, 93 + y200), 8: LINE (13 + x200, 223 + y200)-(12 + x200, 224 + y200), 8: PSET (12 + x200, 223 + y200), 8
LINE (144 + x200, 50 + y200)-(12 + x200, 50 + y200), 8: LINE -(12 + x200, 85 + y200), 8: LINE (13 + x200, 84 + y200)-(13 + x200, 51 + y200), 8: LINE -(143 + x200, 51 + y200), 8
LINE (145 + x200, 51 + y200)-(145 + x200, 86 + y200), 15: LINE -(13 + x200, 86 + y200), 15: LINE (14 + x200, 85 + y200)-(144 + x200, 85 + y200), 15: LINE -(144 + x200, 52 + y200), 15
wijn:
LINE (66 + x200, 56 + y200)-(91 + x200, 81 + y200), 8, B
LINE (67 + x200, 57 + y200)-(90 + x200, 80 + y200), 15, BF
LINE (90 + x200, 57 + y200)-(90 + x200, 80 + y200), 0: LINE -(67 + x200, 80 + y200), 0
LINE (68 + x200, 58 + y200)-(89 + x200, 79 + y200), 8, BF: LINE (68 + x200, 58 + y200)-(88 + x200, 78 + y200), 7, BF
CIRCLE (78 + x200, 68 + y200), 8, 0: PAINT (78 + x200, 68 + y200), 14, 0
LINE (75 + x200, 65 + y200)-(76 + x200, 66 + y200), 0, B: LINE (80 + x200, 65 + y200)-(81 + x200, 66 + y200), 0, B: LINE (74 + x200, 70 + y200)-(76 + x200, 72 + y200), 0: LINE -(80 + x200, 72 + y200), 0: LINE -(82 + x200, 70 + y200), 0

LINE (19 + x200, 56 + y200)-(59 + x200, 80 + y200), 8, B
LINE (59 + x200, 57 + y200)-(59 + x200, 80 + y200), 15: LINE -(20 + x200, 80 + y200), 15
LINE (20 + x200, 57 + y200)-(58 + x200, 79 + y200), 0, BF
LINE (96 + x200, 56 + y200)-(136 + x200, 80 + y200), 8, B
LINE (136 + x200, 57 + y200)-(136 + x200, 80 + y200), 15: LINE -(97 + x200, 80 + y200), 15
LINE (97 + x200, 57 + y200)-(135 + x200, 79 + y200), 0, BF
LINE (3 + x200, 3 + y200)-(154 + x200, 20 + y200), 1, BF

LINE (24 + x200, 7 + y200)-(25 + x200, 15 + y200), 15, B: LINE (26 + x200, 9 + y200)-(26 + x200, 12 + y200), 15: LINE (27 + x200, 11 + y200)-(28 + x200, 14 + y200), 15, B: LINE (29 + x200, 9 + y200)-(29 + x200, 12 + y200), 15: LINE (30 + x200, 7 + y200)-(31 + x200, 15 + y200), 15, B: LINE (34 + x200, 7 + y200)-(35 + x200, 7 + y200), 15: LINE (34 + x200, 10 + y200)-(35 + x200, 15 + y200), 15, B
LINE (37 + x200, 10 + y200)-(38 + x200, 15 + y200), 15, B: LINE (39 + x200, 10 + y200)-(41 + x200, 10 + y200), 15: LINE (41 + x200, 11 + y200)-(42 + x200, 15 + y200), 15, B: PSET (39 + x200, 11 + y200), 15
LINE (44 + x200, 11 + y200)-(45 + x200, 14 + y200), 15, B: LINE (45 + x200, 10 + y200)-(48 + x200, 12 + y200), 15, B: LINE (49 + x200, 11 + y200)-(49 + x200, 12 + y200), 15: LINE (45 + x200, 15 + y200)-(48 + x200, 15 + y200), 15: LINE (48 + x200, 14 + y200)-(49 + x200, 14 + y200), 15
LINE (52 + x200, 10 + y200)-(54 + x200, 10 + y200), 15: LINE (54 + x200, 11 + y200)-(55 + x200, 11 + y200), 15: LINE (51 + x200, 11 + y200)-(54 + x200, 14 + y200), 15: LINE (52 + x200, 11 + y200)-(55 + x200, 14 + y200), 15: LINE (54 + x200, 15 + y200)-(52 + x200, 15 + y200), 15: LINE (52 + x200, 14 + y200)-(51 + x200, 14 + y200), 15
LINE (57 + x200, 10 + y200)-(58 + x200, 13 + y200), 15, B: LINE (58 + x200, 12 + y200)-(59 + x200, 15 + y200), 15, B: LINE (60 + x200, 10 + y200)-(61 + x200, 13 + y200), 15, B: LINE (62 + x200, 12 + y200)-(63 + x200, 15 + y200), 15, B: LINE (63 + x200, 10 + y200)-(64 + x200, 13 + y200), 15, B
LINE (66 + x200, 11 + y200)-(67 + x200, 14 + y200), 15, B: LINE (67 + x200, 10 + y200)-(70 + x200, 12 + y200), 15, B: LINE (71 + x200, 11 + y200)-(71 + x200, 12 + y200), 15: LINE (67 + x200, 15 + y200)-(70 + x200, 15 + y200), 15: LINE (70 + x200, 14 + y200)-(71 + x200, 14 + y200), 15
LINE (73 + x200, 11 + y200)-(74 + x200, 14 + y200), 15, B: LINE (74 + x200, 10 + y200)-(77 + x200, 12 + y200), 15, B: LINE (78 + x200, 11 + y200)-(78 + x200, 12 + y200), 15: LINE (74 + x200, 15 + y200)-(77 + x200, 15 + y200), 15: LINE (77 + x200, 14 + y200)-(78 + x200, 14 + y200), 15: LINE (80 + x200, 10 + y200)-(81 + x200, 17 + y200), 15, B: LINE (81 + x200, 10 + y200)-(84 + x200, 15 + y200), 15, B: LINE (85 + x200, 11 + y200)-(85 + x200, 14 + y200), 15
LINE (87 + x200, 11 + y200)-(88 + x200, 14 + y200), 15, B: LINE (88 + x200, 10 + y200)-(91 + x200, 12 + y200), 15, B: LINE (92 + x200, 11 + y200)-(92 + x200, 12 + y200), 15: LINE (88 + x200, 15 + y200)-(91 + x200, 15 + y200), 15: LINE (91 + x200, 14 + y200)-(92 + x200, 14 + y200), 15: LINE (94 + x200, 10 + y200)-(95 + x200, 15 + y200), 15, B: PSET (96 + x200, 10 + y200), 15

LINE (137 + x200, 5 + y200)-(152 + x200, 18 + y200), 15, B: LINE (152 + x200, 5 + y200)-(152 + x200, 18 + y200), 0: LINE -(137 + x200, 18 + y200), 0
LINE (138 + x200, 6 + y200)-(151 + x200, 17 + y200), 7, BF: LINE (151 + x200, 6 + y200)-(151 + x200, 17 + y200), 0: LINE -(138 + x200, 17 + y200), 8
LINE (141 + x200, 8 + y200)-(147 + x200, 14 + y200), 0: LINE (142 + x200, 8 + y200)-(148 + x200, 14 + y200), 0: LINE (147 + x200, 8 + y200)-(141 + x200, 14 + y200), 0: LINE (148 + x200, 8 + y200)-(142 + x200, 14 + y200), 0

LINE (11 + x200, 26 + y200)-(14 + x200, 26 + y200), 0: PSET (15 + x200, 27 + y200), 0: LINE (10 + x200, 27 + y200)-(10 + x200, 33 + y200), 0: LINE (11 + x200, 34 + y200)-(13 + x200, 34 + y200), 0: LINE (15 + x200, 34 + y200)-(15 + x200, 30 + y200), 0: LINE -(13 + x200, 30 + y200), 0: PSET (14 + x200, 33 + y200), 0: LINE (10 + x200, 36 + y200)-(16 + x200, 36 + y200), 0
LINE (19 + x200, 29 + y200)-(21 + x200, 29 + y200), 0: LINE (22 + x200, 30 + y200)-(22 + x200, 34 + y200), 0: LINE -(19 + x200, 34 + y200), 0: LINE (18 + x200, 33 + y200)-(18 + x200, 32 + y200), 0: LINE (19 + x200, 31 + y200)-(21 + x200, 31 + y200), 0
LINE (24 + x200, 34 + y200)-(24 + x200, 29 + y200), 0: LINE -(26 + x200, 29 + y200), 0: LINE (27 + x200, 30 + y200)-(27 + x200, 34 + y200), 0: LINE (28 + x200, 29 + y200)-(29 + x200, 29 + y200), 0: LINE (30 + x200, 30 + y200)-(30 + x200, 34 + y200), 0
LINE (32 + x200, 30 + y200)-(32 + x200, 33 + y200), 0: LINE (33 + x200, 29 + y200)-(35 + x200, 29 + y200), 0: PSET (36 + x200, 30 + y200), 0: LINE (36 + x200, 31 + y200)-(32 + x200, 31 + y200), 0: LINE (33 + x200, 34 + y200)-(35 + x200, 34 + y200), 0: PSET (36 + x200, 33 + y200), 0





x100 = 15 + x200: y100 = 96 + y200
FOR x000 = 0 + x100 TO 127 + x100 STEP 16
 FOR y000 = 0 + y100 TO 127 + y100 STEP 16
  LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE -(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: LINE -(15 + x000, 0 + y000), 0: LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE -(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
 NEXT y000
NEXT x000
END SUB

