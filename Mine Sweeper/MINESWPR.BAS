DECLARE SUB seg7 (tom$, zr!, x0!, y0!)
DECLARE SUB wind1 (x1%, y1%, px!, py!, title$, B!, xsh)
DECLARE SUB game (mark!, nn!, select$(), yslct!(), tick)
DECLARE SUB door (x1!, y1!, px!, py!, select$(), yslct())
DECLARE SUB font (title$, B!, c!, x0!, y0!)
DECLARE SUB mint (min!)
DECLARE SUB miti (tom!)
DECLARE SUB save (n$, resv(), nn)
DECLARE SUB wind (x0%, y0%, nw!, ng!, nb!, x1%, y1%, k!)
DECLARE SUB best (besv(), gsave(), q)
DECLARE SUB mines (nm!, nn, lr%, lc%, gsave(), cusv(), cncl)
DECLARE SUB numbers (x000!, y000!, m!)
DECLARE SUB pooch (bomb!(), r, c, fl, flag(), x100, y100)
DECLARE SUB bombs (room!(), bomb!())
DECLARE SUB scene (x100, y100)
3 COMMON SHARED lr%, lc%, x200%, y200%
5 CLEAR , , 23000
8 lc% = 8: lr% = 8: mark = 1: mark0 = 0: nn = 10: min0 = min: nm = 10: min = nm
10 CLS : SCREEN 0: TIMER OFF
11 ON TIMER(1) GOSUB delay
15 COLOR 30: LOCATE 25, 3: PRINT "Please wait..................."
20 RANDOMIZE TIMER
30 DIM room(lr%, lc%)
35 DIM bomb(lr%, lc%)
40 DIM r(nm * 2)
50 DIM c(nm * 2)
52 DIM flag(lr%, lc%)
53 DIM gsave(4540)
54 DIM besv(5922)
55 DIM resv(2900)
56 DIM select$(12)
57 DIM yslct(12)
58 DIM cusv(2448)
60 fl = 0: s = 0: s1 = 0: n = 0: min = nm
70  DO
80   n = n + 1
90   r(n) = INT(RND * lr%) + 1: c(n) = INT(RND * lc%) + 1
110  IF room(r(n), c(n)) = 1 THEN n = n - 1: GOTO 140
130  room(r(n), c(n)) = 1
140 LOOP UNTIL n >= nm
160 CALL bombs(room(), bomb())
150 CALL scene(x100, y100)
155 CALL mint(min)
158 tom = 0: CALL miti(tom)
161 f$ = INPUT$(1)
165 tom = 1: CALL miti(tom)
170 time = TIMER - 2: x00 = x100: y00 = y100: r = 1: c = 1: COLOR 12: nm = min
175 TIMER ON
180 DO
182   min0 = min
184   r = INT(((y00 - y100) / 16) + 1): c = INT(((x00 - x100) / 16) + 1)
186   x000 = (c - 1) * 16 + x100: y000 = (r - 1) * 16 + y100
188   LINE (x000, y000)-(x000 + 15, y000 + 15), 12, B
190   f$ = ""
200   f$ = INKEY$
210   IF LEN(f$) = 2 THEN g$ = RIGHT$(f$, 1) ELSE g$ = f$
220   SELECT CASE g$
        CASE "M"
234       IF s = 0 THEN
236         GOSUB 1500
238         IF x00 = ((lc% - 1) * 16) + x100 THEN x00 = x100 - 16
240         x00 = x00 + 16
242       END IF
244     CASE "K"
246       IF s = 0 THEN
248         GOSUB 1500
250         IF x00 = x100 THEN x00 = lc% * 16 + x100
252         x00 = x00 - 16
254       END IF
256     CASE "H"
258       IF s = 0 THEN
260         GOSUB 1500
262         IF y00 = y100 THEN y00 = lr% * 16 + y100
264         y00 = y00 - 16
268       END IF
270     CASE "P"
272       IF s = 0 THEN
274         GOSUB 1500
276         IF y00 = (lr% - 1) * 16 + y100 THEN y00 = y100 - 16
278         y00 = y00 + 16
280       END IF
282     CASE CHR$(13)
284       IF s = 0 THEN
286         IF r > 0 AND r < lr% + 1 THEN
288           IF c > 0 AND c < lc% + 1 THEN
290             IF flag(r, c) <> 1 AND flag(r, c) <> 3 THEN IF bomb(r, c) = 9 THEN s = 1: TIMER OFF: GOSUB 1100
292             IF flag(r, c) <> 1 AND flag(r, c) <> 3 THEN IF bomb(r, c) = 0 THEN LINE (0 + x00, 0 + y00)-(15 + x00, 15 + y00), 7, BF: LINE (0 + x00, 15 + y00)-(15 + x00, 15 + y00), 0, , &H5555: LINE (15 + x00, 15 + y00)-(15 + x00, 0 + y00), 0, , &H5555: CALL pooch(bomb(), r, c, fl, flag(), x100, y100)
294             IF flag(r, c) <> 1 AND flag(r, c) <> 3 THEN IF bomb(r, c) <> 0 THEN flag(r, c) = 3: fl = fl + 1: m = bomb(r, c): CALL numbers(x000, y000, m)
296           END IF
298         END IF
300       END IF
310     CASE CHR$(32)
320       IF s = 0 THEN
322         IF r > 0 AND r < lr% + 1 THEN
324           IF c > 0 AND c < lc% + 1 THEN
326             IF min > 0 THEN IF flag(r, c) = 0 THEN flag(r, c) = 1: min = min - 1: GOSUB 1200
328             IF mark = 1 THEN
330               IF flag(r, c) = 1 THEN flag(r, c) = 2: min = min + 1: GOSUB 1300
332                ELSE IF flag(r, c) = 1 THEN flag(r, c) = 2: min = min + 1
334             END IF
336             IF flag(r, c) = 2 THEN flag(r, c) = 0: GOSUB 1400
338           END IF
340         END IF
342       END IF
344     CASE CHR$(60)
346       GOSUB new
348     CASE CHR$(24)
350       GOSUB endp
352     CASE CHR$(9)
354       TIMER STOP
356       time1 = TIMER
358       IF s1 = 0 THEN GOSUB 1500: LINE ((lc% * 8) + 2 + x200%, 56 + y200%)-((lc% * 8) + 27 + x200%, 81 + y200%), 12, B: s1 = 1
360       DO WHILE s1 <> 0
362         f$ = INPUT$(1)
364         IF s1 = 1 THEN IF f$ = CHR$(13) THEN GOSUB new
366         IF s1 = 2 THEN IF f$ = CHR$(13) THEN LINE (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 8, B: LINE (3 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 15: LINE -(42 + x200%, 22 + y200%), 15: GET (3 + x200%, 40 + y200%)-(130 + x200%, 218 + y200%), gsave: GOSUB games: LINE (42 + x200%, 22 + y200%)-(3 + x200%, 22 + y200%), 15, B: LINE -(3 + x200%, 39 + y200%), 15: LINE (4 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 8: LINE -(42 + x200%, 23 + y200%), 8
368         IF s1 = 3 THEN IF f$ = CHR$(13) THEN GOSUB endp
370         IF f$ = CHR$(9) THEN
372           IF s1 = 1 THEN s1 = 2: LINE ((lc% * 8) + 2 + x200%, 56 + y200%)-((lc% * 8) + 27 + x200%, 81 + y200%), 8, B: LINE (42 + x200%, 22 + y200%)-(3 + x200%, 22 + y200%), 15, B: LINE -(3 + x200%, 39 + y200%), 15: LINE (4 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 8: LINE -(42 + x200%, 23 + y200%), 8: GOTO 112
374           IF s1 = 2 THEN s1 = 3: LINE (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 7, B: LINE ((lc% * 16) + 9 + x200%, 5 + y200%)-((lc% * 16) + 24 + x200%, 18 + y200%), 12, B: GOTO 112
376           IF s1 = 3 THEN s1 = 0: LINE ((lc% * 16) + 9 + x200%, 5 + y200%)-((lc% * 16) + 24 + x200%, 18 + y200%), 15, B: LINE ((lc% * 16) + 24 + x200%, 5 + y200%)-((lc% * 16) + 24 + x200%, 18 + y200%), 0: LINE -((lc% * 16) + 9 + x200%, 18 + y200%), 0
378         END IF
112       LOOP
380       time = time + (TIMER - time1): TIMER ON
382     CASE CHR$(34)
384       TIMER STOP
386       time1 = TIMER
388       LINE (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 8, B: LINE (3 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 15: LINE -(42 + x200%, 22 + y200%), 15: GET (3 + x200%, 40 + y200%)-(130 + x200%, 218 + y200%), gsave: GOSUB games: LINE (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 7, B
390       time = time + (TIMER - time1): TIMER ON
400     CASE ELSE
410   END SELECT
415   IF s = 0 THEN IF tom > 999 THEN s = 1: TIMER OFF: GOSUB 1100
420   IF min0 <> min THEN CALL mint(min)
425   IF s = 0 THEN IF fl = (lr% * lc%) - nm THEN GOSUB 1110
430 LOOP
440 END

1100
  LINE ((lc% * 8) + 10 + x200%, 64 + y200%)-((lc% * 8) + 12 + x200%, 66 + y200%), 0, BF: CIRCLE ((lc% * 8) + 11 + x200%, 65 + y200%), 1, 14: LINE ((lc% * 8) + 16 + x200%, 64 + y200%)-((lc% * 8) + 18 + x200%, 66 + y200%), 0, BF: CIRCLE ((lc% * 8) + 17 + x200%, 65 + y200%), 1, 14: LINE ((lc% * 8) + 10 + x200%, 70 + y200%)-((lc% * 8) + 18 + x200%, 72 + y200%), 14, BF: LINE ((lc% * 8) + 10 + x200%, 72 + y200%)-((lc% * 8) + 12 + x200%, 70 + y200%), 0: LINE -((lc% * 8) + 16 + x200%, 70 + y200%), 0:   LINE -((lc% * 8) + 18 + x200%, 72 + y200%), 0
  FOR x = 1 TO lr%
    FOR y = 1 TO lc%
      IF room(x, y) = 1 AND flag(x, y) <> 1 THEN y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100: GOSUB bomp: flag(x, y) = 3
      IF room(x, y) <> 1 AND flag(x, y) = 1 THEN y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100: GOSUB cross: flag(x, y) = 3
    NEXT y
  NEXT x
RETURN

1110
  LINE ((lc% * 8) + 6 + x200%, 69 + y200%)-((lc% * 8) + 10 + x200%, 65 + y200%), 0: LINE -((lc% * 8) + 13 + x200%, 67 + y200%), 0, BF: PSET ((lc% * 8) + 14 + x200%, 65 + y200%), 0: LINE ((lc% * 8) + 22 + x200%, 69 + y200%)-((lc% * 8) + 18 + x200%, 65 + y200%), 0: LINE -((lc% * 8) + 15 + x200%, 67 + y200%), 0, BF: LINE ((lc% * 8) + 10 + x200%, 68 + y200%)-((lc% * 8) + 18 + x200%, 68 + y200%), 0, , &H6300
  FOR x = 1 TO lr%
    FOR y = 1 TO lc%
      IF room(x, y) = 1 AND flag(x, y) <> 1 THEN
        y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100
        LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE -(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: LINE -(15 + x000, 0 + y000), 0: LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE -(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
        LINE (4 + x000, 3 + y000)-(8 + x000, 6 + y000), 4, BF: LINE (4 + x000, 6 + y000)-(7 + x000, 3 + y000), 12: LINE (4 + x000, 5 + y000)-(6 + x000, 3 + y000), 12: LINE (4 + x000, 3 + y000)-(5 + x000, 4 + y000), 12, B: LINE (8 + x000, 7 + y000)-(8 + x000, 10 + y000), 0: LINE (7 + x000, 11 + y000)-(9 + x000, 11 + y000), 0: LINE (5 + x000, 12 + y000)-(11 + x000, 12 + y000), 0
      END IF
    NEXT y
  NEXT x
  min = 0: CALL mint(min)
  s = 1: TIMER OFF
  IF nn = 10 OR nn = 15 OR nn = 20 THEN
    OPEN "c:\dosmine0.db" FOR RANDOM AS #1 LEN = 17
      FIELD #1, 15 AS nam$, 2 AS t$
      GET #1, nn
      IF tim < CVI(t$) THEN
        CALL save(n$, resv(), nn)
        LSET nam$ = n$
        LSET t$ = MKI$(tim)
        PUT #1, nn
        q = 1
        CALL best(besv(), gsave(), q)
      END IF
    CLOSE #1
  END IF
RETURN

1200
  LINE (4 + x000, 3 + y000)-(8 + x000, 6 + y000), 4, BF: LINE (4 + x000, 6 + y000)-(7 + x000, 3 + y000), 12: LINE (4 + x000, 5 + y000)-(6 + x000, 3 + y000), 12
  LINE (4 + x000, 3 + y000)-(5 + x000, 4 + y000), 12, B: LINE (8 + x000, 7 + y000)-(8 + x000, 10 + y000), 0: LINE (7 + x000, 11 + y000)-(9 + x000, 11 + y000), 0: LINE (5 + x000, 12 + y000)-(11 + x000, 12 + y000), 0
GOTO 420: RETURN

1300
  LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE (x000, y000)-(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0
  LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0: LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE (14 + x000, 14 + y000)-(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
  LINE (4 + x000, 4 + y000)-(5 + x000, 5 + y000), 1, B: LINE (5 + x000, 3 + y000)-(8 + x000, 4 + y000), 1, B: LINE (8 + x000, 4 + y000)-(9 + x000, 7 + y000), 1, B
  LINE (10 + x000, 5 + y000)-(10 + x000, 6 + y000), 1: LINE (8 + x000, 7 + y000)-(6 + x000, 9 + y000), 1, BF: PSET (6 + x000, 7 + y000), 7: LINE (6 + x000, 11 + y000)-(8 + x000, 12 + y000), 1, B
GOTO 420: RETURN

1400
  LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE (x000, y000)-(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0
  LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0: LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE (14 + x000, 14 + y000)-(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
GOTO 420: RETURN

1500
  LINE (x000, y000)-(x000 + 15, y000 + 15), 7, B
  IF flag(r, c) = 0 OR flag(r, c) = 1 OR flag(r, c) = 2 THEN
    LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE (x000, y000)-(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0
  END IF
  IF flag(r, c) = 3 THEN
    LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 7: LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 7: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
  END IF
RETURN

bomp:
  LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
  LINE (5 + x000, 5 + y000)-(9 + x000, 9 + y000), 0, BF: PSET (6 + x000, 6 + y000), 15
  LINE (4 + x000, 4 + y000)-(10 + x000, 10 + y000), 8, B, &H5555
  LINE (4 + x000, 7 + y000)-(2 + x000, 7 + y000), 0: LINE (7 + x000, 10 + y000)-(7 + x000, 12 + y000), 0: LINE (10 + x000, 7 + y000)-(12 + x000, 7 + y000), 0: LINE (7 + x000, 4 + y000)-(7 + x000, 2 + y000), 0
RETURN

cross:
  LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
  LINE (3 + x000, 3 + y000)-(12 + x000, 12 + y000), 12: LINE (4 + x000, 3 + y000)-(12 + x000, 11 + y000), 12: LINE (3 + x000, 4 + y000)-(11 + x000, 12 + y000), 12
  LINE (3 + x000, 11 + y000)-(11 + x000, 3 + y000), 12: LINE (3 + x000, 12 + y000)-(12 + x000, 3 + y000), 12: LINE (4 + x000, 12 + y000)-(12 + x000, 4 + y000), 12
RETURN

games:
   CALL game(mark, nn, select$(), yslct(), tick):
   po = 1: ro = 5: co = 4: cb = 9: cf = 14: GOSUB 13
   DO
     f$ = INKEY$
     IF LEN(f$) = 2 THEN g$ = RIGHT$(f$, 1) ELSE g$ = f$
     SELECT CASE g$
       CASE CHR$(72)
         cb = 7: cf = 0: GOSUB 13: cb = 9: cf = 14
         IF po > 3 AND po < 7 THEN
           ro = ro - 1: po = po - 1
          ELSE
           ro = ro - 2: po = po - 2
         END IF
         IF ro < 5 THEN ro = 16: po = 12
         GOSUB 13
       CASE CHR$(80)
         cb = 7: cf = 0: GOSUB 13: cb = 9: cf = 14
         IF po > 2 AND po < 6 THEN
           ro = ro + 1: po = po + 1
          ELSE
           ro = ro + 2: po = po + 2
         END IF
         IF ro > 16 THEN ro = 5: po = 1
         GOSUB 13
       CASE CHR$(13)
         SELECT CASE po
           CASE 1: GOSUB new
           CASE 3: nm = 10: lr% = 8: lc% = 8: nn = 10: GOSUB new
           CASE 4: nm = 40: lr% = 16: lc% = 16: nn = 15: GOSUB new
           CASE 5: nm = 99: lr% = 16: lc% = 30: nn = 20: GOSUB new
           CASE 6: CALL mines(nm, nn, lr%, lc%, gsave(), cusv(), cncl): IF cncl = 1 THEN GOTO 12 ELSE GOSUB new
           CASE 8: SWAP mark, mark0: GOTO pak
           CASE 10: CALL best(besv(), gsave(), q): GOTO 12
           CASE 12: GOSUB endp
           CASE ELSE: GOTO pak
         END SELECT
       CASE CHR$(27)
pak:     PUT (3 + x200%, 40 + y200%), gsave, PSET: GOTO 12
       CASE ELSE
     END SELECT
   LOOP
12 RETURN

13
  LINE (6 + x200%, yslct(po))-(127 + x200%, yslct(po) - 16), cb, BF
  x0 = 28 + x200%: y0 = yslct(po) - 5
  CALL font(select$(po), 0, cf, x0, y0)
  IF po = 8 THEN IF mark = 1 THEN x = 12 + x200%: y = yslct(8) - 7: LINE (x, y)-(x + 2, y + 2), cf: LINE -(x + 6, y - 2), cf: LINE (x, y - 1)-(x + 2, y + 1), cf: LINE -(x + 6, y - 3), cf: LINE (x, y - 2)-(x + 2, y), cf: LINE -(x + 6, y - 4), cf
  IF po = tick THEN x = 12 + x200%: y = yslct(tick) - 7: LINE (x, y)-(x + 2, y + 2), cf: LINE -(x + 6, y - 2), cf: LINE (x, y - 1)-(x + 2, y + 1), cf: LINE -(x + 6, y - 3), cf: LINE (x, y - 2)-(x + 2, y), cf: LINE -(x + 6, y - 4), cf
RETURN

new:
  IF nm = 0 THEN nm = 10
  ERASE flag, room, bomb, r, c
  GOTO 10
RETURN

delay:
  tom = INT(TIMER - time): tim = tom
  IF s = 0 THEN CALL miti(tom)
RETURN

endp:
  CLS
  s = 0
  CALL wind1(200, 190, 240, 100, "Shut Down Minesweeper", 1, 0)
  CALL font("Thank you for playing this game", 0, 0, 242, 230)
  CALL font("Omid Khoshniat Aram", 1, 0, 260, 250)
  CALL font("Press any key to exit", 0, 0, 270, 280)
  COLOR 0
  END
RETURN

SUB best (besv(), gsave(), q)
2 IF q <> 1 THEN PUT (3 + x200%, 40 + y200%), gsave, PSET
  q = 0
  GET (155, 79 + y200%)-(484, 219 + y200%), besv
  CALL wind1(155, 79 + y200%, 329, 140, "Best Times", 1, 0)
  LINE (169, 110 + y200%)-(470, 204 + y200%), 8, B
  LINE (170, 111 + y200%)-(471, 205 + y200%), 15, B
  LINE (176, 110 + y200%)-(289, 111 + y200%), 7, B
  CLOSE
  CALL font("Fastest Mine Sweepers", 0, 0, 179, 114 + y200%)
  OPEN "c:\dosmine0.db" FOR RANDOM AS #1 LEN = 17
    FIELD #1, 15 AS nam$, 2 AS t$
    GET #1, 10
    CALL font("Beginner:                 " + STR$(CVI(t$)) + " seconds            " + nam$, 0, 0, 180, 137 + y200%)
    GET #1, 15
    CALL font("Intermediate:            " + STR$(CVI(t$)) + " seconds            " + nam$, 0, 0, 179, 163 + y200%)
    GET #1, 20
    CALL font("Expert:                     " + STR$(CVI(t$)) + " seconds            " + nam$, 0, 0, 180, 188 + y200%)
  CLOSE #1
  DO WHILE ak$ <> CHR$(27): ak$ = INKEY$:
    IF ak$ = CHR$(79) THEN ak$ = INPUT$(1): IF ak$ = CHR$(109) THEN ak$ = INPUT$(1): IF ak$ = CHR$(73) THEN ak$ = INPUT$(1): IF ak$ = CHR$(100) THEN GOSUB us
  LOOP
  PUT (155, 79 + y200%), besv, PSET
EXIT SUB

us:
  OPEN "c:\dosmine0.db" FOR RANDOM AS #1 LEN = 17
    FIELD #1, 15 AS nam$, 2 AS t$
    LSET nam$ = "Anonymous"
    LSET t$ = MKI$(999)
    PUT #1, 10
    PUT #1, 15
    PUT #1, 20
  CLOSE #1
  PUT (155, 79 + y200%), besv, PSET
  GOTO 2
RETURN

END SUB

SUB bombs (room(), bomb())
  FOR x = 1 TO lr%
    x1 = x
    FOR y = 1 TO lc%
      y1 = y
      IF room(x, y) <> 1 THEN
        x0 = x1: y0 = y1: x0 = x0 - 1: y0 = y0 - 1: IF x0 > 0 AND x0 < lr% + 1 THEN IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
        x0 = x1: y0 = y1: y0 = y0 - 1: IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
        x0 = x1: y0 = y1: x0 = x0 + 1: y0 = y0 - 1: IF x0 > 0 AND x0 < lr% + 1 THEN IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
        x0 = x1: y0 = y1: x0 = x0 + 1: IF x0 > 0 AND x0 < lr% + 1 THEN IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
        x0 = x1: y0 = y1: x0 = x0 + 1: y0 = y0 + 1: IF x0 > 0 AND x0 < lr% + 1 THEN IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
        x0 = x1: y0 = y1: y0 = y0 + 1: IF x0 > 0 AND x0 < lr% + 1 THEN IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
        x0 = x1: y0 = y1: x0 = x0 - 1: y0 = y0 + 1: IF x0 > 0 AND x0 < lr% + 1 THEN IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
        x0 = x1: y0 = y1: x0 = x0 - 1: IF x0 > 0 AND x0 < lr% + 1 THEN IF y0 > 0 AND y0 < lc% + 1 THEN IF room(x0, y0) = 1 THEN n = n + 1
       ELSE
        n = 9
      END IF
      bomb(x, y) = n: n = 0
    NEXT y
  NEXT x

END SUB

SUB door (x1, y1, px, py, select$(), yslct())
  x2 = x1 + px: y2 = y1 + py
  LINE (x1, y1)-(x2, y2), 7, BF
  LINE (x2, y1 + 1)-(x2, y2), 0: LINE -(x1 + 1, y2), 0
  LINE (x2 - 1, y1 + 1)-(x1 + 1, y1 + 1), 15: LINE -(x1 + 1, y2 - 1), 15
  LINE (x1 + 2, y2 - 1)-(x2 - 1, y2 - 1), 8: LINE -(x2 - 1, y1 + 2), 8
  y0 = y1 + 15: c = 0
  FOR s = 1 TO UBOUND(select$)
    x0 = x1 + 25
    yslct(s) = y0 + 5
    IF select$(s) = "__" THEN
      y0 = y0 + 9
      LINE (x1 + 4, y0)-(x2 - 4, y0), 8
      LINE (x1 + 4, y0 + 1)-(x2 - 4, y0 + 1), 15
      y0 = y0 + 17
      GOTO ns
    END IF
    CALL font(select$(s), 0, c, x0, y0)
    IF s < UBOUND(select$) THEN IF select$(s + 1) <> "__" THEN y0 = y0 + 17
ns: NEXT s

END SUB

SUB font (title$, B, c, x0, y0)
FOR i = 1 TO LEN(title$)
  harf$ = MID$(title$, i, 1)
  IF harf$ = "_" THEN ul = 1: GOTO ni
  SELECT CASE harf$
  CASE "a"
   b0 = B
a: a$ = "bu5 br1 r2 f1 d4 l3 h1 u1 e1 r2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + a$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO a
   x0 = x0 + 6 + ul: ul = 0
 CASE "b"
   b0 = B
B: B$ = "u8 bd3 r3 f1 d3 g1 l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + B$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO B
   x0 = x0 + 6 + ul: ul = 0
 CASE "c"
   b0 = B
c: c$ = "br4 bu1 g1 l2 h1 u3 e1 r2 f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + c$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO c
   x0 = x0 + 6 + ul: ul = 0
 CASE "d"
   b0 = B
d: d$ = "br1 r3 u8 d3 l3 g1 d3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + d$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO d
   x0 = x0 + 6 + ul: ul = 0
 CASE "e"
   b0 = B
e: e$ = "bu3 r4 u1 h1 l2 g1 d3 f1 r2 e1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + e$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO e
   x0 = x0 + 6 + ul: ul = 0
CASE "f"
   b0 = B
f: f$ = "u7 e1 bd3 d0": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + f$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO f
   x0 = x0 + 3 + ul: ul = 0
 CASE "g"
   b0 = B
g: g$ = "br3 l2 h1 u3 e1 r3 d6 g1 l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + g$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO g
   x0 = x0 + 6 + ul: ul = 0
 CASE "h"
   b0 = B
h: h$ = "u8 bd4 r1 e1 r1 f1 d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + h$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO h
   x0 = x0 + 6 + ul: ul = 0
 CASE "i"
   b0 = B
i: i$ = "u5 bu3 d0": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + i$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO i
   x0 = x0 + 2 + ul: ul = 0
 CASE "j"
   b0 = B
j: j$ = "d2 u7 bu3 d0": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + j$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO j
   x0 = x0 + 2 + ul: ul = 0
 CASE "k"
   b0 = B
k: k$ = "u8 d5 r1 e2 g2 f3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + k$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO k
   x0 = x0 + 6 + ul: ul = 0
 CASE "l"
   b0 = B
L: L$ = "u8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + L$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO L
   x0 = x0 + 2 + ul: ul = 0
 CASE "m"
   b0 = B
m: m$ = "u5 r2 f1 d4 u4 e1 r1 f1 d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + m$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO m
   x0 = x0 + 8 + ul: ul = 0
 CASE "n"
   b0 = B
n: n$ = "u5 d1 r1 e1 r1 f1 d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO n
   x0 = x0 + 6 + ul: ul = 0
 CASE "o"
   b0 = B
o: o$ = "bu1 u3 e1 r2 f1 d3 g1 l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + o$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO o
   x0 = x0 + 6 + ul: ul = 0
 CASE "p"
   b0 = B
p: p$ = "r3 e1 u3 h1 l3 d7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + p$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO p
   x0 = x0 + 6 + ul: ul = 0
 CASE "q"
   b0 = B
q: q$ = "br3 l2 h1 u3 e1 r3 d7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + q$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO q
   x0 = x0 + 6 + ul: ul = 0
 CASE "r"
   b0 = B
r: r$ = "u5 r1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + r$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO r
   x0 = x0 + 3 + ul: ul = 0
 CASE "s"
   b0 = B
s: s$ = "bu1 f1 r1 e1 h3 e1 r1 f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + s$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO s
   x0 = x0 + 5 + ul: ul = 0
 CASE "t"
   b0 = B
t: t$ = "br1 h1 u6 d2 r1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + t$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO t
   x0 = x0 + 3 + ul: ul = 0
 CASE "u"
   b0 = B
u: u$ = "bu5 d4 f1 r1 e1 r1 d1 u5": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + u$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO u
   x0 = x0 + 6 + ul: ul = 0
 CASE "v"
   b0 = B
v: v$ = "bu5 d1 f1 d1 f1 d1 u1 e1 u1 e1 u1 ": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + v$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO v
   x0 = x0 + 6 + ul: ul = 0
 CASE "w"
   b0 = B
w: w$ = "bu5 d3 f1 d1 u1 e1 u1 e1 u1 d1 f1 d1 f1 d1 u1 e1 u3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + w$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO w
   x0 = x0 + 8 + ul: ul = 0
 CASE "x"
   b0 = B
x: x$ = "u1 e1 r1 f1 d1 bu5 d1 g1 l1 h1 u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + x$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO x
   x0 = x0 + 5 + ul: ul = 0
 CASE "y"
   b0 = B
y: y$ = "bu5 d3 f1 r1 e1 u3 bd4 bl1 d1 g1 l1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + y$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO y
   x0 = x0 + 5 + ul: ul = 0
 CASE "z"
   b0 = B
z: z$ = "bu5 r3 d1 g3 d1 r3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + z$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO z
   x0 = x0 + 5 + ul: ul = 0
 CASE "A"
   b0 = B
aa: a$ = "u1 e1 u2 e1 u1 e1 u1 d1 f1 d1 f1 d2 l3 r3 f1 d1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + a$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO aa
   x0 = x0 + 8 + ul: ul = 0
 CASE "B"
   b0 = B
bb: B$ = "u8 r3 f1 d2 g1 l2 r2 f1d2g1l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + B$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO bb
   x0 = x0 + 6 + ul: ul = 0
 CASE "C"
   b0 = B
cc: c$ = "br5bu1g1l3h1u6e1r3f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + c$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO cc
   x0 = x0 + 7 + ul: ul = 0
 CASE "D"
   b0 = B
dd: d$ = "u8r3f2d4g2l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + d$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO dd
   x0 = x0 + 7 + ul: ul = 0
 CASE "E"
   b0 = B
ee: e$ = "r4l4u8r4bd4bl1l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + e$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ee
   x0 = x0 + 6 + ul: ul = 0
 CASE "F"
   b0 = B
ff: f$ = "u8r4bd4bl1l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + f$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ff
   x0 = x0 + 6 + ul: ul = 0
 CASE "G"
   b0 = B
gg: g$ = "bu1u6e1r3f1bd3l2r2d4h1g1l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + g$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO gg
   x0 = x0 + 7 + ul: ul = 0
 CASE "H"
   b0 = B
hh: h$ = "u8d4r5u4d8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + h$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO hh
   x0 = x0 + 7 + ul: ul = 0
 CASE "I"
   b0 = B
ii: i$ = "u8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + i$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ii
   x0 = x0 + 2 + ul: ul = 0
 CASE "J"
   b0 = B
jj: j$ = "bu2d1f1r1e1u7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + j$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO jj
   x0 = x0 + 5 + ul: ul = 0
 CASE "K"
   b0 = B
kk: k$ = "u8d3r1e3g3d1f4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + k$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO kk
   x0 = x0 + 7 + ul: ul = 0
 CASE "L"
   b0 = B
ll: L$ = "u8d8r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + L$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ll
   x0 = x0 + 6 + ul: ul = 0
 CASE "M"
   b0 = B
mm: m$ = "u8d2r1d1f1d1f1d1u1e1u1e1u1r1u2d8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + m$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO mm
   x0 = x0 + 8 + ul: ul = 0
 CASE "N"
   b0 = B
nn: n$ = "u8f1d1f1d1f2d1f1u8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO nn
   x0 = x0 + 7 + ul: ul = 0
 CASE "O"
   b0 = B
oo: o$ = "bu1u6e1r3f1d6g1l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + o$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO oo
   x0 = x0 + 7 + ul: ul = 0
 CASE "P"
   b0 = B
pp: p$ = "u8r4f1d2g1l4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + p$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO pp
   x0 = x0 + 7 + ul: ul = 0
 CASE "Q"
   b0 = B
qq: q$ = "bu1u6e1r3f1d6g1l3br2bu2f1d1f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + q$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO qq
   x0 = x0 + 7 + ul: ul = 0
 CASE "R"
   b0 = B
rr: r$ = "u8r4f1d2g1l4r4f1d3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + r$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO rr
   x0 = x0 + 7 + ul: ul = 0
 CASE "S"
   b0 = B
ss: s$ = "bu1f1r2e1u2h1l2h1u2e1r2f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + s$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ss
   x0 = x0 + 6 + ul: ul = 0
 CASE "T"
   b0 = B
tt: t$ = "br2u8l2r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + t$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO tt
   x0 = x0 + 6 + ul: ul = 0
 CASE "U"
   b0 = B
uu: u$ = "bu8d7f1r3e1u7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + u$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO uu
   x0 = x0 + 7 + ul: ul = 0
 CASE "V"
   b0 = B
vv: v$ = "bu8d1f1d2f1d1f1d1u1e1u1e1u2e1u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + v$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO vv
   x0 = x0 + 8 + ul: ul = 0
 CASE "W"
   b0 = B
ww: w$ = "bu8d1f1d2f1d1f1d1u1e1u1e1u2d2f1d1f1d1u1e1u1e1u2e1u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + w$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r12"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ww
   x0 = x0 + 12 + ul: ul = 0
 CASE "X"
   b0 = B
xx: x$ = "u1e6u1bl6d1f6d1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + x$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO xx
   x0 = x0 + 8 + ul: ul = 0
 CASE "Y"
   b0 = B
yy: y$ = "bu8d1f3d4u4e3u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + y$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO yy
   x0 = x0 + 8 + ul: ul = 0
 CASE "Z"
   b0 = B
zz: z$ = "bu8r6d1g6d1r6": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + z$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO zz
   x0 = x0 + 8 + ul: ul = 0
 CASE " "
  x0 = x0 + 3
 CASE "1"
  b0 = B
one: n$ = "bu7r2u1d8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r4"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO one
   x0 = x0 + 4 + ul: ul = 0
 CASE "2"
  b0 = B
two: n$ = "bu7e1r2f1d2g4d1r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO two
   x0 = x0 + 6 + ul: ul = 0
 CASE "3"
 b0 = B
three: n$ = "bu7e1r2f1d2g1l1r1f1d2g1l2h1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO three
   x0 = x0 + 6 + ul: ul = 0
 CASE "4"
  b0 = B
four: n$ = "br3u8g1d1g1d1g1d1r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO four
   x0 = x0 + 6 + ul: ul = 0
 CASE "5"
 b0 = B
five: n$ = "bu1f1r2e1u3h1l3d1u4r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO five
 x0 = x0 + 6 + ul: ul = 0
 CASE "6"
 b0 = B
six: n$ = "bu4r3f1d2g1l2h1u6e1r2f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO six
 x0 = x0 + 6 + ul: ul = 0
 CASE "7"
 b0 = B
seven: n$ = "bu8r4d1g1d1g1d1g1d2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO seven
 x0 = x0 + 6 + ul: ul = 0
 CASE "8"
 b0 = B
eight: n$ = "bu1u2e1r2f1d2g1l2bl1bu5u2e1r2f1d2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO eight
 x0 = x0 + 6 + ul: ul = 0
 CASE "9"
 b0 = B
nine: n$ = "bu1f1r2e1u6h1l2g1d2f1r2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO nine
 x0 = x0 + 6 + ul: ul = 0
 CASE "0"
 b0 = B
zero: n$ = "bu1u6e1r2f1d6g1l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO zero
  x0 = x0 + 6 + ul: ul = 0
 CASE "-"
 b0 = B
lin: n$ = "bu3r1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO lin
 x0 = x0 + 3
 CASE "."
 b0 = B
dot: DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + "u0"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO dot
 x0 = x0 + 2
 CASE "?"
 b0 = B
qsmk: n$ = "br2u0bu2u1e2u2h1l2g1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO qsmk
   x0 = x0 + 6 + ul: ul = 0
 CASE "("
 b0 = B
paro: n$ = "bf1h1u8e1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO paro
   x0 = x0 + 3 + ul: ul = 0
 CASE ")"
  b0 = B
parc: n$ = "bd1e1u8h1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO parc
   x0 = x0 + 3 + ul: ul = 0
 CASE "_"
  b0 = B
ulin: n$ = "bd1r5": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ulin
   x0 = x0 + 7
 CASE "+"
  b0 = B
plus: n$ = "bu3r4l2u2d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO plus
   x0 = x0 + 6 + ul: ul = 0
 CASE "="
  b0 = B
equl: n$ = "bu2r4bu2l4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO equl
   x0 = x0 + 6 + ul: ul = 0
 CASE "<"
  b0 = B
bigm: n$ = "br3h3e3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO bigm
   x0 = x0 + 5 + ul: ul = 0
 CASE ">"
  b0 = B
smlm: n$ = "e3h3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO smlm
   x0 = x0 + 5 + ul: ul = 0
 CASE "{"
  b0 = B
aklo: n$ = "br2bd1h1u3h1e1u3e1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r4"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO aklo
   x0 = x0 + 4 + ul: ul = 0
 CASE "}"
  b0 = B
aklc: n$ = "bd1e1u3e1h1u3h1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r4"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO aklc
   x0 = x0 + 4 + ul: ul = 0
  CASE ":"
   b0 = B
twdt: n$ = "u0bu5d0": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO aklc
   x0 = x0 + 2 + ul: ul = 0
  CASE ELSE
END SELECT
ni: NEXT i

END SUB

SUB game (mark, nn, select$(), yslct(), tick)
  select$(1) = "_New               F2"
  select$(2) = "__"
  select$(3) = "_Beginner"
  select$(4) = "_Intermediate"
  select$(5) = "_Expert"
  select$(6) = "_Custom..."
  select$(7) = "__"
  select$(8) = "_Marks (?)"
  select$(9) = "__"
  select$(10) = "Best _Times..."
  select$(11) = "__"
  select$(12) = "E_xit"
  CALL door(3 + x200%, 40 + y200%, 127, 178, select$(), yslct())
  tc = nn / 5 + 1
  tick = tc
  x = 12 + x200%: y = yslct(tc) - 7: LINE (x, y)-(x + 2, y + 2), 0: LINE -(x + 6, y - 2), 0: LINE (x, y - 1)-(x + 2, y + 1), 0: LINE -(x + 6, y - 3), 0: LINE (x, y - 2)-(x + 2, y), 0: LINE -(x + 6, y - 4), 0
  IF mark = 1 THEN x = 12 + x200%: y = yslct(8) - 7: LINE (x, y)-(x + 2, y + 2), 0: LINE -(x + 6, y - 2), 0: LINE (x, y - 1)-(x + 2, y + 1), 0: LINE -(x + 6, y - 3), 0: LINE (x, y - 2)-(x + 2, y), 0: LINE -(x + 6, y - 4), 0

END SUB

SUB mines (nm, nn, lr%, lc%, gsave(), cusv(), cncl)
 
  PUT (3 + x200%, 40 + y200%), gsave, PSET
  GET (245, 79 + y200%)-(384, 214 + y200%), cusv: lr0 = lr%: lc0 = lc%: nm0 = nm: nn0 = nn
  CALL wind1(245, 79 + y200%, 139, 135, "Custom Field", 1, 0)
  nn = 25
  CALL font("_Height:", 0, 0, 257, 126 + y200%)
  CALL font("_Width:", 0, 0, 256, 160 + y200%)
  CALL font("_Mines:", 0, 0, 257, 194 + y200%)
  yj = 0: GOSUB rege
  yj = 34: GOSUB rege
  yj = 68: GOSUB rege
  yj = 0
dlc:
  DO UNTIL harf$ = CHR$(13)
    y0 = 125 + yj + y200%
    LINE (x0, 127 + yj + y200%)-(x0, 115 + yj + y200%), 0
    harf$ = INPUT$(1)
    LINE (x0, 127 + yj + y200%)-(x0, 115 + yj + y200%), 15
    IF harf$ = CHR$(8) THEN
      nu = 0: GOSUB rege
      IF yj = 0 THEN rm$ = ""
      IF yj = 34 THEN cm$ = ""
      IF yj = 68 THEN mm$ = ""
    END IF
    IF harf$ = CHR$(27) THEN
      PUT (245, 79 + y200%), cusv, PSET
      lr% = lr0: lc% = lc0: nm = nm0: nn = nn0
      cncl = 1: EXIT SUB
    END IF
    IF harf$ = CHR$(9) THEN
      yj = yj + 34
      x0 = 314
      IF yj = 102 THEN yj = 0
      nu = 0
      GOSUB rege
      IF yj = 0 THEN rm$ = ""
      IF yj = 34 THEN cm$ = ""
      IF yj = 68 THEN mm$ = ""
      GOTO dlc
    END IF
    IF ASC(harf$) > 47 AND ASC(harf$) < 58 THEN
      nu = nu + 1
      IF nu > 3 THEN GOTO dlc
      IF yj = 0 THEN rm$ = rm$ + harf$
      IF yj = 34 THEN cm$ = cm$ + harf$
      IF yj = 68 THEN mm$ = mm$ + harf$
      CALL font(harf$, 0, 0, x0, y0)
    END IF
  LOOP
  lr% = VAL(rm$): lc% = VAL(cm$): nm = VAL(mm$)
  IF lr% < 8 THEN lr% = 8
  IF lr% > 22 THEN lr% = 22
  IF lc% < 8 THEN lc% = 8
  IF lc% > 32 THEN lc% = 32
  IF nm > ((lr% - 1) * (lc% - 1)) THEN nm = ((lr% - 1) * (lc% - 1))
  cncl = 0
EXIT SUB

rege:
  x0 = 314
  LINE (310, 112 + yj + y200%)-(369, 131 + yj + y200%), 15, BF
  LINE (310, 130 + yj + y200%)-(310, 112 + yj + y200%), 8: LINE -(368, 112 + yj + y200%), 8
  LINE (368, 113 + yj + y200%)-(368, 130 + yj + y200%), 7: LINE -(309, 130 + yj + y200%), 7
  LINE (311, 129 + yj + y200%)-(311, 113 + yj + y200%), 0: LINE -(367, 113 + yj + y200%), 0
RETURN

END SUB

SUB mint (min)
  x0 = 20 + x200%: y0 = 79 + y200%
  tom$ = STR$(min / 1000)
  CALL seg7(tom$, zr, x0, y0)
  IF LEN(tom$) <> 5 THEN
    t = 5 - LEN(tom$)
    FOR t = 1 TO t
      tom$ = "0"
      zr = 1: CALL seg7(tom$, zr, x0, y0)
    NEXT t
  END IF

END SUB

SUB miti (tom)
  x0 = (lc% - 3) * 16 + 18 + x200%: y0 = 79 + y200%: zr = 0
  tom$ = STR$(tom / 1000)
  CALL seg7(tom$, zr, x0, y0)
  IF LEN(tom$) <> 5 THEN
    t = 5 - LEN(tom$)
    FOR t = 1 TO t
      tom$ = "0"
      zr = 1: CALL seg7(tom$, zr, x0, y0)
    NEXT t
  END IF

END SUB

SUB numbers (x000, y000, m)
LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
SELECT CASE m
 CASE 1
  k = 1
  LINE (7 + x000, 3 + y000)-(8 + x000, 10 + y000), k, BF: LINE (5 + x000, 11 + y000)-(10 + x000, 11 + y000), k: LINE (5 + x000, 5 + y000)-(6 + x000, 5 + y000), k: PSET (6 + x000, 5 + y000), k
 CASE 2
  k = 5
  LINE (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: LINE (10 + x000, 4 + y000)-(4 + x000, 10 + y000), k: LINE (10 + x000, 5 + y000)-(4 + x000, 11 + y000), k
  LINE (4 + x000, 11 + y000)-(10 + x000, 11 + y000), k: PSET (4 + x000, 4 + y000), k: PSET (9 + x000, 4 + y000), k
 CASE 3
  k = 4
  LINE (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: LINE (9 + x000, 4 + y000)-(10 + x000, 5 + y000), k, B: LINE (5 + x000, 6 + y000)-(9 + x000, 6 + y000), k
  LINE (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: LINE (4 + x000, 11 + y000)-(9 + x000, 11 + y000), k
 CASE 4
  k = 9
  LINE (4 + x000, 3 + y000)-(5 + x000, 7 + y000), k, B: LINE (6 + x000, 7 + y000)-(8 + x000, 7 + y000), k: LINE (9 + x000, 3 + y000)-(10 + x000, 11 + y000), k, B
 CASE 5
  k = 6
  LINE (4 + x000, 3 + y000)-(5 + x000, 6 + y000), k, B: LINE (6 + x000, 3 + y000)-(10 + x000, 3 + y000), k: LINE (6 + x000, 6 + y000)-(9 + x000, 6 + y000), k
  LINE (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: LINE (4 + x000, 10 + y000)-(5 + x000, 10 + y000), k: LINE (4 + x000, 11 + y000)-(9 + x000, 11 + y000), k
 CASE 6
  k = 11
  LINE (4 + x000, 4 + y000)-(5 + x000, 10 + y000), k, B: LINE (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: LINE (9 + x000, 4 + y000)-(10 + x000, 4 + y000), k
  LINE (6 + x000, 6 + y000)-(9 + x000, 6 + y000), k: LINE (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: LINE (5 + x000, 11 + y000)-(9 + x000, 11 + y000), k
 CASE 7
  k = 13
  LINE (4 + x000, 3 + y000)-(5 + x000, 4 + y000), k, B: LINE (6 + x000, 3 + y000)-(10 + x000, 3 + y000), k
  LINE (6 + x000, 10 + y000)-(9 + x000, 4 + y000), k: LINE (7 + x000, 10 + y000)-(10 + x000, 4 + y000), k: LINE (6 + x000, 11 + y000)-(7 + x000, 11 + y000), k
 CASE 8
  k = 0
  LINE (5 + x000, 3 + y000)-(9 + x000, 11 + y000), k, B: LINE (4 + x000, 3 + y000)-(4 + x000, 11 + y000), k, , &H6F00: LINE (10 + x000, 3 + y000)-(10 + x000, 11 + y000), k, , &H6F00: LINE (5 + x000, 6 + y000)-(9 + x000, 6 + y000), k
END SELECT

END SUB

SUB pooch (bomb(), r, c, fl, flag(), x100, y100)
 
    n = 0: n2 = 0: j = 0: n0 = 0
    DIM a(lr% * lc%)
    DIM B(lr% * lc%)
    DIM a0(lr% * lc%)
    DIM b0(lr% * lc%)
    DIM n10(lr% * lc%)
    DIM g0(lr% * lc%)
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
1008 fl = fl - 1: ERASE a, B, a0, b0, n10, g0
EXIT SUB

1005
  IF r + B(n2) > 0 AND r + B(n2) < lr% + 1 THEN
    IF c + a(n2) > 0 AND c + a(n2) < lc% + 1 THEN
      IF flag(r + B(n2), c + a(n2)) = 3 OR flag(r + B(n2), c + a(n2)) = 1 THEN g = g + 1: d = 1: GOTO 1004
      IF bomb(r + B(n2), c + a(n2)) = 0 THEN n0 = n0 + 1: a0(n0) = a(n2): b0(n0) = B(n2): g0(n0) = g: n10(n0) = n2
      IF r + B(n2) > 0 AND r + B(n2) < lr% + 1 THEN
        IF c + a(n2) > 0 AND c + a(n2) < lc% + 1 THEN
          m = bomb(r + B(n2), c + a(n2))
          x000 = (c + a(n2) - 1) * 16 + x100: y000 = (r + B(n2) - 1) * 16 + y100
          IF m = 0 THEN
            LINE (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: LINE (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
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

SUB save (n$, resv(), nn)

  GET (206, 79 + y200%)-(433, 178 + y200%), resv
  CALL wind1(206, 79 + y200%, 227, 99, "Congratulations", 1, 0)
  IF nn = 10 THEN L$ = "beginner"
  IF nn = 15 THEN L$ = "intermediate"
  IF nn = 20 THEN L$ = "expert"
  CALL font("You have the fastest time for " + L$, 0, 0, 220, 116 + y200%)
  CALL font("level. Please type your name:", 0, 0, 221, 129 + y200%)
reg: LINE (220, 144 + y200%)-(420, 163 + y200%), 15, BF
  LINE (220, 162 + y200%)-(220, 144 + y200%), 8: LINE -(419, 144 + y200%), 8
  LINE (419, 145 + y200%)-(419, 162 + y200%), 7: LINE -(221, 162 + y200%), 7
  LINE (221, 161 + y200%)-(221, 145 + y200%), 0: LINE -(418, 145 + y200%), 0
  x0 = 224: n$ = ""
dl: DO UNTIL harf$ = CHR$(13)
      LINE (x0, 159 + y200%)-(x0, 148 + y200%), 0
      harf$ = INPUT$(1)
      LINE (x0, 159 + y200%)-(x0, 148 + y200%), 15
      IF harf$ = CHR$(8) THEN GOTO reg
      IF x0 > 314 THEN GOTO dl
      n$ = n$ + harf$
      CALL font(harf$, 0, 0, x0, 157 + y200%)
    LOOP
  PUT (206, 79 + y200%), resv, PSET

END SUB

SUB scene (x100, y100)

SCREEN 12: PALETTE 12, 63: PALETTE 4, 31
  tx = 320 - ((lc% * 16) + 30) / 2: ty = 240 - ((lr% * 16) + 110) / 2
  x200% = INT(tx): y200% = INT(ty)
  PAINT (0, 0), 8
  CALL wind1(x200%, y200%, (16 * lc%) + 29, (16 * lr%) + 110, "Minesweeper", 1, 15)
  CALL font("_Game", 0, 0, 10 + x200%, 34 + y200%)
  CALL wind(x200%, 81 + y200%, 3, 6, 3, (16 * lc%) + 29 + x200%, (16 * lr%) + 110 + y200%, 7)
  CALL wind(x200%, 38 + y200%, 3, 6, 2, (16 * lc%) + 29 + x200%, 98 + y200%, 7)
  LINE (6 + x200%, 83 + y200%)-(11 + x200%, 96 + y200%), 7, BF
  LINE (3 + x200%, 92 + y200%)-(5 + x200%, 96 + y200%), 15, BF
  LINE ((lc% * 16) + 18 + x200%, 83 + y200%)-((lc% * 16) + 23 + x200%, 96 + y200%), 7, BF
  LINE ((lc% * 16) + 16 + x200%, 95 + y200%)-((lc% * 16) + 17 + x200%, 94 + y200%), 15: PSET STEP(0, 1), 15
 
  LINE ((lc% * 8) + 2 + x200%, 56 + y200%)-((lc% * 8) + 27 + x200%, 81 + y200%), 8, B
  LINE ((lc% * 8) + 3 + x200%, 57 + y200%)-((lc% * 8) + 26 + x200%, 80 + y200%), 15, BF
  LINE ((lc% * 8) + 26 + x200%, 57 + y200%)-((lc% * 8) + 26 + x200%, 80 + y200%), 0: LINE -((lc% * 8) + 3 + x200%, 80 + y200%), 0
  LINE ((lc% * 8) + 4 + x200%, 58 + y200%)-((lc% * 8) + 25 + x200%, 79 + y200%), 8, BF: LINE ((lc% * 8) + 4 + x200%, 58 + y200%)-((lc% * 8) + 24 + x200%, 78 + y200%), 7, BF
  CIRCLE ((lc% * 8) + 14 + x200%, 68 + y200%), 8, 0: PAINT ((lc% * 8) + 14 + x200%, 68 + y200%), 14, 0
  LINE ((lc% * 8) + 11 + x200%, 65 + y200%)-((lc% * 8) + 12 + x200%, 66 + y200%), 0, B: LINE ((lc% * 8) + 16 + x200%, 65 + y200%)-((lc% * 8) + 17 + x200%, 66 + y200%), 0, B
  LINE ((lc% * 8) + 10 + x200%, 70 + y200%)-((lc% * 8) + 12 + x200%, 72 + y200%), 0: LINE -((lc% * 8) + 16 + x200%, 72 + y200%), 0: LINE -((lc% * 8) + 18 + x200%, 70 + y200%), 0

  LINE (19 + x200%, 56 + y200%)-(59 + x200%, 80 + y200%), 8, B
  LINE (59 + x200%, 57 + y200%)-(59 + x200%, 80 + y200%), 15: LINE -(20 + x200%, 80 + y200%), 15
  LINE (20 + x200%, 57 + y200%)-(58 + x200%, 79 + y200%), 0, BF
  LINE ((lc% - 3) * 16 + 17 + x200%, 56 + y200%)-((lc% - 3) * 16 + 57 + x200%, 80 + y200%), 8, B
  LINE ((lc% - 3) * 16 + 57 + x200%, 57 + y200%)-((lc% - 3) * 16 + 57 + x200%, 80 + y200%), 15: LINE -((lc% - 3) * 16 + 18 + x200%, 80 + y200%), 15
  LINE ((lc% - 3) * 16 + 18 + x200%, 57 + y200%)-((lc% - 3) * 16 + 56 + x200%, 79 + y200%), 0, BF

  PSET (18 + x200%, 7 + y200%), 0: DRAW "d1 l1 d1 l1 d2 r3 l4 d1 r2 l4 u1 d2 l2 r8 l3 d1 l4 g1 r6 d1 r1 d1 g1 h2 l4 d3 r2 u3 l4 d1 l1 d1 l1 "
  PSET (17 + x200%, 17 + y200%), 8: DRAW "h1 bg2 bl2 u1 bl5 e2 r1 e1 l1 h1 e1 g2 u1 l3 u2 r2 e1 h2 e1 r1 f1 r1 e1 u2 e1 f1 d2 f1 r1 e2 g2 d2 g1 u1 h1 g1 bd2 u0 br6 r1"
  PSET (6 + x200%, 17 + y200%), 7: DRAW "e8 br3 bu2 g1 bl3 l1 d1 l1 d1 l1 d1 l1 d1 l1 bu3 h1"
  PSET (6 + x200%, 12 + y200%), 15: DRAW "r1 e5 u2 bl4 bd3 f1 bf2 f0"

  x100 = 15 + x200%: y100 = 96 + y200%
  FOR x000 = 0 + x100 TO (lc% * 16) - 1 + x100 STEP 16
    FOR y000 = 0 + y100 TO (lr% * 16) - 1 + y100 STEP 16
      LINE (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: LINE -(0 + x000, 14 + y000), 15: LINE (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: LINE -(15 + x000, 0 + y000), 0
      LINE (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: LINE -(1 + x000, 14 + y000), 8: LINE (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
    NEXT y000
  NEXT x000

END SUB

SUB seg7 (tom$, zr, x0, y0)

IF zr = 1 THEN e$ = tom$: GOTO num
FOR i = 3 TO LEN(tom$)
  e$ = MID$(tom$, i, 1)
num:
  SELECT CASE e$
    CASE "1"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br3bu1e2f2bl2e2f2 c0br1h2g2h2g2br2e2f2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br3bu21f2e2br2g2h2 c0bl1f2e2br2g2h2g2h2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4bu11br3f1e2f2e1h1g2h2 c0bl2bd1f1e2f2e2f1g1h2g2h2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "2"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br10bu1l8e1r6h1l4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu2u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12bu11br2r8g1l6u2r6": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br11bu10g2f2g2f2bu2h2e2 c0bu1g2f2bd2h2e2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "3"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br10bu1l8e1r6h1l4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12bu11br2r8g1l6u2r6": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "4"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu12u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12bu11br2r8g1l6u2r6": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br3bu1e2f2bl2e2f2 c0br1h2g2h2g2br2e2f2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br3bu21f2e2br2g2h2 c0bl1f2e2br2g2h2g2h2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "5"
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br10bu1l8e1r6h1l4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu12u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12bu11br2r8g1l6u2r6": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br11bu20g2f2g2f2bu2h2e2 c0bu1g2f2bd2h2e2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "6"
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br10bu1l8e1r6h1l4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu2u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu12u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12bu11br2r8g1l6u2r6": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br11bu20g2f2g2f2bu2h2e2 c0bu1g2f2bd2h2e2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "7"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br3bu1e2f2bl2e2f2 c0br1h2g2h2g2br2e2f2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4bu11br3f1e2f2e1h1g2h2 c0bl2bd1f1e2f2e2f1g1h2g2h2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "8"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br10bu1l8e1r6h1l4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu2u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu12u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12bu11br2r8g1l6u2r6": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "9"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br10bu1l8e1r6h1l4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu12u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12bu11br2r8g1l6u2r6": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE "0"
      n$ = "c12br11bu20d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br11bu10d8h1u6g1d4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br10bu1l8e1r6h1l4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu2u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br1bu12u8f1d6e1u4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c12br2bu21r8g1l6f1r4": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      n$ = "c4bu11br3f1e2f2e1h1g2h2 c0bl2bd1f1e2f2e2f1g1h2g2h2": DRAW "bm" + STR$(x0) + "," + STR$(y0) + n$
      x0 = x0 + 13
    CASE ELSE
  END SELECT
  IF zr = 1 THEN GOTO 14
NEXT i
14
END SUB

SUB wind (x0%, y0%, nw, ng, nb, x1%, y1%, k)

 FOR n = 1 TO nw
  LINE (x0% + 2 + n, y1% - 3 - n)-(x0% + 2 + n, y0% + 2 + n), 15: LINE -(x1% - 2 - n, y0% + 2 + n), 15
  LINE (x0% + 2 + n, y1% - 2 - n)-(x1% - 2 - n, y1% - 2 - n), 8: LINE -(x1% - 2 - n, y0% + 2 + n), 8
 NEXT n
 FOR n = 1 TO nb
  LINE (x0% + 2 + nw + ng + n, y1% - 2 - nw - ng - n)-(x0% + 2 + nw + ng + n, y0% + 2 + nw + ng + n), 8: LINE -(x1% - 2 - nw - ng - n, y0% + 2 + nw + ng + n), 8
  LINE (x0% + 3 + nw + ng + n, y1% - 2 - nw - ng - n)-(x1% - 2 - nw - ng - n, y1% - 2 - nw - ng - n), 15: LINE -(x1% - 2 - nw - ng - n, y0% + 3 + nw + ng + n), 15
 NEXT n
 LINE (x0% + 3 + nw + ng + nb, y0% + 3 + nw + ng + nb)-(x1% - 3 - nw - ng - nb, y1% - 3 - nw - ng - nb), k, BF

END SUB

SUB wind1 (x1%, y1%, px, py, title$, B, xsh)
  x2% = x1% + px: y2% = y1% + py
  LINE (x1%, y1%)-(x2%, y2%), 7, BF
  LINE (x2%, y1% + 1)-(x2%, y2%), 0: LINE -(x1% + 1, y2%), 0
  LINE (x2% - 1, y1% + 1)-(x1% + 1, y1% + 1), 15: LINE -(x1% + 1, y2% - 1), 15
  LINE (x1% + 2, y2% - 1)-(x2% - 1, y2% - 1), 8: LINE -(x2% - 1, y1% + 2), 8
  LINE (x1% + 3, y1% + 3)-(x2% - 3, y1% + 20), 1, BF
  x0 = x1% + xsh + 9: y0 = y1% + 15: c = 15
  CALL font(title$, B, c, x0, y0)
  LINE (x2% - 5, y1% + 6)-(x2% - 5, y1% + 18), 0: LINE -(x2% - 19, y1% + 18), 0
  LINE (x2% - 20, y1% + 18)-(x2% - 20, y1% + 5), 15: LINE -(x2% - 5, y1% + 5), 15
  LINE (x2% - 6, y1% + 6)-(x2% - 6, y1% + 17), 8: LINE -(x2% - 19, y1% + 17), 8
  LINE (x2% - 7, y1% + 6)-(x2% - 19, y1% + 16), 7, BF
  LINE (x2% - 9, y1% + 8)-(x2% - 15, y1% + 14), 0: LINE (x2% - 10, y1% + 8)-(x2% - 16, y1% + 14), 0: LINE (x2% - 16, y1% + 8)-(x2% - 10, y1% + 14), 0: LINE (x2% - 15, y1% + 8)-(x2% - 9, y1% + 14), 0

END SUB

