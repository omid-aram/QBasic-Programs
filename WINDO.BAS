DECLARE SUB object (x1!, y1!, px!, py!, sbj$, x0!)
DECLARE SUB door (x1!, y1!, px!, py!, select$())
DECLARE SUB font (title$, b!, c!, x0!, y0!)
DECLARE SUB wind (x1!, y1!, x2!, y2!, shape, title$, b, option$())
COMMON x1, y1, x2, y2
CLS : SCREEN 12
PAINT (0, 0), 3
'wind(x1,y1,x2,y2,title$,b,number of option,option$)
DIM option$(2)
option$(1) = "_Game"
option$(2) = "_Help"
CALL wind(10, 10, 300, 400, 0, "Minesweeper", 1, option$())
'door(x1,y1,px,py,select$())
DIM select$(12)
select$(1) = "_New               F2"
select$(2) = "__"
select$(3) = "_Beginner"
select$(4) = "_Intermediate"
select$(5) = "_Expert"
select$(6) = "_Custom..."
select$(7) = "__"
select$(8) = "_Marks (?)"
select$(9) = "__"
select$(10) = "_Best Times..."
select$(11) = "__"
select$(12) = "_Exit"

CALL door(25, 50, 125, 180, select$())
CALL object(200, 300, 83, 22, "_Reset Scores", 212)

SUB door (x1, y1, px, py, select$())
x2 = x1 + px: y2 = y1 + py
LINE (x1, y1)-(x2, y2), 7, BF
LINE (x2, y1 + 1)-(x2, y2), 0: LINE -(x1 + 1, y2), 0
LINE (x2 - 1, y1 + 1)-(x1 + 1, y1 + 1), 15: LINE -(x1 + 1, y2 - 1), 15
LINE (x1 + 2, y2 - 1)-(x2 - 1, y2 - 1), 8: LINE -(x2 - 1, y1 + 2), 8
y0 = y1 + 15: c = 0
FOR s = 1 TO UBOUND(select$)
x0 = x1 + 25
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

SUB font (title$, b, c, x0, y0)
FOR i = 1 TO LEN(title$)
harf$ = MID$(title$, i, 1)
IF harf$ = "_" THEN ul = 1: GOTO ni
SELECT CASE harf$
 CASE "a"
   b0 = b
a: a$ = "bu5 br1 r2 f1 d4 l3 h1 u1 e1 r2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + a$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO a
   x0 = x0 + 6 + ul: ul = 0
 CASE "b"
   b0 = b
b: b$ = "u8 bd3 r3 f1 d3 g1 l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + b$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO b
   x0 = x0 + 6 + ul: ul = 0
 CASE "c"
   b0 = b
c: c$ = "br4 bu1 g1 l2 h1 u3 e1 r2 f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + c$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO c
   x0 = x0 + 6 + ul: ul = 0
 CASE "d"
   b0 = b
d: d$ = "br1 r3 u8 d3 l3 g1 d3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + d$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO d
   x0 = x0 + 6 + ul: ul = 0
 CASE "e"
   b0 = b
e: e$ = "bu3 r4 u1 h1 l2 g1 d3 f1 r2 e1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + e$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO e
   x0 = x0 + 6 + ul: ul = 0

 CASE "f"
   b0 = b
f: f$ = "u7 e1 bd3 d0": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + f$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO f
   x0 = x0 + 3 + ul: ul = 0
 CASE "g"
   b0 = b
g: g$ = "br3 l2 h1 u3 e1 r3 d6 g1 l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + g$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO g
   x0 = x0 + 6 + ul: ul = 0
 CASE "h"
   b0 = b
h: h$ = "u8 bd4 r1 e1 r1 f1 d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + h$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO h
   x0 = x0 + 6 + ul: ul = 0
 CASE "i"
   b0 = b
i: i$ = "u5 bu3 d0": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + i$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO i
   x0 = x0 + 2 + ul: ul = 0
 CASE "j"
   b0 = b
j: j$ = "d2 u7 bu3 d0": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + j$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO j
   x0 = x0 + 2 + ul: ul = 0
 CASE "k"
   b0 = b
k: k$ = "u8 d5 r1 e2 g2 f3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + k$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO k
   x0 = x0 + 6 + ul: ul = 0
 CASE "l"
   b0 = b
l: l$ = "u8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + l$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO l
   x0 = x0 + 2 + ul: ul = 0
 CASE "m"
   b0 = b
m: m$ = "u5 r2 f1 d4 u4 e1 r1 f1 d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + m$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO m
   x0 = x0 + 8 + ul: ul = 0
 CASE "n"
   b0 = b
n: n$ = "u5 d1 r1 e1 r1 f1 d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO n
   x0 = x0 + 6 + ul: ul = 0
 CASE "o"
   b0 = b
o: o$ = "bu1 u3 e1 r2 f1 d3 g1 l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + o$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO o
   x0 = x0 + 6 + ul: ul = 0
 CASE "p"
   b0 = b
p: p$ = "r3 e1 u3 h1 l3 d7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + p$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO p
   x0 = x0 + 6 + ul: ul = 0
 CASE "q"
   b0 = b
q: q$ = "br3 l2 h1 u3 e1 r3 d7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + q$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO q
   x0 = x0 + 6 + ul: ul = 0
 CASE "r"
   b0 = b
r: r$ = "u5 r1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + r$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO r
   x0 = x0 + 3 + ul: ul = 0
 CASE "s"
   b0 = b
s: s$ = "bu1 f1 r1 e1 h3 e1 r1 f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + s$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO s
   x0 = x0 + 5 + ul: ul = 0
 CASE "t"
   b0 = b
t: t$ = "br1 h1 u6 d2 r1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + t$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO t
   x0 = x0 + 3 + ul: ul = 0
 CASE "u"
   b0 = b
u: u$ = "bu5 d4 f1 r1 e1 r1 d1 u5": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + u$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO u
   x0 = x0 + 6 + ul: ul = 0
 CASE "v"
   b0 = b
v: v$ = "bu5 d1 f1 d1 f1 d1 u1 e1 u1 e1 u1 ": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + v$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO v
   x0 = x0 + 6 + ul: ul = 0
 CASE "w"
   b0 = b
w: w$ = "bu5 d3 f1 d1 u1 e1 u1 e1 u1 d1 f1 d1 f1 d1 u1 e1 u3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + w$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO w
   x0 = x0 + 8 + ul: ul = 0
 CASE "x"
   b0 = b
x: x$ = "u1 e1 r1 f1 d1 bu5 d1 g1 l1 h1 u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + x$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO x
   x0 = x0 + 5 + ul: ul = 0
 CASE "y"
   b0 = b
y: y$ = "bu5 d3 f1 r1 e1 u3 bd4 bl1 d1 g1 l1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + y$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO y
   x0 = x0 + 5 + ul: ul = 0
 CASE "z"
   b0 = b
z: z$ = "bu5 r3 d1 g3 d1 r3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + z$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO z
   x0 = x0 + 5 + ul: ul = 0
 CASE "A"
   b0 = b
aa: a$ = "u1 e1 u2 e1 u1 e1 u1 d1 f1 d1 f1 d2 l3 r3 f1 d1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + a$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO aa
   x0 = x0 + 8 + ul: ul = 0
 CASE "B"
   b0 = b
bb: b$ = "u8 r3 f1 d2 g1 l2 r2 f1d2g1l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + b$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO bb
   x0 = x0 + 6 + ul: ul = 0
 CASE "C"
   b0 = b
cc: c$ = "br5bu1g1l3h1u6e1r3f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + c$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO cc
   x0 = x0 + 7 + ul: ul = 0
 CASE "D"
   b0 = b
dd: d$ = "u8r3f2d4g2l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + d$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO dd
   x0 = x0 + 7 + ul: ul = 0
 CASE "E"
   b0 = b
ee: e$ = "r4l4u8r4bd4bl1l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + e$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ee
   x0 = x0 + 6 + ul: ul = 0
 CASE "F"
   b0 = b
ff: f$ = "u8r4bd4bl1l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + f$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ff
   x0 = x0 + 6 + ul: ul = 0
 CASE "G"
   b0 = b
gg: g$ = "bu1u6e1r3f1bd3l2r2d4h1g1l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + g$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO gg
   x0 = x0 + 7 + ul: ul = 0
 CASE "H"
   b0 = b
hh: h$ = "u8d4r5u4d8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + h$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO hh
   x0 = x0 + 7 + ul: ul = 0
 CASE "I"
   b0 = b
ii: i$ = "u8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + i$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r2"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ii
   x0 = x0 + 2 + ul: ul = 0
 CASE "J"
   b0 = b
jj: j$ = "bu2d1f1r1e1u7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + j$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO jj
   x0 = x0 + 5 + ul: ul = 0
 CASE "K"
   b0 = b
kk: k$ = "u8d3r1e3g3d1f4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + k$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO kk
   x0 = x0 + 7 + ul: ul = 0
 CASE "L"
   b0 = b
ll: l$ = "u8d8r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + l$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ll
   x0 = x0 + 6 + ul: ul = 0
 CASE "M"
   b0 = b
mm: m$ = "u8d2r1d1f1d1f1d1u1e1u1e1u1r1u2d8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + m$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO mm
   x0 = x0 + 8 + ul: ul = 0
 CASE "N"
   b0 = b
nn: n$ = "u8f1d1f1d1f2d1f1u8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO nn
   x0 = x0 + 7 + ul: ul = 0
 CASE "O"
   b0 = b
oo: o$ = "bu1u6e1r3f1d6g1l3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + o$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO oo
   x0 = x0 + 7 + ul: ul = 0
 CASE "P"
   b0 = b
pp: p$ = "u8r4f1d2g1l4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + p$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO pp
   x0 = x0 + 7 + ul: ul = 0
 CASE "Q"
   b0 = b
qq: q$ = "bu1u6e1r3f1d6g1l3br2bu2f1d1f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + q$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO qq
   x0 = x0 + 7 + ul: ul = 0
 CASE "R"
   b0 = b
rr: r$ = "u8r4f1d2g1l4r4f1d3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + r$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO rr
   x0 = x0 + 7 + ul: ul = 0
 CASE "S"
   b0 = b
ss: s$ = "bu1f1r2e1u2h1l2h1u2e1r2f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + s$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ss
   x0 = x0 + 6 + ul: ul = 0
 CASE "T"
   b0 = b
tt: t$ = "br2u8l2r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + t$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO tt
   x0 = x0 + 6 + ul: ul = 0
 CASE "U"
   b0 = b
uu: u$ = "bu8d7f1r3e1u7": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + u$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r7"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO uu
   x0 = x0 + 7 + ul: ul = 0
 CASE "V"
   b0 = b
vv: v$ = "bu8d1f1d2f1d1f1d1u1e1u1e1u2e1u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + v$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO vv
   x0 = x0 + 8 + ul: ul = 0
 CASE "W"
   b0 = b
ww: w$ = "bu8d1f1d2f1d1f1d1u1e1u1e1u2d2f1d1f1d1u1e1u1e1u2e1u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + w$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r12"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ww
   x0 = x0 + 12 + ul: ul = 0
 CASE "X"
   b0 = b
xx: x$ = "u1e6u1bl6d1f6d1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + x$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO xx
   x0 = x0 + 8 + ul: ul = 0
 CASE "Y"
   b0 = b
yy: y$ = "bu8d1f3d4u4e3u1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + y$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO yy
   x0 = x0 + 8 + ul: ul = 0
 CASE "Z"
   b0 = b
zz: z$ = "bu8r6d1g6d1r6": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + z$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r8"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO zz
   x0 = x0 + 8 + ul: ul = 0
 CASE " "
  x0 = x0 + 3
 CASE "1"
  b0 = b
one: n$ = "bu7r2u1d8": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r4"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO one
   x0 = x0 + 4 + ul: ul = 0
 CASE "2"
  b0 = b
two: n$ = "bu7e1r2f1d2g4d1r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO two
   x0 = x0 + 6 + ul: ul = 0
 CASE "3"
 b0 = b
three: n$ = "bu7e1r2f1d2g1l1r1f1d2g1l2h1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO three
   x0 = x0 + 6 + ul: ul = 0
 CASE "4"
  b0 = b
four: n$ = "br3u8g1d1g1d1g1d1r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO four
   x0 = x0 + 6 + ul: ul = 0
 CASE "5"
 b0 = b
five: n$ = "bu1f1r2e1u3h1l3d1u4r4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO five
 x0 = x0 + 6 + ul: ul = 0
 CASE "6"
 b0 = b
six: n$ = "bu4r3f1d2g1l2h1u6e1r2f1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO six
 x0 = x0 + 6 + ul: ul = 0
 CASE "7"
 b0 = b
seven: n$ = "bu8r4d1g1d1g1d1g1d2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO seven
 x0 = x0 + 6 + ul: ul = 0
 CASE "8"
 b0 = b
eight: n$ = "bu1u2e1r2f1d2g1l2bl1bu5u2e1r2f1d2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO eight
 x0 = x0 + 6 + ul: ul = 0
 CASE "9"
 b0 = b
nine: n$ = "bu1f1r2e1u6h1l2g1d2f1r2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO nine
 x0 = x0 + 6 + ul: ul = 0
 CASE "0"
 b0 = b
zero: n$ = "bu1u6e1r2f1d6g1l2": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO zero
  x0 = x0 + 6 + ul: ul = 0
 CASE "-"
 b0 = b
lin: n$ = "bu3r1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO lin
 x0 = x0 + 3
 CASE "."
 b0 = b
dot: DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + "u0"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO dot
 x0 = x0 + 2
 CASE "?"
 b0 = b
qsmk: n$ = "br2u0bu2u1e2u2h1l2g1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO qsmk
   x0 = x0 + 6 + ul: ul = 0
 CASE "("
 b0 = b
paro: n$ = "bf1h1u8e1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO paro
   x0 = x0 + 3 + ul: ul = 0
 CASE ")"
  b0 = b
parc: n$ = "bd1e1u8h1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r3"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO parc
   x0 = x0 + 3 + ul: ul = 0
 CASE "_"
  b0 = b
ulin: n$ = "bd1r5": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO ulin
   x0 = x0 + 7
 CASE "+"
  b0 = b
plus: n$ = "bu3r4l2u2d4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO plus
   x0 = x0 + 6 + ul: ul = 0
 CASE "="
  b0 = b
equl: n$ = "bu2r4bu2l4": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r6"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO equl
   x0 = x0 + 6 + ul: ul = 0
 CASE "<"
  b0 = b
bigm: n$ = "br3h3e3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO bigm
   x0 = x0 + 5 + ul: ul = 0
 CASE ">"
  b0 = b
smlm: n$ = "e3h3": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r5"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO smlm
   x0 = x0 + 5 + ul: ul = 0
 CASE "{"
  b0 = b
aklo: n$ = "br2bd1h1u3h1e1u3e1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r4"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO aklo
   x0 = x0 + 4 + ul: ul = 0
 CASE "}"
  b0 = b
aklc: n$ = "bd1e1u3e1h1u3h1": DRAW "c" + STR$(c) + "bm" + STR$(x0) + "," + STR$(y0) + n$
   IF ul = 1 THEN DRAW "c" + STR$(c) + "bm" + STR$(x0 - 1) + "," + STR$(y0 + 2) + "r4"
   IF b0 = 1 THEN x0 = x0 + 1: b0 = 0: GOTO aklc
   x0 = x0 + 4 + ul: ul = 0
END SELECT
ni: NEXT i

END SUB

SUB object (x1, y1, px, py, sbj$, x0)
x2 = x1 + px: y2 = y1 + py
LINE (x1, y2)-(x1, y1), 15: LINE -(x2, y1), 15
LINE (x2, y1 + 1)-(x2, y2), 0: LINE -(x1 + 1, y2), 0
LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), 7, BF
LINE (x1 + 2, y2 - 1)-(x2 - 1, y2 - 1), 8: LINE -(x2 - 1, y1 + 3), 8
CALL font(sbj$, 0, 0, x0, INT((py - 9) / 2) + 8 + y1)

END SUB

SUB wind (x1, y1, x2, y2, shape, title$, b, option$())
LINE (x1, y1)-(x2, y2), 7, BF
LINE (x2, y1 + 1)-(x2, y2), 0: LINE -(x1 + 1, y2), 0
LINE (x2 - 1, y1 + 1)-(x1 + 1, y1 + 1), 15: LINE -(x1 + 1, y2 - 1), 15
LINE (x1 + 2, y2 - 1)-(x2 - 1, y2 - 1), 8: LINE -(x2 - 1, y1 + 2), 8
LINE (x1 + 4, y1 + 4)-(x2 - 4, y1 + 21), 1, BF
IF shape = 1 THEN
xsh = 0
END IF
x0 = x1 + xsh + 10: y0 = y1 + 16: c = 15
CALL font(title$, b, c, x0, y0)
LINE (x2 - 6, y1 + 7)-(x2 - 6, y1 + 19), 0: LINE -(x2 - 20, y1 + 19), 0
LINE (x2 - 21, y1 + 19)-(x2 - 21, y1 + 6), 15: LINE -(x2 - 6, y1 + 6), 15
LINE (x2 - 7, y1 + 7)-(x2 - 7, y1 + 18), 8: LINE -(x2 - 20, y1 + 18), 8
LINE (x2 - 8, y1 + 7)-(x2 - 20, y1 + 17), 7, BF
LINE (x2 - 10, y1 + 9)-(x2 - 16, y1 + 15), 0: LINE (x2 - 11, y1 + 9)-(x2 - 17, y1 + 15), 0: LINE (x2 - 17, y1 + 9)-(x2 - 11, y1 + 15), 0: LINE (x2 - 16, y1 + 9)-(x2 - 10, y1 + 15), 0
x0 = x1 + 10: y0 = y1 + 34: c = 0
FOR o = 1 TO UBOUND(option$)
CALL font(option$(o), 0, c, x0, y0)
x0 = x0 + 12
NEXT o
END SUB

