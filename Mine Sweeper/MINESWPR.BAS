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
3 Common Shared lr%, lc%, x200%, y200%
5 Clear , , 23000
8 lc% = 8: lr% = 8: mark = 1: mark0 = 0: nn = 10: min0 = min: nm = 10: min = nm
10 Cls: Screen 0: Timer Off
11 On Timer(1) GoSub delay
15 Color 30: Locate 25, 3: Print "Please wait..................."
20 Randomize Timer
30 Dim room(lr%, lc%)
35 Dim bomb(lr%, lc%)
40 Dim r(nm * 2)
50 Dim c(nm * 2)
52 Dim flag(lr%, lc%)
53 Dim gsave(4540)
54 Dim besv(5922)
55 Dim resv(2900)
56 Dim select$(12)
57 Dim yslct(12)
58 Dim cusv(2448)
60 fl = 0: s = 0: s1 = 0: n = 0: min = nm
70 Do
    80 n = n + 1
    90 r(n) = Int(Rnd * lr%) + 1: c(n) = Int(Rnd * lc%) + 1
    110 If room(r(n), c(n)) = 1 Then n = n - 1: GoTo 140
    130 room(r(n), c(n)) = 1
140 Loop Until n >= nm
160 Call bombs(room(), bomb())
150 Call scene(x100, y100)
155 Call mint(min)
158 tom = 0: Call miti(tom)
161 f$ = Input$(1)
165 tom = 1: Call miti(tom)
170 time = Timer - 2: x00 = x100: y00 = y100: r = 1: c = 1: Color 12: nm = min
175 Timer On
180 Do
    182 min0 = min
    184 r = Int(((y00 - y100) / 16) + 1): c = Int(((x00 - x100) / 16) + 1)
    186 x000 = (c - 1) * 16 + x100: y000 = (r - 1) * 16 + y100
    188 Line (x000, y000)-(x000 + 15, y000 + 15), 12, B
    190 f$ = ""
    200 f$ = InKey$
    210 If Len(f$) = 2 Then g$ = Right$(f$, 1) Else g$ = f$
    220 Select Case g$
        Case "M"
            234 If s = 0 Then
                236 GoSub 1500
                238 If x00 = ((lc% - 1) * 16) + x100 Then x00 = x100 - 16
                240 x00 = x00 + 16
            242 End If
        244 Case "K"
            246 If s = 0 Then
                248 GoSub 1500
                250 If x00 = x100 Then x00 = lc% * 16 + x100
                252 x00 = x00 - 16
            254 End If
        256 Case "H"
            258 If s = 0 Then
                260 GoSub 1500
                262 If y00 = y100 Then y00 = lr% * 16 + y100
                264 y00 = y00 - 16
            268 End If
        270 Case "P"
            272 If s = 0 Then
                274 GoSub 1500
                276 If y00 = (lr% - 1) * 16 + y100 Then y00 = y100 - 16
                278 y00 = y00 + 16
            280 End If
        282 Case Chr$(13)
            284 If s = 0 Then
                286 If r > 0 And r < lr% + 1 Then
                    288 If c > 0 And c < lc% + 1 Then
                        290 If flag(r, c) <> 1 And flag(r, c) <> 3 Then If bomb(r, c) = 9 Then s = 1: Timer Off: GoSub 1100
                        292 If flag(r, c) <> 1 And flag(r, c) <> 3 Then If bomb(r, c) = 0 Then Line (0 + x00, 0 + y00)-(15 + x00, 15 + y00), 7, BF: Line (0 + x00, 15 + y00)-(15 + x00, 15 + y00), 0, , &H5555: Line (15 + x00, 15 + y00)-(15 + x00, 0 + y00), 0, , &H5555: Call pooch(bomb(), r, c, fl, flag(), x100, y100)
                        294 If flag(r, c) <> 1 And flag(r, c) <> 3 Then If bomb(r, c) <> 0 Then flag(r, c) = 3: fl = fl + 1: m = bomb(r, c): Call numbers(x000, y000, m)
                    296 End If
                298 End If
            300 End If
        310 Case Chr$(32)
            320 If s = 0 Then
                322 If r > 0 And r < lr% + 1 Then
                    324 If c > 0 And c < lc% + 1 Then
                        326 If min > 0 Then If flag(r, c) = 0 Then flag(r, c) = 1: min = min - 1: GoSub 1200
                        328 If mark = 1 Then
                            330 If flag(r, c) = 1 Then flag(r, c) = 2: min = min + 1: GoSub 1300
                        332 Else If flag(r, c) = 1 Then flag(r, c) = 2: min = min + 1
                        334 End If
                        336 If flag(r, c) = 2 Then flag(r, c) = 0: GoSub 1400
                    338 End If
                340 End If
            342 End If
        344 Case Chr$(60)
            346 GoSub new
        348 Case Chr$(24)
            350 GoSub endp
        352 Case Chr$(9)
            354 Timer Stop
            356 time1 = Timer
            358 If s1 = 0 Then GoSub 1500: Line ((lc% * 8) + 2 + x200%, 56 + y200%)-((lc% * 8) + 27 + x200%, 81 + y200%), 12, B: s1 = 1
            360 Do While s1 <> 0
                362 f$ = Input$(1)
                364 If s1 = 1 Then If f$ = Chr$(13) Then GoSub new
                366 If s1 = 2 Then If f$ = Chr$(13) Then Line (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 8, B: Line (3 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 15: Line -(42 + x200%, 22 + y200%), 15: Get (3 + x200%, 40 + y200%)-(130 + x200%, 218 + y200%), gsave(): GoSub games: Line (42 + x200%, 22 + y200%)-(3 + x200%, 22 + y200%), 15, B: Line -(3 + x200%, 39 + y200%), 15: Line (4 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 8: Line -(42 + x200%, 23 + y200%), 8
                368 If s1 = 3 Then If f$ = Chr$(13) Then GoSub endp
                370 If f$ = Chr$(9) Then
                    372 If s1 = 1 Then s1 = 2: Line ((lc% * 8) + 2 + x200%, 56 + y200%)-((lc% * 8) + 27 + x200%, 81 + y200%), 8, B: Line (42 + x200%, 22 + y200%)-(3 + x200%, 22 + y200%), 15, B: Line -(3 + x200%, 39 + y200%), 15: Line (4 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 8: Line -(42 + x200%, 23 + y200%), 8: GoTo 112
                    374 If s1 = 2 Then s1 = 3: Line (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 7, B: Line ((lc% * 16) + 9 + x200%, 5 + y200%)-((lc% * 16) + 24 + x200%, 18 + y200%), 12, B: GoTo 112
                    376 If s1 = 3 Then s1 = 0: Line ((lc% * 16) + 9 + x200%, 5 + y200%)-((lc% * 16) + 24 + x200%, 18 + y200%), 15, B: Line ((lc% * 16) + 24 + x200%, 5 + y200%)-((lc% * 16) + 24 + x200%, 18 + y200%), 0: Line -((lc% * 16) + 9 + x200%, 18 + y200%), 0
                378 End If
            112 Loop
            380 time = time + (Timer - time1): Timer On
        382 Case Chr$(34)
            384 Timer Stop
            386 time1 = Timer
            388 Line (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 8, B: Line (3 + x200%, 39 + y200%)-(42 + x200%, 39 + y200%), 15: Line -(42 + x200%, 22 + y200%), 15: Get (3 + x200%, 40 + y200%)-(130 + x200%, 218 + y200%), gsave(): GoSub games: Line (3 + x200%, 22 + y200%)-(42 + x200%, 39 + y200%), 7, B
            390 time = time + (Timer - time1): Timer On
        400 Case Else
    410 End Select
    415 If s = 0 Then If tom > 999 Then s = 1: Timer Off: GoSub 1100
    420 If min0 <> min Then Call mint(min)
    425 If s = 0 Then If fl = (lr% * lc%) - nm Then GoSub 1110
430 Loop
440 End

1100
Line ((lc% * 8) + 10 + x200%, 64 + y200%)-((lc% * 8) + 12 + x200%, 66 + y200%), 0, BF: Circle ((lc% * 8) + 11 + x200%, 65 + y200%), 1, 14: Line ((lc% * 8) + 16 + x200%, 64 + y200%)-((lc% * 8) + 18 + x200%, 66 + y200%), 0, BF: Circle ((lc% * 8) + 17 + x200%, 65 + y200%), 1, 14: Line ((lc% * 8) + 10 + x200%, 70 + y200%)-((lc% * 8) + 18 + x200%, 72 + y200%), 14, BF: Line ((lc% * 8) + 10 + x200%, 72 + y200%)-((lc% * 8) + 12 + x200%, 70 + y200%), 0: Line -((lc% * 8) + 16 + x200%, 70 + y200%), 0: Line -((lc% * 8) + 18 + x200%, 72 + y200%), 0
For x = 1 To lr%
    For y = 1 To lc%
        If room(x, y) = 1 And flag(x, y) <> 1 Then y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100: GoSub bomp: flag(x, y) = 3
        If room(x, y) <> 1 And flag(x, y) = 1 Then y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100: GoSub cross: flag(x, y) = 3
    Next y
Next x
Return

1110
Line ((lc% * 8) + 6 + x200%, 69 + y200%)-((lc% * 8) + 10 + x200%, 65 + y200%), 0: Line -((lc% * 8) + 13 + x200%, 67 + y200%), 0, BF: PSet ((lc% * 8) + 14 + x200%, 65 + y200%), 0: Line ((lc% * 8) + 22 + x200%, 69 + y200%)-((lc% * 8) + 18 + x200%, 65 + y200%), 0: Line -((lc% * 8) + 15 + x200%, 67 + y200%), 0, BF: Line ((lc% * 8) + 10 + x200%, 68 + y200%)-((lc% * 8) + 18 + x200%, 68 + y200%), 0, , &H6300
For x = 1 To lr%
    For y = 1 To lc%
        If room(x, y) = 1 And flag(x, y) <> 1 Then
            y000 = (x - 1) * 16 + y100: x000 = (y - 1) * 16 + x100
            Line (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: Line -(0 + x000, 14 + y000), 15: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: Line -(15 + x000, 0 + y000), 0: Line (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: Line -(1 + x000, 14 + y000), 8: Line (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
            Line (4 + x000, 3 + y000)-(8 + x000, 6 + y000), 4, BF: Line (4 + x000, 6 + y000)-(7 + x000, 3 + y000), 12: Line (4 + x000, 5 + y000)-(6 + x000, 3 + y000), 12: Line (4 + x000, 3 + y000)-(5 + x000, 4 + y000), 12, B: Line (8 + x000, 7 + y000)-(8 + x000, 10 + y000), 0: Line (7 + x000, 11 + y000)-(9 + x000, 11 + y000), 0: Line (5 + x000, 12 + y000)-(11 + x000, 12 + y000), 0
        End If
    Next y
Next x
min = 0: Call mint(min)
s = 1: Timer Off
If nn = 10 Or nn = 15 Or nn = 20 Then
    Open "dosmine0.db" For Random As #1 Len = 17
    Field #1, 15 As nam$, 2 As t$
    Get #1, nn
    If tim < CVI(t$) Then
        Call save(n$, resv(), nn)
        LSet nam$ = n$
        LSet t$ = MKI$(tim)
        Put #1, nn
        q = 1
        Call best(besv(), gsave(), q)
    End If
    Close #1
End If
Return

1200
Line (4 + x000, 3 + y000)-(8 + x000, 6 + y000), 4, BF: Line (4 + x000, 6 + y000)-(7 + x000, 3 + y000), 12: Line (4 + x000, 5 + y000)-(6 + x000, 3 + y000), 12
Line (4 + x000, 3 + y000)-(5 + x000, 4 + y000), 12, B: Line (8 + x000, 7 + y000)-(8 + x000, 10 + y000), 0: Line (7 + x000, 11 + y000)-(9 + x000, 11 + y000), 0: Line (5 + x000, 12 + y000)-(11 + x000, 12 + y000), 0
GoTo 420: Return

1300
Line (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: Line (x000, y000)-(0 + x000, 14 + y000), 15: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0
Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0: Line (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: Line (14 + x000, 14 + y000)-(1 + x000, 14 + y000), 8: Line (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
Line (4 + x000, 4 + y000)-(5 + x000, 5 + y000), 1, B: Line (5 + x000, 3 + y000)-(8 + x000, 4 + y000), 1, B: Line (8 + x000, 4 + y000)-(9 + x000, 7 + y000), 1, B
Line (10 + x000, 5 + y000)-(10 + x000, 6 + y000), 1: Line (8 + x000, 7 + y000)-(6 + x000, 9 + y000), 1, BF: PSet (6 + x000, 7 + y000), 7: Line (6 + x000, 11 + y000)-(8 + x000, 12 + y000), 1, B
GoTo 420: Return

1400
Line (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: Line (x000, y000)-(0 + x000, 14 + y000), 15: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0
Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0: Line (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: Line (14 + x000, 14 + y000)-(1 + x000, 14 + y000), 8: Line (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
GoTo 420: Return

1500
Line (x000, y000)-(x000 + 15, y000 + 15), 7, B
If flag(r, c) = 0 Or flag(r, c) = 1 Or flag(r, c) = 2 Then
    Line (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: Line (x000, y000)-(0 + x000, 14 + y000), 15: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0
End If
If flag(r, c) = 3 Then
    Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 7: Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 7: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
End If
Return

bomp:
Line (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
Line (5 + x000, 5 + y000)-(9 + x000, 9 + y000), 0, BF: PSet (6 + x000, 6 + y000), 15
Line (4 + x000, 4 + y000)-(10 + x000, 10 + y000), 8, B , &H5555
Line (4 + x000, 7 + y000)-(2 + x000, 7 + y000), 0: Line (7 + x000, 10 + y000)-(7 + x000, 12 + y000), 0: Line (10 + x000, 7 + y000)-(12 + x000, 7 + y000), 0: Line (7 + x000, 4 + y000)-(7 + x000, 2 + y000), 0
Return

cross:
Line (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
Line (3 + x000, 3 + y000)-(12 + x000, 12 + y000), 12: Line (4 + x000, 3 + y000)-(12 + x000, 11 + y000), 12: Line (3 + x000, 4 + y000)-(11 + x000, 12 + y000), 12
Line (3 + x000, 11 + y000)-(11 + x000, 3 + y000), 12: Line (3 + x000, 12 + y000)-(12 + x000, 3 + y000), 12: Line (4 + x000, 12 + y000)-(12 + x000, 4 + y000), 12
Return

games:
Call game(mark, nn, select$(), yslct(), tick):
po = 1: ro = 5: co = 4: cb = 9: cf = 14: GoSub 13
Do
    f$ = InKey$
    If Len(f$) = 2 Then g$ = Right$(f$, 1) Else g$ = f$
    Select Case g$
        Case Chr$(72)
            cb = 7: cf = 0: GoSub 13: cb = 9: cf = 14
            If po > 3 And po < 7 Then
                ro = ro - 1: po = po - 1
            Else
                ro = ro - 2: po = po - 2
            End If
            If ro < 5 Then ro = 16: po = 12
            GoSub 13
        Case Chr$(80)
            cb = 7: cf = 0: GoSub 13: cb = 9: cf = 14
            If po > 2 And po < 6 Then
                ro = ro + 1: po = po + 1
            Else
                ro = ro + 2: po = po + 2
            End If
            If ro > 16 Then ro = 5: po = 1
            GoSub 13
        Case Chr$(13)
            Select Case po
                Case 1: GoSub new
                Case 3: nm = 10: lr% = 8: lc% = 8: nn = 10: GoSub new
                Case 4: nm = 40: lr% = 16: lc% = 16: nn = 15: GoSub new
                Case 5: nm = 99: lr% = 16: lc% = 30: nn = 20: GoSub new
                Case 6: Call mines(nm, nn, lr%, lc%, gsave(), cusv(), cncl): If cncl = 1 Then GoTo 12 Else GoSub new
                Case 8: Swap mark, mark0: GoTo pak
                Case 10: Call best(besv(), gsave(), q): GoTo 12
                Case 12: GoSub endp
                Case Else: GoTo pak
            End Select
        Case Chr$(27)
            pak: Put (3 + x200%, 40 + y200%), gsave(), PSet: GoTo 12
        Case Else
    End Select
Loop
12 Return

13
Line (6 + x200%, yslct(po))-(127 + x200%, yslct(po) - 16), cb, BF
x0 = 28 + x200%: y0 = yslct(po) - 5
Call font(select$(po), 0, cf, x0, y0)
If po = 8 Then If mark = 1 Then x = 12 + x200%: y = yslct(8) - 7: Line (x, y)-(x + 2, y + 2), cf: Line -(x + 6, y - 2), cf: Line (x, y - 1)-(x + 2, y + 1), cf: Line -(x + 6, y - 3), cf: Line (x, y - 2)-(x + 2, y), cf: Line -(x + 6, y - 4), cf
If po = tick Then x = 12 + x200%: y = yslct(tick) - 7: Line (x, y)-(x + 2, y + 2), cf: Line -(x + 6, y - 2), cf: Line (x, y - 1)-(x + 2, y + 1), cf: Line -(x + 6, y - 3), cf: Line (x, y - 2)-(x + 2, y), cf: Line -(x + 6, y - 4), cf
Return

new:
If nm = 0 Then nm = 10
Erase flag, room, bomb, r, c
GoTo 10
Return

delay:
tom = Int(Timer - time): tim = tom
If s = 0 Then Call miti(tom)
Return

endp:
Cls
s = 0
Call wind1(200, 190, 240, 100, "Shut Down Minesweeper", 1, 0)
Call font("Thank you for playing this game", 0, 0, 242, 230)
Call font("Omid Khoshniat Aram", 1, 0, 260, 250)
Call font("Press any key to exit", 0, 0, 270, 280)
Color 0
End
Return

Sub best (besv(), gsave(), q)
    2 If q <> 1 Then Put (3 + x200%, 40 + y200%), gsave(), PSet
    q = 0
    Get (155, 79 + y200%)-(484, 219 + y200%), besv()
    Call wind1(155, 79 + y200%, 329, 140, "Best Times", 1, 0)
    Line (169, 110 + y200%)-(470, 204 + y200%), 8, B
    Line (170, 111 + y200%)-(471, 205 + y200%), 15, B
    Line (176, 110 + y200%)-(289, 111 + y200%), 7, B
    Close
    Call font("Fastest Mine Sweepers", 0, 0, 179, 114 + y200%)
    Open "dosmine0.db" For Random As #1 Len = 17
    Field #1, 15 As nam$, 2 As t$
    Get #1, 10
    Call font("Beginner:                 " + Str$(CVI(t$)) + " seconds            " + nam$, 0, 0, 180, 137 + y200%)
    Get #1, 15
    Call font("Intermediate:            " + Str$(CVI(t$)) + " seconds            " + nam$, 0, 0, 179, 163 + y200%)
    Get #1, 20
    Call font("Expert:                     " + Str$(CVI(t$)) + " seconds            " + nam$, 0, 0, 180, 188 + y200%)
    Close #1
    Do While ak$ <> Chr$(27): ak$ = InKey$:
        If ak$ = Chr$(79) Then ak$ = Input$(1): If ak$ = Chr$(109) Then ak$ = Input$(1): If ak$ = Chr$(73) Then ak$ = Input$(1): If ak$ = Chr$(100) Then GoSub us
    Loop
    Put (155, 79 + y200%), besv(), PSet
    Exit Sub

    us:
    Open "dosmine0.db" For Random As #1 Len = 17
    Field #1, 15 As nam$, 2 As t$
    LSet nam$ = "Anonymous"
    LSet t$ = MKI$(999)
    Put #1, 10
    Put #1, 15
    Put #1, 20
    Close #1
    Put (155, 79 + y200%), besv(), PSet
    GoTo 2
    Return

End Sub

Sub bombs (room(), bomb())
    For x = 1 To lr%
        x1 = x
        For y = 1 To lc%
            y1 = y
            If room(x, y) <> 1 Then
                x0 = x1: y0 = y1: x0 = x0 - 1: y0 = y0 - 1: If x0 > 0 And x0 < lr% + 1 Then If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
                x0 = x1: y0 = y1: y0 = y0 - 1: If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
                x0 = x1: y0 = y1: x0 = x0 + 1: y0 = y0 - 1: If x0 > 0 And x0 < lr% + 1 Then If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
                x0 = x1: y0 = y1: x0 = x0 + 1: If x0 > 0 And x0 < lr% + 1 Then If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
                x0 = x1: y0 = y1: x0 = x0 + 1: y0 = y0 + 1: If x0 > 0 And x0 < lr% + 1 Then If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
                x0 = x1: y0 = y1: y0 = y0 + 1: If x0 > 0 And x0 < lr% + 1 Then If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
                x0 = x1: y0 = y1: x0 = x0 - 1: y0 = y0 + 1: If x0 > 0 And x0 < lr% + 1 Then If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
                x0 = x1: y0 = y1: x0 = x0 - 1: If x0 > 0 And x0 < lr% + 1 Then If y0 > 0 And y0 < lc% + 1 Then If room(x0, y0) = 1 Then n = n + 1
            Else
                n = 9
            End If
            bomb(x, y) = n: n = 0
        Next y
    Next x

End Sub

Sub door (x1, y1, px, py, select$(), yslct())
    x2 = x1 + px: y2 = y1 + py
    Line (x1, y1)-(x2, y2), 7, BF
    Line (x2, y1 + 1)-(x2, y2), 0: Line -(x1 + 1, y2), 0
    Line (x2 - 1, y1 + 1)-(x1 + 1, y1 + 1), 15: Line -(x1 + 1, y2 - 1), 15
    Line (x1 + 2, y2 - 1)-(x2 - 1, y2 - 1), 8: Line -(x2 - 1, y1 + 2), 8
    y0 = y1 + 15: c = 0
    For s = 1 To UBound(select$)
        x0 = x1 + 25
        yslct(s) = y0 + 5
        If select$(s) = "__" Then
            y0 = y0 + 9
            Line (x1 + 4, y0)-(x2 - 4, y0), 8
            Line (x1 + 4, y0 + 1)-(x2 - 4, y0 + 1), 15
            y0 = y0 + 17
            GoTo ns
        End If
        Call font(select$(s), 0, c, x0, y0)
        If s < UBound(select$) Then If select$(s + 1) <> "__" Then y0 = y0 + 17
    ns: Next s

End Sub

Sub font (title$, B, c, x0, y0)
    For i = 1 To Len(title$)
        harf$ = Mid$(title$, i, 1)
        If harf$ = "_" Then ul = 1: GoTo ni
        Select Case harf$
            Case "a"
                b0 = B
                a: a$ = "bu5 br1 r2 f1 d4 l3 h1 u1 e1 r2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + a$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo a
                x0 = x0 + 6 + ul: ul = 0
            Case "b"
                b0 = B
                B: B$ = "u8 bd3 r3 f1 d3 g1 l3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + B$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo B
                x0 = x0 + 6 + ul: ul = 0
            Case "c"
                b0 = B
                c: c$ = "br4 bu1 g1 l2 h1 u3 e1 r2 f1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + c$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo c
                x0 = x0 + 6 + ul: ul = 0
            Case "d"
                b0 = B
                d: d$ = "br1 r3 u8 d3 l3 g1 d3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + d$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo d
                x0 = x0 + 6 + ul: ul = 0
            Case "e"
                b0 = B
                e: e$ = "bu3 r4 u1 h1 l2 g1 d3 f1 r2 e1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + e$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo e
                x0 = x0 + 6 + ul: ul = 0
            Case "f"
                b0 = B
                f: f$ = "u7 e1 bd3 d0": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + f$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r3"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo f
                x0 = x0 + 3 + ul: ul = 0
            Case "g"
                b0 = B
                g: g$ = "br3 l2 h1 u3 e1 r3 d6 g1 l3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + g$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo g
                x0 = x0 + 6 + ul: ul = 0
            Case "h"
                b0 = B
                h: h$ = "u8 bd4 r1 e1 r1 f1 d4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + h$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo h
                x0 = x0 + 6 + ul: ul = 0
            Case "i"
                b0 = B
                i: i$ = "u5 bu3 d0": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + i$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r2"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo i
                x0 = x0 + 2 + ul: ul = 0
            Case "j"
                b0 = B
                j: j$ = "d2 u7 bu3 d0": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + j$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r2"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo j
                x0 = x0 + 2 + ul: ul = 0
            Case "k"
                b0 = B
                k: k$ = "u8 d5 r1 e2 g2 f3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + k$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo k
                x0 = x0 + 6 + ul: ul = 0
            Case "l"
                b0 = B
                L: L$ = "u8": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + L$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r2"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo L
                x0 = x0 + 2 + ul: ul = 0
            Case "m"
                b0 = B
                m: m$ = "u5 r2 f1 d4 u4 e1 r1 f1 d4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + m$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo m
                x0 = x0 + 8 + ul: ul = 0
            Case "n"
                b0 = B
                n: n$ = "u5 d1 r1 e1 r1 f1 d4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo n
                x0 = x0 + 6 + ul: ul = 0
            Case "o"
                b0 = B
                o: o$ = "bu1 u3 e1 r2 f1 d3 g1 l2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + o$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo o
                x0 = x0 + 6 + ul: ul = 0
            Case "p"
                b0 = B
                p: p$ = "r3 e1 u3 h1 l3 d7": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + p$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo p
                x0 = x0 + 6 + ul: ul = 0
            Case "q"
                b0 = B
                q: q$ = "br3 l2 h1 u3 e1 r3 d7": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + q$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo q
                x0 = x0 + 6 + ul: ul = 0
            Case "r"
                b0 = B
                r: r$ = "u5 r1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + r$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r3"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo r
                x0 = x0 + 3 + ul: ul = 0
            Case "s"
                b0 = B
                s: s$ = "bu1 f1 r1 e1 h3 e1 r1 f1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + s$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r5"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo s
                x0 = x0 + 5 + ul: ul = 0
            Case "t"
                b0 = B
                t: t$ = "br1 h1 u6 d2 r1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + t$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r3"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo t
                x0 = x0 + 3 + ul: ul = 0
            Case "u"
                b0 = B
                u: u$ = "bu5 d4 f1 r1 e1 r1 d1 u5": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + u$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo u
                x0 = x0 + 6 + ul: ul = 0
            Case "v"
                b0 = B
                v: v$ = "bu5 d1 f1 d1 f1 d1 u1 e1 u1 e1 u1 ": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + v$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo v
                x0 = x0 + 6 + ul: ul = 0
            Case "w"
                b0 = B
                w: w$ = "bu5 d3 f1 d1 u1 e1 u1 e1 u1 d1 f1 d1 f1 d1 u1 e1 u3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + w$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo w
                x0 = x0 + 8 + ul: ul = 0
            Case "x"
                b0 = B
                x: x$ = "u1 e1 r1 f1 d1 bu5 d1 g1 l1 h1 u1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + x$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r5"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo x
                x0 = x0 + 5 + ul: ul = 0
            Case "y"
                b0 = B
                y: y$ = "bu5 d3 f1 r1 e1 u3 bd4 bl1 d1 g1 l1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + y$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r5"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo y
                x0 = x0 + 5 + ul: ul = 0
            Case "z"
                b0 = B
                z: z$ = "bu5 r3 d1 g3 d1 r3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + z$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r5"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo z
                x0 = x0 + 5 + ul: ul = 0
            Case "A"
                b0 = B
                aa: a$ = "u1 e1 u2 e1 u1 e1 u1 d1 f1 d1 f1 d2 l3 r3 f1 d1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + a$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo aa
                x0 = x0 + 8 + ul: ul = 0
            Case "B"
                b0 = B
                bb: B$ = "u8 r3 f1 d2 g1 l2 r2 f1d2g1l2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + B$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo bb
                x0 = x0 + 6 + ul: ul = 0
            Case "C"
                b0 = B
                cc: c$ = "br5bu1g1l3h1u6e1r3f1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + c$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo cc
                x0 = x0 + 7 + ul: ul = 0
            Case "D"
                b0 = B
                dd: d$ = "u8r3f2d4g2l3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + d$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo dd
                x0 = x0 + 7 + ul: ul = 0
            Case "E"
                b0 = B
                ee: e$ = "r4l4u8r4bd4bl1l3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + e$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo ee
                x0 = x0 + 6 + ul: ul = 0
            Case "F"
                b0 = B
                ff: f$ = "u8r4bd4bl1l3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + f$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo ff
                x0 = x0 + 6 + ul: ul = 0
            Case "G"
                b0 = B
                gg: g$ = "bu1u6e1r3f1bd3l2r2d4h1g1l2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + g$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo gg
                x0 = x0 + 7 + ul: ul = 0
            Case "H"
                b0 = B
                hh: h$ = "u8d4r5u4d8": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + h$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo hh
                x0 = x0 + 7 + ul: ul = 0
            Case "I"
                b0 = B
                ii: i$ = "u8": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + i$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r2"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo ii
                x0 = x0 + 2 + ul: ul = 0
            Case "J"
                b0 = B
                jj: j$ = "bu2d1f1r1e1u7": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + j$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r5"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo jj
                x0 = x0 + 5 + ul: ul = 0
            Case "K"
                b0 = B
                kk: k$ = "u8d3r1e3g3d1f4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + k$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo kk
                x0 = x0 + 7 + ul: ul = 0
            Case "L"
                b0 = B
                ll: L$ = "u8d8r4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + L$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo ll
                x0 = x0 + 6 + ul: ul = 0
            Case "M"
                b0 = B
                mm: m$ = "u8d2r1d1f1d1f1d1u1e1u1e1u1r1u2d8": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + m$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo mm
                x0 = x0 + 8 + ul: ul = 0
            Case "N"
                b0 = B
                nn: n$ = "u8f1d1f1d1f2d1f1u8": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo nn
                x0 = x0 + 7 + ul: ul = 0
            Case "O"
                b0 = B
                oo: o$ = "bu1u6e1r3f1d6g1l3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + o$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo oo
                x0 = x0 + 7 + ul: ul = 0
            Case "P"
                b0 = B
                pp: p$ = "u8r4f1d2g1l4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + p$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo pp
                x0 = x0 + 7 + ul: ul = 0
            Case "Q"
                b0 = B
                qq: q$ = "bu1u6e1r3f1d6g1l3br2bu2f1d1f1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + q$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo qq
                x0 = x0 + 7 + ul: ul = 0
            Case "R"
                b0 = B
                rr: r$ = "u8r4f1d2g1l4r4f1d3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + r$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo rr
                x0 = x0 + 7 + ul: ul = 0
            Case "S"
                b0 = B
                ss: s$ = "bu1f1r2e1u2h1l2h1u2e1r2f1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + s$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo ss
                x0 = x0 + 6 + ul: ul = 0
            Case "T"
                b0 = B
                tt: t$ = "br2u8l2r4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + t$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo tt
                x0 = x0 + 6 + ul: ul = 0
            Case "U"
                b0 = B
                uu: u$ = "bu8d7f1r3e1u7": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + u$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r7"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo uu
                x0 = x0 + 7 + ul: ul = 0
            Case "V"
                b0 = B
                vv: v$ = "bu8d1f1d2f1d1f1d1u1e1u1e1u2e1u1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + v$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo vv
                x0 = x0 + 8 + ul: ul = 0
            Case "W"
                b0 = B
                ww: w$ = "bu8d1f1d2f1d1f1d1u1e1u1e1u2d2f1d1f1d1u1e1u1e1u2e1u1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + w$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r12"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo ww
                x0 = x0 + 12 + ul: ul = 0
            Case "X"
                b0 = B
                xx: x$ = "u1e6u1bl6d1f6d1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + x$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo xx
                x0 = x0 + 8 + ul: ul = 0
            Case "Y"
                b0 = B
                yy: y$ = "bu8d1f3d4u4e3u1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + y$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo yy
                x0 = x0 + 8 + ul: ul = 0
            Case "Z"
                b0 = B
                zz: z$ = "bu8r6d1g6d1r6": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + z$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r8"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo zz
                x0 = x0 + 8 + ul: ul = 0
            Case " "
                x0 = x0 + 3
            Case "1"
                b0 = B
                one: n$ = "bu7r2u1d8": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r4"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo one
                x0 = x0 + 4 + ul: ul = 0
            Case "2"
                b0 = B
                two: n$ = "bu7e1r2f1d2g4d1r4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo two
                x0 = x0 + 6 + ul: ul = 0
            Case "3"
                b0 = B
                three: n$ = "bu7e1r2f1d2g1l1r1f1d2g1l2h1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo three
                x0 = x0 + 6 + ul: ul = 0
            Case "4"
                b0 = B
                four: n$ = "br3u8g1d1g1d1g1d1r4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo four
                x0 = x0 + 6 + ul: ul = 0
            Case "5"
                b0 = B
                five: n$ = "bu1f1r2e1u3h1l3d1u4r4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo five
                x0 = x0 + 6 + ul: ul = 0
            Case "6"
                b0 = B
                six: n$ = "bu4r3f1d2g1l2h1u6e1r2f1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo six
                x0 = x0 + 6 + ul: ul = 0
            Case "7"
                b0 = B
                seven: n$ = "bu8r4d1g1d1g1d1g1d2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo seven
                x0 = x0 + 6 + ul: ul = 0
            Case "8"
                b0 = B
                eight: n$ = "bu1u2e1r2f1d2g1l2bl1bu5u2e1r2f1d2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo eight
                x0 = x0 + 6 + ul: ul = 0
            Case "9"
                b0 = B
                nine: n$ = "bu1f1r2e1u6h1l2g1d2f1r2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo nine
                x0 = x0 + 6 + ul: ul = 0
            Case "0"
                b0 = B
                zero: n$ = "bu1u6e1r2f1d6g1l2": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo zero
                x0 = x0 + 6 + ul: ul = 0
            Case "-"
                b0 = B
                lin: n$ = "bu3r1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo lin
                x0 = x0 + 3
            Case "."
                b0 = B
                dot: Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + "u0"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo dot
                x0 = x0 + 2
            Case "?"
                b0 = B
                qsmk: n$ = "br2u0bu2u1e2u2h1l2g1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo qsmk
                x0 = x0 + 6 + ul: ul = 0
            Case "("
                b0 = B
                paro: n$ = "bf1h1u8e1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r3"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo paro
                x0 = x0 + 3 + ul: ul = 0
            Case ")"
                b0 = B
                parc: n$ = "bd1e1u8h1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r3"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo parc
                x0 = x0 + 3 + ul: ul = 0
            Case "_"
                b0 = B
                ulin: n$ = "bd1r5": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo ulin
                x0 = x0 + 7
            Case "+"
                b0 = B
                plus: n$ = "bu3r4l2u2d4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo plus
                x0 = x0 + 6 + ul: ul = 0
            Case "="
                b0 = B
                equl: n$ = "bu2r4bu2l4": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r6"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo equl
                x0 = x0 + 6 + ul: ul = 0
            Case "<"
                b0 = B
                bigm: n$ = "br3h3e3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r5"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo bigm
                x0 = x0 + 5 + ul: ul = 0
            Case ">"
                b0 = B
                smlm: n$ = "e3h3": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r5"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo smlm
                x0 = x0 + 5 + ul: ul = 0
            Case "{"
                b0 = B
                aklo: n$ = "br2bd1h1u3h1e1u3e1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r4"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo aklo
                x0 = x0 + 4 + ul: ul = 0
            Case "}"
                b0 = B
                aklc: n$ = "bd1e1u3e1h1u3h1": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r4"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo aklc
                x0 = x0 + 4 + ul: ul = 0
            Case ":"
                b0 = B
                twdt: n$ = "u0bu5d0": Draw "c" + Str$(c) + "bm" + Str$(x0) + "," + Str$(y0) + n$
                If ul = 1 Then Draw "c" + Str$(c) + "bm" + Str$(x0 - 1) + "," + Str$(y0 + 2) + "r2"
                If b0 = 1 Then x0 = x0 + 1: b0 = 0: GoTo aklc
                x0 = x0 + 2 + ul: ul = 0
            Case Else
        End Select
    ni: Next i

End Sub

Sub game (mark, nn, select$(), yslct(), tick)
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
    Call door(3 + x200%, 40 + y200%, 127, 178, select$(), yslct())
    tc = nn / 5 + 1
    tick = tc
    x = 12 + x200%: y = yslct(tc) - 7: Line (x, y)-(x + 2, y + 2), 0: Line -(x + 6, y - 2), 0: Line (x, y - 1)-(x + 2, y + 1), 0: Line -(x + 6, y - 3), 0: Line (x, y - 2)-(x + 2, y), 0: Line -(x + 6, y - 4), 0
    If mark = 1 Then x = 12 + x200%: y = yslct(8) - 7: Line (x, y)-(x + 2, y + 2), 0: Line -(x + 6, y - 2), 0: Line (x, y - 1)-(x + 2, y + 1), 0: Line -(x + 6, y - 3), 0: Line (x, y - 2)-(x + 2, y), 0: Line -(x + 6, y - 4), 0

End Sub

Sub mines (nm, nn, lr%, lc%, gsave(), cusv(), cncl)
 
    Put (3 + x200%, 40 + y200%), gsave(), PSet
    Get (245, 79 + y200%)-(384, 214 + y200%), cusv(): lr0 = lr%: lc0 = lc%: nm0 = nm: nn0 = nn
    Call wind1(245, 79 + y200%, 139, 135, "Custom Field", 1, 0)
    nn = 25
    Call font("_Height:", 0, 0, 257, 126 + y200%)
    Call font("_Width:", 0, 0, 256, 160 + y200%)
    Call font("_Mines:", 0, 0, 257, 194 + y200%)
    yj = 0: GoSub rege
    yj = 34: GoSub rege
    yj = 68: GoSub rege
    yj = 0
    dlc:
    Do Until harf$ = Chr$(13)
        y0 = 125 + yj + y200%
        Line (x0, 127 + yj + y200%)-(x0, 115 + yj + y200%), 0
        harf$ = Input$(1)
        Line (x0, 127 + yj + y200%)-(x0, 115 + yj + y200%), 15
        If harf$ = Chr$(8) Then
            nu = 0: GoSub rege
            If yj = 0 Then rm$ = ""
            If yj = 34 Then cm$ = ""
            If yj = 68 Then mm$ = ""
        End If
        If harf$ = Chr$(27) Then
            Put (245, 79 + y200%), cusv(), PSet
            lr% = lr0: lc% = lc0: nm = nm0: nn = nn0
            cncl = 1: Exit Sub
        End If
        If harf$ = Chr$(9) Then
            yj = yj + 34
            x0 = 314
            If yj = 102 Then yj = 0
            nu = 0
            GoSub rege
            If yj = 0 Then rm$ = ""
            If yj = 34 Then cm$ = ""
            If yj = 68 Then mm$ = ""
            GoTo dlc
        End If
        If Asc(harf$) > 47 And Asc(harf$) < 58 Then
            nu = nu + 1
            If nu > 3 Then GoTo dlc
            If yj = 0 Then rm$ = rm$ + harf$
            If yj = 34 Then cm$ = cm$ + harf$
            If yj = 68 Then mm$ = mm$ + harf$
            Call font(harf$, 0, 0, x0, y0)
        End If
    Loop
    lr% = Val(rm$): lc% = Val(cm$): nm = Val(mm$)
    If lr% < 8 Then lr% = 8
    If lr% > 22 Then lr% = 22
    If lc% < 8 Then lc% = 8
    If lc% > 32 Then lc% = 32
    If nm > ((lr% - 1) * (lc% - 1)) Then nm = ((lr% - 1) * (lc% - 1))
    cncl = 0
    Exit Sub

    rege:
    x0 = 314
    Line (310, 112 + yj + y200%)-(369, 131 + yj + y200%), 15, BF
    Line (310, 130 + yj + y200%)-(310, 112 + yj + y200%), 8: Line -(368, 112 + yj + y200%), 8
    Line (368, 113 + yj + y200%)-(368, 130 + yj + y200%), 7: Line -(309, 130 + yj + y200%), 7
    Line (311, 129 + yj + y200%)-(311, 113 + yj + y200%), 0: Line -(367, 113 + yj + y200%), 0
    Return

End Sub

Sub mint (min)
    x0 = 20 + x200%: y0 = 79 + y200%
    tom$ = Str$(min / 1000)
    Call seg7(tom$, zr, x0, y0)
    If Len(tom$) <> 5 Then
        t = 5 - Len(tom$)
        For t = 1 To t
            tom$ = "0"
            zr = 1: Call seg7(tom$, zr, x0, y0)
        Next t
    End If

End Sub

Sub miti (tom)
    x0 = (lc% - 3) * 16 + 18 + x200%: y0 = 79 + y200%: zr = 0
    tom$ = Str$(tom / 1000)
    Call seg7(tom$, zr, x0, y0)
    If Len(tom$) <> 5 Then
        t = 5 - Len(tom$)
        For t = 1 To t
            tom$ = "0"
            zr = 1: Call seg7(tom$, zr, x0, y0)
        Next t
    End If

End Sub

Sub numbers (x000, y000, m)
    Line (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
    Select Case m
        Case 1
            k = 1
            Line (7 + x000, 3 + y000)-(8 + x000, 10 + y000), k, BF: Line (5 + x000, 11 + y000)-(10 + x000, 11 + y000), k: Line (5 + x000, 5 + y000)-(6 + x000, 5 + y000), k: PSet (6 + x000, 5 + y000), k
        Case 2
            k = 5
            Line (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: Line (10 + x000, 4 + y000)-(4 + x000, 10 + y000), k: Line (10 + x000, 5 + y000)-(4 + x000, 11 + y000), k
            Line (4 + x000, 11 + y000)-(10 + x000, 11 + y000), k: PSet (4 + x000, 4 + y000), k: PSet (9 + x000, 4 + y000), k
        Case 3
            k = 4
            Line (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: Line (9 + x000, 4 + y000)-(10 + x000, 5 + y000), k, B: Line (5 + x000, 6 + y000)-(9 + x000, 6 + y000), k
            Line (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: Line (4 + x000, 11 + y000)-(9 + x000, 11 + y000), k
        Case 4
            k = 9
            Line (4 + x000, 3 + y000)-(5 + x000, 7 + y000), k, B: Line (6 + x000, 7 + y000)-(8 + x000, 7 + y000), k: Line (9 + x000, 3 + y000)-(10 + x000, 11 + y000), k, B
        Case 5
            k = 6
            Line (4 + x000, 3 + y000)-(5 + x000, 6 + y000), k, B: Line (6 + x000, 3 + y000)-(10 + x000, 3 + y000), k: Line (6 + x000, 6 + y000)-(9 + x000, 6 + y000), k
            Line (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: Line (4 + x000, 10 + y000)-(5 + x000, 10 + y000), k: Line (4 + x000, 11 + y000)-(9 + x000, 11 + y000), k
        Case 6
            k = 11
            Line (4 + x000, 4 + y000)-(5 + x000, 10 + y000), k, B: Line (5 + x000, 3 + y000)-(9 + x000, 3 + y000), k: Line (9 + x000, 4 + y000)-(10 + x000, 4 + y000), k
            Line (6 + x000, 6 + y000)-(9 + x000, 6 + y000), k: Line (9 + x000, 7 + y000)-(10 + x000, 10 + y000), k, B: Line (5 + x000, 11 + y000)-(9 + x000, 11 + y000), k
        Case 7
            k = 13
            Line (4 + x000, 3 + y000)-(5 + x000, 4 + y000), k, B: Line (6 + x000, 3 + y000)-(10 + x000, 3 + y000), k
            Line (6 + x000, 10 + y000)-(9 + x000, 4 + y000), k: Line (7 + x000, 10 + y000)-(10 + x000, 4 + y000), k: Line (6 + x000, 11 + y000)-(7 + x000, 11 + y000), k
        Case 8
            k = 0
            Line (5 + x000, 3 + y000)-(9 + x000, 11 + y000), k, B: Line (4 + x000, 3 + y000)-(4 + x000, 11 + y000), k, , &H6F00: Line (10 + x000, 3 + y000)-(10 + x000, 11 + y000), k, , &H6F00: Line (5 + x000, 6 + y000)-(9 + x000, 6 + y000), k
    End Select

End Sub

Sub pooch (bomb(), r, c, fl, flag(), x100, y100)
 
    n = 0: n2 = 0: j = 0: n0 = 0
    Dim a(lr% * lc%)
    Dim B(lr% * lc%)
    Dim a0(lr% * lc%)
    Dim b0(lr% * lc%)
    Dim n10(lr% * lc%)
    Dim g0(lr% * lc%)
    flag(r + B(n2), c + a(n2)) = 3: fl = fl + 1: n2 = n2 + 1: n = n2
    g = 1
    a(n2) = 0
    B(n2) = 0
    If bomb(r + B(n), c + a(n)) = 0 Then n0 = n0 + 1: a0(n0) = a(n2): b0(n0) = B(n2): g0(n0) = g: n10(n0) = n2
    901 flag(r + B(n2), c + a(n2)) = 3: fl = fl + 1: n2 = n2 + 1: n = n2
    1004
    If g = 1 Then a(n2) = a(n - 1) - 1: B(n2) = B(n - 1) - 1: GoSub 1005
    If d = 0 And g = 2 Then a(n2) = a(n - 1) + 1: B(n2) = B(n - 1): GoSub 1005
    If d = 1 And g = 2 Then a(n2) = a0(n0): B(n2) = b0(n0) - 1: GoSub 1005
    If d = 0 And g = 3 Then a(n2) = a(n - 1) + 1: B(n2) = B(n - 1): GoSub 1005
    If d = 1 And g = 3 Then a(n2) = a0(n0) + 1: B(n2) = b0(n0) - 1: GoSub 1005
    If d = 0 And g = 4 Then a(n2) = a(n - 1): B(n2) = B(n - 1) + 1: GoSub 1005
    If d = 1 And g = 4 Then a(n2) = a0(n0) + 1: B(n2) = b0(n0): GoSub 1005
    If d = 0 And g = 5 Then a(n2) = a(n - 1): B(n2) = B(n - 1) + 1: GoSub 1005
    If d = 1 And g = 5 Then a(n2) = a0(n0) + 1: B(n2) = b0(n0) + 1: GoSub 1005
    If d = 0 And g = 6 Then a(n2) = a(n - 1) - 1: B(n2) = B(n - 1): GoSub 1005
    If d = 1 And g = 6 Then a(n2) = a0(n0): B(n2) = b0(n0) + 1: GoSub 1005
    If d = 0 And g = 7 Then a(n2) = a(n - 1) - 1: B(n2) = B(n - 1): GoSub 1005
    If d = 1 And g = 7 Then a(n2) = a0(n0) - 1: B(n2) = b0(n0) + 1: GoSub 1005
    If d = 0 And g = 8 Then a(n2) = a(n - 1): B(n2) = B(n - 1) - 1: GoSub 1005
    If d = 1 And g = 8 Then a(n2) = a0(n0) - 1: B(n2) = b0(n0): GoSub 1005
    If g = 9 Then j = j + 1: j1 = j: n0 = n0 - 1: If n0 = -1 Then GoTo 1008 Else n = n10(n0) + 1: g = g0(n0 + 1) + 1: d = 1: GoTo 1004
    1008 fl = fl - 1: Erase a, B, a0, b0, n10, g0
    Exit Sub

    1005
    If r + B(n2) > 0 And r + B(n2) < lr% + 1 Then
        If c + a(n2) > 0 And c + a(n2) < lc% + 1 Then
            If flag(r + B(n2), c + a(n2)) = 3 Or flag(r + B(n2), c + a(n2)) = 1 Then g = g + 1: d = 1: GoTo 1004
            If bomb(r + B(n2), c + a(n2)) = 0 Then n0 = n0 + 1: a0(n0) = a(n2): b0(n0) = B(n2): g0(n0) = g: n10(n0) = n2
            If r + B(n2) > 0 And r + B(n2) < lr% + 1 Then
                If c + a(n2) > 0 And c + a(n2) < lc% + 1 Then
                    m = bomb(r + B(n2), c + a(n2))
                    x000 = (c + a(n2) - 1) * 16 + x100: y000 = (r + B(n2) - 1) * 16 + y100
                    If m = 0 Then
                        Line (0 + x000, 0 + y000)-(15 + x000, 15 + y000), 7, BF: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0, , &H5555: Line (15 + x000, 15 + y000)-(15 + x000, 0 + y000), 0, , &H5555
                        g = 1
                        GoTo 901
                    Else
                        Call numbers(x000, y000, m)
                        g = g + 1
                        GoTo 901
                    End If
                End If
            End If
            g = g + 1: d = 1
        End If
    End If
    g = g + 1: d = 1: GoTo 1004
    Return

End Sub

Sub save (n$, resv(), nn)

    Get (206, 79 + y200%)-(433, 178 + y200%), resv()
    Call wind1(206, 79 + y200%, 227, 99, "Congratulations", 1, 0)
    If nn = 10 Then L$ = "beginner"
    If nn = 15 Then L$ = "intermediate"
    If nn = 20 Then L$ = "expert"
    Call font("You have the fastest time for " + L$, 0, 0, 220, 116 + y200%)
    Call font("level. Please type your name:", 0, 0, 221, 129 + y200%)
    reg: Line (220, 144 + y200%)-(420, 163 + y200%), 15, BF
    Line (220, 162 + y200%)-(220, 144 + y200%), 8: Line -(419, 144 + y200%), 8
    Line (419, 145 + y200%)-(419, 162 + y200%), 7: Line -(221, 162 + y200%), 7
    Line (221, 161 + y200%)-(221, 145 + y200%), 0: Line -(418, 145 + y200%), 0
    x0 = 224: n$ = ""
    dl: Do Until harf$ = Chr$(13)
        Line (x0, 159 + y200%)-(x0, 148 + y200%), 0
        harf$ = Input$(1)
        Line (x0, 159 + y200%)-(x0, 148 + y200%), 15
        If harf$ = Chr$(8) Then GoTo reg
        If x0 > 314 Then GoTo dl
        n$ = n$ + harf$
        Call font(harf$, 0, 0, x0, 157 + y200%)
    Loop
    Put (206, 79 + y200%), resv(), PSet

End Sub

Sub scene (x100, y100)

    Screen 12: Palette 12, 63: Palette 4, 31
    tx = 320 - ((lc% * 16) + 30) / 2: ty = 240 - ((lr% * 16) + 110) / 2
    x200% = Int(tx): y200% = Int(ty)
    Paint (0, 0), 8
    Call wind1(x200%, y200%, (16 * lc%) + 29, (16 * lr%) + 110, "Minesweeper", 1, 15)
    Call font("_Game", 0, 0, 10 + x200%, 34 + y200%)
    Call wind(x200%, 81 + y200%, 3, 6, 3, (16 * lc%) + 29 + x200%, (16 * lr%) + 110 + y200%, 7)
    Call wind(x200%, 38 + y200%, 3, 6, 2, (16 * lc%) + 29 + x200%, 98 + y200%, 7)
    Line (6 + x200%, 83 + y200%)-(11 + x200%, 96 + y200%), 7, BF
    Line (3 + x200%, 92 + y200%)-(5 + x200%, 96 + y200%), 15, BF
    Line ((lc% * 16) + 18 + x200%, 83 + y200%)-((lc% * 16) + 23 + x200%, 96 + y200%), 7, BF
    Line ((lc% * 16) + 16 + x200%, 95 + y200%)-((lc% * 16) + 17 + x200%, 94 + y200%), 15: PSet Step(0, 1), 15
 
    Line ((lc% * 8) + 2 + x200%, 56 + y200%)-((lc% * 8) + 27 + x200%, 81 + y200%), 8, B
    Line ((lc% * 8) + 3 + x200%, 57 + y200%)-((lc% * 8) + 26 + x200%, 80 + y200%), 15, BF
    Line ((lc% * 8) + 26 + x200%, 57 + y200%)-((lc% * 8) + 26 + x200%, 80 + y200%), 0: Line -((lc% * 8) + 3 + x200%, 80 + y200%), 0
    Line ((lc% * 8) + 4 + x200%, 58 + y200%)-((lc% * 8) + 25 + x200%, 79 + y200%), 8, BF: Line ((lc% * 8) + 4 + x200%, 58 + y200%)-((lc% * 8) + 24 + x200%, 78 + y200%), 7, BF
    Circle ((lc% * 8) + 14 + x200%, 68 + y200%), 8, 0: Paint ((lc% * 8) + 14 + x200%, 68 + y200%), 14, 0
    Line ((lc% * 8) + 11 + x200%, 65 + y200%)-((lc% * 8) + 12 + x200%, 66 + y200%), 0, B: Line ((lc% * 8) + 16 + x200%, 65 + y200%)-((lc% * 8) + 17 + x200%, 66 + y200%), 0, B
    Line ((lc% * 8) + 10 + x200%, 70 + y200%)-((lc% * 8) + 12 + x200%, 72 + y200%), 0: Line -((lc% * 8) + 16 + x200%, 72 + y200%), 0: Line -((lc% * 8) + 18 + x200%, 70 + y200%), 0

    Line (19 + x200%, 56 + y200%)-(59 + x200%, 80 + y200%), 8, B
    Line (59 + x200%, 57 + y200%)-(59 + x200%, 80 + y200%), 15: Line -(20 + x200%, 80 + y200%), 15
    Line (20 + x200%, 57 + y200%)-(58 + x200%, 79 + y200%), 0, BF
    Line ((lc% - 3) * 16 + 17 + x200%, 56 + y200%)-((lc% - 3) * 16 + 57 + x200%, 80 + y200%), 8, B
    Line ((lc% - 3) * 16 + 57 + x200%, 57 + y200%)-((lc% - 3) * 16 + 57 + x200%, 80 + y200%), 15: Line -((lc% - 3) * 16 + 18 + x200%, 80 + y200%), 15
    Line ((lc% - 3) * 16 + 18 + x200%, 57 + y200%)-((lc% - 3) * 16 + 56 + x200%, 79 + y200%), 0, BF

    PSet (18 + x200%, 7 + y200%), 0: Draw "d1 l1 d1 l1 d2 r3 l4 d1 r2 l4 u1 d2 l2 r8 l3 d1 l4 g1 r6 d1 r1 d1 g1 h2 l4 d3 r2 u3 l4 d1 l1 d1 l1 "
    PSet (17 + x200%, 17 + y200%), 8: Draw "h1 bg2 bl2 u1 bl5 e2 r1 e1 l1 h1 e1 g2 u1 l3 u2 r2 e1 h2 e1 r1 f1 r1 e1 u2 e1 f1 d2 f1 r1 e2 g2 d2 g1 u1 h1 g1 bd2 u0 br6 r1"
    PSet (6 + x200%, 17 + y200%), 7: Draw "e8 br3 bu2 g1 bl3 l1 d1 l1 d1 l1 d1 l1 d1 l1 bu3 h1"
    PSet (6 + x200%, 12 + y200%), 15: Draw "r1 e5 u2 bl4 bd3 f1 bf2 f0"

    x100 = 15 + x200%: y100 = 96 + y200%
    For x000 = 0 + x100 To (lc% * 16) - 1 + x100 Step 16
        For y000 = 0 + y100 To (lr% * 16) - 1 + y100 Step 16
            Line (14 + x000, 0 + y000)-(0 + x000, 0 + y000), 15: Line -(0 + x000, 14 + y000), 15: Line (0 + x000, 15 + y000)-(15 + x000, 15 + y000), 0: Line -(15 + x000, 0 + y000), 0
            Line (14 + x000, 1 + y000)-(14 + x000, 14 + y000), 8: Line -(1 + x000, 14 + y000), 8: Line (1 + x000, 1 + y000)-(13 + x000, 13 + y000), 7, BF
        Next y000
    Next x000

End Sub

Sub seg7 (tom$, zr, x0, y0)

    If zr = 1 Then e$ = tom$: GoTo num
    For i = 3 To Len(tom$)
        e$ = Mid$(tom$, i, 1)
        num:
        Select Case e$
            Case "1"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br3bu1e2f2bl2e2f2 c0br1h2g2h2g2br2e2f2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br3bu21f2e2br2g2h2 c0bl1f2e2br2g2h2g2h2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4bu11br3f1e2f2e1h1g2h2 c0bl2bd1f1e2f2e2f1g1h2g2h2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "2"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br10bu1l8e1r6h1l4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu2u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12bu11br2r8g1l6u2r6": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br11bu10g2f2g2f2bu2h2e2 c0bu1g2f2bd2h2e2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "3"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br10bu1l8e1r6h1l4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12bu11br2r8g1l6u2r6": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "4"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu12u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12bu11br2r8g1l6u2r6": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br3bu1e2f2bl2e2f2 c0br1h2g2h2g2br2e2f2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br3bu21f2e2br2g2h2 c0bl1f2e2br2g2h2g2h2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "5"
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br10bu1l8e1r6h1l4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu12u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12bu11br2r8g1l6u2r6": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br11bu20g2f2g2f2bu2h2e2 c0bu1g2f2bd2h2e2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "6"
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br10bu1l8e1r6h1l4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu2u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu12u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12bu11br2r8g1l6u2r6": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br11bu20g2f2g2f2bu2h2e2 c0bu1g2f2bd2h2e2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "7"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br3bu1e2f2bl2e2f2 c0br1h2g2h2g2br2e2f2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu12e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4bu11br3f1e2f2e1h1g2h2 c0bl2bd1f1e2f2e2f1g1h2g2h2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "8"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br10bu1l8e1r6h1l4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu2u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu12u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12bu11br2r8g1l6u2r6": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "9"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br10bu1l8e1r6h1l4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu12u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12bu11br2r8g1l6u2r6": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4br1bu2e2h2e2h2bd2f2g2 c0bd1e2h2bu2f2g2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case "0"
                n$ = "c12br11bu20d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br11bu10d8h1u6g1d4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br10bu1l8e1r6h1l4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu2u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br1bu12u8f1d6e1u4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c12br2bu21r8g1l6f1r4": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                n$ = "c4bu11br3f1e2f2e1h1g2h2 c0bl2bd1f1e2f2e2f1g1h2g2h2": Draw "bm" + Str$(x0) + "," + Str$(y0) + n$
                x0 = x0 + 13
            Case Else
        End Select
        If zr = 1 Then GoTo 14
    Next i
    14
End Sub

Sub wind (x0%, y0%, nw, ng, nb, x1%, y1%, k)

    For n = 1 To nw
        Line (x0% + 2 + n, y1% - 3 - n)-(x0% + 2 + n, y0% + 2 + n), 15: Line -(x1% - 2 - n, y0% + 2 + n), 15
        Line (x0% + 2 + n, y1% - 2 - n)-(x1% - 2 - n, y1% - 2 - n), 8: Line -(x1% - 2 - n, y0% + 2 + n), 8
    Next n
    For n = 1 To nb
        Line (x0% + 2 + nw + ng + n, y1% - 2 - nw - ng - n)-(x0% + 2 + nw + ng + n, y0% + 2 + nw + ng + n), 8: Line -(x1% - 2 - nw - ng - n, y0% + 2 + nw + ng + n), 8
        Line (x0% + 3 + nw + ng + n, y1% - 2 - nw - ng - n)-(x1% - 2 - nw - ng - n, y1% - 2 - nw - ng - n), 15: Line -(x1% - 2 - nw - ng - n, y0% + 3 + nw + ng + n), 15
    Next n
    Line (x0% + 3 + nw + ng + nb, y0% + 3 + nw + ng + nb)-(x1% - 3 - nw - ng - nb, y1% - 3 - nw - ng - nb), k, BF

End Sub

Sub wind1 (x1%, y1%, px, py, title$, B, xsh)
    x2% = x1% + px: y2% = y1% + py
    Line (x1%, y1%)-(x2%, y2%), 7, BF
    Line (x2%, y1% + 1)-(x2%, y2%), 0: Line -(x1% + 1, y2%), 0
    Line (x2% - 1, y1% + 1)-(x1% + 1, y1% + 1), 15: Line -(x1% + 1, y2% - 1), 15
    Line (x1% + 2, y2% - 1)-(x2% - 1, y2% - 1), 8: Line -(x2% - 1, y1% + 2), 8
    Line (x1% + 3, y1% + 3)-(x2% - 3, y1% + 20), 1, BF
    x0 = x1% + xsh + 9: y0 = y1% + 15: c = 15
    Call font(title$, B, c, x0, y0)
    Line (x2% - 5, y1% + 6)-(x2% - 5, y1% + 18), 0: Line -(x2% - 19, y1% + 18), 0
    Line (x2% - 20, y1% + 18)-(x2% - 20, y1% + 5), 15: Line -(x2% - 5, y1% + 5), 15
    Line (x2% - 6, y1% + 6)-(x2% - 6, y1% + 17), 8: Line -(x2% - 19, y1% + 17), 8
    Line (x2% - 7, y1% + 6)-(x2% - 19, y1% + 16), 7, BF
    Line (x2% - 9, y1% + 8)-(x2% - 15, y1% + 14), 0: Line (x2% - 10, y1% + 8)-(x2% - 16, y1% + 14), 0: Line (x2% - 16, y1% + 8)-(x2% - 10, y1% + 14), 0: Line (x2% - 15, y1% + 8)-(x2% - 9, y1% + 14), 0

End Sub

