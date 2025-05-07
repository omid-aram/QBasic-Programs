Cls: Screen 12: x0 = 320: f = 2: k = 14

For r = 1 To 200 Step f * 2
    x1 = x0 + f
    Circle (x0, 240), r, k, 0, 3.14
    SLEEP 1
    Circle (x1, 240), r + f, k, 3.14, 6.28
Next r

