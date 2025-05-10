Cls: Screen 12: t = 0: y = 240: f = 2: f1 = 2: k = 14

For l = 320 - f To 200 Step -f
    Sleep 1
    y = y - f1
    t = t + (f * 2)
    t1 = t1 + (f1 * 2)
    Line (l, y)-(l + t, y), k
    Line -(l + t, y + t1), k
    Line -(l - f, y + t1), k
    Line -(l - f, y - f1), k
Next l

