CLS
INPUT td
tu = 1
DIM far(td)
FOR mot = 1 TO td
INPUT far(mot)
IF BF < far(mot) THEN BF = far(mot)
NEXT mot
CLS : SCREEN 12: WINDOW (-.1 * td * tu, -.1 * BF)-(td * tu + .1 * td * tu, BF + .1 * BF)
LINE (0, 0)-(0, BF + .1 * BF), 15
LINE (0, 0)-(td * tu + .1 * td * tu, 0), 15
FOR mot = 1 TO td - 1
kl = kl + 1
LINE (hf + (tu / 2), far(mot))-(tu + hf + (tu / 2), far(mot + 1)), kl
hf = hf + tu
NEXT mot

