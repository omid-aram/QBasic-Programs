CLS
INPUT td
tu = 1
DIM far(td)
FOR mot = 1 TO td
INPUT far(mot)
IF BF < far(mot) THEN BF = far(mot)
NEXT mot
CLS : SCREEN 12: WINDOW (-.1 * td * tu, -.1 * BF)-(td * tu + .1 * td * tu, BF + .1 * BF)
FOR mot = 1 TO td
kl = kl + 1
LINE (hf + (tu / 2), 0)-(hf + (tu / 2), far(mot)), kl
hf = hf + tu
NEXT mot

