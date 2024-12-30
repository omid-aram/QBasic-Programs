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
LINE (0 + hf, 0)-(hf + tu, far(mot)), kl, BF
hf = hf + tu
NEXT mot


