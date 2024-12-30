COMMON flag%, x(), y()
on error goto errhandler
for i%=0 to  639
 x=x(i%)
 y(i%) =sin(x)/(1-cos(x))
next i%
chain "GRAPH"
errhandler:
if err then
  y(i%)=  3.402822E+38
  resume next
end if
