library(Ternary)

TernaryPlot(point=dir, atip='Resistant', btip='Tolerant', ctip='Insect')

datos <- list(
  A = c(1, 0, 0), 
  B = c(0, 1, 0),
  C = c(0, .3, .7),
  D = c(.3, .3, .3),
  E = c(.5, 0, .75),
  H = c(.4, .2, 0)
)

AddToTernary(points, datos, pch=c(0,1,8,15,16,17))
legend("topright",pch=c(0,1,8,15,16,17), legend=c('(0,R,0)', '(0,0,T)', '(I,0,T)', '(I,R,T)','(I,R,0)','(0,R,T)'))