library(Ternary)

TernaryPlot(point=dir, atip='Resistant', btip='Tolerant', ctip='Insect')

datos <- list(
  A = c(1, 0, 0), 
  B = c(0, 1, 0),
  C = c(0, .3, .7),
  D = c(.3, .3, .3),
  E = c(.05, .2, .75),
  H = c(.4, .2, .4)
)

AddToTernary(points, datos, bg=vapply(data_points), character(1),Pch=21, cex=2.8)
#AddToTernary(text, datos, names(datos), cex=0.8, font=2)

#legend('bottomright')