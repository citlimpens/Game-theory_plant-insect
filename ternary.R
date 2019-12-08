library("Ternary")
TernaryPlot(atip = "Insect", btip ="Resistant", ctip="Tolerant", point='right', lab.cex=0.8,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white', 
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08)
data_points <- list(
  To = c(0, 0, 250), 
  Ro = c(0, 180, 0),
  RI = c(210, 222, 0),
  TI = c(111, 0, 16),
  RT = c(0, 160, 243),
  ALL = c(92, 120, 243),
  ALL = c(225, 240, 208)
)
AddToTernary(points, data_points, bg=vapply(data_points, function (x) rgb(x[1], x[2], x[3], 128, maxColorValue=255), character(1)), pch=21, cex=2.8)
AddToTernary(text, data_points, cex=0.8, font=2)
legend('bottomright', 
       pch=21, pt.cex=1.8,
       legend=c('(0,0,T)', '(0,R,T)', '(0,R,0)', '(0,R,0)'), 
       cex=0.8, bty='n')
