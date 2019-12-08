#Resistencia-Tolerancia
library(deSolve)
library(rgp)

RT <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    dxr = w + (B * fr * Xr* Y) - ((1-fr)* B * Xr * Y)
    dxt = w - ((1-ft)*B*Xt*Y)
    dy = (B*(1-fr)*Xr*Y) - (n * Y) + (B * Xt *Y)
    return(list(c(dxr, dxt, dy)))
  })
}

Pars <- c(B = .2, fr = 1, ft = 0, n = .2, w = 2)
State <- c(Xr = 5, Xt = 5, Y = 5)
Time <- seq(0,40, by =1)

out <- as.data.frame(ode(func = RT, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("Resistentes", "Tolerantes", "Insecto"), lty=c(1,2,3), col =c(1,2,3), box.lwd =0)


write.csv(out,file="lvout.csv")

