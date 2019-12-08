library(deSolve)
library(RGP)

LV <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    dxt = o*xt - (p^2 - t)*xt*y
    dxr = o*xr - B*xr*y
    y = p*xt*y*m - p*(r-g)*xr*y*m - n*y
    return(list(c(dxt,dxr,y)))
  })
}

Pars <- c(o = 1, p = 1, t = 1, B = 1, m=1, r=1, g=1,n=1)
State <- c(xt = .5, xr = .5, y =1)
Time <- seq(0,40, by =1)

out <- as.data.frame(ode(func = LV, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("presa", "depredador"), lty=c(1,2), col =c(1,2), box.lwd =0)
legend("topleft", "Lotka-Volterra", box.lwd =0)

write.csv(out,file="lvout.csv")
