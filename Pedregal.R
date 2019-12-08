#Resistencia-Tolerancia
library(deSolve)
library(RGP)

RT <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    N = Xr + Xt
    Yo = 200
    dxr = (Xr/N)*(w/N+a*Xr/N+b*Xt/N) - f*P*Xr/N*Y/Yo
    dxt = Xt/N*(w/N+c*Xr/N+d*Xt/N) - (P^2-t)*Xt/N*Y/Yo
    dy = P*Xt/N*Y/Yo-P*(R-f)*Xr/N*Y/Yo-n*Y/Yo
    return(list(c(dxr, dxt, dy)))
  })
}

Pars <- c(R=.37, t=0, w=500, f=.2, a=.2,b=-.1,c=0,d=-.2, P=201, n=.15)
State <- c(Xr = 100, Xt = 100, Y = 200)
Time <- seq(0,100, by =1)

out <- as.data.frame(ode(func = RT, y=State, parms = Pars, times=Time))
matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion", main="Pedregal")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
save(out, file="datos.Rda")

legend("topright", c("Resistentes", "Tolerantes", "Insecto"), lty=c(1,2,3), col =c(1,2,3), box.lwd =0)


write.csv(out,file="lvout.csv")
#R=slider(0,1),t=slider(0,1),w=slider(0,1),f=slider(0,1), a=slider(0,1), b=slider(0,1), c=slider(0,1), d=slider(0,1),P=slider(0,1),n=slider(0,1))

