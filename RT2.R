#Resistencia-Tolerancia
library(deSolve)
library(rgp)

RT <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    dxr = Xr*(w+a*Xr+b*Xt) - f*P*Xr*Y
    dxt = Xt*(w + c*Xr + d*Xt) - (P*P-t)*Xt*Y
    dy = P*Xt*Y - (R-f)*P*Xr*Y - n*Y
    return(list(c(dxr, dxt, dy)))
  })
}

Pars <- c(w=.8, a=0, b=0,c =0, d=0, P=1,t=0,R=1,f=1,n=.1)
State <- c(Xr = .8, Xt = .2, Y = .5)
Time <- seq(0,400, by =1)

out <- as.data.frame(ode(func = RT, y=State, parms = Pars, times=Time))
#matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
#save(out, file="datos.Rda")

#legend("topright", c("Resistentes", "Tolerantes", "Insecto"), lty=c(1,2,3), col =c(1,2,3), box.lwd =0)


#write.csv(out,file="lvout.csv")



#formato
pdf("nofdselection")
matplot(out[,-1], type = "l", xlab = "time", ylab = "population", lty=c(2,3,1), col=gray.colors(n))
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
#save(out, file="datos.Rda")

legend("topright", c("Resistant plants", "Tolerant plants", "Insect"), lty=c(2,3,1), col =gray.colors(n), box.lwd =0)
dev.off()


