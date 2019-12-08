#Resistencia-Tolerancia
library(deSolve)
library(rgp)
gray.colors(6, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)



RT <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    dxt = Xt*(w + c*(1-Xt) + d*Xt) - (P*P-t)*Xt*Y
    dy = P*Xt*Y - (R-f)*P*(1-Xt)*Y - n*Y
    return(list(c(dxt, dy)))
  })
}

R_r <-seq(0,.1,0.1)
f_r<-seq(0,.1,0.1)
t_r<-seq(0,.1,0.1)



for(i in 1:length(R_r)){
  for(j in 1: length(f_r)){
    for(k in 1: length(t_r)){
      
Pars <- c(w=.8, a=-.5, b=.5,c =.5, d=-.5, P=1,t=0,R=1,f=1,n=.1)
State <- c(Xt = .5, Y = .5)
Time <- seq(0,400, by =1)

out <- as.data.frame(ode(func = RT, y=State, parms = Pars, times=Time))
out["Xr"] <- 1-out$Xt
#total <- merge.data.frame(Xr,out, by=Xt)

pdf(paste(R_r[i],"_",f_r[j],"_",t_r[k], "mu = 0.5.pdf"))
matplot(out[,-1], type = "l", xlab = "time", ylab = "population", lty=c(1,1,2), col=gray.colors(6), 
        main=paste("R = ",R_r[i],"_ phi = ",f_r[j],"_ t = ", t_r[k]))

#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
#save(out, file="datos.Rda")


#legend("topright", c("Resistentes", "Tolerantes", "Insecto"), lty=c(1,1,2), box.lwd =0)
legend("topright", c("Resistant plants", "Tolerant plants", "Insect"), lty=c(1,1,2), col=gray.colors(6), box.lwd =0)

dev.off()
    }}}