#Resistencia-Tolerancia
library(deSolve)
library(rgp)
gray.colors(6, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)

RT <- function (Time, State, Pars) { 
  with(as.list(c(State, Pars)), {
    dxr = Xr*(w+a*Xr+b*Xt) - f*P*Xr*Y
    dxt = Xt*(w + c*Xr + d*Xt) - (P*P-t)*Xt*Y
    dy = P*Xt*Y - (R-f)*P*Xr*Y - n*Y
    return(list(c(dxr, dxt, dy)))
  })
}

R_r <-seq(0,1,0.25)
f_r<-seq(0,1,0.25)
t_r<-seq(0,1,0.25)
P_r<-seq(0,1,0.25)


for(i in 1:length(R_r)){
  for(j in 1: length(f_r)){
    for(k in 1: length(t_r)){
      for(l in 1: length(P_r)){
      
Pars <- c(w=.4, a=-0.5, b=0.5,c =0.5, d=-0.5, P=P_r[l],t=t_r[k],R=R_r[i],f=f_r[j],n=.5)
State <- c(Xr = .8, Xt = .2, Y = .5)
Time <- seq(0,400, by =1)

  
out <- as.data.frame(ode(func = RT, y=State, parms = Pars, times=Time))
#matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
#save(out, file="datos.Rda")

#legend("topright", c("Resistentes", "Tolerantes", "Insecto"), lty=c(1,2,3), col =c(1,2,3), box.lwd =0)


#write.csv(out,file="lvout.csv")



#formato
pdf(paste("NFDS", R_r[i],"_",f_r[j],"_",t_r[k], "_",P_r[l], "mu = 0.5.pdf"))
matplot(out[,-1], type = "l", xlab = "time", ylab = "population", lty=c(1,1,2), col=gray.colors(6), 
        main=paste("R = ",R_r[i],"_ phi = ",f_r[j],"_ t = ", t_r[k],"_ P = ", P_r[l]))
#Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
#save(out, file="datos.Rda")

legend("topright", c("Resistant plants", "Tolerant plants", "Insect"), lty=c(1,1,2), col=gray.colors(6), box.lwd =0)
dev.off()
}}}}


