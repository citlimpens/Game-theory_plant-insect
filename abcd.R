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

a_r <-seq(-1,1,0.1)
b_r <-seq(-1,1,0.1)
c_r<-seq(-1,1,0.1)
d_r<-seq(-1,1,0.1)


for(i in 1:length(a_r)){
  for(j in 1: length(b_r)){
    for(k in 1: length(c_r)){
      for(l in 1:length(d_r)){
      
      Pars <- c(w=.4, a=a_r[i], b=b_r[j],c =c_r[k], d=d_r[l], P=.8,t=.5,R=.6,f=.6,n=.5)
      State <- c(Xr = .8, Xt = .2, Y = .5)
      Time <- seq(0,400, by =1)
      
      
      out <- as.data.frame(ode(func = RT, y=State, parms = Pars, times=Time))
      #matplot(out[,-1], type = "l", xlab = "tiempo", ylab = "poblacion")
      #Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
      #save(out, file="datos.Rda")
      
      #legend("topright", c("Resistentes", "Tolerantes", "Insecto"), lty=c(1,2,3), col =c(1,2,3), box.lwd =0)
      
      
      #write.csv(out,file="lvout.csv")
      
      
      
      #formato
      pdf(paste("abcd cambian", a_r[i],"_",b_r[j],"_",c_r[k], "_", d_r[l], "mu = 0.5.pdf"))
      matplot(out[,-1], type = "l", xlab = "time", ylab = "population", lty=c(1,1,2), col=gray.colors(6), 
              main=paste("a = ",a_r[i],"_ b = ",b_r[j],"_ c = ", c_r[k],"_ d = ", d_r[l]))
      #Comandos para guardar la tabla: save(), load(), write.table(txt), saveRDS()
      #save(out, file="datos.Rda")
      
      legend("topright", c("Resistant plants", "Tolerant plants", "Insect"), lty=c(1,1,2), col=gray.colors(6), box.lwd =0)
      dev.off()
    }}}}


