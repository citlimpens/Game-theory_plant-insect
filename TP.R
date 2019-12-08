data <- read.csv('DATA1.csv', header = T, sep = ",", row.names = 1)
matplot(data, ylab="plant fitness", xlab="insect preference",type="l",lty=1,lwd=1)
abline(h=0,lty=2, v=0)