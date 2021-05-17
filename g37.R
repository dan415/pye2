if (!require(tidyverse))  install.packages("tidyverse")
if (!require(broom))  install.packages("broom")
if (!require(plyr))  install.packages("plyr")
if (!require(ggplot2))  install.packages("ggplot2")
if (!require(lattice))  install.packages("lattice")
if (!require(Rmisc))  install.packages("Rmisc")
if (!require(DescTools))  install.packages("DescTools")
if (!require(MASS))  install.packages("MASS")
if (!require(car))  install.packages("car")
if (!require(EstimationTools))  install.packages("EstimationTools")
if (!require(ISwR))  install.packages("ISwR")
if (!require(IndependenceTests))  install.packages("IndependenceTests")
if (!require(lmtest))  install.packages("lmtest")
if (!require(boot))  install.packages("boot")
if (!require(vcd))  install.packages("vcd")
if (!require(rcompanion))  install.packages("rcompanion")
if (!require(FSA))  install.packages("FSA")
if (!require(psych))  install.packages("psych")
if (!require(e1071))  install.packages("e1071")

library(tidyverse)
library(broom)
library(plyr)
library(ggplot2)
library(lattice)
library(Rmisc)
library(DescTools)
library(MASS)
library(car)
library(EstimationTools)
library(ISwR)
library(IndependenceTests)
library(lmtest)
library(boot)
library(vcd)
library(rcompanion)
library(FSA)
library(psych)
library(e1071) 


Data <- read.csv(file=paste("PYE2DataSet", "37", ".csv", sep=""), header=TRUE)
data.frame(Data)
head(Data)
str(Data)



# 1. Descriptiva SLEEPTIME
head(Data$sleeptime)
summary(Data$sleeptime)
describe(Data$sleeptime)
hist(Data$sleeptime, freq=FALSE, xlab = "Sleeptime", main = "Histograma de Sleeptime")

par(mfrow=c(1,1))
boxplot(Data$sleeptime, main="Sleeptime")
stem(Data$sleeptime, scale = 3)

e1071::kurtosis(Data$sleeptime)
e1071::skewness(Data$sleeptime)

# 1. Descriptiva STEPS
head(Data$steps)
summary(Data$steps)
describe(Data$steps)
hist(Data$steps, freq=FALSE, xlab = "Steps", main = "Histograma de Steps")

steps1 = Data$steps[Data$steps<9600]
steps2 = Data$steps[Data$steps>=9600 & Data$steps<11700]
steps3 = Data$steps[Data$steps>=11700]

summary(steps1)
describe(steps1)
summary(steps2)
describe(steps2)
summary(steps3)
describe(steps3)


par(mfrow=c(2,2))
hist(steps1, freq=FALSE, xlab="Steps", main="Histograma de Steps", c="yellow", xlim =c(8000, 14000))
hist(steps2, freq=FALSE, add=TRUE, c="red")
hist(steps3, freq=FALSE, add=TRUE, c="blue")

hist(steps1, freq=FALSE, xlab="Steps grupo 1", main="Histograma de Steps grupo 1", c="yellow")
hist(steps2, xlab="Steps grupo 2", main="Histograma de Steps grupo 2", c="red")
hist(steps3, xlab="Steps grupo 3", main="Histograma de Steps grupo 3", c="blue")

par(mfrow=c(1,1))
boxplot(steps1, main="Steps grupo 1", col=c("yellow"))
boxplot(steps2, main="Steps grupo 2",  col=c("red"))
boxplot(steps3, main="Steps grupo 3",  col=c("blue"))

stem(steps1)
stem(steps2)
stem(steps3)

e1071::kurtosis(steps1)
e1071::kurtosis(steps2)
e1071::kurtosis(steps3)

e1071::skewness(steps1)
e1071::skewness(steps2)
e1071::skewness(steps3)


# 2.AJUSTE SLEEPIME
x=seq(0, 23, by=0.01)
nsleep <- MASS::fitdistr(Data$sleeptime, "normal")
esleep <- MASS::fitdistr(Data$sleeptime, "exponential")
gsleep <- MASS::fitdistr(Data$sleeptime, "gamma", lower = c(0, 0)) 

par(mfrow=c(1,1))
plot(x,dnorm(x, nsleep$estimate[1], nsleep$estimate[2]),
     type="l", ylab="Density", xlab="Sleeptime", main="Ajuste sobre distribuci贸n Normal")
plot(x, dexp(x, esleep$estimate[1]), 
     type="l", ylab="Density", xlab="Sleeptime", main="Ajuste sobre distribuci贸n Exponencial")
plot(x,dgamma(x, gsleep$estimate[1], gsleep$estimate[2]), 
     type="l", ylab="Density", xlab="Sleeptime", main="Ajuste sobre distribuci贸n Gamma")

ks.test(Data$sleeptime, pnorm, nsleep$estimate[1], nsleep$estimate[2])
ks.test(Data$sleeptime, pexp, esleep$estimate[1])
ks.test(Data$sleeptime, pgamma, gsleep$estimate[1], gsleep$estimate[2])

par(mfrow=c(2,2))
x=seq(0, 23, by=0.001)
plot(x, pnorm(x, mean=nsleep$estimate[1], sd=nsleep$estimate[2]),
     col = "blue", type="l", xlim = c(0, 23), ylim = c(0, 1), xlab = "Sleeptime", ylab="Probabilidad acumulada", main="ECDF vs normal CDF")
plot(ecdf(Data$sleeptime), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))

plot(x, pgamma(x, gsleep$estimate[1], gsleep$estimate[2]), 
     col = "blue", type="l", xlim = c(0, 23), ylim = c(0, 1), xlab = "Sleeptime", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(Data$sleeptime), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))

plot(x, pexp(x, esleep$estimate[1]),
     col = "blue", type="l", xlim = c(0, 23), ylim = c(0, 1), xlab = "Sleeptime", ylab="Probabilidad acumulada",main="ECDF vs exponential CDF")
plot(ecdf(Data$sleeptime), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))

par(mfrow=c(1,2))
x=seq(0, 23, by=0.001)
plot(x, dnorm(x, mean=nsleep$estimate[1], sd=nsleep$estimate[2]),
     col = "blue", type="l", xlim = c(0, 23), ylim=c(0,0.1), xlab = "Sleeptime", ylab="Density", main="Funci贸n de densidad normal")
plot(density(Data$sleeptime), xlim = c(0, 23), col = "red", xlab = "Sleeptime", ylab="Density", main="Funci贸n de densidad emp铆rica")


par(mfrow=c(1,2))
x=seq(0, 23, by=0.001)
plot(x, dgamma(x, gsleep$estimate[1], gsleep$estimate[2]),
     col = "blue", type="l", xlim = c(0, 23), ylim=c(0,0.1), xlab = "Sleeptime", ylab="Density", main="Funci贸n de densidad gamma")
plot(density(Data$sleeptime), xlim = c(0, 23), col = "red", xlab = "Sleeptime", ylab="Density", main="Funci贸n de densidad emp铆rica")

par(mfrow=c(1,2))
x=seq(0, 23, by=0.001)
plot(x, dexp(x, esleep$estimate[1]),
     col = "blue", type="l", xlim = c(0, 23), ylim=c(0,0.1), xlab = "Sleeptime", ylab="Density", main="Funci贸n de densidad exponencial")
plot(density(Data$sleeptime), xlim = c(0, 23), col = "red", xlab = "Sleeptime", ylab="Density", main="Funci贸n de densidad emp铆rica")


# 2.AJUSTE STEPS
y1=seq(8000, 9600, by=0.01)
y2=seq(9600, 11700, by=0.01)
y3=seq(11700, 13600, by=0.01)
y=seq(0, 14000, by=0.001)
cerouno=seq(0, 1, by=0.001)

nsteps1 <- MASS::fitdistr(steps1, "normal")
nsteps2 <- MASS::fitdistr(steps2, "normal")
nsteps3 <- MASS::fitdistr(steps3, "normal")

esteps1 <- MASS::fitdistr((steps1-min(steps1))/max(steps1), "exponential")
esteps2 <- MASS::fitdistr((steps2-min(steps2))/max(steps2), "exponential")
esteps3 <- MASS::fitdistr((steps3-min(steps3))/max(steps3), "exponential")

gsteps1 <- MASS::fitdistr(steps1, "gamma",  lower = c(0, 0))
gsteps2 <- MASS::fitdistr(steps2, "gamma",  lower = c(0, 0))
gsteps3 <- MASS::fitdistr(steps3, "gamma",  lower = c(0, 0))


par(mfrow=c(2,2))
plot(y1,dnorm(y1, nsteps1$estimate[1], nsteps1$estimate[2]),
     type="l", xlim=c(8000, 9600), ylab="Density", xlab="Steps Grupo 1", main="Ajuste sobre distribuci贸n Normal", col="gold")
plot(cerouno, dexp(cerouno, esteps1$estimate[1]), xlim=c(0, 1),
     type="l", ylab="Density", xlab="Steps Grupo 1", main="Ajuste sobre distribuci贸n Exponencial", col="gold", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(8000, 9500, by=500))
plot(y1,dgamma(y1, gsteps1$estimate[1], gsteps1$estimate[2]), 
     type="l", xlim=c(8000, 9600), ylab="Density", xlab="Steps Grupo 1", main="Ajuste sobre distribuci贸n Gamma", col="gold")
plot(density(steps1), xlim = c(8000, 9600), col = "gold", xlab = "Steps 1", ylab="Density", main="Funci贸n de densidad emp铆rica")

par(mfrow=c(2,2))
plot(y2,dnorm(y2, nsteps2$estimate[1], nsteps2$estimate[2]),  xlim=c(9600,11700),
     type="l", ylab="Density", xlab="Steps Grupo 2", main="Ajuste sobre distribuci贸n Normal", col="red")
plot(cerouno, dexp(cerouno, esteps2$estimate[1]),  xlim=c(0,1),
     type="l", ylab="Density", xlab="Steps Grupo 2", main="Ajuste sobre distribuci贸n Exponencial", col="red", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(9600, 11700, by=600))
plot(y2,dgamma(y2, gsteps2$estimate[1], gsteps2$estimate[2]),  xlim=c(9600,11700),
     type="l", ylab="Density", xlab="Steps Grupo 2", main="Ajuste sobre distribuci贸n Gamma", col="red")
plot(density(steps2), xlim = c(9600, 11700), col = "red", xlab = "Steps 3", ylab="Density", main="Funci贸n de densidad emp铆rica")


par(mfrow=c(2,2))
plot(y3,dnorm(y3, nsteps3$estimate[1], nsteps3$estimate[2]),
     type="l", ylab="Density", xlab="Steps Grupo 3", main="Ajuste sobre distribuci贸n Normal", col="blue")
plot(cerouno, dexp(cerouno, esteps3$estimate[1]),
     type="l", ylab="Density",  xlab="Steps Grupo 3", main="Ajuste sobre distribuci贸n Exponencial", col="blue", xlim=c(0,1), xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(11700, 13600, by=600))
plot(y3,dgamma(y3, gsteps3$estimate[1], gsteps3$estimate[2]),
     type="l", ylab="Density",  xlab="Steps Grupo 3", main="Ajuste sobre distribuci贸n Gamma", col="blue")
plot(density(steps3), xlim = c(11700, 13600), col = "blue", xlab = "Steps 3", ylab="Density", main="Funci贸n de densidad emp铆rica")

ks.test(steps1, pnorm, nsteps1$estimate[1], nsteps1$estimate[2])
ks.test(steps1, pgamma, gsteps1$estimate[1], gsteps1$estimate[2])
ks.test(steps1, pexp, esteps1$estimate[1])

ks.test(steps2, pnorm, nsteps2$estimate[1], nsteps2$estimate[2])
ks.test(steps2, pgamma, gsteps2$estimate[1], gsteps2$estimate[2])
ks.test(steps2, pexp, nsteps2$estimate[1], nsteps2$estimate[2])

ks.test(steps3, pnorm, nsteps3$estimate[1], nsteps3$estimate[2])
ks.test(steps3, pgamma, gsteps3$estimate[1], gsteps3$estimate[2])
ks.test(steps3, pexp, esteps3$estimate[1])


par(mfrow=c(2,2))
plot(y1, pnorm(y1, mean=nsteps1$estimate[1], sd=nsteps1$estimate[2]),
     col = "blue", type="l",  xlim=c(7000, 9600), ylim = c(0, 1), xlab = "Steps Grupo 1", ylab="Probabilidad acumulada", main="ECDF vs normal CDF")
plot(ecdf(steps1), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))

plot(y1, pgamma(y1, gsteps1$estimate[1], gsteps1$estimate[2]),
     col = "blue", type="l", xlim=c(7000, 9600), ylim = c(0, 1), xlab = "Steps Grupo 1", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(steps1), col = "red",  add = TRUE)
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))

plot(cerouno, pexp(cerouno, esteps1$estimate[1]),
     col = "blue", type="l",  xlim=c(0, 1), ylim = c(0, 1), xlab = "Steps Grupo 1", ylab="Probabilidad acumulada", main="ECDF vs exponential CDF", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(8000, 9500, by=500))
plot(ecdf((steps1-min(steps1))/max(steps1)), col = "red", add=TRUE)
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))

par(mfrow=c(2,2))
plot(y2, pnorm(y2, mean=nsteps2$estimate[1], sd=nsteps2$estimate[2]), 
     col = "blue", type="l",  xlim=c(9600,11700), ylim = c(0, 1),xlab = "Steps Grupo 2", ylab="Probabilidad acumulada", main="ECDF vs normal CDF")
plot(ecdf(steps2), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))
plot(y2, pgamma(y2, gsteps2$estimate[1], gsteps2$estimate[2]),
     col = "blue", type="l", xlim=c(9600,11700), ylim = c(0, 1),xlab = "Steps Grupo 2", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(steps2), col = "red",  add = TRUE)
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))
plot(cerouno, pexp(cerouno, esteps2$estimate[1]), 
     col = "blue", type="l",  xlim=c(0,1), ylim = c(0, 1), xlab = "Steps Grupo 2", ylab="Probabilidad acumulada", main="ECDF vs exponential CDF", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(9600, 11700, by=600))
plot(ecdf((steps2-min(steps2))/max(steps2)), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))

par(mfrow=c(2,2))
plot(y3, pnorm(y3, mean=nsteps3$estimate[1], sd=nsteps3$estimate[2]), 
     col = "blue", type="l",  xlim=c(11700, 13600), ylim = c(0, 1), xlab = "Steps Grupo 3", ylab="Probabilidad acumulada", main="ECDF vs normal CDF")
plot(ecdf(steps3), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))
plot(y3, pgamma(y3, gsteps3$estimate[1], gsteps3$estimate[2]), 
     col = "blue", type="l", xlim=c(11700, 13600), ylim = c(0, 1), xlab = "Steps Grupo 3", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(steps3), col = "red",  add = TRUE)
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))
plot(cerouno, pexp(cerouno, esteps3$estimate[1]), 
     col = "blue", type="l",  xlim=c(0, 1), ylim = c(0, 1), xlab = "Steps Grupo 3", ylab="Probabilidad acumulada", main="ECDF vs exponential CDF", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(11700, 13600, by=600))
plot(ecdf((steps3-min(steps3))/max(steps3)), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Emp铆rica", "Te贸rica"), fill = c("red", "blue"))


# MUESTREO

plot(density(Data$Age), main="Densidad de Age", xlab="Age")
head(Data)

agemas30 <- c()
agemas50 <-  c()
agemas100 <- c()

mean30 <- c()
mean50 <-  c()
mean100 <- c()


var30 <- c()
var50 <-  c()
var100 <- c()

male <- Data[Data$Sex=="V",]
female <- Data[Data$Sex=="M",]

popM30 <- c()
popM50 <-  c()
popM100 <- c()

popF30 <- c()
popF50 <-  c()
popF100 <- c()

set.seed(2021)
for(i in 1:30) {
  agemas30 <- rbind(agemas30, Data$Age[sample(dim(Data)[1],200)])
  mean30[i] <- mean(agemas30[i,])
  var30[i] <- var(agemas30[i,])
  # propM30 <- male$Age[sample(dim(male)[1],30)]
  # propF30 <- female$Age[sample(dim(female)[1],30)]
}

set.seed(634)
for(i in 1:50) {
  agemas50 <- rbind(agemas50, Data$Age[sample(dim(Data)[1],200)])
  mean50[i] = mean(agemas50[i,])
  var50[i] = var(agemas50[i,])
  # propM50 <- male$Age[sample(dim(male)[1],50)]
  # propF50 <- female$Age[sample(dim(female)[1],50)]
}

set.seed(1927)
for(i in 1:100) {
  agemas100 <- rbind(agemas100, Data$Age[sample(dim(Data)[1],200)])
  mean100[i] = mean(agemas100[i,])
  var100[i] = var(agemas100[i,])
  # propM100 <- male$Age[sample(dim(male)[1],100)]
  # propF100 <- female$Age[sample(dim(female)[1],100)]
}

par(mfrow=c(1,1))
hist(mean30, freq=FALSE, breaks=10, main="Histograma de distribuci贸n de media muestral en grupo de tama帽o 30", xlab="Mean", col="gold")
hist(mean50, freq=FALSE, breaks=10, main="Histograma de distribuci贸n de media muestral en grupo de tama帽o 50", xlab="Mean", col="red")
hist(mean100, freq=FALSE, breaks=10, main="Histograma de distribuci贸n de media muestral en grupo de tama帽o 100", xlab="Mean", col="blue")

mean(Data$Age)
var(Data$Age)

boxplot(mean30, main="Distribuci贸n de media muestral en grupo de tama帽o 30", col="gold")
boxplot(mean50, main="Distribuci贸n de media muestral en grupo de tama帽o 50", col="red")
boxplot(mean100, main="Distribuci贸n de media muestral en grupo de tama帽o 100", col="blue")

normean30 <- MASS::fitdistr(mean30, "normal")
normean50 <- MASS::fitdistr(mean50, "normal")
normean100 <- MASS::fitdistr(mean100, "normal")

par(mfrow=c(1,1))
xmean=seq(25.5, 26.4, by=0.001)
plot(xmean, dnorm(xmean, normean30$estimate[1], normean30$estimate[2]),
     type="l", col="gold", lwd=1.5, main="Distribuci贸n de la media muestral", xlab="Media", ylab="Densidad")
lines(xmean, dnorm(xmean, normean50$estimate[1], normean50$estimate[2]), col="red",lwd=1.5)
lines(xmean, dnorm(xmean, normean100$estimate[1], normean100$estimate[2]), col="blue", lwd=1.5)
abline(v=mean(Data$Age), col="green")
legend(x="topright", legend = c("Grupo de 30", "Grupo de 50", "Grupo de 100", "Media de Age"), fill = c("gold", "red", "blue", "green"))

hist(var30, freq=FALSE, breaks=10,  main="Histograma de distribuci贸n de varianza muestral en grupo de tama帽o 30", xlab="Varianza", col="gold")
hist(var50, freq=FALSE,  breaks=10, main="Histograma de distribuci贸n de varianza muestral en grupo de tama帽o 50", xlab="Varianza", col="red")
hist(var100, freq=FALSE,  breaks=10, main="Histograma de distribuci贸n de varianza muestral en grupo de tama帽o 100", xlab="Varianza", col="blue")

boxplot(var30, main="Distribuci贸n de varianza muestral en grupo de tama帽o 30", col="gold")
boxplot(var50, main="Distribuci贸n de varianza muestral en grupo de tama帽o 50", col="red")
boxplot(var100, main="Distribuci贸n de varianza muestral en grupo de tama帽o 100", col="blue")

normvar30 <- MASS::fitdistr(var30, "normal")
normvar50 <- MASS::fitdistr(var50, "normal")
normvar100 <- MASS::fitdistr(var100, "normal")

xvar=seq(8.5, 14, by=0.01)
par(mfrow=c(1,1))
xmean=seq(25.5, 26.4, by=0.001)
plot(xvar, dnorm(xvar, normvar30$estimate[1], normvar30$estimate[2]),
     type="l", col="gold", lwd=1.5, main="Distribuci贸n de la varianza muestral", xlab="Varianza", ylab="Densidad")
lines(xvar, dnorm(xvar, normvar50$estimate[1], normvar50$estimate[2]), col="red",lwd=1.5)
lines(xvar, dnorm(xvar, normvar100$estimate[1], normvar100$estimate[2]), col="blue", lwd=1.5)
abline(v=var(Data$Age), col="green")
legend(x="topright", legend = c("Grupo de 30", "Grupo de 50", "Grupo de 100", "Varianza de Age"), fill = c("gold", "red", "blue", "green"))


# PARTE 2

# 2.1 Estimaci髇 Puntual

# Media y varianza de Data$sleeptime con el conjunto de datos completo

mean(Data$sleeptime)
var(Data$sleeptime)

# Media y varianza de Data$sleeptime con muestras de 200
# En el enunciado se utiliza el plural (muestras), pero no se 
# especifica cuantas, por lo que hemos considerado hacer 10.

stSample <- c()

mean10 <- c()

var10 <- c()

set.seed(1892)
for(i in 1:10) {
  stSample <- rbind(stSample, Data$sleeptime[sample(dim(Data)[1],200)])
  mean10[i] <- mean(stSample[i,])
  var10[i] <- var(stSample[i,])
}

# Media y varianza de Data$steps con el conjunto de datos completo

mean(Data$steps)
var(Data$steps)

# Media y varianza de Data$steps con muestras de 200
# En el enunciado se utiliza el plural (muestras), pero no se 
# especifica cuantas, por lo que hemos considerado hacer 20.

stepsSample <- c()

mean20 <- c()

var20 <- c()

set.seed(1897)
for(i in 1:10) {
  stepsSample <- rbind(stepsSample, Data$steps[sample(dim(Data)[1],200)])
  mean20[i] <- mean(stepsSample[i,])
  var20[i] <- var(stetpsSample[i,])
}

# Lo mismo de antes, entre las mujeres

# Media y varianza de Data$sleeptime con el conjunto de datos completo

female <- Data[Data$Sex=="M",]

mean(female$sleeptime)
var(female$sleeptime)

# Media y varianza de Data$sleeptime con muestras de 200
# En el enunciado se utiliza el plural (muestras), pero no se 
# especifica cuantas, por lo que hemos considerado hacer 10.

stfSample <- c()

meanf10 <- c()

varf10 <- c()

set.seed(1792)
for(i in 1:10) {
  stfSample <- rbind(stfSample, female$sleeptime[sample(dim(female)[1],200)])
  meanf10[i] <- mean(stfSample[i,])
  varf10[i] <- var(stfSample[i,])
}

# Lo mismo de antes, entre los varones

# Media y varianza de Data$sleeptime con el conjunto de datos completo

male <- Data[Data$Sex=="V",]

mean(male$sleeptime)
var(male$sleeptime)

# Media y varianza de Data$sleeptime con muestras de 200
# En el enunciado se utiliza el plural (muestras), pero no se 
# especifica cuantas, por lo que hemos considerado hacer 10.

stmSample <- c()

meanm10 <- c()

varm10 <- c()

set.seed(1772)
for(i in 1:10) {
  stmSample <- rbind(stmSample, male$sleeptime[sample(dim(male)[1],200)])
  meanm10[i] <- mean(stmSample[i,])
  varm10[i] <- var(stmSample[i,])
}

# 2.2 Estimaci髇 por intervalos, una poblaci髇

# muestras de tama駉 200

# IC del 90% para media con varianza desconocida 
# Par醡etro: Data$sleeptime

ic90st <- c()

ic90st <- rbind(ic90st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic90st <- mean(ic90st)
sic90st <- sqrt(var(ic90st[1:200]))

std_err_IC90st <- qt(0.05,199,lower.tail = F)*sic90st/sqrt(dim(ic90st)[2])

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st

# Par醡etro: Data$steps

ic90steps <- c()

ic90steps <- rbind(ic90steps, Data$steps[sample(dim(Data)[1],200)])
meanic90steps <- mean(ic90steps)
sic90steps <- sqrt(var(ic90steps[1:200]))

std_err_IC90steps <- qt(0.05,199,lower.tail = F)*sic90steps/sqrt(dim(ic90steps)[2])

mediaIC90max_steps <- meanic90steps + std_err_IC90steps
mediaIC90min_steps <- meanic90steps - std_err_IC90steps

# IC del 95% para media con varianza desconocida 
# Par醡etro: Data$sleeptime

ic95st <- c()

ic95st <- rbind(ic95st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic95st <- mean(ic95st)
sic95st <- sqrt(var(ic95st[1:200]))

std_err_IC95st <- qt(0.025,199,lower.tail = F)*sic95st/sqrt(dim(ic95st)[2])

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Par醡etro: Data$steps

ic95steps <- c()

ic95steps <- rbind(ic95steps, Data$steps[sample(dim(Data)[1],200)])
meanic95steps <- mean(ic95steps)
sic95steps <- sqrt(var(ic95steps[1:200]))

std_err_IC95steps <- qt(0.025,199,lower.tail = F)*sic95steps/sqrt(dim(ic95steps)[2])

mediaIC95max_steps <- meanic95steps + std_err_IC95steps
mediaIC95min_steps <- meanic95steps - std_err_IC95steps

# IC del 99% para media con varianza desconocida 
# Par醡etro: Data$sleeptime

ic99st <- c()

ic99st <- rbind(ic99st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic99st <- mean(ic99st)
sic99st <- sqrt(var(ic99st[1:200]))

std_err_IC99st <- qt(0.005,199,lower.tail = F)*sic99st/sqrt(dim(ic99st)[2])

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Par醡etro: Data$steps

ic99steps <- c()

ic99steps <- rbind(ic99steps, Data$steps[sample(dim(Data)[1],200)])
meanic99steps <- mean(ic99steps)
sic99steps <- sqrt(var(ic99steps[1:200]))

std_err_IC99steps <- qt(0.005,199,lower.tail = F)*sic99steps/sqrt(dim(ic99steps)[2])

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% para media con varianza conocida
# z (multiplicador de confianza) = 1.645
# Par醡etro: Data$sleeptime

ic90st <- c()

ic90st <- rbind(ic90st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic90st <- mean(ic90st)
varic90st <- var(ic90st[1:200])
zIC90 <- 1.645

std_err_IC90st <- zIC90*varic90st/sqrt(dim(ic90st)[2])

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st

# Par醡etro: Data$steps

ic90steps <- c()

ic90steps <- rbind(ic90steps, Data$steps[sample(dim(Data)[1],200)])
meanic90steps <- mean(ic90steps)
varic90steps <- var(ic90steps[1:200])
zIC90 <- 1.645

std_err_IC90steps <- zIC90*varic90steps/sqrt(dim(ic90steps)[2])

mediaIC90max_steps <- meanic90steps + std_err_IC90steps
mediaIC90min_steps <- meanic90steps - std_err_IC90steps

# IC del 95% para media con varianza conocida 
# z (multiplicador de confianza) = 2
# Par醡etro: Data$sleeptime

ic95st <- c()

ic95st <- rbind(ic95st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic95st <- mean(ic95st)
varic95st <- var(ic95st[1:200])
zIC95 <- 2

std_err_IC95st <- zIC95*varic95st/sqrt(dim(ic95st)[2])

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Par醡etro: Data$steps

ic95steps <- c()

ic95steps <- rbind(ic95steps, Data$steps[sample(dim(Data)[1],200)])
meanic95steps <- mean(ic95steps)
varic95steps <- var(ic95steps[1:200])
z95IC <- 2

std_err_IC95steps <- zIC95*varic95steps/sqrt(dim(ic95steps)[2])

mediaIC95max_steps <- meanic95steps + std_err_IC95steps
mediaIC95min_steps <- meanic95steps - std_err_IC95steps

# IC del 99% para media con varianza conocida 
# z (multiplicador de confianza) = 2.576
# Par醡etro: Data$sleeptime

ic99st <- c()

ic99st <- rbind(ic99st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic99st <- mean(ic99st)
varic99st <- var(ic99st[1:200])
zIC99 <- 2.576

std_err_IC99st <- zIC99*varic99st/sqrt(dim(ic99st)[2])

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Par醡etro: Data$steps

ic99steps <- c()

ic99steps <- rbind(ic99steps, Data$steps[sample(dim(Data)[1],200)])
meanic99steps <- mean(ic99steps)
varic99steps <- var(ic99steps[1:200])
zIC99 <- 2.576

std_err_IC99steps <- zIC99*varic99steps/sqrt(dim(ic99steps)[2])

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% para varianza
# Par醡etro: Data$sleeptime

ic90st2 <- c()

ic90st2 <- rbind(ic90st2, Data$sleeptime[sample(dim(Data)[1],200)])
s2IC90st <- var(ic90st2[1:200])

minIC90st2 <- (199*s2IC90st)/qchisq(0.05,199,lower.tail = FALSE)
maxIC90st2 <- (199*s2IC90st)/qchisq(0.95,199,lower.tail = FALSE)

# Par醡etro: Data$steps

ic90steps2 <- c()

ic90steps2 <- rbind(ic90steps2, Data$steps[sample(dim(Data)[1],200)])
s2IC90steps <- var(ic90steps2[1:200])

minIC90steps2 <- (199*s2IC90steps)/qchisq(0.05,199,lower.tail = FALSE)
maxIC90steps2 <- (199*s2IC90steps)/qchisq(0.95,199,lower.tail = FALSE)

# IC del 95% para varianza
# Par醡etro: Data$sleeptime

ic95st2 <- c()

ic95st2 <- rbind(ic95st2, Data$sleeptime[sample(dim(Data)[1],200)])
s2IC95st <- var(ic95st2[1:200])

minIC95st2 <- (199*s2IC95st)/qchisq(0.025,199,lower.tail = FALSE)
maxIC95st2 <- (199*s2IC95st)/qchisq(0.975,199,lower.tail = FALSE)

# Par醡etro: Data$steps

ic95steps2 <- c()

ic95steps2 <- rbind(ic95steps2, Data$steps[sample(dim(Data)[1],200)])
s2IC95steps <- var(ic95steps2[1:200])

minIC95steps2 <- (199*s2IC90steps)/qchisq(0.025,199,lower.tail = FALSE)
maxIC95steps2 <- (199*s2IC90steps)/qchisq(0.975,199,lower.tail = FALSE)

# IC del 99% para varianza
# Par醡etro: Data$sleeptime

ic99st2 <- c()

ic99st2 <- rbind(ic99st2, Data$sleeptime[sample(dim(Data)[1],200)])
s2IC99st <- var(ic99st2[1:200])

minIC99st2 <- (199*s2IC99st)/qchisq(0.005,199,lower.tail = FALSE)
maxIC99st2 <- (199*s2IC99st)/qchisq(0.995,199,lower.tail = FALSE)

# Par醡etro: Data$steps

ic99steps2 <- c()

ic99steps2 <- rbind(ic99steps2, Data$steps[sample(dim(Data)[1],200)])
s2IC99steps <- var(ic99steps2[1:200])

minIC99steps2 <- (199*s2IC99steps)/qchisq(0.005,199,lower.tail = FALSE)
maxIC99steps2 <- (199*s2IC99steps)/qchisq(0.995,199,lower.tail = FALSE)

# 2.3 Estimaci髇 por intervalos, dos poblaciones (M y V)

male <- Data[Data$Sex=="V",]
female <- Data[Data$Sex=="M",]

# IC del 90% dif medias con varianza desconocida pero iguales
# Par醡etro: Data$sleeptime

t.test(female$sleeptime,male$sleeptime,conf.level = 0.90,alternative = "greater",var.equal = TRUE)

# Par醡etro: Data$steps

t.test(female$steps,male$steps,conf.level = 0.90,alternative = "greater",var.equal = TRUE)


# IC del 95% dif medias con varianza desconocida pero iguales
# Par醡etro: Data$sleeptime

t.test(female$sleeptime,male$sleeptime,conf.level = 0.95,alternative = "greater",var.equal = TRUE)

# Par醡etro: Data$steps

t.test(female$steps,male$steps,conf.level = 0.95,alternative = "greater",var.equal = TRUE)

# IC del 99% dif medias con varianza desconocida pero iguales
# Par醡etro: Data$sleeptime

t.test(female$sleeptime,male$sleeptime,conf.level = 0.99,alternative = "greater",var.equal = TRUE)

# Par醡etro: Data$steps

t.test(female$steps,male$steps,conf.level = 0.99,alternative = "greater",var.equal = TRUE)

# IC del 90% dif medias con varianza desconocida y diferentes
# Par醡etro: Data$sleeptime

t.test(female$sleeptime,male$sleeptime,conf.level = 0.90,alternative = "greater", var.equal = FALSE)

# Par醡etro: Data$steps

t.test(female$steps,male$steps,conf.level = 0.90,alternative = "greater",var.equal = FALSE)

# IC del 95% dif medias con varianza desconocida y diferentes
# Par醡etro: Data$sleeptime

t.test(female$sleeptime,male$sleeptime,conf.level = 0.95,alternative = "greater", var.equal = FALSE)

# Par醡etro: Data$steps

t.test(female$steps,male$steps,conf.level = 0.95,alternative = "greater",var.equal = FALSE)

# IC del 99% dif medias con varianza desconocida y diferentes
# Par醡etro: Data$sleeptime

t.test(female$sleeptime,male$sleeptime,conf.level = 0.99,alternative = "greater", var.equal = FALSE)

# Par醡etro: Data$steps

t.test(female$steps,male$steps,conf.level = 0.99,alternative = "greater",var.equal = FALSE)

# IC del 90% dif medias con varianza conocida
# Par醡etro: Data$sleeptime

varfem_IC90st <- var(female$sleeptime)/dim(female)[1]
varm_IC90st <- var(male$sleeptime)/dim(male)[1]

lim_infIC90st <- mean(female$sleeptime) - mean(male$sleeptime) - qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)
lim_supIC90st <- mean(female$sleeptime) - mean(male$sleeptime) + qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)

# Par醡etro: Data$steps

varfem_IC90steps <- var(female$steps)/dim(female)[1]
varm_IC90steps <- var(male$steps)/dim(male)[1]

lim_infIC90steps <- mean(female$steps) - mean(male$steps) - qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)
lim_supIC90steps <- mean(female$steps) - mean(male$steps) + qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)

# IC del 95% dif medias con varianza conocida
# Par醡etro: Data$sleeptime

varfem_IC95st <- var(female$sleeptime)/dim(female)[1]
varm_IC95st <- var(male$sleeptime)/dim(male)[1]

lim_infIC95st <- mean(female$sleeptime) - mean(male$sleeptime) - qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)
lim_supIC95st <- mean(female$sleeptime) - mean(male$sleeptime) + qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)

# Par醡etro: Data$steps

varfem_IC95steps <- var(female$steps)/dim(female)[1]
varm_IC95steps <- var(male$steps)/dim(male)[1]

lim_infIC95steps <- mean(female$steps) - mean(male$steps) - qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)
lim_supIC95steps <- mean(female$steps) - mean(male$steps) + qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)

# IC del 99% dif medias con varianza conocida
# Par醡etro: Data$sleeptime

varfem_IC99st <- var(female$sleeptime)/dim(female)[1]
varm_IC99st <- var(male$sleeptime)/dim(male)[1]

lim_infIC99st <- mean(female$sleeptime) - mean(male$sleeptime) - qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)
lim_supIC99st <- mean(female$sleeptime) - mean(male$sleeptime) + qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)

# Par醡etro: Data$steps

varfem_IC99steps <- var(female$steps)/dim(female)[1]
varm_IC99steps <- var(male$steps)/dim(male)[1]

lim_infIC99steps <- mean(female$steps) - mean(male$steps) - qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)
lim_supIC99steps <- mean(female$steps) - mean(male$steps) + qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)

# IC del 90% para raz髇 de varianzas
# Par醡etro Data$sleeptime

lim_inf_razvar_IC90st <- (var(female$sleeptime)/var(male$sleeptime))*(1/qf(0.05,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC90st <- (var(female$sleeptime)/var(male$sleeptime))*qf(0.05,dim(male)[1],dim(female)[1],lower.tail = F)

# Par醡etro Data$steps

lim_inf_razvar_IC90steps <- (var(female$steps)/var(male$steps))*(1/qf(0.05,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC90steps <- (var(female$steps)/var(male$steps))*qf(0.05,dim(male)[1],dim(female)[1],lower.tail = F)

# IC del 95% para raz髇 de varianzas
# Par醡etro Data$sleeptime

lim_inf_razvar_IC95st <- (var(female$sleeptime)/var(male$sleeptime))*(1/qf(0.025,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC95st <- (var(female$sleeptime)/var(male$sleeptime))*qf(0.025,dim(male)[1],dim(female)[1],lower.tail = F)

# Par醡etro Data$steps

lim_inf_razvar_IC95steps <- (var(female$steps)/var(male$steps))*(1/qf(0.025,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC95steps <- (var(female$steps)/var(male$steps))*qf(0.025,dim(male)[1],dim(female)[1],lower.tail = F)

# IC del 99% para raz髇 de varianzas
# Par醡etro Data$sleeptime

lim_inf_razvar_IC99st <- (var(female$sleeptime)/var(male$sleeptime))*(1/qf(0.005,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC99st <- (var(female$sleeptime)/var(male$sleeptime))*qf(0.005,dim(male)[1],dim(female)[1],lower.tail = F)

# Par醡etro Data$steps

lim_inf_razvar_IC99steps <- (var(female$steps)/var(male$steps))*(1/qf(0.005,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC99steps <- (var(female$steps)/var(male$steps))*qf(0.005,dim(male)[1],dim(female)[1],lower.tail = F)

# Idem con poblaciones generales
# Muestra de tama駉 200

male <- Data[Data$Sex=="V",]
female <- Data[Data$Sex=="M",]

fem200st <- c()
fem200steps <- c()
male200st <- c()
male200steps <- c()

fem200st <- rbind(fem200st, female$sleeptime[sample(dim(Data)[1],200)])
fem200steps <- rbind(fem200steps, female$steps[sample(dim(Data)[1],200)])
male200st <- rbind(male200st, male$sleeptime[sample(dim(Data)[1],200)])
male200steps <- rbind(male200steps, male$steps[sample(dim(Data)[1],200)])

# IC del 90% dif medias con varianza desconocida pero iguales
# Par醡etro: Data$sleeptime

t.test(fem200st,male200st,conf.level = 0.90,alternative = "greater",var.equal = TRUE)

# Par醡etro: Data$steps

t.test(fem200steps,male200steps,conf.level = 0.90,alternative = "greater",var.equal = TRUE)


# IC del 95% dif medias con varianza desconocida pero iguales
# Par醡etro: Data$sleeptime

t.test(fem200st,male200st,conf.level = 0.95,alternative = "greater",var.equal = TRUE)

# Par醡etro: Data$steps

t.test(fem200steps,male200steps,conf.level = 0.95,alternative = "greater",var.equal = TRUE)

# IC del 99% dif medias con varianza desconocida pero iguales
# Par醡etro: Data$sleeptime

t.test(fem200st,male200st,conf.level = 0.99,alternative = "greater",var.equal = TRUE)

# Par醡etro: Data$steps

t.test(fem200steps,male200steps,conf.level = 0.99,alternative = "greater",var.equal = TRUE)

# IC del 90% dif medias con varianza desconocida y diferentes
# Par醡etro: Data$sleeptime

t.test(fem200st,male200st,conf.level = 0.90,alternative = "greater", var.equal = FALSE)

# Par醡etro: Data$steps

t.test(fem200steps,male200steps,conf.level = 0.90,alternative = "greater",var.equal = FALSE)

# IC del 95% dif medias con varianza desconocida y diferentes
# Par醡etro: Data$sleeptime

t.test(fem200st,male200st,conf.level = 0.95,alternative = "greater", var.equal = FALSE)

# Par醡etro: Data$steps

t.test(fem200steps,male200steps,conf.level = 0.95,alternative = "greater",var.equal = FALSE)

# IC del 99% dif medias con varianza desconocida y diferentes
# Par醡etro: Data$sleeptime

t.test(fem200st,male200st,conf.level = 0.99,alternative = "greater", var.equal = FALSE)

# Par醡etro: Data$steps

t.test(fem200steps,male200steps,conf.level = 0.99,alternative = "greater",var.equal = FALSE)

# IC del 90% dif medias con varianza conocida
# Par醡etro: Data$sleeptime

varfem_IC90st <- var(fem200st)/200
varm_IC90st <- var(male200st)/200

lim_infIC90st <- mean(fem200st) - mean(male200st) - qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)
lim_supIC90st <- mean(fem200st) - mean(male200st) + qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)

# Par醡etro: Data$steps

varfem_IC90steps <- var(fem200steps)/200
varm_IC90steps <- var(male200steps)/200

lim_infIC90steps <- mean(fem200steps) - mean(male200steps) - qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)
lim_supIC90steps <- mean(fem200steps) - mean(male200steps) + qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)

# IC del 95% dif medias con varianza conocida
# Par醡etro: Data$sleeptime

varfem_IC95st <- var(fem200st)/200
varm_IC95st <- var(male200st)/200

lim_infIC95st <- mean(fem200st) - mean(male200st) - qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)
lim_supIC95st <- mean(fem200st) - mean(male200st) + qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)

# Par醡etro: Data$steps

varfem_IC95steps <- var(fem200steps)/200
varm_IC95steps <- var(male200steps)/200

lim_infIC95steps <- mean(fem200steps) - mean(male200steps) - qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)
lim_supIC95steps <- mean(fem200steps) - mean(male200steps) + qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)

# IC del 99% dif medias con varianza conocida
# Par醡etro: Data$sleeptime

varfem_IC99st <- var(fem200st)/200
varm_IC99st <- var(male200st)/200

lim_infIC99st <- mean(fem200st) - mean(male200st) - qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)
lim_supIC99st <- mean(fem200st) - mean(male200st) + qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)

# Par醡etro: Data$steps

varfem_IC99steps <- var(fem200steps)/200
varm_IC99steps <- var(male200steps)/200

lim_infIC99steps <- mean(fem200steps) - mean(male200steps) - qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)
lim_supIC99steps <- mean(fem200steps) - mean(male200steps) + qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)

# IC del 90% para raz髇 de varianzas
# Par醡etro Data$sleeptime

lim_inf_razvar_IC90st <- (var(fem200st)/var(male200st))*(1/qf(0.05,200,200,lower.tail = F))
lim_sup_razvar_IC90st <- (var(fem200st)/var(male200st))*qf(0.05,200,200,lower.tail = F)

# Par醡etro Data$steps

lim_inf_razvar_IC90steps <- (var(fem200steps)/var(male200steps))*(1/qf(0.05,200,200,lower.tail = F))
lim_sup_razvar_IC90steps <- (var(fem200steps)/var(male200steps))*qf(0.05,200,200,lower.tail = F)

# IC del 95% para raz髇 de varianzas
# Par醡etro Data$sleeptime

lim_inf_razvar_IC95st <- (var(fem200st)/var(male200st))*(1/qf(0.025,200,200,lower.tail = F))
lim_sup_razvar_IC95st <- (var(fem200st)/var(male200st))*qf(0.025,200,200,lower.tail = F)

# Par醡etro Data$steps

lim_inf_razvar_IC95steps <- (var(fem200steps)/var(male200steps))*(1/qf(0.025,200,200,lower.tail = F))
lim_sup_razvar_IC95steps <- (var(fem200steps)/var(male200steps))*qf(0.025,200,200,lower.tail = F)

# IC del 99% para raz髇 de varianzas
# Par醡etro Data$sleeptime

lim_inf_razvar_IC99st <- (var(fem200st)/var(male200st))*(1/qf(0.005,200,200,lower.tail = F))
lim_sup_razvar_IC99st <- (var(fem200st)/var(male200st))*qf(0.005,200,200,lower.tail = F)

# Par醡etro Data$steps

lim_inf_razvar_IC99steps <- (var(fem200steps)/var(male200steps))*(1/qf(0.005,200,200,lower.tail = F))
lim_sup_razvar_IC99steps <- (var(fem200steps)/var(male200steps))*qf(0.005,200,200,lower.tail = F)







