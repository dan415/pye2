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
     type="l", ylab="Density", xlab="Sleeptime", main="Ajuste sobre distribución Normal")
plot(x, dexp(x, esleep$estimate[1]), 
     type="l", ylab="Density", xlab="Sleeptime", main="Ajuste sobre distribución Exponencial")
plot(x,dgamma(x, gsleep$estimate[1], gsleep$estimate[2]), 
     type="l", ylab="Density", xlab="Sleeptime", main="Ajuste sobre distribución Gamma")

ks.test(Data$sleeptime, pnorm, nsleep$estimate[1], nsleep$estimate[2])
ks.test(Data$sleeptime, pexp, esleep$estimate[1])
ks.test(Data$sleeptime, pgamma, gsleep$estimate[1], gsleep$estimate[2])

par(mfrow=c(2,2))
x=seq(0, 23, by=0.001)
plot(x, pnorm(x, mean=nsleep$estimate[1], sd=nsleep$estimate[2]),
     col = "blue", type="l", xlim = c(0, 23), ylim = c(0, 1), xlab = "Sleeptime", ylab="Probabilidad acumulada", main="ECDF vs normal CDF")
plot(ecdf(Data$sleeptime), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))

plot(x, pgamma(x, gsleep$estimate[1], gsleep$estimate[2]), 
     col = "blue", type="l", xlim = c(0, 23), ylim = c(0, 1), xlab = "Sleeptime", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(Data$sleeptime), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))

plot(x, pexp(x, esleep$estimate[1]),
     col = "blue", type="l", xlim = c(0, 23), ylim = c(0, 1), xlab = "Sleeptime", ylab="Probabilidad acumulada",main="ECDF vs exponential CDF")
plot(ecdf(Data$sleeptime), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))

par(mfrow=c(1,2))
x=seq(0, 23, by=0.001)
plot(x, dnorm(x, mean=nsleep$estimate[1], sd=nsleep$estimate[2]),
     col = "blue", type="l", xlim = c(0, 23), ylim=c(0,0.1), xlab = "Sleeptime", ylab="Density", main="Función de densidad normal")
plot(density(Data$sleeptime), xlim = c(0, 23), col = "red", xlab = "Sleeptime", ylab="Density", main="Función de densidad empírica")


par(mfrow=c(1,2))
x=seq(0, 23, by=0.001)
plot(x, dgamma(x, gsleep$estimate[1], gsleep$estimate[2]),
     col = "blue", type="l", xlim = c(0, 23), ylim=c(0,0.1), xlab = "Sleeptime", ylab="Density", main="Función de densidad gamma")
plot(density(Data$sleeptime), xlim = c(0, 23), col = "red", xlab = "Sleeptime", ylab="Density", main="Función de densidad empírica")

par(mfrow=c(1,2))
x=seq(0, 23, by=0.001)
plot(x, dexp(x, esleep$estimate[1]),
     col = "blue", type="l", xlim = c(0, 23), ylim=c(0,0.1), xlab = "Sleeptime", ylab="Density", main="Función de densidad exponencial")
plot(density(Data$sleeptime), xlim = c(0, 23), col = "red", xlab = "Sleeptime", ylab="Density", main="Función de densidad empírica")


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
     type="l", xlim=c(8000, 9600), ylab="Density", xlab="Steps Grupo 1", main="Ajuste sobre distribución Normal", col="gold")
plot(cerouno, dexp(cerouno, esteps1$estimate[1]), xlim=c(0, 1),
     type="l", ylab="Density", xlab="Steps Grupo 1", main="Ajuste sobre distribución Exponencial", col="gold", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(8000, 9500, by=500))
plot(y1,dgamma(y1, gsteps1$estimate[1], gsteps1$estimate[2]), 
     type="l", xlim=c(8000, 9600), ylab="Density", xlab="Steps Grupo 1", main="Ajuste sobre distribución Gamma", col="gold")
plot(density(steps1), xlim = c(8000, 9600), col = "gold", xlab = "Steps 1", ylab="Density", main="Función de densidad empírica")

par(mfrow=c(2,2))
plot(y2,dnorm(y2, nsteps2$estimate[1], nsteps2$estimate[2]),  xlim=c(9600,11700),
     type="l", ylab="Density", xlab="Steps Grupo 2", main="Ajuste sobre distribución Normal", col="red")
plot(cerouno, dexp(cerouno, esteps2$estimate[1]),  xlim=c(0,1),
     type="l", ylab="Density", xlab="Steps Grupo 2", main="Ajuste sobre distribución Exponencial", col="red", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(9600, 11700, by=600))
plot(y2,dgamma(y2, gsteps2$estimate[1], gsteps2$estimate[2]),  xlim=c(9600,11700),
     type="l", ylab="Density", xlab="Steps Grupo 2", main="Ajuste sobre distribución Gamma", col="red")
plot(density(steps2), xlim = c(9600, 11700), col = "red", xlab = "Steps 3", ylab="Density", main="Función de densidad empírica")


par(mfrow=c(2,2))
plot(y3,dnorm(y3, nsteps3$estimate[1], nsteps3$estimate[2]),
     type="l", ylab="Density", xlab="Steps Grupo 3", main="Ajuste sobre distribución Normal", col="blue")
plot(cerouno, dexp(cerouno, esteps3$estimate[1]),
     type="l", ylab="Density",  xlab="Steps Grupo 3", main="Ajuste sobre distribución Exponencial", col="blue", xlim=c(0,1), xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(11700, 13600, by=600))
plot(y3,dgamma(y3, gsteps3$estimate[1], gsteps3$estimate[2]),
     type="l", ylab="Density",  xlab="Steps Grupo 3", main="Ajuste sobre distribución Gamma", col="blue")
plot(density(steps3), xlim = c(11700, 13600), col = "blue", xlab = "Steps 3", ylab="Density", main="Función de densidad empírica")

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
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))

plot(y1, pgamma(y1, gsteps1$estimate[1], gsteps1$estimate[2]),
     col = "blue", type="l", xlim=c(7000, 9600), ylim = c(0, 1), xlab = "Steps Grupo 1", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(steps1), col = "red",  add = TRUE)
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))

plot(cerouno, pexp(cerouno, esteps1$estimate[1]),
     col = "blue", type="l",  xlim=c(0, 1), ylim = c(0, 1), xlab = "Steps Grupo 1", ylab="Probabilidad acumulada", main="ECDF vs exponential CDF", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(8000, 9500, by=500))
plot(ecdf((steps1-min(steps1))/max(steps1)), col = "red", add=TRUE)
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))

par(mfrow=c(2,2))
plot(y2, pnorm(y2, mean=nsteps2$estimate[1], sd=nsteps2$estimate[2]), 
     col = "blue", type="l",  xlim=c(9600,11700), ylim = c(0, 1),xlab = "Steps Grupo 2", ylab="Probabilidad acumulada", main="ECDF vs normal CDF")
plot(ecdf(steps2), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))
plot(y2, pgamma(y2, gsteps2$estimate[1], gsteps2$estimate[2]),
     col = "blue", type="l", xlim=c(9600,11700), ylim = c(0, 1),xlab = "Steps Grupo 2", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(steps2), col = "red",  add = TRUE)
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))
plot(cerouno, pexp(cerouno, esteps2$estimate[1]), 
     col = "blue", type="l",  xlim=c(0,1), ylim = c(0, 1), xlab = "Steps Grupo 2", ylab="Probabilidad acumulada", main="ECDF vs exponential CDF", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(9600, 11700, by=600))
plot(ecdf((steps2-min(steps2))/max(steps2)), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))

par(mfrow=c(2,2))
plot(y3, pnorm(y3, mean=nsteps3$estimate[1], sd=nsteps3$estimate[2]), 
     col = "blue", type="l",  xlim=c(11700, 13600), ylim = c(0, 1), xlab = "Steps Grupo 3", ylab="Probabilidad acumulada", main="ECDF vs normal CDF")
plot(ecdf(steps3), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))
plot(y3, pgamma(y3, gsteps3$estimate[1], gsteps3$estimate[2]), 
     col = "blue", type="l", xlim=c(11700, 13600), ylim = c(0, 1), xlab = "Steps Grupo 3", ylab="Probabilidad acumulada", main="ECDF vs gamma CDF")
plot(ecdf(steps3), col = "red",  add = TRUE)
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))
plot(cerouno, pexp(cerouno, esteps3$estimate[1]), 
     col = "blue", type="l",  xlim=c(0, 1), ylim = c(0, 1), xlab = "Steps Grupo 3", ylab="Probabilidad acumulada", main="ECDF vs exponential CDF", xaxt="n")
axis(1, at=seq(0, 1, by=0.3), labels=seq(11700, 13600, by=600))
plot(ecdf((steps3-min(steps3))/max(steps3)), add = TRUE, col = "red")
legend(x="bottomright", legend = c("Empírica", "Teórica"), fill = c("red", "blue"))


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

prop30 <- c()
prop50 <-  c()
prop100 <- c()

prop30V <- c()
prop30F <- c()

prop50V <- c()
prop50F <- c()

prop100V <- c()
prop100F <- c()

set.seed(2021)
for(i in 1:30) {
  muestra30 <- sample(dim(Data)[1],200)
  agemas30 <- rbind(agemas30, Data$Age[muestra30])
  mean30[i] <- mean(agemas30[i,])
  var30[i] <- var(agemas30[i,])
  prop30 <- rbind(prop30, Data$Sex[muestra30])
  prop30V[i] <- table(prop30[i,])["V"]/200
  prop30F[i] <- table(prop30[i,])["M"]/200
}

set.seed(634)
for(i in 1:50) {
  muestra50 <- sample(dim(Data)[1],200)
  agemas50 <- rbind(agemas50, Data$Age[muestra50])
  mean50[i] = mean(agemas50[i,])
  var50[i] = var(agemas50[i,])
  prop50 <- rbind(prop50, Data$Sex[muestra50])
  prop50V[i] <- table(prop50[i,])["V"]/200
  prop50F[i] <- table(prop50[i,])["M"]/200
}


set.seed(1927)
for(i in 1:100) {
  muestra100 <- sample(dim(Data)[1],200)
  agemas100 <- rbind(agemas100, Data$Age[muestra100])
  mean100[i] = mean(agemas100[i,])
  var100[i] = var(agemas100[i,])
  prop100 <- rbind(prop100, Data$Sex[muestra100])
  prop100V[i] <- table(prop100[i,])["V"]/200
  prop100F[i] <- table(prop100[i,])["M"]/200
}


# MEAN
par(mfrow=c(1,1))
hist(mean30, freq=FALSE, breaks=10, main="Histograma de distribución de media muestral en grupo de tamaño 30", xlab="Mean", col="gold")
hist(mean50, freq=FALSE, breaks=10, main="Histograma de distribución de media muestral en grupo de tamaño 50", xlab="Mean", col="red")
hist(mean100, freq=FALSE, breaks=10, main="Histograma de distribución de media muestral en grupo de tamaño 100", xlab="Mean", col="blue")

mean(Data$Age)
var(Data$Age)


boxplot(mean30, main="Distribución de media muestral en grupo de tamaño 30", col="gold")
boxplot(mean50, main="Distribución de media muestral en grupo de tamaño 50", col="red")
boxplot(mean100, main="Distribución de media muestral en grupo de tamaño 100", col="blue")

normean30 <- MASS::fitdistr(mean30, "normal")
normean50 <- MASS::fitdistr(mean50, "normal")
normean100 <- MASS::fitdistr(mean100, "normal")

par(mfrow=c(1,1))
xmean=seq(25.5, 26.4, by=0.001)
plot(xmean, dnorm(xmean, normean30$estimate[1], normean30$estimate[2]),
     type="l", col="gold", lwd=1.5, main="Distribución de la media muestral", xlab="Media", ylab="Densidad")
lines(xmean, dnorm(xmean, normean50$estimate[1], normean50$estimate[2]), col="red",lwd=1.5)
lines(xmean, dnorm(xmean, normean100$estimate[1], normean100$estimate[2]), col="blue", lwd=1.5)
abline(v=mean(Data$Age), col="green")
legend(x="topright", legend = c("Grupo de 30", "Grupo de 50", "Grupo de 100", "Media de Age"), fill = c("gold", "red", "blue", "green"))


# VAR 

hist(var30, freq=FALSE, breaks=10,  main="Histograma de distribución de varianza muestral en grupo de tamaño 30", xlab="Varianza", col="gold")
hist(var50, freq=FALSE,  breaks=10, main="Histograma de distribución de varianza muestral en grupo de tamaño 50", xlab="Varianza", col="red")
hist(var100, freq=FALSE,  breaks=10, main="Histograma de distribución de varianza muestral en grupo de tamaño 100", xlab="Varianza", col="blue")

boxplot(var30, main="Distribución de varianza muestral en grupo de tamaño 30", col="gold")
boxplot(var50, main="Distribución de varianza muestral en grupo de tamaño 50", col="red")
boxplot(var100, main="Distribución de varianza muestral en grupo de tamaño 100", col="blue")

normvar30 <- MASS::fitdistr(var30, "normal")
normvar50 <- MASS::fitdistr(var50, "normal")
normvar100 <- MASS::fitdistr(var100, "normal")

xvar=seq(8.5, 14, by=0.01)
par(mfrow=c(1,1))
xmean=seq(25.5, 26.4, by=0.001)
plot(xvar, dnorm(xvar, normvar30$estimate[1], normvar30$estimate[2]),
     type="l", col="gold", lwd=1.5, main="Distribución de la varianza muestral", xlab="Varianza", ylab="Densidad")
lines(xvar, dnorm(xvar, normvar50$estimate[1], normvar50$estimate[2]), col="red",lwd=1.5)
lines(xvar, dnorm(xvar, normvar100$estimate[1], normvar100$estimate[2]), col="blue", lwd=1.5)
abline(v=var(Data$Age), col="green")
legend(x="topright", legend = c("Grupo de 30", "Grupo de 50", "Grupo de 100", "Varianza de Age"), fill = c("gold", "red", "blue", "green"))


#PROPORCION




hist(prop30V,  main="Distribución de proporción muestral de hombres en grupo de tamaño 30", xlab="Proporción", col="gold")
hist(prop50V, main="Distribución de proporción muestral de hombres en grupo de tamaño 50", xlab="Propoción", col="red")
hist(prop100V, main="Distribución de proporción muestral de hombres, grupo de tamaño 100", xlab="Proporción", col="blue")

hist(prop30F,  main="Distribución de proporción muestral de mujeres en grupo de tamaño 30", xlab="Proporción", col="gold")
hist(prop50F, main="Distribución de proporción muestral de mujeres en grupo de tamaño 50", xlab="Propoción", col="red")
hist(prop100F, main="Distribución de proporción muestral de mujeres en grupo de tamaño 100", xlab="Proporción", col="blue")

boxplot(prop30V,  main="Distribución de proporción muestral de hombres en grupo de tamaño 30", xlab="Proporción", col="gold")
boxplot(prop50V, main="Distribución de proporción muestral de hombres en grupo de tamaño 50", xlab="Propoción", col="red")
boxplot(prop100V, main="Distribución de proporción muestral de hombres en grupo de tamaño 100", xlab="Proporción", col="blue")

boxplot(prop30F,  main="Distribución de proporción muestral de mujeres en grupo de tamaño 30", xlab="Proporción", col="gold")
boxplot(prop50F, main="Distribución de proporción muestral de mujeres en grupo de tamaño 50", xlab="Propoción", col="red")
boxplot(prop100F, main="Distribución de proporción muestral de mujeres en grupo de tamaño 100", xlab="Proporción", col="blue")

normpropv30 <-  MASS::fitdistr(prop30V, "normal")
normpropv50 <-  MASS::fitdistr(prop50V, "normal")
normpropv100 <-  MASS::fitdistr(prop100V, "normal")

normpropf30 <-  MASS::fitdistr(prop30F, "normal")
normpropf50 <-  MASS::fitdistr(prop50F, "normal")
normpropf100 <-  MASS::fitdistr(prop100F, "normal")

par(mfrow=c(1,1))
xprop = seq(0.4, 0.6,by=0.01)
plot(xprop, dnorm(xprop, normpropv30$estimate[1], normpropv30$estimate[2]),
     type="l", col="gold", lwd=1.5, main="Distribución de la proporción de hombres muestral", xlab="proporción", ylab="Densidad")
lines(xprop, dnorm(xprop, normpropv50$estimate[1], normpropv50$estimate[2]), col="red",lwd=1.5)
lines(xprop, dnorm(xprop, normpropv100$estimate[1], normpropv100$estimate[2]), col="blue", lwd=1.5)
abline(v=(table(Data$Sex)["V"]/length(Data$Sex)), col="green")
legend(x="topright", legend = c("Grupo de 30", "Grupo de 50", "Grupo de 100", "Proporción de hombres"), fill = c("gold", "red", "blue", "green"))

par(mfrow=c(1,1))
xprop = seq(0.4, 0.6,by=0.01)
plot(xprop, dnorm(xprop, normpropf30$estimate[1], normpropf30$estimate[2]),
     type="l", col="gold", lwd=1.5, main="Distribución de la proporción de mujeres muestral", xlab="proporción", ylab="Densidad")
lines(xprop, dnorm(xprop, normpropf50$estimate[1], normpropf50$estimate[2]), col="red",lwd=1.5)
lines(xprop, dnorm(xprop, normpropf100$estimate[1], normpropf100$estimate[2]), col="blue", lwd=1.5)
abline(v=(table(Data$Sex)["M"]/length(Data$Sex)), col="green")
legend(x="topright", legend = c("Grupo de 30", "Grupo de 50", "Grupo de 100", "Proporción de mujeres"), fill = c("gold", "red", "blue", "green"))

table(Data$Sex)/length(Data$Sex)


# PARTE 2

# 2.1 Estimación Puntual

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
# especifica cuantas, por lo que hemos considerado hacer 10.

stepsSample <- c()

mean20 <- c()

var20 <- c()

set.seed(1897)
for(i in 1:10) {
  stepsSample <- rbind(stepsSample, Data$steps[sample(dim(Data)[1],200)])
  mean20[i] <- mean(stepsSample[i,])
  var20[i] <- var(stepsSample[i,])
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

# Media y varianza de Data$steps con el conjunto de datos completo

mean(female$steps)
var(female$steps)

# Media y varianza de Data$steps con muestras de 200
# En el enunciado se utiliza el plural (muestras), pero no se 
# especifica cuantas, por lo que hemos considerado hacer 10.

stfSample <- c()

meanf10 <- c()

varf10 <- c()

set.seed(1192)
for(i in 1:10) {
  stfSample <- rbind(stfSample, female$steps[sample(dim(female)[1],200)])
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

# Media y varianza de Data$steps con el conjunto de datos completo

mean(male$steps)
var(male$steps)

# Media y varianza de Data$steps con muestras de 200
# En el enunciado se utiliza el plural (muestras), pero no se 
# especifica cuantas, por lo que hemos considerado hacer 10.

stmSample <- c()

meanm10 <- c()

varm10 <- c()

set.seed(1777)
for(i in 1:10) {
  stmSample <- rbind(stmSample, male$steps[sample(dim(male)[1],200)])
  meanm10[i] <- mean(stmSample[i,])
  varm10[i] <- var(stmSample[i,])
}

# 2.2 Estimación por intervalos, una población

# muestras de tamaño 200

# IC del 90% para media con varianza desconocida 
# Parámetro: Data$sleeptime

ic90st <- c()

ic90st <- rbind(ic90st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic90st <- mean(ic90st)
sic90st <- sqrt(var(ic90st[1:200]))

std_err_IC90st <- qt(0.05,199,lower.tail = F)*sic90st/sqrt(dim(ic90st)[2])

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st

# Parámetro: Data$steps

ic90steps <- c()

ic90steps <- rbind(ic90steps, Data$steps[sample(dim(Data)[1],200)])
meanic90steps <- mean(ic90steps)
sic90steps <- sqrt(var(ic90steps[1:200]))

std_err_IC90steps <- qt(0.05,199,lower.tail = F)*sic90steps/sqrt(dim(ic90steps)[2])

mediaIC90max_steps <- meanic90steps + std_err_IC90steps
mediaIC90min_steps <- meanic90steps - std_err_IC90steps

# IC del 95% para media con varianza desconocida 
# Parámetro: Data$sleeptime

ic95st <- c()

ic95st <- rbind(ic95st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic95st <- mean(ic95st)
sic95st <- sqrt(var(ic95st[1:200]))

std_err_IC95st <- qt(0.025,199,lower.tail = F)*sic95st/sqrt(dim(ic95st)[2])

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Parámetro: Data$steps

ic95steps <- c()

ic95steps <- rbind(ic95steps, Data$steps[sample(dim(Data)[1],200)])
meanic95steps <- mean(ic95steps)
sic95steps <- sqrt(var(ic95steps[1:200]))

std_err_IC95steps <- qt(0.025,199,lower.tail = F)*sic95steps/sqrt(dim(ic95steps)[2])

mediaIC95max_steps <- meanic95steps + std_err_IC95steps
mediaIC95min_steps <- meanic95steps - std_err_IC95steps

# IC del 99% para media con varianza desconocida 
# Parámetro: Data$sleeptime

ic99st <- c()

ic99st <- rbind(ic99st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic99st <- mean(ic99st)
sic99st <- sqrt(var(ic99st[1:200]))

std_err_IC99st <- qt(0.005,199,lower.tail = F)*sic99st/sqrt(dim(ic99st)[2])

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Parámetro: Data$steps

ic99steps <- c()

ic99steps <- rbind(ic99steps, Data$steps[sample(dim(Data)[1],200)])
meanic99steps <- mean(ic99steps)
sic99steps <- sqrt(var(ic99steps[1:200]))

std_err_IC99steps <- qt(0.005,199,lower.tail = F)*sic99steps/sqrt(dim(ic99steps)[2])

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% para media con varianza conocida
# z (multiplicador de confianza) = 1.645
# Parámetro: Data$sleeptime

ic90st <- c()

ic90st <- rbind(ic90st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic90st <- mean(ic90st)
varic90st <- var(ic90st[1:200])
zIC90 <- 1.645

std_err_IC90st <- zIC90*varic90st/sqrt(dim(ic90st)[2])

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st

# Parámetro: Data$steps

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
# Parámetro: Data$sleeptime

ic95st <- c()

ic95st <- rbind(ic95st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic95st <- mean(ic95st)
varic95st <- var(ic95st[1:200])
zIC95 <- 2

std_err_IC95st <- zIC95*varic95st/sqrt(dim(ic95st)[2])

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Parámetro: Data$steps

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
# Parámetro: Data$sleeptime

ic99st <- c()

ic99st <- rbind(ic99st, Data$sleeptime[sample(dim(Data)[1],200)])
meanic99st <- mean(ic99st)
varic99st <- var(ic99st[1:200])
zIC99 <- 2.576

std_err_IC99st <- zIC99*varic99st/sqrt(dim(ic99st)[2])

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Parámetro: Data$steps

ic99steps <- c()

ic99steps <- rbind(ic99steps, Data$steps[sample(dim(Data)[1],200)])
meanic99steps <- mean(ic99steps)
varic99steps <- var(ic99steps[1:200])
zIC99 <- 2.576

std_err_IC99steps <- zIC99*varic99steps/sqrt(dim(ic99steps)[2])

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% para varianza
# Parámetro: Data$sleeptime

ic90st2 <- c()

ic90st2 <- rbind(ic90st2, Data$sleeptime[sample(dim(Data)[1],200)])
s2IC90st <- var(ic90st2[1:200])

minIC90st2 <- (199*s2IC90st)/qchisq(0.05,199,lower.tail = FALSE)
maxIC90st2 <- (199*s2IC90st)/qchisq(0.95,199,lower.tail = FALSE)

# Parámetro: Data$steps

ic90steps2 <- c()

ic90steps2 <- rbind(ic90steps2, Data$steps[sample(dim(Data)[1],200)])
s2IC90steps <- var(ic90steps2[1:200])

minIC90steps2 <- (199*s2IC90steps)/qchisq(0.05,199,lower.tail = FALSE)
maxIC90steps2 <- (199*s2IC90steps)/qchisq(0.95,199,lower.tail = FALSE)

# IC del 95% para varianza
# Parámetro: Data$sleeptime

ic95st2 <- c()

ic95st2 <- rbind(ic95st2, Data$sleeptime[sample(dim(Data)[1],200)])
s2IC95st <- var(ic95st2[1:200])

minIC95st2 <- (199*s2IC95st)/qchisq(0.025,199,lower.tail = FALSE)
maxIC95st2 <- (199*s2IC95st)/qchisq(0.975,199,lower.tail = FALSE)

# Parámetro: Data$steps

ic95steps2 <- c()

ic95steps2 <- rbind(ic95steps2, Data$steps[sample(dim(Data)[1],200)])
s2IC95steps <- var(ic95steps2[1:200])

minIC95steps2 <- (199*s2IC90steps)/qchisq(0.025,199,lower.tail = FALSE)
maxIC95steps2 <- (199*s2IC90steps)/qchisq(0.975,199,lower.tail = FALSE)

# IC del 99% para varianza
# Parámetro: Data$sleeptime

ic99st2 <- c()

ic99st2 <- rbind(ic99st2, Data$sleeptime[sample(dim(Data)[1],200)])
s2IC99st <- var(ic99st2[1:200])

minIC99st2 <- (199*s2IC99st)/qchisq(0.005,199,lower.tail = FALSE)
maxIC99st2 <- (199*s2IC99st)/qchisq(0.995,199,lower.tail = FALSE)

# Parámetro: Data$steps

ic99steps2 <- c()

ic99steps2 <- rbind(ic99steps2, Data$steps[sample(dim(Data)[1],200)])
s2IC99steps <- var(ic99steps2[1:200])

minIC99steps2 <- (199*s2IC99steps)/qchisq(0.005,199,lower.tail = FALSE)
maxIC99steps2 <- (199*s2IC99steps)/qchisq(0.995,199,lower.tail = FALSE)

# 2.3 Estimación por intervalos, dos poblaciones (M y V)

male <- Data[Data$Sex=="V",]
female <- Data[Data$Sex=="M",]

tM <- dim(male)[1]
tF <- dim(female)[1]

# IC del 90% dif medias con varianza desconocida pero iguales
# Parámetro: Data$sleeptime

meanic90stF <- mean(female$sleeptime)
meanic90stM <- mean(male$sleeptime)
meanic90st <- meanic90stF - meanic90stM
spic90st <- sqrt(((tF-1)*var(female$sleeptime)+(tM-1)*var(male$sleeptime))/tF+tM-2)


std_err_IC90st <- qt(0.05,tM+tF-2,lower.tail = F)*spic90st*sqrt(1/tF+1/tM)

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st

# Parámetro: Data$steps

meanic90stepsF <- mean(female$steps)
meanic90stepsM <- mean(male$steps)
meanic90steps <- meanic90stepsF - meanic90stepsM
spic90steps <- sqrt(((tF-1)*var(female$steps)+(tM-1)*var(male$steps))/tF+tM-2)


std_err_IC90steps <- qt(0.05,tM+tF-2,lower.tail = F)*spic90steps*sqrt(1/tF+1/tM)

mediaIC90max_steps <- meanic90steps + std_err_IC90steps
mediaIC90min_steps <- meanic90steps - std_err_IC90steps


# IC del 95% dif medias con varianza desconocida pero iguales
# Parámetro: Data$sleeptime

meanic95stF <- mean(female$sleeptime)
meanic95stM <- mean(male$sleeptime)
meanic95st <- meanic95stF - meanic95stM
spic95st <- sqrt(((tF-1)*var(female$sleeptime)+(tM-1)*var(male$sleeptime))/tF+tM-2)


std_err_IC95st <- qt(0.025,tM+tF-2,lower.tail = F)*spic95st*sqrt(1/tF+1/tM)

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Parámetro: Data$steps

meanic95stepsF <- mean(female$steps)
meanic95stepsM <- mean(male$steps)
meanic95steps <- meanic95stepsF - meanic95stepsM
spic95steps <- sqrt(((tF-1)*var(female$steps)+(tM-1)*var(male$steps))/tF+tM-2)


std_err_IC95steps <- qt(0.025,tM+tF-2,lower.tail = F)*spic95steps*sqrt(1/tF+1/tM)

mediaIC95max_steps <- meanic95steps + std_err_IC95steps
mediaIC95min_steps <- meanic95steps - std_err_IC95steps

# IC del 99% dif medias con varianza desconocida pero iguales
# Parámetro: Data$sleeptime

meanic99stF <- mean(female$sleeptime)
meanic99stM <- mean(male$sleeptime)
meanic99st <- meanic99stF - meanic99stM
spic99st <- sqrt(((tF-1)*var(female$sleeptime)+(tM-1)*var(male$sleeptime))/tF+tM-2)


std_err_IC99st <- qt(0.005,tM+tF-2,lower.tail = F)*spic99st*sqrt(1/tF+1/tM)

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Parámetro: Data$steps

meanic99stepsF <- mean(female$steps)
meanic99stepsM <- mean(male$steps)
meanic99steps <- meanic99stepsF - meanic90stepsM
spic99steps <- sqrt(((tF-1)*var(female$steps)+(tM-1)*var(male$steps))/tF+tM-2)


std_err_IC99steps <- qt(0.005,tM+tF-2,lower.tail = F)*spic99steps*sqrt(1/tF+1/tM)

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% dif medias con varianza desconocida y diferentes

Afemst <- var(female$sleeptime)/tF
Bmalest <- var(male$sleeptime)/tM
Afemsteps <- var(female$steps)/tF
Bmalesteps <- var(male$steps)/tM

# Parámetro: Data$sleeptime

meanic90stF <- mean(female$sleeptime)
meanic90stM <- mean(male$sleeptime)
meanic90st <- meanic90stF - meanic90stM

num_delta <- ((tM-1)*Afemst-(tF-1)*Bmalest)^2
den_delta <- (tM-1)*Afemst^2+(tF-1)*Bmalest^2
delta <- num_delta/den_delta

std_err_IC90st <- qt(0.05,tM+tF-2-delta,lower.tail = F)*sqrt(Afemst+Bmalest)

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st
# Parámetro: Data$steps

meanic90stepsF <- mean(female$steps)
meanic90stepsM <- mean(male$steps)
meanic90steps <- meanic90stepsF - meanic90stepsM

num_delta <- ((tM-1)*Afemsteps-(tF-1)*Bmalesteps)^2
den_delta <- (tM-1)*Afemsteps^2+(tF-1)*Bmalesteps^2
delta <- num_delta/den_delta

std_err_IC90steps <- qt(0.05,tM+tF-2-delta,lower.tail = F)*sqrt(Afemsteps+Bmalesteps)

mediaIC90max_steps <- meanic90steps + std_err_IC90steps
mediaIC90min_steps <- meanic90steps - std_err_IC90steps

# IC del 95% dif medias con varianza desconocida y diferentes
# Parámetro: Data$sleeptime

meanic95stF <- mean(female$sleeptime)
meanic95stM <- mean(male$sleeptime)
meanic95st <- meanic95stF - meanic95stM

num_delta <- ((tM-1)*Afemst-(tF-1)*Bmalest)^2
den_delta <- (tM-1)*Afemst^2+(tF-1)*Bmalest^2
delta <- num_delta/den_delta

std_err_IC95st <- qt(0.025,tM+tF-2-delta,lower.tail = F)*sqrt(Afemst+Bmalest)

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Parámetro: Data$steps

meanic95stepsF <- mean(female$steps)
meanic95stepsM <- mean(male$steps)
meanic95steps <- meanic95stepsF - meanic95stepsM

num_delta <- ((tM-1)*Afemsteps-(tF-1)*Bmalesteps)^2
den_delta <- (tM-1)*Afemsteps^2+(tF-1)*Bmalesteps^2
delta <- num_delta/den_delta

std_err_IC95steps <- qt(0.025,tM+tF-2-delta,lower.tail = F)*sqrt(Afemsteps+Bmalesteps)

mediaIC95max_steps <- meanic95steps + std_err_IC95steps
mediaIC95min_steps <- meanic95steps - std_err_IC95steps

# IC del 99% dif medias con varianza desconocida y diferentes
# Parámetro: Data$sleeptime

meanic99stF <- mean(female$sleeptime)
meanic99stM <- mean(male$sleeptime)
meanic99st <- meanic99stF - meanic99stM

num_delta <- ((tM-1)*Afemst-(tF-1)*Bmalest)^2
den_delta <- (tM-1)*Afemst^2+(tF-1)*Bmalest^2
delta <- num_delta/den_delta

std_err_IC99st <- qt(0.005,tM+tF-2-delta,lower.tail = F)*sqrt(Afemst+Bmalest)

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Parámetro: Data$steps

meanic99stepsF <- mean(female$steps)
meanic99stepsM <- mean(male$steps)
meanic99steps <- meanic99stepsF - meanic99stepsM

num_delta <- ((tM-1)*Afemsteps-(tF-1)*Bmalesteps)^2
den_delta <- (tM-1)*Afemsteps^2+(tF-1)*Bmalesteps^2
delta <- num_delta/den_delta

std_err_IC95steps <- qt(0.005,tM+tF-2-delta,lower.tail = F)*sqrt(Afemsteps+Bmalesteps)

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% dif medias con varianza conocida
# Parámetro: Data$sleeptime

varfem_IC90st <- var(female$sleeptime)/dim(female)[1]
varm_IC90st <- var(male$sleeptime)/dim(male)[1]

lim_infIC90st <- mean(female$sleeptime) - mean(male$sleeptime) - qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)
lim_supIC90st <- mean(female$sleeptime) - mean(male$sleeptime) + qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)

# Parámetro: Data$steps

varfem_IC90steps <- var(female$steps)/dim(female)[1]
varm_IC90steps <- var(male$steps)/dim(male)[1]

lim_infIC90steps <- mean(female$steps) - mean(male$steps) - qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)
lim_supIC90steps <- mean(female$steps) - mean(male$steps) + qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)

# IC del 95% dif medias con varianza conocida
# Parámetro: Data$sleeptime

varfem_IC95st <- var(female$sleeptime)/dim(female)[1]
varm_IC95st <- var(male$sleeptime)/dim(male)[1]

lim_infIC95st <- mean(female$sleeptime) - mean(male$sleeptime) - qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)
lim_supIC95st <- mean(female$sleeptime) - mean(male$sleeptime) + qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)

# Parámetro: Data$steps

varfem_IC95steps <- var(female$steps)/dim(female)[1]
varm_IC95steps <- var(male$steps)/dim(male)[1]

lim_infIC95steps <- mean(female$steps) - mean(male$steps) - qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)
lim_supIC95steps <- mean(female$steps) - mean(male$steps) + qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)

# IC del 99% dif medias con varianza conocida
# Parámetro: Data$sleeptime

varfem_IC99st <- var(female$sleeptime)/dim(female)[1]
varm_IC99st <- var(male$sleeptime)/dim(male)[1]

lim_infIC99st <- mean(female$sleeptime) - mean(male$sleeptime) - qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)
lim_supIC99st <- mean(female$sleeptime) - mean(male$sleeptime) + qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)

# Parámetro: Data$steps

varfem_IC99steps <- var(female$steps)/dim(female)[1]
varm_IC99steps <- var(male$steps)/dim(male)[1]

lim_infIC99steps <- mean(female$steps) - mean(male$steps) - qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)
lim_supIC99steps <- mean(female$steps) - mean(male$steps) + qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)

# IC del 90% para razón de varianzas
# Parámetro Data$sleeptime

lim_inf_razvar_IC90st <- (var(female$sleeptime)/var(male$sleeptime))*(1/qf(0.05,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC90st <- (var(female$sleeptime)/var(male$sleeptime))*qf(0.05,dim(male)[1],dim(female)[1],lower.tail = F)

# Parámetro Data$steps

lim_inf_razvar_IC90steps <- (var(female$steps)/var(male$steps))*(1/qf(0.05,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC90steps <- (var(female$steps)/var(male$steps))*qf(0.05,dim(male)[1],dim(female)[1],lower.tail = F)

# IC del 95% para razón de varianzas
# Parámetro Data$sleeptime

lim_inf_razvar_IC95st <- (var(female$sleeptime)/var(male$sleeptime))*(1/qf(0.025,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC95st <- (var(female$sleeptime)/var(male$sleeptime))*qf(0.025,dim(male)[1],dim(female)[1],lower.tail = F)

# Parámetro Data$steps

lim_inf_razvar_IC95steps <- (var(female$steps)/var(male$steps))*(1/qf(0.025,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC95steps <- (var(female$steps)/var(male$steps))*qf(0.025,dim(male)[1],dim(female)[1],lower.tail = F)

# IC del 99% para razón de varianzas
# Parámetro Data$sleeptime

lim_inf_razvar_IC99st <- (var(female$sleeptime)/var(male$sleeptime))*(1/qf(0.005,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC99st <- (var(female$sleeptime)/var(male$sleeptime))*qf(0.005,dim(male)[1],dim(female)[1],lower.tail = F)

# Parámetro Data$steps

lim_inf_razvar_IC99steps <- (var(female$steps)/var(male$steps))*(1/qf(0.005,dim(female)[1],dim(male)[1],lower.tail = F))
lim_sup_razvar_IC99steps <- (var(female$steps)/var(male$steps))*qf(0.005,dim(male)[1],dim(female)[1],lower.tail = F)

# Idem con poblaciones generales
# Muestra de tamaño 200

male <- Data[Data$Sex=="V",]
female <- Data[Data$Sex=="M",]

fem200st <- c()
fem200steps <- c()
male200st <- c()
male200steps <- c()

fem200st <- rbind(fem200st, female$sleeptime[sample(dim(female)[1],200)])
fem200steps <- rbind(fem200steps, female$steps[sample(dim(female)[1],200)])
male200st <- rbind(male200st, male$sleeptime[sample(dim(male)[1],200)])
male200steps <- rbind(male200steps, male$steps[sample(dim(male)[1],200)])

tM <- 200
tF <- 200

# IC del 90% dif medias con varianza desconocida pero iguales
# Parámetro: Data$sleeptime

meanic90stF <- mean(fem200st)
meanic90stM <- mean(male200st)
meanic90st <- meanic90stF - meanic90stM
spic90st <- sqrt(((tF-1)*var(fem200st[1:200])+(tM-1)*var(male200st[1:200]))/tF+tM-2)


std_err_IC90st <- qt(0.05,tM+tF-2,lower.tail = F)*spic90st*sqrt(1/tF+1/tM)

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st

# Parámetro: Data$steps

meanic90stepsF <- mean(fem200steps)
meanic90stepsM <- mean(male200steps)
meanic90steps <- meanic90stepsF - meanic90stepsM
spic90steps <- sqrt(((tF-1)*var(fem200steps[1:200])+(tM-1)*var(male200steps[1:200]))/tF+tM-2)


std_err_IC90steps <- qt(0.05,tM+tF-2,lower.tail = F)*spic90steps*sqrt(1/tF+1/tM)

mediaIC90max_steps <- meanic90steps + std_err_IC90steps
mediaIC90min_steps <- meanic90steps - std_err_IC90steps


# IC del 95% dif medias con varianza desconocida pero iguales
# Parámetro: Data$sleeptime

meanic95stF <- mean(fem200st)
meanic95stM <- mean(male200st)
meanic95st <- meanic95stF - meanic95stM
spic95st <- sqrt(((tF-1)*var(fem200st[1:200])+(tM-1)*var(male200st[1:200]))/tF+tM-2)


std_err_IC95st <- qt(0.025,tM+tF-2,lower.tail = F)*spic95st*sqrt(1/tF+1/tM)

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Parámetro: Data$steps

meanic95stepsF <- mean(fem200steps)
meanic95stepsM <- mean(male200steps)
meanic95steps <- meanic95stepsF - meanic95stepsM
spic95steps <- sqrt(((tF-1)*var(fem200st[1:200])+(tM-1)*var(male200steps[1:200]))/tF+tM-2)


std_err_IC95steps <- qt(0.025,tM+tF-2,lower.tail = F)*spic95steps*sqrt(1/tF+1/tM)

mediaIC95max_steps <- meanic95steps + std_err_IC95steps
mediaIC95min_steps <- meanic95steps - std_err_IC95steps

# IC del 99% dif medias con varianza desconocida pero iguales
# Parámetro: Data$sleeptime

meanic99stF <- mean(fem200st)
meanic99stM <- mean(male200st)
meanic99st <- meanic99stF - meanic99stM
spic99st <- sqrt(((tF-1)*var(fem200st[1:200])+(tM-1)*var(male200st[1:200]))/tF+tM-2)


std_err_IC99st <- qt(0.005,tM+tF-2,lower.tail = F)*spic99st*sqrt(1/tF+1/tM)

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Parámetro: Data$steps

meanic99stepsF <- mean(fem200steps)
meanic99stepsM <- mean(male200steps)
meanic99steps <- meanic99stepsF - meanic90stepsM
spic99steps <- sqrt(((tF-1)*var(fem200steps[1:200])+(tM-1)*var(male200steps[1:200]))/tF+tM-2)


std_err_IC99steps <- qt(0.005,tM+tF-2,lower.tail = F)*spic99steps*sqrt(1/tF+1/tM)

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% dif medias con varianza desconocida y diferentes

Afemst <- var(fem200st[1:200])/tF
Bmalest <- var(male200st[1:200])/tM
Afemsteps <- var(fem200steps[1:200])/tF
Bmalesteps <- var(male200steps[1:200])/tM

# Parámetro: Data$sleeptime

meanic90stF <- mean(fem200st)
meanic90stM <- mean(male200st)
meanic90st <- meanic90stF - meanic90stM

num_delta <- ((tM-1)*Afemst-(tF-1)*Bmalest)^2
den_delta <- (tM-1)*Afemst^2+(tF-1)*Bmalest^2
delta <- num_delta/den_delta

std_err_IC90st <- qt(0.05,tM+tF-2-delta,lower.tail = F)*sqrt(Afemst+Bmalest)

mediaIC90max_st <- meanic90st + std_err_IC90st
mediaIC90min_st <- meanic90st - std_err_IC90st
# Parámetro: Data$steps

meanic90stepsF <- mean(fem200steps)
meanic90stepsM <- mean(male200steps)
meanic90steps <- meanic90stepsF - meanic90stepsM

num_delta <- ((tM-1)*Afemsteps-(tF-1)*Bmalesteps)^2
den_delta <- (tM-1)*Afemsteps^2+(tF-1)*Bmalesteps^2
delta <- num_delta/den_delta

std_err_IC90steps <- qt(0.05,tM+tF-2-delta,lower.tail = F)*sqrt(Afemsteps+Bmalesteps)

mediaIC90max_steps <- meanic90steps + std_err_IC90steps
mediaIC90min_steps <- meanic90steps - std_err_IC90steps

# IC del 95% dif medias con varianza desconocida y diferentes
# Parámetro: Data$sleeptime

meanic95stF <- mean(fem200st)
meanic95stM <- mean(male200st)
meanic95st <- meanic95stF - meanic95stM

num_delta <- ((tM-1)*Afemst-(tF-1)*Bmalest)^2
den_delta <- (tM-1)*Afemst^2+(tF-1)*Bmalest^2
delta <- num_delta/den_delta

std_err_IC95st <- qt(0.025,tM+tF-2-delta,lower.tail = F)*sqrt(Afemst+Bmalest)

mediaIC95max_st <- meanic95st + std_err_IC95st
mediaIC95min_st <- meanic95st - std_err_IC95st

# Parámetro: Data$steps

meanic95stepsF <- mean(fem200steps)
meanic95stepsM <- mean(male200steps)
meanic95steps <- meanic95stepsF - meanic95stepsM

num_delta <- ((tM-1)*Afemsteps-(tF-1)*Bmalesteps)^2
den_delta <- (tM-1)*Afemsteps^2+(tF-1)*Bmalesteps^2
delta <- num_delta/den_delta

std_err_IC95steps <- qt(0.025,tM+tF-2-delta,lower.tail = F)*sqrt(Afemsteps+Bmalesteps)

mediaIC95max_steps <- meanic95steps + std_err_IC95steps
mediaIC95min_steps <- meanic95steps - std_err_IC95steps

# IC del 99% dif medias con varianza desconocida y diferentes
# Parámetro: Data$sleeptime

meanic99stF <- mean(female$sleeptime)
meanic99stM <- mean(male$sleeptime)
meanic99st <- meanic99stF - meanic99stM

num_delta <- ((tM-1)*Afemst-(tF-1)*Bmalest)^2
den_delta <- (tM-1)*Afemst^2+(tF-1)*Bmalest^2
delta <- num_delta/den_delta

std_err_IC99st <- qt(0.005,tM+tF-2-delta,lower.tail = F)*sqrt(Afemst+Bmalest)

mediaIC99max_st <- meanic99st + std_err_IC99st
mediaIC99min_st <- meanic99st - std_err_IC99st

# Parámetro: Data$steps

meanic99stepsF <- mean(female$steps)
meanic99stepsM <- mean(male$steps)
meanic99steps <- meanic99stepsF - meanic99stepsM

num_delta <- ((tM-1)*Afemsteps-(tF-1)*Bmalesteps)^2
den_delta <- (tM-1)*Afemsteps^2+(tF-1)*Bmalesteps^2
delta <- num_delta/den_delta

std_err_IC95steps <- qt(0.005,tM+tF-2-delta,lower.tail = F)*sqrt(Afemsteps+Bmalesteps)

mediaIC99max_steps <- meanic99steps + std_err_IC99steps
mediaIC99min_steps <- meanic99steps - std_err_IC99steps

# IC del 90% dif medias con varianza conocida
# Parámetro: Data$sleeptime

varfem_IC90st <- var(fem200st[1:200])/200
varm_IC90st <- var(male200st[1:200])/200

lim_infIC90st <- mean(fem200st) - mean(male200st) - qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)
lim_supIC90st <- mean(fem200st) - mean(male200st) + qnorm((0.9+1)/2)*sqrt(varfem_IC90st+varm_IC90st)

# Parámetro: Data$steps

varfem_IC90steps <- var(fem200steps[1:200])/200
varm_IC90steps <- var(male200steps[1:200])/200

lim_infIC90steps <- mean(fem200steps) - mean(male200steps) - qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)
lim_supIC90steps <- mean(fem200steps) - mean(male200steps) + qnorm((0.9+1)/2)*sqrt(varfem_IC90steps+varm_IC90steps)

# IC del 95% dif medias con varianza conocida
# Parámetro: Data$sleeptime

varfem_IC95st <- var(fem200st[1:200])/200
varm_IC95st <- var(male200st[1:200])/200

lim_infIC95st <- mean(fem200st) - mean(male200st) - qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)
lim_supIC95st <- mean(fem200st) - mean(male200st) + qnorm((0.95+1)/2)*sqrt(varfem_IC95st+varm_IC95st)

# Parámetro: Data$steps

varfem_IC95steps <- var(fem200steps[1:200])/200
varm_IC95steps <- var(male200steps[1:200])/200

lim_infIC95steps <- mean(fem200steps) - mean(male200steps) - qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)
lim_supIC95steps <- mean(fem200steps) - mean(male200steps) + qnorm((0.95+1)/2)*sqrt(varfem_IC95steps+varm_IC95steps)

# IC del 99% dif medias con varianza conocida
# Parámetro: Data$sleeptime

varfem_IC99st <- var(fem200st[1:200])/200
varm_IC99st <- var(male200st[1:200])/200

lim_infIC99st <- mean(fem200st) - mean(male200st) - qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)
lim_supIC99st <- mean(fem200st) - mean(male200st) + qnorm((0.99+1)/2)*sqrt(varfem_IC99st+varm_IC99st)

# Parámetro: Data$steps

varfem_IC99steps <- var(fem200steps[1:200])/200
varm_IC99steps <- var(male200steps[1:200])/200

lim_infIC99steps <- mean(fem200steps) - mean(male200steps) - qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)
lim_supIC99steps <- mean(fem200steps) - mean(male200steps) + qnorm((0.99+1)/2)*sqrt(varfem_IC99steps+varm_IC99steps)

# IC del 90% para razón de varianzas
# Parámetro Data$sleeptime

lim_inf_razvar_IC90st <- (var(fem200st[1:200])/var(male200st[1:200]))*(1/qf(0.05,200,200,lower.tail = F))
lim_sup_razvar_IC90st <- (var(fem200st[1:200])/var(male200st[1:200]))*qf(0.05,200,200,lower.tail = F)

# Parámetro Data$steps

lim_inf_razvar_IC90steps <- (var(fem200steps[1:200])/var(male200steps[1:200]))*(1/qf(0.05,200,200,lower.tail = F))
lim_sup_razvar_IC90steps <- (var(fem200steps[1:200])/var(male200steps[1:200]))*qf(0.05,200,200,lower.tail = F)

# IC del 95% para razón de varianzas
# Parámetro Data$sleeptime

lim_inf_razvar_IC95st <- (var(fem200st[1:200])/var(male200st[1:200]))*(1/qf(0.025,200,200,lower.tail = F))
lim_sup_razvar_IC95st <- (var(fem200st[1:200])/var(male200st[1:200]))*qf(0.025,200,200,lower.tail = F)

# Parámetro Data$steps

lim_inf_razvar_IC95steps <- (var(fem200steps[1:200])/var(male200steps[1:200]))*(1/qf(0.025,200,200,lower.tail = F))
lim_sup_razvar_IC95steps <- (var(fem200steps[1:200])/var(male200steps[1:200]))*qf(0.025,200,200,lower.tail = F)

# IC del 99% para razón de varianzas
# Parámetro Data$sleeptime

lim_inf_razvar_IC99st <- (var(fem200st[1:200])/var(male200st[1:200]))*(1/qf(0.005,200,200,lower.tail = F))
lim_sup_razvar_IC99st <- (var(fem200st[1:200])/var(male200st[1:200]))*qf(0.005,200,200,lower.tail = F)

# Parámetro Data$steps

lim_inf_razvar_IC99steps <- (var(fem200steps[1:200])/var(male200steps[1:200]))*(1/qf(0.005,200,200,lower.tail = F))
lim_sup_razvar_IC99steps <- (var(fem200steps[1:200])/var(male200steps[1:200]))*qf(0.005,200,200,lower.tail = F)




# Parte 3 Estimación bayesiana
set.seed(2021)
n = 200
sample200 = Data[sample(dim(Data)[1], n),]
sample.proports <- table(sample200$Nation)/n
nE <- sample.proports["SP"]
alfa.post = 5 + nE
beta.post = 10 + 1
betaPosteriori = beta(alfa.post, beta.post)

print("IC 95%")
inf95 <- qbeta(0.025,alfa.post,beta.post)
sup95 <- qbeta(0.975,alfa.post,beta.post)
cat("IC al 95%  (",inf95,sup95,")\n")

sample.height <- sample200[ sample200$Nation %in% c("SP","FR","IT"),]$height

mu_0 = 170
n0 = sd(sample.height)^2/7^2
alpha = n/(n+n0)
mu_p = alpha*mu_0+(1-alpha)*mean(sample.height)


#Parte 4


#a)



PYE2DataSet37 <- read.csv("C:/Users/ADRIAN JIMENEZ/Desktop/pye2-main/PYE2DataSet37.csv")
View(PYE2DataSet37)
attach(PYE2DataSet37)
names(PYE2DataSet37)

set.seed(2021)
n <- 200
Sample1 <- sample(1:nrow(PYE2DataSet37),size = n, replace = FALSE)

summary(IMC[Sample1])

mediaSample1 <- mean(IMC[Sample1])
quantile1Sample1 <- quantile(IMC[Sample1],c(0.25))
quantile3Sample1 <- quantile(IMC[Sample1],c(0.75))
var1 <- var(IMC[Sample1])
sd <- sd(IMC[Sample1])

n <- 200
Sample2 <- sample(1:nrow(PYE2DataSet37),size = n, replace = FALSE)

summary(IMC[Sample2])

mediaSample2 <- mean(IMC[Sample2])
quantile1Sample2 <- quantile(IMC[Sample2],c(0.25))
quantile3Sample2 <- quantile(IMC[Sample2],c(0.75))
Var2 <- var(IMC[Sample2])

#Contrastar si la media µ1 de Sample1 es Q1 ≤ µ1, con varianza desconocida
t.test(IMC[Sample1], alternative = "less", mu=quantile1Sample1)

#Contrastar si la media µ1 de Sample1 es µ1 ≤ Q3, con varianza desconocida
t.test(IMC[Sample1], alternative = "greater", mu=quantile3Sample1)

#Contrastar si la varianza σ 2 de Sample1 es mayor que 1.0, con media desconocida
library(TeachingDemos)
sigma.test(IMC[Sample1], sigma=1, alternative="less")

#Contrastar si µ1 − µ2 = 0, con varianzas desconocidas
t.test(IMC[Sample1], IMC[Sample2],paired = TRUE)

#Contrastar si σ1^2/σ2^2 = 1
var.test(IMC[Sample1], IMC[Sample2])

#b)

n <- 200
Sample <- sample(1:nrow(PYE2DataSet37),size = n, replace = FALSE)

#Contrastar la normalidad de Sample, mediante el test de Pearson y el test de Kolmogorov-Smirnov
mediaSample <- mean(IMC[Sample])
sdSample <- sd(IMC[Sample])
ks.test(Sample, pnorm, mediaSample, sdSample)
cor.test(IMC[Sample],IMC[Sample], alternative="two.sided", conf.lvl = 0.95, method = "pearson")

#Contrastar la independencia de Sample, mediante el test de Durbin-Watson. Se trata de ver si hay
#dependencia de IMC respecto a algunas variables de conjunto de datos. Sugerencia: paquete lmtest y
#función dwtest(), es decir, se toma una muestra de tamaño 200 del conjunto de datos y se proponen
#algunas variables independientes de las que puedea depender IMC, y tras hacer el test se saca una
#conclusión.
library(lmtest)
library(zoo)
y <- matrix(Sample[height], ncol = 1)
x <- matrix(Sample[weight], ncol = 1)
w <- matrix(Sample[IMC], ncol = 1)
dwtest(w~x+y)

#Contrastar la homogeneidad de Sample, mediante el test de Wilcoxon. Se trata de ver si varias
#muestras provienen de la misma poblaci´on, es decir, tomamos dos muestras de tama˜no 200 de la
#variable IMC (de la misma poblaci´on) y tras hacer el test se saca una conclusi´on.
#Sugerencia: paquete stats y funci´on wilcox.test()

#Para contrastar este test, tomo la muestra anterior Sample2 y Sample

wilcox.test(Sample2[IMC], Sample[IMC])
