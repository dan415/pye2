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


# Parte 2.3 Manuel
set.seed(2021)
freqs = table(Data$Nation)
x = seq(0, 1, 0.01)
bdist <- beta(a = 5, b = 10)
plot(x, dbeta(x, 5, 10))
