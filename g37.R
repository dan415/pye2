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

summary(Data$sleeptime)
summary(Data$steps)

hist(Data$sleeptime)
hist(Data$steps)

boxplot(Data$sleeptime)
boxplot(Data$steps)

stem.leaf(Data$sleeptime)
stem.leaf(Data$sleeptime)

kurtosis(Data$sleeptime)
kurtosis(Data$steps)

skewness(Data$sleeptime)
skewness(Data$steps)

x=seq(0, 23, by=0.01)
nsleep <- fitdistr(Data$sleeptime, "normal")
plot(x,dnorm(x, nsleep$estimate[1], nsleep$estimate[2]))

x=seq(0, 23, by=0.01)
esleep <- fitdistr(Data$sleeptime, "exponential")
plot(x, dexp(x, esleep$estimate[1]))

x=seq(0, 23, by=0.01)
gsleep <- fitdistr(Data$sleeptime, "gamma", lower = c(0, 0)) 
plot(x, dgamma(x, gsleep$estimate[1], gsleep$estimate[2]))

x=seq(8082, 13711, by=1)
nsteps <- fitdistr(Data$steps, "normal")
plot(x,dnorm(x, nsteps$estimate[1], nsteps$estimate[2]))

x=seq(8082, 13711, by=1)
esteps <- fitdistr(Data$steps, "exponential")
plot(x, dexp(x, esteps$estimate[1]))

x=seq(8082, 13711, by=1)
gsteps <- fitdistr(Data$steps, "gamma",  lower = c(0, 0))
plot(x, dgamma(x, gsteps$estimate[1], gsteps$estimate[2]))


# Parte 2.3 Manuel
set.seed(2021)
freqs = table(Data$Nation)
x = seq(0, 1, 0.01)
bdist <- beta(a = 5, b = 10)
plot(x, dbeta(x, 5, 10))

