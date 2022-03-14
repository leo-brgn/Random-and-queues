# installation du package randtoolbox s'il n'est pas installe
if(!require(randtoolbox)) install.packages("randtoolbox")
# chargement du package randtoolbox
library(randtoolbox)
# chargement du module generateurs.R
# source('~/Documents/INSA/IFA3/PROB/TP-Probas/generateurs.R')
source('generateurs.R')

n <- 10
p <- 0.7
valeurs <- array(numeric(),c(0))
for (i in 1:1000) {
  valeurs <- append(valeurs, LoiBinomiale(n, p))
}
gaussienne <- array(numeric(), c(0))
for (i in 1:12) {
  gaussienne <- append(gaussienne, dnorm(i, n*p, sqrt(n*p*(1-p))) * 1000)
}

par(mfrow=c(1,1))
plot(table(valeurs), 
     col="blue",
     xlab = "nombre de succès",
     ylab = "nombre de réalisations",
     main = "Loi binomiale (n=10, p=0.7)")

lines(gaussienne,
      lwd = 2,
      col = "chocolate3")
