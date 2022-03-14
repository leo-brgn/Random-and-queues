
# installation du package randtoolbox s'il n'est pas installe
if(!require(randtoolbox)) install.packages("randtoolbox")
# chargement du package randtoolbox
library(randtoolbox)
# chargement du module generateurs.R
# source('~/Documents/INSA/IFA3/PROB/TP-Probas/generateurs.R')
source('generateurs.R')

# definition des parametres
sVN <- 3454 # graine pour la methode de Von Neumann
sMT <- 1502 # graine pour la methode de Mersenne-Twister
Nsimu <- 1000 # nombre de simulations (nombre d'entiers par sequences generee)
Nrepet <- 20 # nombre de repetitions (nombre de sequences generees)


############################################################
##  Section 2
############################################################

# appel de la fonction VonNeumann du module generateurs.R 
# et sauvegarde du resultat dans la variable vn
vn <- VonNeumann(Nsimu, Nrepet, sVN) 

# appel de la fonction MarsenneTwister du module generateurs.R 
# et sauvegarde du resultat dans la variable mt
mt <- MersenneTwister(Nsimu, Nrepet, sMT)

ru <- randu(Nsimu, Nrepet, sVN)

stm <- standardMinimal(Nsimu, Nrepet, sVN)

# max valeur possible 
max_mersenne <- 2^32-1
max_vn <- 9999
max_ru <- 2^31-1
max_stm <- 2^31-2

# Visualiation de donnees generees sous forme des histogrammes (1ere colonne) 
# sur le meme graphique
par(mfrow=c(2,2)) #
hist(vn[,1], breaks=seq(0,max_vn,length=20), xlab='', main='Von Neumann')
hist(mt[,1], breaks=seq(0,max_mersenne,length=20), xlab='', 
     main='Mersenne Twister')
hist(ru[,1], breaks=seq(0,max_ru,length=20), xlab='', main='RANDU')
hist(stm[,1], breaks=seq(0,max_stm,length=20), xlab='', 
     main='Standad Minimal')

# Visualiation de donnees generees sous forme des nuages des points (scatterplot) 
# sur le meme graphique
# chaque donnee est prise en fonction de la donnee precedente
par(mfrow=c(2,2))
plot(vn[1:(Nsimu-1),1], vn[2:Nsimu,1], xlab='VN(i)', ylab='VN(i+1)', 
     main='Von Neumann')
plot(mt[1:(Nsimu-1),1], mt[2:Nsimu,1], xlab='MT(i)', ylab='MT(i+1)', 
     main='Mersenne Twister')
plot(ru[1:(Nsimu-1),1], ru[2:Nsimu,1], xlab='RU(i)', ylab='RU(i+1)', 
     main='RANDU')
plot(stm[1:(Nsimu-1),1], stm[2:Nsimu,1], xlab='STM(i)', ylab='STM(i+1)', 
     main='Standard Minimal')

# Tests des générateurs pour 25+1 graines
vec_mt <- mt
vec_vn <- vn
vec_ru <- ru
vec_stm <- stm
# génération des graines
graines <- sample.int(10000,25)
# génération des nombres aléatoires pour chaque graine et pour chaque algorithme
for (i in 1:25)
{
  vec_mt <- append(vec_mt, MersenneTwister(Nsimu, Nrepet, graines[i]))
  vec_vn <- append(vec_vn, VonNeumann(Nsimu, Nrepet, graines[i]))
  vec_ru <- append(vec_ru, randu(Nsimu, Nrepet, graines[i]))
  vec_stm <- append(vec_stm, standardMinimal(Nsimu, Nrepet, graines[i]))
}

# Test de fréquence monobit
fr_mt <- Frequency(vec_mt,32)
fr_vn <- Frequency(vec_vn,14)
fr_ru <- Frequency(vec_ru,31)
fr_stm <- Frequency(vec_stm,31)

p_mt <- 2 * (1-pnorm(fr_mt))
p_mt <- sum(p_mt)/length(p_mt)
p_vn <- 2 * (1-pnorm(fr_vn))
p_vn <- sum(p_vn)/length(p_vn)
p_ru <- 2 * (1-pnorm(fr_ru))
p_ru <- sum(p_ru)/length(p_ru)
p_stm <- 2 * (1-pnorm(fr_stm))
p_stm<- sum(p_stm)/length(p_stm)

# Test des runs
p_mt <- Runs(mt,32)
p_vn <- Runs(vn,14)
p_ru <- Runs(ru,31)
p_stm <- Runs(stm,31)

p_mt <- mean(p_mt < 0.01)
p_vn <- mean(p_vn < 0.01)
p_ru <- mean(p_ru < 0.01)
p_stm <- mean(p_stm < 0.01)

# Tests d'ordre
order.test(vec_mt, d = 4, echo = FALSE)$p.value
order.test(vec_vn, d = 4, echo = FALSE)$p.value
order.test(vec_ru, d = 4, echo = FALSE)$p.value
order.test(vec_stm, d = 4, echo = FALSE)$p.value




