
# installation du package randtoolbox s'il n'est pas installe
if(!require(randtoolbox)) install.packages("randtoolbox")
# chargement du package randtoolbox
library(randtoolbox)
# chargement du module generateurs.R
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

# max valeur possible 
max_mersenne <- 2^32-1
max_vn <- 9999

# Visualiation de donnees generees sous forme des histogrammes (1ere colonne) 
# sur le meme graphique
par(mfrow=c(1,2)) #
hist(vn[,1], breaks=seq(0,max_vn,length=20), xlab='', main='Von Neumann')
hist(mt[,1], breaks=seq(0,max_mersenne,length=20), xlab='', 
     main='Mersenne Twister')

# Visualiation de donnees generees sous forme des nuages des points (scatterplot) 
# sur le meme graphique
# chaque donnee est prise en fonction de la donnee precedente
par(mfrow=c(1,2))
plot(vn[1:(Nsimu-1),1], vn[2:Nsimu,1], xlab='VN(i)', ylab='VN(i+1)', 
     main='Von Neumann')
plot(mt[1:(Nsimu-1),1], mt[2:Nsimu,1], xlab='MT(i)', ylab='MT(i+1)', 
     main='Mersenne Twister')

# Sequence de bits pour les tests
(bit_mt <- binary(mt[1,1]))

