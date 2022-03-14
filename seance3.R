
# installation du package randtoolbox s'il n'est pas installe
if(!require(randtoolbox)) install.packages("randtoolbox")
# chargement du package randtoolbox
library(randtoolbox)
source('queues.R')


list <- FileMM1(2,2,1)
plot(list[[1]])

people <- People(list, 1)
plot(people)
