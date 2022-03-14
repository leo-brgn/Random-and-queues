FileMM1 <- function(lambda,  # minutes^-1
                    mu,      # minutes^-1
                    D        # heure tau
) 
{
  set.seed(1)
  NC <- 100 * D # Upper Bound of number of clients connexions
  D_mn <- D * 60 # Tps observation en min
  expo1 <- rexp(NC, lambda) # Loi exponentielle attente arrivées
  time <- expo1[1]
  arrivees <- array(numeric(), c(0))
  i <- 0
  while (D_mn[1] > time[1] && i+1 < NC) 
  {
    arrivees <- append(arrivees, time)
    time <- time + expo1[i+1]
    i <- i+1
  }

  # Tableau des départs.
  expo2 <- rexp(NC, lambda) # Loi exponentielle attente arrivées
  departs <- array(numeric(), c(0))
  departs <- append(departs, arrivees[1] + expo2[1])
  for (j in 2:i)
  {
    departs <- append(departs, max(arrivees[j], departs[j-1]) + expo2[j])
  }
  
  return(list(arrivees, departs))
}

People <- function(list,D) {
  nbPeople <- rep(0, D*60*100)
  for (i in 1:length(list[[1]])) {
    for (j in 1:D*60*100) 
    {
      if (list[[1]][i] < i/1000) 
        nbPeople[i] <- nbPeople[i] + 1
    }
  }
  return(nbPeople)
}
