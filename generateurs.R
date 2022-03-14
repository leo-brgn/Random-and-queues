# generateur de von Neumann
# @params p: nombre de sequences generees
# @params n: nombre d'entiers par sequences generee 
# @params graine: etat initiale, graine
# @return: une matrice qui contient p sequences (colonnes) de n entiers
VonNeumann <- function(n, p=1, graine)
{
  # vecteur de la longueur (n*p+1) construit en base de repetition de la valeur
  # graine
  x <- rep(graine, n*p+1)
  
  for(i in 2:(n*p+1))
  {
    # elever x[i-1] au carre
    # conversion en string et decoupage par caractere
    numbers <- strsplit(format(x[i-1]^2, scientific=FALSE), '')[[1]]
    
    while(length(numbers) > 4){ 
        # suppression du premier et dernier elements en prenant un sous-ensemble
        numbers <- numbers[2:(length(numbers)-1)] 
    }
    # conversion des elements de numbers en nombres
    # '%*%' est matrix multiplication
    # seq() creation d'une sequence d'entiers avec le pas
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  # creation d'une matrice avec n lignes et p colonnes
  x <- matrix(x[2:(n*p+1)], nrow=n, ncol=p)
  
  return(x)
}

# generateur Mersenne-Twister
# @params p: nombre de sequences generees
# @params n: nombre d'entiers par sequences generee 
# @params graine: etat initiale, graine
# @return: une matrice qui contient p sequences (colonnes) de n entiers
MersenneTwister <- function(n, p=1, graine)
{
  # definition d'une graine
  set.seed(graine, kind='Mersenne-Twister')
  # prise d'un echantillon de la taille n*p a partir de 1:2^32-1
  x <- sample.int(2^32-1, n*p)
  # creation d'une matrice avec n lignes et p colonnes
  x <- matrix(x, nrow=n, ncol=p)
  
  return(x)
}

# generateur RANDU
# @params p: nombre de sequences generees
# @params n: nombre d'entiers par sequences generee 
# @params graine: etat initiale, graine
# @return: une matrice qui contient p sequences (colonnes) de n entiers
randu <- function(n, p=1, graine) 
{
  # vecteur de la longueur (n*p+1) construit en base de repetition de la valeur
  # graine
  x <- rep(graine, n*p+1)
  
  for(i in 2:(n*p+1))
  {
    x[i] <- (x[i-1] * 65539) %% 2^31
  }

  x <- matrix(x[2:(n*p+1)], nrow=n, ncol=p)
}

standardMinimal <- function(n, p=1, graine) 
{
  # vecteur de la longueur (n*p+1) construit en base de repetition de la valeur
  # graine
  x <- rep(graine, n*p+1)
  
  for(i in 2:(n*p+1))
  {
    x[i] <- (x[i-1] * 16807) %% (2^31 - 1)
  }
  
  x <- matrix(x[2:(n*p+1)], nrow=n, ncol=p)
}

# transformation des nombres en sequences de 32 bits
# selon la convention Big-Indian
binary <- function(x)
{
  # utilisation de la fonction standard de R intToBits() 
  # si les entiers sont compris entre 0 et +(2^31-1)
  if((x < 2^31) & (x >= 0))
    return(as.integer(rev(intToBits(as.integer(x)))))
  else{
    if((x < 2^32) & (x > 0))
      return(c(1,binary(x-2^31)[2:32]))
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

Frequency <- function(x, nb)
{
  s_obs <- rep(0, length(x))
  
  for (i in 1:length(x))
  {
    bit_x <- rev(binary(x[i]))
    s <- 0
    for (j in 1:nb)
    {
      if (bit_x[j] == 0)
      {
        s <- s - 1
      } else {
        s <-s + 1
      }
    }
    s <- abs(s)
    s_obs[i] <- s / sqrt(nb)
  }
  return(s_obs)
}

Runs <- function(x, nb)
{
  p_vals <- rep(0, length(x))
  tau = 2 / sqrt(nb)

  for (i in 1:length(x))
  {
    bit_x <- rev(binary(x[i]))
    Pi <- sum(bit_x[1:nb])
    Pi <- Pi / nb
    if (Pi - 0.5 >= tau)
    {
      p_vals[i] = 0
    } else {
      v <- sum(bit_x[1:nb]) + 1
      p_vals[i] <- 2 * (1-pnorm((abs(v - 2 * nb * Pi * (1-Pi)))/(2*sqrt(nb)*Pi*(1-Pi))))
      
    }
  }
  return(p_vals)
}

LoiBinomiale <- function(n, p)
{
  sum <- 0
  for (i in 1:n)
  {
    if (runif(1) <= p)
      sum <- sum + 1
  }
  return(sum)
}
