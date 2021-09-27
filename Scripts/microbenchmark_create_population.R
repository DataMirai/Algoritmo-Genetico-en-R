
library("microbenchmark")
library("tidyverse")

n_population<-10
n_variables<- 5
inf_limit<- rep(-10^3,n_variables)
sup_limit<- rep( 10^3,n_variables)

# //////////////////////////////////////////////////////////////////////
# version 1 ----
# //////////////////////////////////////////////////////////////////////

population <- matrix(data = NA, nrow = n_population, ncol = n_variables)

# Bucle para crear cada individual.
for (i in 1:n_population) {
  # Se crea un vector de NA que representa el individual.
  individual <- rep(NA, times = n_variables)
  
  for (j in 1:n_variables) {
    # Para cada posición, se genera un valor aleatorio dentro del rango permitido
    # para cada variable.
    individual[j] <- runif(n = 1, min = inf_limit[j], max = sup_limit[j])
  }
  # Se añade el nuevo individual a la población.
  population[i, ] <- individual
}

population



# //////////////////////////////////////////////////////////////////////
# Version 2
# //////////////////////////////////////////////////////////////////////

howMany <- 10
lowerBounds <- c(0 ,100, 10000)
upperBounds <- c(10,200, 20000)
args <- list(howMany, lowerBounds, upperBounds)
args %>% 
  pmap(runif) %>% 
  unlist(.) %>% 
  matrix(, ncol=3, byrow = F) 


# //////////////////////////////////////////////////////////////////////
# Version 3
# //////////////////////////////////////////////////////////////////////
population <- matrix(data = NA, nrow = n_population, ncol = n_variables)








# //////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////


# //////////////////////////////////////////////////////////////////////
# version 1 funcion ----
# //////////////////////////////////////////////////////////////////////

test_n_pop_1 <- function(
  n_population,
  n_variables,
  inf_limit=NULL,
  sup_limit=NULL){
  
  population <- matrix(data = NA, nrow = n_population, ncol = n_variables)
  
  # Bucle para crear cada individual.
  for (i in 1:n_population) {
    # Se crea un vector de NA que representa el individual.
    individual <- rep(NA, times = n_variables)
    for (j in 1:n_variables) {
      # Para cada posición, se genera un valor aleatorio dentro del rango permitido
      # para cada variable.
      individual[j] <- runif(n = 1, min = inf_limit[j], max = sup_limit[j])
    }
    # Se añade el nuevo individual a la población.
    population[i, ] <- individual
  }
  population
}

test_n_pop_2 <- function(
  n_population,
  n_variables,
  inf_limit=NULL,
  sup_limit=NULL){
  
  args <- list(n_population, inf_limit, sup_limit)
  args %>% 
    pmap(runif) %>% 
    unlist(.) %>% 
    matrix(., ncol= n_variables, byrow = F) 
  
  
}

microbenchmark(
  test_n_pop_1(10000,100, rep(-10^3,100), rep(10^3,100) ),
  test_n_pop_2(10000,100, rep(-10^3,100), rep(10^3,100) )
)


test_n_pop_2(100,5,
             c(0,1 ,50 , 200 , 10000 ),
             c(1,10,100, 1000, 100000  ))

# version 2 ----