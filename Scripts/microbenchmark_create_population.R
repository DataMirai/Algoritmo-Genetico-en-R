
n_population<-1000
n_variables<- 10
inf_limit<- rep(-10^3,n_variables)
sup_limit<- rep( 10^3,n_variables)

# version 1 ----

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

# version 1 funcion ----
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



microbenchmark(
  test_n_pop_1(100,10, rep(-10^3), rep(10^3) )
)



# version 2 ----