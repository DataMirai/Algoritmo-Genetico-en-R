Individual_fitness_calculation <- function(
  individual, 
  objective_funtion, 
  optimization,
  verbose = TRUE, ...) {
  
  # This function returns the fitness of EACH individual of a population.
  
  # //////////////////////////////////////////////////////////////////////////////
  # ARGUMENTS
  # individual:           Vector with the data of individuals.
  #                       The order of the data must coincide with
  #                       with the one for the individuals column's.
  # objective_function:   Name of the function object to be optimized.
  #                       Must be defined previously
  # optimization:         Has 2 parameters; "maximize" or "minimize". 
  #                       Depending on this, the relation of the fitness
  #                       is directly or indirectly proportional to the value of the function.
  #                       Maximize: the individual has higher fitness the HIGHER the value of the objective function
  #                       Minimize: the individual has higher fitness the LOWER  the value of the objective function
  # verbose:              Display information on screen.
  #
  # Output: The individual fitness for the chosen function.
  
  # INITIAL COMPROBATIONS
  # //////////////////////////////////////////////////////////////////////////////
  if (length(individual) != length(names(formals(objective_funtion)))) {
    stop(paste("The individuals must have as many values as the target function has arguments."))
  }

  # CÁLCULO FITNESS
  # //////////////////////////////////////////////////////////////////////////////
  if (optimization == "maximize") {
    fitness <- do.call(objective_funtion, args = as.list(individual))
  } 
  else if (optimization == "minimize") {
    fitness <- -(do.call(objective_funtion, args = as.list(individual)))
  } 
  else {
    stop("The optimisation argument must be to maximise or minimise.")
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # //////////////////////////////////////////////////////////////////////////////
  if (verbose) {
    cat("The individual has been evaluated", "\n")
    cat("-----------------------------", "\n")
    cat("Optimization    =", optimization, "\n")
    cat("Ind_conditions  =", paste("(",individual,")", collapse = ","), "\n")
    cat("Fitness         =", fitness, "\n")
    cat("\n")
  }
  print(fitness)
  return(fitness)
}


# //////////////////////////////////////////////////////////
# Example ----
# //////////////////////////////////////////////////////////
# The objective function must be able to receive all variables from the individuals

funcion_test <- function(x1, x2) {
  return(x1 + x2)
}

fitness_ind <- Individual_fitness_calculation(
  individual        = c(10, 10),
  objective_funtion = funcion_test,
  optimization      = "maximize",
  verbose           = TRUE
)





popu<- create_population(
  n_population = 1000,
  n_variables = 5,
  inf_limit = NULL,
  sup_limit = NULL,
  verbose=T) 

popu %>%
  as.data.frame() %>%
  mutate("n_ind" = 1:dim(.)[1]) %>%
  select("n_ind", everything() ) %>%
  nest(-n_ind) %>%
  mutate( )


test1 <- apply(popu[,1:2], 1, Individual_fitness_calculation , funcion_test, "minimize", T )
test2 <- apply(popu[,1:2], 1, Individual_fitness_calculation , funcion_test, "maximize", F )


# //////////////////////////////////////////////////////////
# Function for population ----
# //////////////////////////////////////////////////////////


calcular_fitness_poblacion <- function(
  poblacion, 
  funcion_objetivo, 
  optimizacion,
  verbose = TRUE, ...) {
  
  # Esta función devuelve el fitness de cada individuo de una población.
  #
  # ARGUMENTOS
  # ============================================================================
  # poblacion:        matriz que representa la población de individuos.
  # funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # optimizacion:     "maximizar" o "minimizar". Dependiendo de esto, la relación
  #                   del fitness es directamente o indirectamente proporcional
  #                   al valor de la función.
  # verbose:          mostrar información del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # Vector con el fitness de todos los individuos de la población. El orden de
  # los valores se corresponde con el orden de las filas de la matriz población.
  
  
  # CÁLCULO DEL FITNESS DE CADA INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  for (i in 1:nrow(poblacion)) {
    individuo <- poblacion[i, ]
    
    fitness_individuo <- calcular_fitness_individuo(
      individuo        = individuo,
      funcion_objetivo = funcion_objetivo,
      optimizacion     = optimizacion,
      verbose          = verbose
    )
    fitness_poblacion[i] <- fitness_individuo
  }
  
  # MEJOR INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Se identifica el mejor individuo de toda la población, el de mayor
  # fitness.
  indice_mejor_individuo <- which.max(fitness_poblacion)
  
  # Se identifica el valor de la función objetivo para el mejor individuo.
  if (optimizacion == "maximizar") {
    valor_funcion <- fitness_poblacion[indice_mejor_individuo]
  } else {
    valor_funcion <- -1*fitness_poblacion[indice_mejor_individuo]
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("------------------", "\n")
    cat("Población evaluada", "\n")
    cat("------------------", "\n")
    cat("Optimización              =", optimizacion, "\n")
    cat("Mejor fitness encontrado  =", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor solución encontrada =",
        paste(poblacion[indice_mejor_individuo,], collapse = " "), "\n")
    cat("Valor función objetivo    =", valor_funcion, "\n")
    cat("\n")
  }
  
  return(fitness_poblacion)
}

