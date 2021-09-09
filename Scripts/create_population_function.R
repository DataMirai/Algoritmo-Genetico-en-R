#################################################################################################
# Create population function ----
#################################################################################################
create_population <- function(
  n_population, 
  n_variables, 
  inf_limit = NULL,
  sup_limit = NULL, 
  verbose = TRUE) {
  
  ####
  # Documentation ----
  ####
  
  # This function creates a matrix in which each row consists of a combination of random numeric values. 
  # The range of possible values for each variable can be bounded.
  
  # Arguments
  # ============================================================================
  # n_population: Number of total amount of individuals.
  # n_variables:  Number of columns to fulfill for each individual.
  # inf_limit:    Vector with the lower limit of each variable. 
  #               If you do not want to impose a lower bound on some variables, 
  #               use NA for those variables.
  # sup_limit :   Vector with the upper limit of each variable. 
  #               If you do not want to impose a high bound on some variables, 
  #               use NA for those variables.
  # verbose:      Display information of the process on the screen. 
  #   
  # Returns
  # ============================================================================
  # A matrix of size n_population x n_variables representing a population.
  
  
  ####
  # Checks ----
  ####

  # Check the dimension of lower constraints coincide with the amount of variables  
  if (!is.null(inf_limit) & (length(inf_limit) != n_variables)) {
    stop(paste(
      "Dimension of lower constrainst vector does not coincide with number of variables",
      "inf_limit must have a single value for each variable",
      "If no limit is desired for any variable, use NA. Do not omit the value",
      "Example: inf_limit = c(10, NA, 10)"
    ))
  
  # Check the dimension of upper constraints coincide with the amount of variables
  } else if (!is.null(sup_limit) & length(sup_limit) != n_variables) {
    stop(paste(
      "Dimension of upper constrainst vector does not coincide with number of variables",
      "inf_superior must have a single value for each variable",
      "If no limit is desired for any variable, use NA. Do not omit the value",
      "Example: sup_limit = c(10, NA, 10)"))
  
  # No constraint vectors
  } else if (is.null(sup_limit) | is.null(inf_limit)) {
    warning(paste(
      "It is highly recommended to indicate the limits within which",
      "the solution of each variable must be sought within.\n",
      "Otherwise there is high probability of no conversion.",
      "By default [-10^3, 10^3] is used."))
  
  # NA warning in constraint vectors
  } else if (any(any(is.na(sup_limit)), any(is.na(inf_limit)))) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."))
    
    cat("\n")
  }
  
  ####
  # NA treating in constraints ----
  ####
  
  # If inf_limit is not specified, 
  # the minimum value that the variables can take is -10^3.
  
  if (is.null(inf_limit)) {
    inf_limit <- rep(x = -10^3, times = n_variables)
  }
  
  # If sup_limit is not specified, 
  # the maximum value that the variables can take is 10^3.
  
  if (is.null(sup_limit)) {
    sup_limit <- rep(x = 10^3, times = n_variables)
  }
  
  # If the limits are not null, NA positions are replaced by default value
  # being -10^3 and 10^3 for lower and upper constraints vectors respectively.
  if (!is.null(inf_limit)) {
    inf_limit[is.na(inf_limit)] <- -10^3
  }
  if (!is.null(sup_limit)) {
    sup_limit[is.na(sup_limit)] <- 10^3
  }
  
  ####
  # Create Population matrix ----
  ####
  
  # Final Matrix creation where to store the generated individuals.
  population <- matrix(data = NA, nrow = n_population, ncol = n_variables)
  
  # Loop for individual creation
  for (i in 1:n_population) {
    # A vector full of NA is created representing an individual
    individual <- rep(NA, times = n_variables)
    
    for (j in 1:n_variables) {
      # For each position in the individual vector, it is created a random value 
      # according the boundaries restrictions
      
      individual[j] <- runif(n = 1, min = inf_limit[j], max = sup_limit[j])
    }
    # Add the individual to the population
    population[i, ] <- individual
  }
  cat("\n")
  ####
  # Stored information in the attributes ----
  ####
  attr(population, 'Date_creation')    <- Sys.time()
  attr(population, 'n_individuals') <- n_population
  attr(population, "class") <- c("matrix", "population")
  
  if (verbose) {
    cat("Initial population created", "\n")
    cat("------------------------", "\n")
    cat("Date creation:", as.character(Sys.time()), "\n")
    cat("Individuals number =", n_population, "\n")
    cat("Lower limits of each variable =", paste(inf_limit, collapse = ", "), "\n")
    cat("Upper limits of each variable =", paste(sup_limit, collapse = ", "), "\n")
    cat("\n")
  }
  
  return(population)
}

aaa<-create_population(
  n_population=100, 
  n_variables=10, 
  inf_limit = NULL,
  sup_limit = NULL, 
  verbose = TRUE )


  
