# ////////////////////////////////////////////////////////////////////
# Create population function ----
# ////////////////////////////////////////////////////////////////////

create_population <- function(
  n_population, 
  n_variables, 
  inf_limit = NULL,
  sup_limit = NULL,
  mutation_type = NULL,...) {
  
  require("tidyverse")
  
  ####
  # Documentation ----
  ####
  
  # This function creates a matrix in which each row consists of a combination of random numeric values. 
  # The range of possible values for each variable can be bounded.
  
  # ////////////////////////////////////////////////////////////////////////////////
  # Arguments
  # ////////////////////////////////////////////////////////////////////////////////
  # n_population:  integer// Number of total amount of individuals.
  # n_variables:   integer// Number of columns to fulfill for each individual.
  # inf_limit:     numeric Vector // The lower limit of each variable. 
  #                If you do not want to impose a lower bound on some variables, 
  #                use NA for those variables.
  # sup_limit :    numeric Vector // The upper limit of each variable. 
  #                If you do not want to impose a high bound on some variables, 
  #                use NA for those variables.
  # mutation_type: character vector // 
  #   
  # ////////////////////////////////////////////////////////////////////////////////
  # Returns
  # /////////////////////////////////////////////////////////////////////////////////
  # A matrix of size n_population x n_variables representing a population.
  
  
  ####
  # Checks ----
  ####

  # Check the dimension of lower constraints coincide with the amount of variables ----
  if (!is.null(inf_limit) & (length(inf_limit) != n_variables)) {
    stop(paste(
      "Dimension of lower constrainst vector does not coincide with number of variables",
      "inf_limit must have a single value for each variable",
      "If no limit is desired for any variable, use NA. Do not omit the value",
      "Example: inf_limit = c(10, NA, 10)"
    ))
  
  # Check the dimension of upper constraints coincide with the amount of variables ----
  } else if (!is.null(sup_limit) & length(sup_limit) != n_variables) {
    stop(paste(
      "Dimension of upper constrainst vector does not coincide with number of variables",
      "sup_superior must have a single value for each variable",
      "If no limit is desired for any variable, use NA. Do not omit the value",
      "Example: sup_limit = c(10, NA, 10)"))
    
  } else if (!is.null(mutation_type) & length(mutation_type) != n_variables){
    stop(paste(
      'Dimension of mutation vector does not coincide with number of variables',
      'mutation_type must have a single value for each variable',
      'If mutation type is desired for any variable, use NA. Do not omit the value',
      'Example: mutation_type = c(normal, NA, normal)',
      ))
    
  # No constraint vectors ----
  } else if (is.null(sup_limit) | is.null(inf_limit)) {
    warning(paste(
      "It is highly recommended to indicate the limits within which",
      "the solution of each variable must be sought within.\n",
      "Otherwise there is high probability of no conversion.\n",
      "By default [-10^3, 10^3] is used."))
  
  # NA warning in constraint vectors ----
  } else if (any(any(is.na(sup_limit)), any(is.na(inf_limit)))) {
    warning(paste(
      "The default limits used when they have not been defined are:",
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
  
  # If mutation_type is not specified, 
  # the default value for that variables will be 'normal'.
  
  if (is.null(mutation_type)) {
    mutation_type <- rep(x = 'normal', times = n_variables)
  }
  
  
  # If the inferior limits are not null, NA positions are replaced by default value
  # being -10^3 and 10^3 for lower and upper constraints vectors respectively.
  if (!is.null(inf_limit)) {
    inf_limit[is.na(inf_limit)] <- -10^3
  }
  
  # If the superior limits are not null, NA positions are replaced by default value
  # being 10^3
  
  if (!is.null(sup_limit)) {
    sup_limit[is.na(sup_limit)] <- 10^3
  }
  
  # If mutation type are not null, NA positions are replaced by default value
  # being normal.
  
  if (!is.null(mutation_type)) {
    mutation_type[is.na(mutation_type)] <- 'normal'
  }

  
  ####
  # Create Population matrix ----
  ####

  population <- list(
    'individuals_traits' = pmap(
       list(n_population, inf_limit, sup_limit),
       runif) %>% 
      unlist(.) %>% 
      matrix(., ncol= n_variables, byrow = F) %>%
      as.data.frame() %>%
      mutate("n_ind" = 1:dim(.)[1]) %>%
      select("n_ind", everything() ) %>%
      nest(-n_ind) %>%
      ungroup(),
    'inferior_limits' = inf_limit,
    'superior_limits' = sup_limit,
    'mutation_type' = mutation_type) 
  
  
  ####
  # Stored information in the attributes ----
  ####
  
  cat("\n")
  attr(population,'Date_creation') <- Sys.time()
  attr(population,'class') <- c('list','population')
  cat("\n")
  
  return(population)
}





