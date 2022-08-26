

# ////////////////////////////////////////////////////////////////////
# Individual fitness calculation function ----
# ////////////////////////////////////////////////////////////////////


Individual_fitness_calculation <- function(
    individual, 
    objective_funtion, 
    optimization= 'maximize', ...) {
  
  require("tidyverse")
  
  # This function returns the fitness of an individual.
  
  # //////////////////////////////////////////////////////////////////////////////
  # ARGUMENTS
  # individual:           Vector with the data of individuals.
  #                       The order of the data must coincide with
  #                       with the one for the individuals column's.
  # objective_function:   Name of the function object to be optimized.
  #                       Must be defined previously in the script 'Source_function.R'
  # optimization:         Has 2 parameters; "maximize" or "minimize". 
  #                       Depending on this, the relation of the fitness
  #                       is directly or indirectly proportional to the value of the function.
  #                       Maximize: the individual has higher fitness the HIGHER 
  #                                 the value of the objective function.
  #                       Minimize: the individual has higher fitness the LOWER  
  #                                 the value of the objective function.
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
    fitness <- 1 / ( 1 + (do.call(objective_funtion, args = as.list(individual))) )
  } 
  else {
    stop("The optimisation argument must be to maximise or minimise.")
  }
  
  return(fitness)
}