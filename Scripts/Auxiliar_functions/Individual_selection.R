
# ////////////////////////////////////////////////////////////////////
# Create population function ----
# ////////////////////////////////////////////////////////////////////

Individual_selection <- function(
    population, 
    selection_criteria = 'roulette' ,...){
  
  require('tidyverse')
  # This function receives as argument a vector with the fitness of each individual
  # and selects one of the positions, where the probability of selection is
  # proportional to the fitness.
  
  # ARGUMENTS
  # ============================================================================
  # population: population object. 
  # selection_criteria: method to establish the probability of selection. It can
  # be: "roulette", "rank", or "tournament".
  
  #
  # RETURN
  # ============================================================================
  # the index occupied by the selected individual.

  
  # COMPROBACIONES INICIALES
  # ---------------------------------------------------------------------------
  if (!selection_criteria %in% c("roulette", "rank", "tournament")) {
    stop("The selection method should be roulette, rank or tournament.")
  }
  
  # INDIVIDUALS SELECTION 
  # ----------------------------------------------------------------------------
  # The probability of selection of each individual is calculated as a function
  # of its fitness.

  if( selection_criteria == "roulette"){
    population$individuals_traits <- population$individuals_traits %>%
      mutate(distribution_seleccion = abs(fitness)/ sum(abs(fitness)))
    
    population$selected <- population$individuals_traits[
      sample(
        population$individuals_traits$n_ind, 
        length(population$individuals_traits$n_ind),
        replace = T,
        prob = population$individuals_traits$distribution_seleccion), ]
    
  } else if (selection_criteria == "rank"){
    population$individuals_traits <- population$individuals_traits %>%
      mutate( distribution_seleccion = 1 / rank( - fitness, ties.method = c("first")))
    
    population$selected <- population$individuals_traits[
      sample(
        population$individuals_traits$n_ind, 
        length(population$individuals_traits$n_ind),
        replace = T,
        prob = population$individuals_traits$distribution_seleccion), ]
  }
  population <- population %>% list_modify("individuals_traits" = NULL)
  
  return( population )
}


Individual_selection(popu,selection_criteria = 'roulette')
Individual_selection(popu,selection_criteria = 'rank')
