
# ////////////////////////////////////////////////////////////////////
# Create population function ----
# ////////////////////////////////////////////////////////////////////

Individuals_combination <- function(
    dataframe_2_individuals,
    crossing_method = 'uniform' ,...){

  require('tidyverse')

  # This function returns an individual resulting from crossing two parental individuals
  # parental individuals with the uniform or single point crossover method.

  # ARGUMENTS
  # ============================================================================
  # dataframe_2_individuals: population object.
  # crossing_method: method to establish the way of crossing 2 individuals. It can
  # be: "uniform".

  # RETURN
  # ============================================================================
  # the index occupied by the selected individual.
    
  name_traits<- names(dataframe_2_individuals)
    
  herencia <- sample(
    x = c(T,F), 
    size = dim(dataframe_2_individuals)[2], 
    replace = T)
    
  descendencia<- list()
    
  descendencia$descendencia1[ herencia] <- dataframe_2_individuals[1, herencia]
  descendencia$descendencia1[!herencia] <- dataframe_2_individuals[2,!herencia]
    
  descendencia$descendencia2[!herencia] <- dataframe_2_individuals[1,!herencia]
  descendencia$descendencia2[ herencia] <- dataframe_2_individuals[2, herencia]
    
  descendencia <- descendencia %>%
    map(., data.frame ) %>%
    map(., set_names, name_traits) %>%
    map_dfr(~.x)
    
  return(descendencia)
  
}








