# ////////////////////////////////////////////////////////////////////
# Create population function ----
# ////////////////////////////////////////////////////////////////////

mutate_individual <- function(
    population, 
    mutation_ratio = 0.01, 
    mutation_type = 'normal',
    boundaries = T) {
  
  require("tidyverse")
  
  ####
  # Documentation ----
  ####
  
  # This function creates mutations in individuals to avoid local stagnations in the score function. 
  # By default, the mutation probability will be set to 0.01 and the mutations will be performed
  # with the addition of a normal random number, using the value of the individual as mean 
  # and 5 times variance.
  
  # ////////////////////////////////////////////////////////////////////////////////
  # Arguments
  # ////////////////////////////////////////////////////////////////////////////////
  # individual: Vector. The values of the individuals to evaluate.
  # mutation_ratio: double between 0 and 1. The probability of applying a mutation.
  # mutation_type:  character. The type of criteria applied to made the value transformation.
  # boundaries :  Boolean. Allows to exceed the limits of the variable in the mutation.
  
  # ////////////////////////////////////////////////////////////////////////////////
  # Returns
  # /////////////////////////////////////////////////////////////////////////////////
  # A double with the fitness of the individual towards the score function.
  
  ####
  # Checks ----
  ####
  
  # Check mutation_ratio is a probability ----
  if(!(mutation_ratio >= 0 & mutation_ratio <= 1) | !is.double(mutation_ratio) ){
    stop(paste('The mutation ratio must be a double between 0 and 1'))
  }
   # Check mutation_type is one the options ----
  if(!(mutation_type %in% c('random_normal', 'random_bits'))){
    stop(paste('The mutation_type must be one the choices',
               'between normal and ...'))
  }
  
  # Check boundaries limits ----
  
  if(!(is.logical(boundaries) )){
    stop(paste('Boundaries parameters must be logical'))
  }
  # //////////////////////////////////////////////////////
  # Muatciones de tipo normal ----
  # //////////////////////////////////////////////////////
  if(mutation_type == 'normal'){
    population$individuals_traits %>%
      map_dfc(
        ~ map_dbl(., 
                  ~ .x + 
                    sample(c(T,F),1, prob = c(mutation_rate,1 - mutation_rate)) *
                    rnorm(1, mean = .x , sd = abs(.x) )))
    
  }
  # //////////////////////////////////////////////////////
  # Muatciones de aleatorio por bits ----
  # //////////////////////////////////////////////////////
  if(mutation_type == 'normal'){
    
    
  }
  # //////////////////////////////////////////////////////
  # //////////////////////////////////////////////////////
  if(mutation_type == 'normal'){
    
    
  }
  
  # //////////////////////////////////////////////////////
  # //////////////////////////////////////////////////////
  new_individual <- 'poyas'
  return(new_individual)
}

t<-c('normal','normal','normal','normal', 'random', 'aleatorio') %in% c('normal', 'random')







