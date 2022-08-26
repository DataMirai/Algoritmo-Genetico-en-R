source('Scripts/Auxiliar_functions/Create_population_function.R')
source('Scripts/Auxiliar_functions/Source_function.R')
source('Scripts/Auxiliar_functions/Individual_fitness_calculation.R')
source('Scripts/Auxiliar_functions/Individual_selection.R')
source('Scripts/Auxiliar_functions/Individuals_recombination.R ')
# ////////////////////////////////////////////////////////////////////////////////////
# Settings  ----
# ////////////////////////////////////////////////////////////////////////////////////

population <- create_population(
  n_population = 30,
  n_variables = 3,
  inf_limit = c(-1,10,-20),
  sup_limit = c(1,100,200),
  mutation_type = rep('normal',3)) 


# ////////////////////////////////////////////////////////////////////////////////////
#  Loop ----
# ////////////////////////////////////////////////////////////////////////////////////

population$individuals_traits <- population$individuals_traits %>%
  mutate(
    fitness = map_dbl(
      data, 
      ~ Individual_fitness_calculation(
        individual        = as.vector(unlist(.x)),
        objective_funtion = score_function,
        optimization      = "minimize"))) %>%
  arrange(desc(fitness))

population <- Individual_selection(population, selection_criteria = 'roulette')


population$individuals_traits <- replicate(
  n= dim(population$selected)[1]/2,
  expr = sample( 
    x       = 1:(dim(population$selected)[1]), 
    size    = 2, 
    replace = F), 
  simplify = F) %>% 
  map(~ population$selected[.x,]$data ) %>%
  map(~ bind_rows(.x) ) %>%
  map(~ Individuals_combination(.x)) %>%
  bind_rows() 


population <- population %>% list_modify("selected" = NULL)

population
