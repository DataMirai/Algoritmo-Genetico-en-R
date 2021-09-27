
popu<-create_population(100, 2, c(1,100), 5*c(1,100), verbose=F)

funcion_test <- function(x1, x2) {
  return(x1 + x2)
}

attributes(popu)
popu[1,]

funcion_test(popu[1,1],popu[1,2])


fitness <- Individual_fitness_calculation(
  individual        = popu,
  objective_funtion = funcion_test,
  optimization      = "maximize",
  verbose           = TRUE
)


popu
