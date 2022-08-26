
c(5,1234)
A <- c(1:100)
B <- c(1:100)

sample(1:100, 1)
sample(1:100, 1)
rev(intToBits(64))

rev(intToBits(22))

x <- as.integer(rev(intToBits(5)))
x

mutation_bit <- function(x, mutation_rate = 0.025){
  x <- as.integer(rev(intToBits(5)))
  
  succes_mutation <- rbinom(1,1,mutation_rate)
  if(succes_mutation){
    if(x==1){
      x <- 0
    }
    else {
      x <- 1
    }
  }
  else{
    x<-x
  } 
}

x1<- as.integer(unlist(map(x, mutation_bit)))

x;unlist(x1)
x1

strtoi(paste(x, collapse = ""),base=2);strtoi(paste(x1, collapse = ""),base=2)
