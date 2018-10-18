# renatamuy@gmail.com

# Recovering species names sep. by "_" 

names(en) <- sub("_"," " ,  names(en))

# Set f irst letter to  uppercase

uppercase1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
  }

names(en) <- uppercase1(names(en))