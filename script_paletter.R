# https://datascienceplus.com/how-to-use-paletter-to-automagically-build-palettes-from-pictures/
# Generating nice palletes based on an image

install_github("andreacirilloac/paletter")

library(ReadImages)
library(devtools)
require(paletter)

setwd("C:/Users/renatamuy/Desktop/")

palletec <- create_palette("Piero_della_Francesca_046.jpg",  number_of_colors =20,  type_of_variable = "categorical")

palletec

