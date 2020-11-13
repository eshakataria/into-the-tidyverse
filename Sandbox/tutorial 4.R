2+6
sum(2,6)
mean(2,6)

#have to use c 
mean(c(2,6))

#make our own function for mean
custom_mean <- function(this_vector) {sum(this_vector) / length(this_vector)}

#this works bc supposed to be vector
custom_mean(c(2,6))
my_vector <-  c(2, 6)
#same thing: providing an argument to custom mean which gets assigned to this vector
custom_mean(my_vector)
custom_mean(this_vector = c(2, 6))
custom_mean(this_vector = my_vector)

library("here")
library("readr")
library("dplyr")

covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>%
  read.csv()
