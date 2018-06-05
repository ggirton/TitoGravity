library(tidyverse)
library(magrittr)
library(readxl)

## Read the five destinations locations with their weights
## (This basically becomes just 1 record, although it is in the form of  record for each destination)
# The key must match the key of the incoming point-to-point distance table
location_weights <- read_csv("titos-expanding-locations.csv",  col_types = 
                               cols(
                                 KEY = col_character(),
                                 LATITUDE = col_character(),
                                 LONGITUDE = col_character(),
                                 NAME = col_character(),
                                 PARKING = col_integer()
                               ))

## Drop everything except the key and the weight
lweights <- transmute(location_weights, key=KEY, parking = PARKING)

# check it out!
# location_weights$PARKING
lweights
# lweights[4,2] <- 5123


distance_table <- read_excel("DistExport18.xlsx")
# some grids had pound keys & will have to prepend later

# Only keep the destination location key, the origion location (MicroGrid) key, and the distance field
dtx <- distance_table %>% 
  transmute(key = `key_from:From key`, 
            k_to = `key_to:To key`,
            distance = `dist_mi:Distance (miles)`) %>%
  arrange(k_to) 


## get the grouped values by destination K_TO  (microgrid, of which there are 5 one for each location)

dtxg <- group_by(dtx, k_to) %>%
  do(distances=.$distance, locations = .$key ) %T>% str()

#dtxg <- dtxg %>% mutate(PPALL = I(list(C(5,5,5,5,5))))

## gfunc <- GLA ** alphaAREA / DISTANCE ** betaDISTANCE

## Probability of Patronage    = sigma gfuncs of(12345) 

## the question is, what do you want to have for alpha and beta?

huff_raw <- function(GLA, alphaAREA, DISTANCE, betaDISTANCE) {
  # print(DISTANCE)
  return ((GLA ** alphaAREA) /(DISTANCE ** betaDISTANCE))
}

huff_raw(2,2,3,3)  ## should be 4/27
4/27
huff_raw(2,1,3,1) # should be 2/3

#basis_pairs <- c()
#basis_probabilities <- c(0,0,0,0,0)
#length(basis_pairs) <- length(location_weights)  # preallocate this tiny array for speed

alphaAREA <- 1
betaDISTANCE <- 1.1


# /* nrow(dtx)*/ 


# Wrangle the horse

for (index in 1:nrow(dtxg) ) { 
  rowDistance = unlist(dtxg[index, "distances" ]) # do stuff with the row 
  # print(rowDistance)
  basis_pairs <- huff_raw(lweights$parking, alphaAREA, rowDistance, betaDISTANCE)
  pdivisor <- sum(basis_pairs)
  basis_probabilities <- basis_pairs/pdivisor
  z <- basis_probabilities
  #  print(basis_probabilities) 
  dtxg[index,"DIV"] <- sum(basis_pairs)
  #print(c(rowDistance,basis_pairs[1],basis_probabilities[1]))
  # dtx[index, "minp"] <- min(basis_probabilities)
  pp <- max(basis_probabilities)
  ## print(basis_probabilities)
  dtxg[index,"PP1"] <- pp
} 

# How did the horse get onto the balcony???


#as.vector(z,)
max(dtxg$PP1)
min(dtxg$PP1)
range(dtxg$PP1)

str(basis_pairs)
sum(basis_probabilities)
basis_probabilities

pp1 <- dtxg %>% select(k_to,PP1)
write_excel_csv(pp1,"Tito11a.csv")
