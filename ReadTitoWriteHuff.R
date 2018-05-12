library(tidyverse)
library(magrittr)
library(readxl)

## Read the five locations with their weights
## (This is basically just 1 record.)
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
location_weights$PARKING

# lweights[4,2] <- 5123


# Get the file exported by the Export Distance function.
distance_table <- read_csv("TitosHeavyGravityGrids.csv")
#It didn't work (some grids had pound keys & some didn't and it didn't read in), so try exporting an Excel version

distance_table <- read_excel("DistExport18.xlsx")

# Only keep the location key, the MG key, and the distance field
dtx <- distance_table %>% 
  transmute(key = `key_from:From key`, 
             k_to = `key_to:To key`,
            distance = `dist_mi:Distance (miles)`) %>%
    arrange(k_to) 


# Make a table that has the things necessary for the equation
model_source <- inner_join(dtx,lweights, by ='key', copy = TRUE)
head(model_source)
# That's it! but of course, look at it
View(model_source)
# Actually, that's not it at all!  You have to make 5 GP values for each grid, not one, duh!
#  And then you have to say which is the biggest one, double duh!

# Back to the drawing board!

## get the grouped values by destination K_TO  (microgrid, of which there are 5 one for each location)
dtxg <- group_by(dtx, k_to) %>%
  do(distances=.$distance, locations = .$key ) %T>% str()

#dtxg <- dtxg %>% mutate(PPALL = I(list(C(5,5,5,5,5))))

## gfunc <- GLA ** alphaAREA / DISTANCE ** betaDISTANCE

## Probability of Patronage    = sigma gfuncs of(12345) 

## the question is, what are alpha and beta?

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

######## Other way of doing things
## microgrid 18
distance_table <- read_excel("DistExport18.xlsx")

# Only keep the location key, the MG key, and the distance field
dtx <- distance_table %>% 
  transmute(key = `key_from:From key`, 
            k_to = `key_to:To key`,
            distance = `dist_mi:Distance (miles)`) %>%
      arrange(k_to) 

# dtx %>% mutate(k_to = case_when(NA!=str_match(k_to,"#") ~ '#',
#                         (NA==str_match(k_to,"#")) ~ "##")
# ) -> q

## Some K_to keys are malformed, lacking a # sign
nkeys <- str_match(dtx$k_to,"(#{1})?(.*)")
dtx$k_to <- paste0("#",nkeys[,3])

# The impossible way to do it:
# q <- dtx %>% mutate(
#   area_attractor = lweights[lweights$key==key,]$parking
# )

# The easy way it's done
q <- merge(dtx,lweights, by='key')

# calculate the huff value for each origin-destination pair
q_pvals <- q %>% mutate(
  huff_numerator = huff_raw(parking, alphaAREA, distance, betaDISTANCE)
)

## Total the numerators for the demoninator
sum_huff_location <- aggregate(q_pvals$huff_numerator, by = list(q_pvals$k_to), sum)
str(sum_huff_location)
names(sum_huff_location) <- c("k_to","huff_denominator")
big_table <- merge(q_pvals,sum_huff_location, by='k_to')

# here is the simple calc for all N destination locations
big_table$pp <- big_table$huff_numerator / big_table$huff_denominator
big_table <- big_table %>% arrange(k_to,key)  
# Now we pick the max for each grid
check_max <- group_by(big_table, k_to) %>%
  do(probabilities=.$pp) %T>% str()

# Now we unspool the list of all N probs for each grid, just to put it out there.
# (kind of a pain)
for (index in 1:nrow(check_max) ) { 
  basis_probabilities = unlist(check_max[index, "probabilities" ]) # do stuff with the row 
  pp <- max(basis_probabilities)
  check_max[index,"PP1"] <- pp
} 


# Now we successfully put it into a data frame!
newprob <- ungroup(check_max) %>% group_by(k_to) %>%
dplyr::do(.,data.frame(matrix(unlist(.$probabilities),nrow=1)) ) %>% 
  as.data.frame()

#  Add the max probability back in
newprob$pp1 <- check_max$PP1
names(newprob) <- c("GridKey", "Loc0001", "Loc0002", "Loc0003", "Loc0004", "Loc0005", "PofP")
# Try and removes the dupes so that it will load!!! 
#deduped <- distinct(newprob$k_to,.keep_all = TRUE)
# The presence of dupes was the best Excel bug story ever

# Write the file but there are dupes
write_excel_csv(newprob,"Tito11.csv")

