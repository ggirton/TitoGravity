# RdDestWriteHuff5.R



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

