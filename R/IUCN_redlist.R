library(dplyr)

RedList <- read.csv(file = "assessments.csv", header = T)[c(3,4)]
RedList <- RedList %>%
  arrange(scientificName)

write.csv(RedList, file = "intermediate_data/IUCN_redlist.csv")
