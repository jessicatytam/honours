#loading data
includeh <- read.csv(file = "outputs/includeh.csv")[-c(1)]

#assign binary to complete / incomplete cases
for (i in 1:length(includeh$genus_species)) {
  if (!is.na(includeh$BodyMass.Value[i]) & !is.na(includeh$median_lat[i]) & !is.na(includeh$redlistCategory[i])) {
    includeh$complete[i] <- 1
  } else {
    includeh$complete[i] <- 0
  }
}

#tests
t.test(includeh$complete, includeh$h)
