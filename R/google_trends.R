library(tidyverse)
library(gtrendsR)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


#get the data
search_terms <- includeh$genus_species

output <- list()
for (i in 1:length(includeh$genus_species)) {
  print(paste("getting data for", includeh$genus_species[i]))
  search_term <- includeh$genus_species[i]
  output[[i]] <- gtrends(keyword = search_term,
                         time = "all")
}
gtrends_output <- bind_rows(output)


ggplot(output$interest_over_time, aes(x = date,
                                      y = hits)) +
  geom_line()

#sum, slope, intercept

glm <- glm(hits ~ date, output$interest_over_time, family = poisson)
summary(glm)
plot(glm)

#map

by_country <- output$interest_by_country

by_country <- by_country %>%
  rename(name = location)
by_country$hits[is.na(by_country$hits)] <- 0

world <- ne_countries(scale = "large", returnclass = "sf")

world <- left_join(world, by_country)

ggplot(world) +
  geom_sf(aes(fill = hits)) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Number of hits") +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  scale_fill_viridis_c(option = "plasma") 

#reading and writing

write.csv(includeh, file = "outputs/includeh.csv")
includeh <- read.csv(file = "outputs/includeh.csv", header = T)[-c(1)]

#testing

