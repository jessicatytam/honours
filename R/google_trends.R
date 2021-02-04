library(tidyverse)
library(gtrendsR)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

search_terms <- c("Phascolarctos cinereus")

output <- gtrends(keyword = search_terms,
                  time = "all") 

ggplot(output$interest_over_time, aes(x = date,
                                      y = hits)) +
  geom_line()



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



#testing

write.csv(includeh, file = "outputs/includeh.csv")
includeh <- read.csv(file = "outputs/includeh.csv", header = T)[-c(1)]