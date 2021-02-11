library(tidyverse)
library(gtrendsR)
library(curl)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


#get the data

output3 <- list()
for (i in 1726:6738) {
  print(paste(i, "getting data for", includeh$genus_species[i]))
  search_term <- includeh$genus_species[i]
  output3[[i]] <- gtrends(keyword = search_term,
                         time = "all")
  Sys.sleep(1)
}

saveRDS(output, "intermediate_data/gtrends_results1.RDS") #spp 1-1611
saveRDS(output2, "intermediate_data/gtrends_results2.RDS") #spp 1612-1725
saveRDS(output3, "intermediate_data/gtrends_results3.RDS") #spp 1726-3234


gtrends_output <- bind_rows(output)


output <- c("Bos taurus")
cow <- gtrends(keyword = output,
                 time = "all")

output <- c("Glis glis")
dormouse <- gtrends(keyword = output,
                    time = "all")


output <- c("Abditomys latidens")
mammal <- gtrends(keyword = output,
        time = "all")



ggplot(mammal$interest_over_time, aes(x = date,
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

