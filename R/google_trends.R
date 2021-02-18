library(tidyverse)
library(gtrendsR)
library(curl)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


#get the data

output7 <- list()
for (i in 5453:6788) {
  print(paste(i, "getting data for", includeh$genus_species[i]))
  search_term <- includeh$genus_species[i]
  output7[[i]] <- gtrends(keyword = search_term,
                         time = "all")
  Sys.sleep(1)
}

saveRDS(output, "intermediate_data/gtrends_results1.RDS") #spp 1-1611
saveRDS(output2, "intermediate_data/gtrends_results2.RDS") #spp 1612-1725
saveRDS(output3, "intermediate_data/gtrends_results3.RDS") #spp 1726-3234
saveRDS(output4, "intermediate_data/gtrends_results4.RDS") #spp 3235-4862
saveRDS(output5, "intermediate_data/gtrends_results5.RDS") #spp 4863-5346
saveRDS(output6, "intermediate_data/gtrends_results6.RDS") #spp 5347-5452
saveRDS(output7, "intermediate_data/gtrends_results7.RDS") #spp 5453-


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

#combine them into 1 list

gtrends_results1 <- readRDS(file = "intermediate_data/gtrends_results1.RDS")
gtrends_results2 <- readRDS(file = "intermediate_data/gtrends_results2.RDS")
gtrends_results3 <- readRDS(file = "intermediate_data/gtrends_results3.RDS")
gtrends_results4 <- readRDS(file = "intermediate_data/gtrends_results4.RDS")
gtrends_results5 <- readRDS(file = "intermediate_data/gtrends_results5.RDS")

gtrends_results2 <- gtrends_results2[1612:1725]
gtrends_results3 <- gtrends_results3[1726:3234]
gtrends_results4 <- gtrends_results4[3235:4862]
gtrends_results5 <- gtrends_results5[4863:5346]

gtrends_results1[1]



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

