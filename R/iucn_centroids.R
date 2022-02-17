library(rgdal)
library(rgeos)
library(rredlist)
library(dplyr)
library(ggplot2)
library(plotly)

#get centroids
iucn_loc <- readOGR("data/raw_data/MAMMALS.shp")
centroids <- gCentroid(iucn_loc, byid = TRUE)

#match id to centroid
loc <- data.frame(centroids@coords)
loc$id <- iucn_loc$id_no
loc$id<- as.numeric(loc$id)

#add spp names
iucn_assessments <- read.csv("data/raw_data/assessments.csv")
names(iucn_assessments)[names(iucn_assessments)=="internalTaxonId"] <- "id"
loc_sp <- left_join(loc, iucn_assessments,
                    by = "id")
loc_sp <- loc_sp[,c("y", "id", "scientificName")]

#average latitude
iucn_mean_y <- loc_sp %>% 
  group_by(scientificName) %>% 
  summarise(mean = mean(y))

#compare with gbif
includeh <- read.csv("outputs/data/includeh.csv")
names(iucn_mean_y)[names(iucn_mean_y)=="scientificName"] <- "genus_species"
y <- left_join(iucn_mean_y, includeh,
               by = "genus_species")
y <- y[,c("genus_species", "mean", "y")]

#t-test
cor.test(y$mean, y$y)
plot(x = y$mean, y = y$y)

lat_plot <- ggplot(y, aes(x = y,
              y = mean)) +
  geom_point(size = 2,
             alpha = 0.2) +
  labs(x = "GBIF latitude",
       y = "IUCN latitude")

ggplot2::ggsave("outputs/lat_plot.png", lat_plot, width = 12, height = 9, units = "in", dpi = 300)

ggplotly(ggplot(y, aes(x = y,
              y = mean,
              colour = genus_species)) +
  geom_point(alpha = 0.2) +
  labs(x = "GBIF latitude",
       y = "IUCN latitude"),
  tooltip = c("text", "colour", "genus_species"))

# Pearson's product-moment correlation
# 
# data:  y$mean and y$y
# t = 205.47, df = 3932, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9537053 0.9590353
# sample estimates:
#     cor 
# 0.95645 