library(tidyverse)
library(gtrendsR)
library(curl)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(wesanderson)


#get the data

output8 <- list()
for (i in 6031:6788) {
  print(paste(i, "getting data for", includeh$genus_species[i]))
  search_term <- includeh$genus_species[i]
  output8[[i]] <- gtrends(keyword = search_term,
                         time = "all")
  Sys.sleep(1)
}

saveRDS(output, "intermediate_data/gtrends_results1.RDS") #spp 1-1611
saveRDS(output2, "intermediate_data/gtrends_results2.RDS") #spp 1612-1725
saveRDS(output3, "intermediate_data/gtrends_results3.RDS") #spp 1726-3234
saveRDS(output4, "intermediate_data/gtrends_results4.RDS") #spp 3235-4862
saveRDS(output5, "intermediate_data/gtrends_results5.RDS") #spp 4863-5346
saveRDS(output6, "intermediate_data/gtrends_results6.RDS") #spp 5347-5452
saveRDS(output7, "intermediate_data/gtrends_results7.RDS") #spp 5453-6030
saveRDS(output8, "intermediate_data/gtrends_results8.RDS") #spp 6031-6788

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


#combine them into 1 list + checking

gtrends_results1 <- readRDS(file = "intermediate_data/gtrends_results1.RDS")
gtrends_results2 <- readRDS(file = "intermediate_data/gtrends_results2.RDS")
gtrends_results3 <- readRDS(file = "intermediate_data/gtrends_results3.RDS")
gtrends_results4 <- readRDS(file = "intermediate_data/gtrends_results4.RDS")
gtrends_results5 <- readRDS(file = "intermediate_data/gtrends_results5.RDS")
gtrends_results6 <- readRDS(file = "intermediate_data/gtrends_results6.RDS")
gtrends_results7 <- readRDS(file = "intermediate_data/gtrends_results7.RDS")
gtrends_results8 <- readRDS(file = "intermediate_data/gtrends_results8.RDS")

gtrends_results2 <- gtrends_results2[1612:1725]
gtrends_results3 <- gtrends_results3[1726:3234]
gtrends_results4 <- gtrends_results4[3235:4862]
gtrends_results5 <- gtrends_results5[4863:5346]
gtrends_results6 <- gtrends_results6[5347:5452]
gtrends_results7 <- gtrends_results7[5453:6030]
gtrends_results8 <- gtrends_results8[6031:6788]

gtrends_list <- do.call(c, list(gtrends_results1, gtrends_results2, gtrends_results3, gtrends_results4,
                                gtrends_results5, gtrends_results6, gtrends_results7, gtrends_results8))

spp <- data.frame()
for (i in 1:length(gtrends_list)) {
  print(gtrends_list[[i]]$interest_by_country[1, "keyword"])
  spp[i, 1] <- gtrends_list[[i]]$interest_by_country[1, "keyword"]
}

gtrends_list <- gtrends_list[-c(6030)]

unique(spp$V1)

for (i in 1:length(includeh$genus_species)) {
  if (!includeh$genus_species[i] %in% spp$V1) {
    print(includeh$genus_species[i])
  }
} #the lists match now

#get missing spp

missing_spp <- data.frame(c("Pteropus livingstonii", "Pteropus lombocensis", "Pteropus loochoensis", "Soricomys kalinga"))
missing_spp <- missing_spp %>%
  rename(spp = c..Pteropus.livingstonii....Pteropus.lombocensis....Pteropus.loochoensis...)

output <- list()
for (i in 1:length(missing_spp$spp)) {
  print(paste(i, "getting data for", missing_spp$spp[i]))
  search_term <- missing_spp$spp[i]
  output[[i]] <- gtrends(keyword = search_term,
                          time = "all")
  Sys.sleep(1)
}

gtrends_list <- do.call(c, list(gtrends_list, output))

#check for null

for (i in 1:length(gtrends_list)) {
  if (is.null(gtrends_list[[i]])) {
    print(i)
  }
}

#replace <1 with 0.5

replace(gtrends_list[[2551]]$interest_over_time[, "hits"], #test
        gtrends_list[[2551]]$interest_over_time[, "hits"] == "<1", "0.5")

for (i in 1:length(gtrends_list)) {
  gtrends_list[[i]]$interest_over_time[, "hits"] <- replace(gtrends_list[[i]]$interest_over_time[, "hits"],
                                                            gtrends_list[[i]]$interest_over_time[, "hits"] == "<1", "0.5")
}

#only the sum

hits <- data.frame(genus_species = as.character(),
                   sum_gtrends = as.numeric())

data.frame(genus_species = gtrends_list[[1]]$interest_by_country[1, "keyword"],
           sum_gtrends = 0)

for (i in 1:length(gtrends_list)) {
  if (is.null(gtrends_list[[i]]$interest_by_country)) {
    gtrends_hits <- data.frame(genus_species = gtrends_list[[i]]$interest_by_country[1, "keyword"],
                               sum_gtrends = 0)
  } else {
    gtrends_hits <- data.frame(genus_species = gtrends_list[[i]]$interest_by_country[1, "keyword"],
                               sum_gtrends = sum(as.numeric(gtrends_list[[i]]$interest_over_time[, "hits"])))
  }
  hits <- rbind(hits, gtrends_hits)
}

#checking

sum(hits$sum_gtrends==0) #5799 species with 0

#combine with includeh

includeh <- left_join(includeh, hits, by = "genus_species")
table(is.na(includeh$sum_gtrends))

#slope, intercept

glm <- glm(hits ~ date, output[6787]$interest_over_time, family = poisson)
summary(glm)
plot(glm)


glm_list <- list() #828, 2551
for (i in 1:length(gtrends_list)) {
  if (!is.null(gtrends_list[[i]]$interest_over_time)) {
    print(paste(i, gtrends_list[[i]]$interest_over_time[1, "keyword"]))
    glm_list[[i]] <- summary(glm(hits ~ date, gtrends_list[[i]]$interest_over_time, family = poisson))
    glm_list[[i]][["genus_species"]] <- gtrends_list[[i]]$interest_over_time[1, "keyword"]
  }
}

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

#map of sum

ggplot(world) +
  geom_sf() +
  geom_point(data = includeh, aes(x = x,
                                  y = y,
                                  colour = log_sumgtrends),
             size = 2,
             alpha = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = "Google searches") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey80",
                                  linetype = "dashed")) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) 

ggplot(includeh, aes(x = median_lat,
                     y = log_sumgtrends,
                     colour = clade)) +
  geom_point(size = 2,
             alpha = 0.4) +
  geom_smooth(colour = "black") +
  labs(x = "Latitude (median)",
       y = "Google searches",
       colour = "Clade") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80",
                                        linetype = "longdash")) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1)))

#reading and writing

write.csv(includeh, file = "outputs/includeh.csv")
includeh <- read_csv("outputs/includeh.csv")[-c(1)]

saveRDS(gtrends_list, "intermediate_data/gtrends_list.RDS")
gtrends_list <- readRDS("intermediate_data/gtrends_list.RDS")

#testing

for (i in 1:length(gtrends_list)) {
  if (!is.null(gtrends_list[[i]]$interest_over_time)) {
    print(i)
  }
}

test <- gtrends_list[6071:6080]

test_glm <- summary(glm(hits ~ date, test[[8]]$interest_over_time, family = poisson))

test_stat <- data.frame(matrix(ncol = 9, nrow = 0))
names <- c("genus_species", "intercept", "slope", "se", "p", "null_dev", "null_df", "resid_dev", "resid_df")
colnames(test_stat) <- names

for (i in 1:length(test)) {
  if (!is.null(test[[i]]$interest_over_time)) {
    print(test[[i]]$interest_over_time[1, "keyword"])
    glm <- summary(glm(hits ~ date, test[[i]]$interest_over_time, family = poisson))
    print("glm done.")
    test_stat[test_stat$genus_species[i]] <- test[[i]]$interest_over_time[1, "keyword"]
    test_stat[test_stat$intercept[i]] <- glm$coefficients[1, 1]
    test_stat[test_stat$slope[i]] <- glm$coefficients[2, 1]
    test_stat[test_stat$se[i]] <- glm$coefficients[2, 2]
    test_stat[test_stat$p[i]] <- glm$coefficients[2, 4]
    test_stat[test_stat$null_dev[i]] <- glm$null.deviance
    test_stat[test_stat$null_df[i]] <- glm$df.null
    test_stat[test_stat$resid_dev[i]] <- glm$deviance
    test_stat[test_stat$null_df[i]] <- glm$df.resid
  } else {
    test_stat[test_stat$genus_species[i]] <- test[[i]]$interest_by_country[1, "keyword"]
    test_stat[test_stat[i, 2:9]] <- NA
  }
}

test_stat <- data.frame(genus_species[i] = test[[i]]$interest_over_time[1, "keyword"],
                        intercept[i] = glm$coefficients[1, 1],
                        slope[i] = glm$coefficients[2, 1],
                        se[i] = glm$coefficients[2, 2],
                        p[i] = glm$coefficients[2, 4],
                        null_dev[i] = glm$null.deviance,
                        null_df[i] = glm$df.null,
                        resid_dev[i] = glm$deviance,
                        null_df[i] = glm$df.resid)

test_map <- map(test, glm(hits ~ date, test$interest_over_time, family = poisson))

glm_list <- list()
for (i in 1:length(test)) {
  if (!is.null(test[[i]]$interest_over_time)) {
    print(test[[i]]$interest_over_time[1, "keyword"])
    glm_list[[i]] <- summary(glm(hits ~ date, test[[i]]$interest_over_time, family = poisson))
    glm_list[[i]][["genus_species"]] <- test[[i]]$interest_over_time[1, "keyword"]
  }
}
