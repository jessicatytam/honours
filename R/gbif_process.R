library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

z<-fread("data/0062362-200613084148143.csv")
z2<-select(z,order,family,genus,species,scientificName,decimalLatitude,decimalLongitude,year,issue)
rm(z)
gc()


z3<-filter(z2,!grepl("COUNTRY_COORDINATE_MISMATCH",issue)&
             !grepl("ZERO_COORDINATE",issue)&
             !grepl("COORDINATE_INVALID",issue)&
             !grepl("COUNTRY_MISMATCH",issue)&
             !grepl("COORDINATE_OUT_OF_RANGE",issue))
#excludes 300,000 records

z4<-select(z3,scientificName,decimalLatitude,decimalLongitude,year)

write_csv(z4,"data/gbif_processed.csv")

gbif <- read_csv(file = "data/gbif_processed.csv")

length(unique(gbif$species)) #41850 records

#fron phylogenetic tree.R
sbs<-filter(gbif,species %in% finalspecieslist$specieslist)

length(unique(sbs$species))

sbs %>%
  group_by(species) %>%
  summarize(median_lat=median(decimalLatitude,na.rm=T)) %>%
  filter(!is.na(median_lat)) %>%
  write_csv("data/gbif_lat_medians.csv")->meds

tax<-select(z3,order,species) %>%
  distinct()

meds<-left_join(meds,tax)

ggplot(meds,aes(x=median_lat,fill=order))+geom_histogram()+theme_bw()
