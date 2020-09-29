library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

z<-fread("raw_data/0062362-200613084148143.csv")
z2<-select(z,order,family,genus,species,scientificName,decimalLatitude,decimalLongitude,year,issue)
rm(z)
gc()


z3<-filter(z2,!grepl("COUNTRY_COORDINATE_MISMATCH",issue)&
             !grepl("ZERO_COORDINATE",issue)&
             !grepl("COORDINATE_INVALID",issue)&
             !grepl("COUNTRY_MISMATCH",issue)&
             !grepl("COORDINATE_OUT_OF_RANGE",issue))
#excludes 300,000 records

z4<-select(z3,species,decimalLatitude,decimalLongitude,order)

#fron phylogenetic tree.R
finalspecieslist<-read_csv("intermediate_data/species_list_from_phylo.csv")
sbs <- filter(z4,species %in% finalspecieslist$specieslist)

write_csv(sbs,"intermediate_data/gbif_processed.csv")

sbs <- read_csv(file = "intermediate_data/gbif_processed.csv")

length(unique(sbs$species)) #41850 records

sbs %>%
  group_by(species) %>%
  summarize(median_lat=median(decimalLatitude,na.rm=T),order=order[1]) %>%
  filter(!is.na(median_lat)) %>%
  write_csv("intermediate_data/gbif_lat_medians.csv")->meds

#tax<-select(z3,order,species) %>%
#  distinct()

#meds<-left_join(meds,tax)

ggplot(meds,aes(x=median_lat,fill=order))+geom_histogram()+theme_bw()
ggsave("outputs/lat_median_hist.pdf")
