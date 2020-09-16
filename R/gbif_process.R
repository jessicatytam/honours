library(data.table)
library(dplyr)
library(readr)

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
