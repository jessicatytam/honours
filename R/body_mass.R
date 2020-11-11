library(tidyr)
library(stringr)
library(readr)
library(tidyverse)
library(rvest)

#ADW

#a loop to download the complete dataset
for (i in ...) {
  
}

#scraping data from ADW
tablepage <- read_html("https://animaldiversity.ummz.umich.edu/quaardvark/search/1E2668FF-7319-0001-3FA4-1CB91E871B70/?start=1") #reading the webpage
table <- tablepage %>% #getting the table
  html_nodes("tbody") %>%
  html_text()
tablepg1 <- data.frame(table) %>% #turning it into a df
  str_replace_all("\\s+", " ") #getting rid of the whitespace '\n'

#turning the string into a df with separate entries of species
ADWdata <- str_split(tablepg1, "\\s+(?=[:upper:])") %>% #split the string according the occurrence of an upper case letter; (?=[:upper:]) checks for the upper case letter after the whitespace
  as.data.frame() %>% #convert from list to df
  rename(entry = c......Abditomys.latidens....Abeomelomys.sevia....Abrawayaomys.ruschii...) %>%
  filter(entry != "") #get rid of the first empty entry

#creating new columns for species names
ADWdataName <- ADWdata %>%
  mutate(binomial_name = str_extract(ADWdata$entry, "\\D+")) # separate name from string
ADWdataName <- ADWdataName %>%
  mutate(genus = word(binomial_name, 1), #first word of string
         species = word(binomial_name, start = 2, end = -1)) #second to last word of string




#AnAge
remotes::install_github("mastoffel/AnAgeScrapeR", dependencies = TRUE) #i think this is better for individual species
#found the complete dataset so i'll use that instead
AnAge <- read.delim(file = "mass_databases/anage_data.txt", header = T)

#PanTHERIA
Pantheria1 <- read.delim(file = "mass_databases/PanTHERIA_1-0_WR93_Aug2008.txt", header = T)
Pantheria2 <- read.delim(file = "mass_databases/PanTHERIA_1-0_WR05_Aug2008.txt", header = T)

#Smith et al.
Smith <- read.delim(file = "mass_databases/MOMv3.3.txt", header = F)
Smith <- Smith %>%
  rename(continent = V1, status = V2, order = V3, family = V4, genus = V5, species = V6,
         log_mass = V7, combined_mass = V8, reference = V9) 

#need to replace -999 with NA