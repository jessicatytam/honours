library(tidyr)
library(stringr)
library(readr)
library(tidyverse)

#ADW
#scraping data from ADW
tablepage <- read_html("https://animaldiversity.ummz.umich.edu/quaardvark/search/1E2668FF-7319-0001-3FA4-1CB91E871B70/?start=1") #reading the webpage
table <- tablepage %>% #getting the table
  html_nodes("tbody") %>%
  html_text()
tablepg1 <- data.frame(table) %>% #turning it into a df
  str_replace_all("\\s+", " ") #getting rid of the whitespace '\n'
write_csv(tablepg1,"test.csv")
#turning the string into a df with separate entries of species
ADWdata <- str_split(tablepg1, "\\s+(?=[:upper:])") %>% #split the string according the occurrence of an upper case letter; (?=[:upper:]) checks for the upper case letter after the whitespace
  as.data.frame() #convert from list to df
ADWdata <- as.data.frame(ADWdata[-c(1),]) #get rid of the first entry
#separating each entry into columns
ADWdata %>%
  str_extract_all("\\d+")

#AnAge
remotes::install_github("mastoffel/AnAgeScrapeR", dependencies = TRUE)
#i think this is better for individual species
#found the complete dataset so i'll use that instead
AnAge <- read.delim(file = "C:/Users/iamje/Jess/UNSW/Honours/honours/mass_databases/anage_data.txt", header = T)

#PanTHERIA
Pantheria1 <- read.delim(file = "C:/Users/iamje/Jess/UNSW/Honours/honours/mass_databases/PanTHERIA_1-0_WR93_Aug2008.txt", header = T)
Pantheria2 <- read.delim(file = "C:/Users/iamje/Jess/UNSW/Honours/honours/mass_databases/PanTHERIA_1-0_WR05_Aug2008.txt", header = T)

#Smith et al.
Smith <- read.delim(file = "C:/Users/iamje/Jess/UNSW/Honours/honours/mass_databases/MOMv3.3.txt", header = F)
Smith <- Smith %>%
  rename(continent = V1, status = V2, order = V3, family = V4, genus = V5, species = V6,
         log_mass = V7, combined_mass = V8, reference = V9) 

#need to replace -999 with NA