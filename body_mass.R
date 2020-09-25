library(dplyr)
library(rvest)

#ADW
koalapge <- read_html("https://animaldiversity.org/accounts/Phascolarctos_cinereus/")
koalapge %>%
  html_nodes("body") %>%
  html_text()

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


#have to figure out if "-999" means NA!!!