library(tidyverse)
library(rvest)

#get the data

wiki_url <- read_html("https://en.wikipedia.org/wiki/List_of_domesticated_animals")

domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table()

partially_domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>%
  html_table()

#remove non-mammals

domesticated <- domesticated %>%
  filter(str_extract(domesticated$`Taxon group`, "^1") == 1)

partially_domesticated <- partially_domesticated %>%
  filter(str_extract(partially_domesticated$`Taxon group`, "^1") == 1)

#get binomial names 

domesticated <- domesticated %>% #extract names using the brackets
  mutate(species = str_extract(domesticated$`Species and subspecies`, "(?<=\\().+(?=\\))")) 

partially_domesticated <- partially_domesticated %>% #extract names using the brackets
  mutate(species = str_extract_all(partially_domesticated$`Species and subspecies`, "\\(([^\\)]+)")) 

#cleaning binomial names

partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\(", "") #removing mismatched brackets
partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\)", "")
partially_domesticated$species <- str_replace_all(partially_domesticated$species, "c", "") #removing "c"
partially_domesticated$species <- str_replace_all(partially_domesticated$species, '"', "") #removing quotation marks

partially_domesticated <- partially_domesticated %>% #separate the names using ","
  separate(species, c("species1", "species2", "species3", "species4", "species5",
                      "species6", "species7", "species8", "species9", "species10"), sep = "[,]+")

#fill in genus names




#save and load



#testing

test <- partially_domesticated[c(91:100),]

str_extract_all(test$species2, "[^[:space:]]+")
