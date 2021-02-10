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

#get and clean binomial names 

domesticated <- domesticated %>%
  mutate(species = str_extract(domesticated$`Species and subspecies`, "(?<=\\().+(?=\\))"))

partially_domesticated <- partially_domesticated %>%
  mutate(species = str_extract_all(partially_domesticated$`Species and subspecies`, "\\(([^\\)]+)"))

partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\(", "")
partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\)", "")


for (i in 1:length(partially_domesticated$species)) {
  if (str_detect(partially_domesticated$species[i], "[:punct:]")) {
    print(partially_domesticated$species[i])
  }
}


domesticated$species[36] <- str_extract(domesticated$species[36], "[^)]+")



#cleaning



#testing

str_extract_all(partially_domesticated$`Species and subspecies`, "\\(([^\\)]+)")
