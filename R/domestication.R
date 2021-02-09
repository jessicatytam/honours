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

#get binomial names 

str_extract(domesticated$`Species and subspecies`, "\\w(?!()")

#cleaning