library(purrr)
library(tidyr)
library(Rmisc)

#load data
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 
includeh$genus_species <- str_replace(includeh$genus_species, " ", "_")
tree100 <- tree100 <- readRDS("data/intermediate_data/tree100.nex")
mod_list_1 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_1.rds")
mod_list_2 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_2.rds")
mod_list_3 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_3.rds")
mod_list_4 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_4.rds")
mod_list_5 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_5.rds")

#combine models
mod_list_all <- do.call(c, list(mod_list_1, mod_list_2, mod_list_3, mod_list_4, mod_list_5))

#create a list
mod_results <- vector(mode = "list", length = 500)
for (i in 1:length(mod_list_all)) {
  mod_results[[i]] <- cbind(mod_list_all[[i]]$Sol[901:1000,], mod_list_all[[i]]$VCV[901:1000,])
}

mod_results_flat <- do.call(rbind, mod_results)
mod_results_flat <- as.data.frame(mod_results_flat)

#mean and 95CI

mod_results_flat %>%
  summarise_all(CI) #upper, mean, lower




#pagel's lambda; animal_post.mean/(animal_post.mean + units_post.mean + v_dist) 
dat_sub <- includeh %>%
  filter(genus_species %in% tree100$tree_6061$tip.label)

v_dist <- log(1+ 1/mean(dat_sub$h)) #0.1054779



#getting the mean




#testing

for (i in 1:length(mod_results)) {
  print(mod_results[[i]][[1]])
}

test <- data.frame()

for (i in 1:length(mod_results)) {
  test$animal_post.mean <- mod_results[[i]][[1]]
}

for (i in 1:length(mod_results)) {
  test[i] <- mod_results[[i]][[1]]
}

test <- mod_results[[1:500]][[1]]
