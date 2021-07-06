library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(Rmisc)
library(coda)
library(MCMCglmm)

#load data
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 
includeh$genus_species <- str_replace(includeh$genus_species, " ", "_")
tree100 <- tree100 <- readRDS("data/intermediate_data/tree100.nex")
# mod_list_1 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_1.rds")
# mod_list_2 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_2.rds")
# mod_list_3 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_3.rds")
# mod_list_4 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_4.rds")
# mod_list_5 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_5.rds")
# mod_list_6 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_6.rds")
# mod_list_7 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_7.rds")
# mod_list_8 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_8.rds")
# mod_list_9 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_9.rds")
# mod_list_10 <- readRDS("data/intermediate_data/MCMCglmm/mod_list2_10.rds")

#combine models
mod_list_all <- do.call(c, list(mod_list_1, mod_list_2, mod_list_3, mod_list_4, mod_list_5, mod_list_6, mod_list_7, mod_list_8, mod_list_9, mod_list_10))
# saveRDS(mod_list_all, "data/intermediate_data/MCMCglmm/mod_list_all.rds")

#create a list
mod_results <- vector(mode = "list", length = 500)
for (i in 1:length(mod_list_all)) {
  mod_results[[i]] <- cbind(mod_list_all[[i]]$Sol[901:1000,], mod_list_all[[i]]$VCV[901:1000,])
}
mod_results_flat <- as.data.frame(do.call(rbind, mod_results))

mod_result <- function(model) {
  df <- as.data.frame(cbind(model$Sol[901:1000,], model$VCV[901:1000,]))
  df
}
mod_results_flat <- map_df(mod_list_all, mod_result)

saveRDS(mod_results_flat, "data/intermediate_data/MCMCglmm/mod_results_100_2.rds")
mod_results_flat <- readRDS("data/intermediate_data/MCMCglmm/mod_results_100_2.rds")

# test <- function(x){x + 1}
# map_dbl(1:10, test)

#mean and 95CI

mod_results_flat %>% #this is not accurate for some reasons
  summarise_all(CI) #upper, mean, lower

quantile(mod_results_flat$`(Intercept)`, c(0.025, 0.975))
quantile(mod_results_flat$logmass, c(0.025, 0.975))
quantile(mod_results_flat$abs_lat, c(0.025, 0.975))
quantile(mod_results_flat$humanuse_bin, c(0.025, 0.975))
quantile(mod_results_flat$domestication_bin, c(0.025, 0.975))
quantile(mod_results_flat$`poly(iucn_bin, 2)1`, c(0.025, 0.975))
quantile(mod_results_flat$`poly(iucn_bin, 2)2`, c(0.025, 0.975))
quantile(mod_results_flat$log_sumgtrends, c(0.025, 0.975))
quantile(mod_results_flat$animal, c(0.025, 0.975))
quantile(mod_results_flat$units, c(0.025, 0.975))

mod_95ci <- function(model) {
  df <- as_tibble(quantile(model, c(0.025, 0.975)))
  df
}
map_df(mod_results_flat, mod_95ci)


#phylo; animal_post.mean/(animal_post.mean + units_post.mean + v_dist)? 
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 
tree100 <- tree100 <- readRDS("data/intermediate_data/tree100.nex")
dat_sub <- includeh %>%
  filter(genus_species %in% tree100$tree_6061$tip.label)

v_dist <- log(1+ 1/mean(dat_sub$h)) #0.105849914894506

mean(mod_results_flat$animal)/(mean(mod_results_flat$animal) + mean(mod_results_flat$units) + v_dist) #0.6358852

