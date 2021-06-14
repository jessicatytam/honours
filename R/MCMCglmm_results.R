library(dplyr)
library(purrr)
library(tidyr)
library(Rmisc)
library(coda)
library(MCMCglmm)

#load data
#includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 
#includeh$genus_species <- str_replace(includeh$genus_species, " ", "_")
#tree100 <- tree100 <- readRDS("data/intermediate_data/tree100.nex")
# mod_list_1 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_1.rds")
# mod_list_2 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_2.rds")
# mod_list_3 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_3.rds")
# mod_list_4 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_4.rds")
# mod_list_5 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_5.rds")
# mod_list_all <- readRDS("data/intermediate_data/MCMCglmm/mod_list_all.rds")
mod_results_100 <- readRDS("data/intermediate_data/MCMCglmm/mod_results_100.rds")

#combine models
# mod_list_all <- do.call(c, list(mod_list_1, mod_list_2, mod_list_3, mod_list_4, mod_list_5))
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

saveRDS(mod_results_flat, "data/intermediate_data/MCMCglmm/mod_results_100.rds")

# test <- function(x){x + 1}
# map_dbl(1:10, test)

#mean and 95CI

# mod_results_flat %>% #this is not accurate for some reasons
#   summarise_all(CI) #upper, mean, lower

quantile(mod_results_flat$`(Intercept)`, c(0.025, 0.975))
quantile(mod_results_flat$logmass, c(0.025, 0.975))
quantile(mod_results_flat$abs_lat, c(0.025, 0.975))
quantile(mod_results_flat$humanuse_bin, c(0.025, 0.975))
quantile(mod_results_flat$domestication_bin, c(0.025, 0.975))
quantile(mod_results_flat$iucn_bin, c(0.025, 0.975))
quantile(mod_results_flat$log_sumgtrends, c(0.025, 0.975))
quantile(mod_results_flat$animal, c(0.025, 0.975))
quantile(mod_results_flat$units, c(0.025, 0.975))

mod_95ci <- function(model) {
  df <- as_tibble(quantile(model, c(0.025, 0.975)))
  df
}
map_df(mod_results_flat, mod_95ci)


#pagel's lambda; animal_post.mean/(animal_post.mean + units_post.mean + v_dist) 
dat_sub <- includeh %>%
  filter(genus_species %in% tree100$tree_6061$tip.label)

v_dist <- log(1+ 1/mean(dat_sub$h)) #0.1054779



#getting the mean

