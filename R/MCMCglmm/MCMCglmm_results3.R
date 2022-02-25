library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(Rmisc)
library(coda)
library(MCMCglmm)

#load data
# includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 
# includeh$genus_species <- str_replace(includeh$genus_species, " ", "_")
# tree100 <- tree100 <- readRDS("data/intermediate_data/tree100.nex")
mod_list_1 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_1.rds")
mod_list_2 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_2.rds")
mod_list_3 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_3.rds")
mod_list_4 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_4.rds")
mod_list_5 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_5.rds")
mod_list_6 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_6.rds")
mod_list_7 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_7.rds")
mod_list_8 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_8.rds")
mod_list_9 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_9.rds")
mod_list_10 <- readRDS("data/intermediate_data/MCMCglmm/mod_list3_10.rds")

#combine models
mod_list_all <- do.call(c, list(mod_list_1, mod_list_2, mod_list_3, mod_list_4, mod_list_5, mod_list_6, mod_list_7, mod_list_8, mod_list_9, mod_list_10))
# saveRDS(mod_list_all, "data/intermediate_data/MCMCglmm/mod_list_all.rds")

#create a list
mod_results <- vector(mode = "list", length = 500)
for (i in 1:length(mod_list_all)) {
  mod_results[[i]] <- cbind(mod_list_all[[i]]$Sol[901:1000,], mod_list_all[[i]]$VCV[901:1000,])
}
mod_results_flat <- as.data.frame(do.call(rbind, mod_results))

# mod_result <- function(model) {
#   df <- as.data.frame(cbind(model$Sol[901:1000,], model$VCV[901:1000,]))
#   df
# }
# mod_results_flat <- map_df(mod_list_all, mod_result)

# saveRDS(mod_results_flat, "data/intermediate_data/MCMCglmm/mod_results_100_2.rds")
# mod_results_flat_2 <- readRDS("data/intermediate_data/MCMCglmm/mod_results_100_2.rds")

# test <- function(x){x + 1}
# map_dbl(1:10, test)

#mean and 95CI

mod_results_flat %>% #this is not accurate for some reasons
  summarise_all(CI) #upper, mean, lower

quantile(mod_results_flat$`(Intercept)`, c(0.025, 0.975))
quantile(mod_results_flat$logmass, c(0.025, 0.975))
quantile(mod_results_flat$abs_lat, c(0.025, 0.975))
quantile(mod_results_flat$humanuse_bin, c(0.025, 0.975))
# quantile(mod_results_flat$domestication_bin, c(0.025, 0.975))
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

# includeh_wo_dom <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 
# includeh$genus_species <- str_replace(includeh$genus_species, " ", "_")
tree100 <- tree100 <- readRDS("data/intermediate_data/tree100.nex")
dat_sub <- includeh_wo_dom %>%
  filter(genus_species %in% tree100$tree_6061$tip.label)

v_dist <- log(1 + 1/mean(dat_sub$h)) #0.119970102126888; taking the mean of everything
v_dist_2 <- log(1 + 1/dat_sub$h)

mean(mod_results_flat$animal)/(mean(mod_results_flat$animal) + mean(mod_results_flat$units) + v_dist) #0.6240157

#95CI

post <- mean(mod_results_flat$animal)/(mean(mod_results_flat$animal) + mean(mod_results_flat$units) + v_dist_2)

quantile(post, c(0.025, 0.975)) #0.0000000, 0.6496907
hist(post) 

#exclude the 0s

dat_exclude <- cbind(dat_sub, post)
dat_exclude <- dat_exclude$h[dat_exclude$post>0]

v_dist_exclude <- log(1 + 1/mean(dat_exclude)) #0.0958999021749282; taking the mean of everything excluding the 0s

mean(mod_results_flat$animal)/(mean(mod_results_flat$animal) + mean(mod_results_flat$units) + v_dist_exclude) #0.6300747

post_exclude <- post[post>0]
quantile(post_exclude, c(0.025, 0.975)) #0.5077454, 0.6503294
hist(post_exclude)

length(post_exclude)/length(post) #0.7896313
(length(post)-length(post_exclude))/length(post) #0.2103687
