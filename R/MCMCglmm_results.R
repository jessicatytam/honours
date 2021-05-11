library()

#load models
mod_list_1 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_1.rds")
mod_list_2 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_2.rds")
mod_list_3 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_3.rds")
mod_list_4 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_4.rds")
mod_list_5 <- readRDS("data/intermediate_data/MCMCglmm/mod_list_5.rds")

#look at the insides
sum_mod <- summary(mod_list_1[[1]])
sum_mod$Gcovariances #animal_post.mean
sum_mod$Rcovariances #units_post.mean
sum_mod$solutions #mean, 95CI, p 

