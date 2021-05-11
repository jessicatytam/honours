library()

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

#look at the insides
sum_mod <- summary(mod_list_all[[1]])
sum_mod$Gcovariances[1] #animal_post.mean
sum_mod$Rcovariances[1] #units_post.mean
sum_mod$solutions[1,2] #mean, 95CI, p 

#creating a list to store the data
mod_results <- vector(mode = "list", length = 500)

for (i in 1:length(mod_results)) {
  sum_mod <- summary(mod_list_all[[i]])
  mod_results[[i]] <- list(sum_mod$Gcovariances[1], sum_mod$Rcovariances[1], sum_mod$solutions)
  print(paste("model", i, "extracted"))
}

#pagel's lambda
dat_sub <- includeh %>%
  filter(genus_species %in% tree100$tree_6061$tip.label)

v_dist <- log(1+ 1/mean(dat_sub$h)) #0.1054779

for (i in 1:length(mod_results)) {
  mod_results[[i]][[4]] <- mod_results[[i]][[1]] / (mod_results[[i]][[1]] + mod_results[[i]][[2]] + v_dist) 
}

#animal_post.mean/(animal_post.mean + units_post.mean + v_dist) 

#getting the mean
mod_mean <- vector(mode = "list", length = 15)

for (i in 1:length(mod_results)) {
  mod_mean[[1]] <- mod_results[[i]][[1]] #animal_post.mean
  mod_mean[[2]] <- mod_results[[i]][[2]] #units_post.mean
  mod_mean[[3]] <- mod_results[[i]][[4]] #pagel's lambda
  mod_mean[[4]] <- mod_results[[i]][[3]][2,1] #mass_post.mean
  mod_mean[[5]] <- mod_results[[i]][[3]][3,1] #lat_post.mean 
  mod_mean[[6]] <- mod_results[[i]][[3]][4,1] #use_post.mean 
  mod_mean[[7]] <- mod_results[[i]][[3]][5,1] #domestication_post.mean 
  mod_mean[[8]] <- mod_results[[i]][[3]][6,1] #iucn_post.mean
  mod_mean[[9]] <- mod_results[[i]][[3]][7,1] #gtrends_post.mean
  mod_mean[[10]] <- mod_results[[i]][[3]][2,5] #mass_p
  mod_mean[[11]] <- mod_results[[i]][[3]][3,5] #lat_p
  mod_mean[[12]] <- mod_results[[i]][[3]][4,5] #use_p
  mod_mean[[13]] <- mod_results[[i]][[3]][5,5] #domestication_p
  mod_mean[[14]] <- mod_results[[i]][[3]][6,5] #iucn_p
  mod_mean[[15]] <- mod_results[[i]][[3]][7,5] #gtrends_p
  print(paste("model", i, "extracted"))
}




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
