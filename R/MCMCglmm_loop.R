#installation
install.packages("MCMCglmm")
#libraries
library(MCMCglmm)

#load data
imp_list <- readRDS("data/intermediate_data/MCMCglmm/imp_list.rds")
trimmed_trees <- readRDS("data/intermediate_data/trimmed_trees.rds")

#pick random trees
random_trees <- sample(trimmed_trees, 2, replace = FALSE)
random_df <- sample(imp_list, 2, replace = FALSE)

#non-informative prior
prior1 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*1000)))

#model
mod_list <- list()
for (j in 1:length(random_trees)) {
  for (i in 1:length(random_df)) {
    model <- MCMCglmm(h ~ logmass +
                                abs_lat +
                                humanuse_bin +
                                domestication_bin +
                                iucn_bin +
                                log_sumgtrends,
                              random = ~ animal,
                              family = "poisson",
                              pedigree = random_trees[[j]],
                              dat = random_df[[i]],
                              nitt = 13000*10,
                              thin = 10*10,
                              burnin = 3000*10,
                              prior = prior1)
    mod_list <- list(model, mod_list)
  }
}

#save the models
saveRDS(mod_list, "data/intermediate_data/MCMCglmm/mod_list.rds")



#test

mod_list <- list()
for (i in 1:length(imp_list)) {
  model <- lm(h ~ logmass, data = imp_list[[i]])
  mod_list <- mapply(c, mod_list, model)
}


model <- lm(h ~ logmass, data = imp_list[[1]])
