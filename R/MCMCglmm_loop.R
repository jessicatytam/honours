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
mod_list <- vector(mode = "list", length = 4)
for (i in 1:length(random_trees)) {
  for (j in 1:length(random_df)) {
    # mod_list[[j+(i-1)*length(random_df)]]<-sprintf("%s is i and %s is j\n", i, j)  
    mod_list[[j+(i-1)*length(random_df)]] <- MCMCglmm(h ~ logmass +
                                                        abs_lat +
                                                        humanuse_bin +
                                                        domestication_bin +
                                                        iucn_bin +
                                                        log_sumgtrends,
                                                      random = ~ animal,
                                                      family = "poisson",
                                                      pedigree = random_trees[[i]],
                                                      dat = random_df[[j]],
                                                      nitt = 13000*10,
                                                      thin = 10*10,
                                                      burnin = 3000*10,
                                                      prior = prior1)
  }
}

#save the models
saveRDS(mod_list, "data/intermediate_data/MCMCglmm/mod_list.rds")
