#installation
install.packages("MCMCglmm")
#libraries
library(MCMCglmm)

#load data
imp_list <- readRDS("data/intermediate_data/MCMCglmm/imp_list.rds")
random_trees <- readRDS("data/intermediate_data/random_trees.rds")

#imputation list
imp_4 <- imp_list[7:8]

#non-informative prior
prior1 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*1000)))

#model
mod_list_4 <- vector(mode = "list", length = 100)
for (i in 1:length(random_trees)) {
  for (j in 1:length(imp_4)) {
    #mod_list_4[[j+(i-1)*length(imp_4)]] <- sprintf("%s is i and %s is j\n", i, j)
    mod_list_4[[j+(i-1)*length(imp_4)]] <- MCMCglmm(h ~ logmass +
                                                    abs_lat +
                                                    humanuse_bin +
                                                    domestication_bin +
                                                    iucn_bin +
                                                    log_sumgtrends,
                                                  random = ~ animal,
                                                  family = "poisson",
                                                  pedigree = random_trees[[i]],
                                                  dat = imp_4[[j]],
                                                  nitt = 13000*10,
                                                  thin = 10*10,
                                                  burnin = 3000*10,
                                                  prior = prior1)
  }
}

#save the models
saveRDS(mod_list_4, "data/intermediate_data/MCMCglmm/mod_list_4.rds")
