#libraries
library(MCMCglmm)

#load imputed datasets
imp_list <- readRDS("data/intermediate_data/MCMCglmm/imp_list.rds")

#load trees

#non-informative prior
prior1 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*1000)))

#model
system.time(mod_op_test <- MCMCglmm(h ~ logmass + 
                                      abs_lat + 
                                      humanuse_bin + 
                                      domestication_bin + 
                                      iucn_bin +
                                      log_sumgtrends, 
                                    random = ~ animal, 
                                    family = "poisson", 
                                    pedigree = tree_sub_ultrametric, 
                                    dat = comp1,  
                                    nitt = 13000*10, 
                                    thin = 10*10, 
                                    burnin = 3000*10,
                                    prior = prior1) 
)

#save the models