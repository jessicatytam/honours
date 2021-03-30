# doing MCMCglmm

# package
library(tidyverse)
library(MCMCglmm)
library(lme4)
library(phyr)
library(here)
library(rotl)

dat <- read.csv(here("outputs", "includeh.csv"))[-c(1)]

# MCMCglmm does not like family as a name in the dataset

names(dat)[names(dat) == "family"] <- "spp_family"

#dat <- dat[, -dat$family_spp]

# we need to reduce the data

# jess's one
complete_list <- data.frame() #3393 records
for (i in 1:length(dat$genus_species)) {
  if (!is.na(dat$BodyMass.Value[i]) & !is.na(dat$median_lat[i]) & !is.na(dat$redlistCategory[i])) {
    complete_list <- rbind(complete_list, includeh[i,])
  }
}
# swapping with apply
comp <- apply(dat, 1, function(x){ifelse(!is.na(x["BodyMass.Value"]) & !is.na(x["median_lat"]) & !is.na(x["redlistCategory"]) == TRUE, TRUE, FALSE)})

dat <- dat[comp,] # 3393   53

dat$animal <- dat$genus_species
# get phylo
#tree <- read.tree(here("intermediate_data", "tree.tre"))
#tree <- compute.brlen(tree) 

tree_complete <- tol_induced_subtree(ott_ids = complete_list$id, label_format = "name")

tree <- compute.brlen(tree_complete)

# this seems to make correlation matrix anyways!!!
cov_tree <- vcv2(tree, corr = TRUE)

# overdispersed Poisson

# prior
prior1 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*1000)))

# model
system.time(mod_op <- MCMCglmm(h ~ logmass + 
                                 abs(median_lat) + 
                                 humanuse_bin + 
                                 domestication_bin + 
                                 log_sumgtrends, 
                               random = ~ animal, 
                               family = "poisson", 
                               pedigree = tree, 
                               dat = dat,  
                               nitt=13000*10, 
                               thin=10*10, 
                               burnin=3000*10,
                               prior = piror1)
            )


# user  system elapsed 
# 510.795   3.242 516.163 

summary(mod_op)
plot(mod_op) # this looks good

# probably try ZIP


#prior 
prior2 <- list(R = list(V = diag(2), nu = 0.002),
               G = list(G1 = list(V = diag(2), nu = 2, alpha.mu = c(0, 0), alpha.V = diag(2)*1000)))

# model
system.time(mod_zip <- MCMCglmm(h ~ logmass + 
                                 abs(median_lat) + 
                                 humanuse_bin + 
                                 domestication_bin + 
                                 log_sumgtrends, 
                               random = ~ idh(trait):animal, 
                               rcov = ~idh(trait):units,
                               family = "zppoisson", 
                               pedigree = tree, 
                               dat = dat,  
                               nitt=13000, 
                               thin=10, 
                               burnin=3000,
                               prior = prior2)
)



