# doing MCMCglmm

# package
library(tidyverse)
library(MCMCglmm)
library(lme4)
library(phyr)
library(here)
library(rotl)
library(ape)
library(mice)
library(Rphylopars)
library(GGally)

dat <- read.csv(here("outputs", "includeh.csv"))[-c(1)]

# MCMCglmm does not like family as a name in the dataset

names(dat)[names(dat) == "family"] <- "spp_family"

#dat <- dat[, -dat$family_spp]

# we need to reduce the data

# jess's one
complete_list <- data.frame() #3393 records
for (i in 1:length(dat$genus_species)) {
  if (!is.na(dat$BodyMass.Value[i]) & !is.na(dat$median_lat[i]) & !is.na(dat$redlistCategory[i])) {
    complete_list <- rbind(complete_list, dat[i,])
  }
}
# swapping with apply
comp <- apply(dat, 1, function(x){ifelse(!is.na(x["BodyMass.Value"]) & !is.na(x["median_lat"]) & !is.na(x["redlistCategory"]) == TRUE, TRUE, FALSE)})

#dat <- dat[comp,] # 3393   53

dat$animal <- dat$genus_species
# get phylo
tree <- read.tree(here("intermediate_data", "tree.tre"))
tree <- compute.brlen(tree) 

#tree_complete <- tol_induced_subtree(ott_ids = dat$id, label_format = "name")

#tree <- compute.brlen(tree_complete)

# this seems to make correlation matrix anyways!!!
cov_tree <- vcv2(tree, corr = TRUE)

# read 100 trees
# do not run for now
tree100 <- read.nexus(here("trees/tree_pruner_tip_dated/output.nex"))

sum(tree100[[1]]$tip.label %in% dat$genus_species)

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
                               prior = prior1)
)


# user  system elapsed 
# 510.795   3.242 516.163 

summary(mod_op)
plot(mod_op) # this looks good

saveRDS(mod_op, file = here("Rdata", "mod_op.rds"))

v_dist <- log(1+ 1/mean(dat$h))

# phylogenetic signal

5.097/(5.097 + 0.6279 + v_dist)


### it is fine but takes a lot longer
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
                                family = "zipoisson", 
                                pedigree = tree, 
                                dat = dat,  
                                nitt=13000*100, 
                                thin=10*1000, 
                                burnin=3000*100,
                                prior = prior2)
)


summary(mod_zip)
plot(mod_zip) # this looks good

saveRDS(mod_zip, file = here("Rdata", "mod_zip.rds"))

#look at the models
mod_op <- readRDS("Rdata/mod_op.rds")
mod_zip <- readRDS("Rdata/mod_zip.rds")

###### imputation

# first cleaning up data
# this clasked
data_imp<- dat[, c("animal","h","logmass",  "humanuse_bin", "domestication_bin", "log_sumgtrends","iucn_bin")]
#col.names(data_imp) <- c("species","h","logmass", "median_lat", "humanuse_bin", "domestication_bin", "log_sumgtrends")

data_imp$abs_lab <- abs(dat$median_lat)

#test <- phylopars(trait_data = data_imp,tree = tree, model = "BM")

# let's try mice (prelimary)

dim(data_imp)
md.pattern(data_imp)

# set up redictor matrix and imputation methods
pred_matrix <- make.predictorMatrix(data_imp)
imp_method <- make.method(data_imp)

ggpairs(data_imp[,-1])
# our cluster (-2)
#pred_matrix[ , "species"] <- -2 # cluster varaible needs to be intergers

# stting 0 for non-missing data
no_missing <- c("animal", "h", "humanuse_bin", "domestication_bin", "log_sumgtrends")
pred_matrix[no_missing, ] <- 0

# also put 0 for diag
diag(pred_matrix) <- 0

pred_matrix

# setting 

imp_method

# perform the imputation
imp <- mice(data_imp,
            m = 10, # need to get at least 5 - perferably 10
            maxit = 20, # we probably neeed to 20 to coverge
            method = imp_method,
            predictorMatrix = pred_matrix,
            seed = 777)

comp1 <- complete(imp, 1)





