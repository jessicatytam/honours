library(dplyr)
library(stringr)
library(MCMCglmm)
library(rotl)
library(ape)
library(mice)
library(phytools)
library(rlist)
library(Rphylopars)
library(GGally)

#dataset
dat <- read.csv("outputs/data/includeh.csv")[-c(1)]
dat$genus_species <- str_replace(dat$genus_species, " ", "_")

#MCMCglmm does not like family as a name in the dataset
names(dat)[names(dat) == "family"] <- "spp_family"
dat$animal <- dat$genus_species

#count complete cases
comp <- apply(dat, 1, function(x){ifelse(!is.na(x["BodyMass.Value"]) & !is.na(x["median_lat"]) & !is.na(x["redlistCategory"]) == TRUE, TRUE, FALSE)})
table(comp) #3779 complete cases

#get tree
tree100 <- readRDS("data/intermediate_data/tree100.nex")
table(tree100$tree_6061$tip.label %in% dat$genus_species) #5061
table(dat$genus_species %in% tree100$tree_6061$tip.label) #5029

#trim tree and list so that they match
tree_sub <- tree100$tree_6061

tree_match <- list()
for (i in 1:length(tree_sub$tip.label)) {
  if (tree_sub$tip.label[i] %in% dat$genus_species) {
    tree_match$tip.label[i] <- tree_sub$tip.label[i]
  }
}
tree_match <- lapply(tree_match, function(x) x[!is.na(x)]) #remove 1 NA entry

tree_sub <- keep.tip(tree_sub, tree_match$tip.label) #5029

dat_sub <- dat %>%
  filter(genus_species %in% tree100$tree_6061$tip.label)

table(tree_sub$tip.label %in% dat_sub$genus_species) #5029

#IMPUTATION

#add orders / families, remove h

data_imp <- dat_sub[, c("animal", "h", "logmass", "humanuse_bin", "domestication_bin", "iucn_bin", "log_sumgtrends")]
data_imp$abs_lat <- abs(dat_sub$median_lat)

#let's try mice (preliminary)
dim(data_imp)
md.pattern(data_imp)

#set up predictor matrix and imputation methods
pred_matrix <- make.predictorMatrix(data_imp)
imp_method <- make.method(data_imp)

#correlation matrix
ggpairs(data_imp[,-1])
#our cluster (-2)
#pred_matrix[ , "species"] <- -2 # cluster variable needs to be integers

#setting 0 for non-missing data
no_missing <- c("animal", "h", "humanuse_bin", "domestication_bin", "log_sumgtrends")
pred_matrix[no_missing, ] <- 0

#also put 0 for diag
diag(pred_matrix) <- 0
pred_matrix

#setting 
imp_method

#perform the imputation
imp <- mice(data_imp,
            m = 10, # need to get at least 5 - preferably 10
            maxit = 50, # we probably need to 20 to converge
            method = imp_method,
            predictorMatrix = pred_matrix,
            seed = 777)
plot(imp) #check convergence

comp1 <- complete(imp, 1)
comp2 <- complete(imp, 2)
comp3 <- complete(imp, 3)
comp4 <- complete(imp, 4)
comp5 <- complete(imp, 5)
comp6 <- complete(imp, 6)
comp7 <- complete(imp, 7)
comp8 <- complete(imp, 8)
comp9 <- complete(imp, 9)
comp10 <- complete(imp, 10)

#save imputed datasets
saveRDS(comp1, "data/intermediate_data/MCMCglmm/comp1.rds")
saveRDS(comp2, "data/intermediate_data/MCMCglmm/comp2.rds")
saveRDS(comp3, "data/intermediate_data/MCMCglmm/comp3.rds")
saveRDS(comp4, "data/intermediate_data/MCMCglmm/comp4.rds")
saveRDS(comp5, "data/intermediate_data/MCMCglmm/comp5.rds")
saveRDS(comp6, "data/intermediate_data/MCMCglmm/comp6.rds")
saveRDS(comp7, "data/intermediate_data/MCMCglmm/comp7.rds")
saveRDS(comp8, "data/intermediate_data/MCMCglmm/comp8.rds")
saveRDS(comp9, "data/intermediate_data/MCMCglmm/comp9.rds")
saveRDS(comp10, "data/intermediate_data/MCMCglmm/comp10.rds")

#read imputed datasets
comp1 <- readRDS("data/intermediate_data/MCMCglmm/comp1.rds")
comp2 <- readRDS("data/intermediate_data/MCMCglmm/comp2.rds")
comp3 <- readRDS("data/intermediate_data/MCMCglmm/comp3.rds")
comp4 <- readRDS("data/intermediate_data/MCMCglmm/comp4.rds")
comp5 <- readRDS("data/intermediate_data/MCMCglmm/comp5.rds")
comp6 <- readRDS("data/intermediate_data/MCMCglmm/comp6.rds")
comp7 <- readRDS("data/intermediate_data/MCMCglmm/comp7.rds")
comp8 <- readRDS("data/intermediate_data/MCMCglmm/comp8.rds")
comp9 <- readRDS("data/intermediate_data/MCMCglmm/comp9.rds")
comp10 <- readRDS("data/intermediate_data/MCMCglmm/comp10.rds")

#MCMCGLMM

#non-informative prior
prior1 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*1000)))

#make tree ultrametric
is.ultrametric(tree_sub)
tree_sub_ultrametric <- force.ultrametric(tree_sub)

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
) #13 minutes

#look at the results
summary(mod_op_test)
plot(mod_op_test)

#pagel's lambda
v_dist <- log(1+ 1/mean(dat_sub$h))
1.599/(1.599 + 0.7595 + v_dist) #0.6390647


