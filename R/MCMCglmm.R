# doing MCMCglmm

# package
library(tidyverse)
library(MCMCglmm)
library(lme4)
library(phyr)
library(here)


includeh <- read.csv(here("outputs", "includeh.csv"))[-c(1)]

# get phylo
tree <- read.tree(here("intermediate_data", "tree.tre"))
tree <- compute.brlen(tree) 

# this seems to make correlation matrix anyways!!!
cov_tree <- vcv2(tree, corr = FALSE)

# 