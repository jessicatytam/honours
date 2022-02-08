#load data
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)]

includeh[includeh$domestication_bin==1,] #domesticated spp
sum(includeh$domestication_bin==1) #12
includeh[includeh$domestication_bin==2,] #partially-domesticated spp
sum(includeh$domestication_bin==2) #136
