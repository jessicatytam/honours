library(formattable)

includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 

table_df <- data.frame("Predictor" = c("Body mass",
                                       "Latitude",
                                       "Phylogeny",
                                       "Phylogenetic tree",
                                       "IUCN Red List status",
                                       "Human use",
                                       "Domestication"),
                       "Completeness" = c(5158/7522,
                                          4435/7522,
                                          6549/7522,
                                          5498/7522,
                                          5244/7522,
                                          1397/7522,
                                          7522/7522),
                       "Treatment" = c("Log transformation",
                                       "/",
                                       "/",
                                       "/",
                                       "Converted to ordinal",
                                       "Converted to binomial",
                                       "Converted to ordinal"),
                       "Details of treatment" = c("log10(value)",
                                                  "/",
                                                  "/",
                                                  "/",
                                                  "1 = LC; 2 = VU; 3 = EN; 4 = CE; 5 = EW",
                                                  "0 = data present; 1 = data absent",
                                                  "1 = Domesticated; 2 = Partially-domesticated; 3 = Wild"))

formattable(table_df,
            align = c("l"),
            list("Completeness" = color_bar("#71CA97")))
