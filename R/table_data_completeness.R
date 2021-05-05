library(formattable)

includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 

#generate df

table_df <- data.frame("Predictor" = c("Body mass",
                                       "Latitude",
                                       "Open Tree of Life phylogeny",
                                       "Phylogenetic tree",
                                       "IUCN Red List status",
                                       "Human use",
                                       "Domestication",
                                       "Google trends"),
                       "Completeness" = percent(c(5158/7522,
                                                  4435/7522,
                                                  6549/7522,
                                                  5498/7522,
                                                  5244/7522,
                                                  1397/7522,
                                                  7522/7522,
                                                  1323/7522)),
                       "Treatment" = c("Log transformation",
                                       "/",
                                       "/",
                                       "/",
                                       "Converted to ordinal",
                                       "Converted to binomial",
                                       "Converted to ordinal",
                                       "Converted to binomial"),
                       "Treatment details" = c("log10(value)",
                                               "/",
                                               "/",
                                               "/",
                                               "1 = LC; 2 = VU; 3 = EN; 4 = CE; 5 = EW",
                                               "0 = data present; 1 = data absent",
                                               "1 = Domesticated; 2 = Partially-domesticated; 3 = Wild",
                                               "0 = 0; 1 = not 0"),
                       "Require imputation" = c("Yes",
                                                "Yes",
                                                "No",
                                                "No",
                                                "Yes",
                                                "No",
                                                "No",
                                                "No"))

#renaming

names(table_df)[names(table_df) == "Treatment.details"] <- "Treatment details"
names(table_df)[names(table_df) == "Require.imputation"] <- "Require imputation"

#formatter

title_formatter <- formatter("span",
                             style = x ~ style(color = "black",
                                               "font-size" = 16,
                                               "font-family" = "calibri",
                                               "font-face" = "bold"))

text_formatter <- formatter("span",
                            style = x ~ style(color = "black",
                                              "font-size" = 16,
                                              "font-family" = "calibri"))

bar_formatter <- formatter("span",
                           style = x ~ style(
                             color = "black",
                             display = "inline-block",
                             "background-color" = csscolor("lightblue"),
                             width = percent(proportion(x)),
                             "font-family" = "calibri",
                             "font-size" = 16
                           ))

widget_formatter <- formatter("span",
                              x ~ icontext(ifelse(x == "Yes", "ok", "remove"), ifelse(x == "Yes", "Yes", "No")),
                              style = x ~ style("font-family" = "calibri",
                                                "font-size" = 16,
                                                color = ifelse(x == "Yes", "green", "red")))

#table

names(table_df) <- title_formatter(names(table_df)) #only run this once!

formattable(table_df,
            align = "l",
            list(area(col = c(1, 3, 4)) ~ text_formatter,
                 area(col = c(2)) ~ bar_formatter,
                 area(col = c(5)) ~ widget_formatter))
            