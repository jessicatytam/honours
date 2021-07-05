library(formattable)
library(htmltools)
library(webshot)  

includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 

#generate df

table_df <- data.frame("Predictor" = c("Open Tree of Life mammals list",
                                       "Phylogenetic tree",
                                       "Body mass",
                                       "Latitude",
                                       "Google trends",
                                       "IUCN Red List status",
                                       "Human use",
                                       "Domestication"),
                       "Completeness" = percent(c(6549/7521,
                                                  5498/7521,
                                                  5157/7521,
                                                  4435/7521,
                                                  1322/7521,
                                                  5244/7521,
                                                  1397/7521,
                                                  7521/7521)),
                       "Imputation" = c("No",
                                        "No",
                                        "Yes",
                                        "Yes",
                                        "No",
                                        "Yes",
                                        "No",
                                        "No"),
                       "Treatment (details)" = c("/",
                                                 "/",
                                                 "Transformation (log10(value))",
                                                 "/",
                                                 "Converted to binomial (0 = 0; 1 = not 0)",
                                                 "Converted to ordinal (1 = LC; 2 = VU; 3 = EN; 4 = CE; 5 = EW)",
                                                 "Converted to binomial (0 = data present; 1 = data absent)",
                                                 "Converted to ordinal (1 = Domesticated; 2 = Partially-domesticated; 3 = Wild)"))

#renaming

names(table_df)[names(table_df) == "Treatment..details."] <- "Treatment (details)"

#formatter

title_formatter <- formatter("span",
                             style = x ~ style(color = "black",
                                               "font-size" = 22,
                                               "font-family" = "calibri",
                                               "font-face" = "bold"))

text_formatter <- formatter("span",
                            style = x ~ style(color = "black",
                                              "font-size" = 22,
                                              "font-family" = "calibri"))

bar_formatter <- formatter("span",
                           style = x ~ style(
                             color = "black",
                             display = "inline-block",
                             "background-color" = csscolor("lightblue"),
                             width = percent(proportion(x)),
                             "font-family" = "calibri",
                             "font-size" = 22
                           ))

widget_formatter <- formatter("span",
                              x ~ icontext(ifelse(x == "Yes", "ok", "remove"), ifelse(x == "Yes", "Yes", "No")),
                              style = x ~ style("font-family" = "calibri",
                                                "font-size" = 22,
                                                color = ifelse(x == "Yes", "green", "red")))

#table

names(table_df) <- title_formatter(names(table_df)) #only run this once!

table <- formattable(table_df,
            align = "l",
            list(area(col = c(1, 4)) ~ text_formatter,
                 area(col = c(2)) ~ bar_formatter,
                 area(col = c(3)) ~ widget_formatter))

#save

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(table, "outputs/data_table.png")            
