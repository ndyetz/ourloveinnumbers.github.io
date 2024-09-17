#Clear Environment
rm(list = ls())

library(rmarkdown)

#Libraries and functions (Includes Dates)
source("source_functions.R")

# Load data and formatting
source("source_file.R")

# Google API. for google_maps(). Function will not work without API key.
source("private_key.R")

# Render the .Rmd file and output as index.html document for final website.
rmarkdown::render('wedding_charts.Rmd',
                  output_file = "index.html")