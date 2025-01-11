# Function to check if a package is installed, and install it if not
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of packages to install
packages <- c('tidyverse', 'ggplot2', 'ggrepel', 'ggalt', 'Rmisc', 'gridExtra', 
              'cowplot', 'ggforce', 'GGally', 'ComplexUpset', 'ggplot2movies', 
              'ggpubr', 'treemapify', 'ggparallel', 'ggcharts', 'ggpol', 
              'ggcorrplot', 'readxl', 'lubridate', 'tidygeocoder', 'ggmap', 
              'rvest', 'stringr', 'arrow')

# Install and load packages
lapply(packages, install_if_missing)

# reading sample parquet file
setwd("C:/Users/samid/OneDrive/Desktop/Adv Vis in R/final project")

df <- read_parquet("full_data.parquet")

# cols to convert to integer
integer_cols = c('b2b_from', 'b2b_to', 'permanent_from', 'permanent_to', 'mandate_from', 'mandate_to')

# convert columns to integer
df[integer_cols] <- lapply(df[integer_cols], as.integer)

str(df)


print(paste("top 5 multilocaiton types: ", head(df$multilocation_type, 10)))

# displaying the first 5 rows of the data
head(df)

# displaying the number of rows in the data
nrow(df)

# create a sumple data from the original data
df_sample <- df[sample(nrow(df), 2000000), ]

# write the sample data to a parquet file
write_parquet(df_sample, "sample_data.parquet")

# read the sample data from the parquet file
df_sample <- read_parquet("full_data.parquet")

nrow(df_sample)
str(df_sample)

# memeroey usage of the data
object.size(df_sample)

