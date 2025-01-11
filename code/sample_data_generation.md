### read data and create sample dataset
```
# install arrow package to read parquet file
install.packages('arrow')
library(arrow)

df <- read_parquet("full_data.parquet")

# cols to convert to integer
integer_cols = c('b2b_from', 'b2b_to', 'permanent_from', 'permanent_to', 'mandate_from', 'mandate_to')

# convert columns to integer
df[integer_cols] <- lapply(df[integer_cols], as.integer)

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

```
