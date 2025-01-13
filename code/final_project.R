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

df <- read_parquet("full_data.parquet")

# filter observation for only Poland
df <- df %>% filter(country_code == 'PL')

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


# ploting part 

df <- read_parquet("full_data.parquet")

# cols to convert
float_cols <- c('latitude', 'longitude')
integer_cols <- c('b2b_from', 'b2b_to','permanent_from', 'permanent_to', 'mandate_from', 'mandate_to', 'skills_count')
factor_cols <- c('experience_level', 'company_size_category', 'marker_icon', 'workplace_type', 'b2b_currency', 'permanent_currency', 'mandate_currency')

# convert columns
df[integer_cols] <- lapply(df[integer_cols], as.integer)
df[float_cols] <- lapply(df[float_cols], as.numeric)
df[factor_cols] <- lapply(df[factor_cols], as.factor)
df$published_date <- as.Date(df$published_date)
df$experience_level <- as.factor(df$experience_level)



# Animated line plot grouped jobs by level - senior, middle and junior

# Prepare the data for the animated line chart
trend_data <- df %>%
  group_by(published_date, experience_level) %>%
  summarise(job_count = n(), .groups = "drop") %>%
  arrange(published_date)

# # Create the animated line chart
# p <- ggplot(trend_data, aes(x = published_date, y = job_count, color = experience_level, group = experience_level)) +
#   geom_line() +
#   geom_point() +
#   labs(title = 'Job Market Trends Over Time',
#        x = 'Date',
#        y = 'Number of Job Postings',
#        color = 'Experience Level') +
#   theme_minimal() +
#   transition_reveal(published_date)

# animate(p)

# # save the animation
# anim_save("job_market_trends.gif", animation = p)



# create the animated line chart
p1 <- ggplot(trend_data, aes(x = published_date, y = job_count, color = experience_level, group = experience_level)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = 'Job Market Trends Over Time',
       x = 'Date',
       y = 'Number of Job Postings',
       color = 'Experience Level') +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  transition_reveal(published_date)

animate(p1)

# save the animation
anim_save("job_market_trends_1.gif", animation = p1)


# plot 2 - Job Market Trends - line plot with marker icon trend each month

df <- df %>%
  mutate(year_month = floor_date(published_date, "month"))

trend_data1 <- df %>%
  group_by(year_month) %>%
  summarise(job_count = n(), 
            most_common_marker_icon = names(sort(table(marker_icon), decreasing = TRUE)[1:3]), 
            .groups = 'drop') %>%
  arrange(year_month)


# Create the line chart without lines and using a different theme
p <- ggplot(trend_data1, aes(x = year_month, y = job_count)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(aes(label = most_common_marker_icon), 
                  nudge_y = 10, 
                  arrow = arrow(length = unit(0.02, "npc")), 
                  size = 5, 
                  color = "red",
                  max.overlaps = 20) +
  labs(title = 'Number of Jobs Each Month',
       x = '',
       y = 'Number of Job Postings') +
  theme_light() +  # Use a different theme
  theme(
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "#eadbdb", color = NA),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_line(color = "#dad5d5"),  # Remove minor grid lines
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma)

ggsave("job_market_trends_wide.png", plot = p, width = 14, height = 12)


# Display the plot
print(p)


# plot 3 - salary analysis

# Prepare the data for the box plot with jitter
salary_data <- df %>%
  filter(!is.na(b2b_from) & !is.na(b2b_to) & (b2b_currency == 'pln')) %>%
  mutate(b2b_range = b2b_to - b2b_from)


# Create the box plot with jitter
p <- ggplot(salary_data, aes(x = experience_level, y = b2b_range, color = b2b_currency)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7) +
  labs(title = 'Salary Analysis by Experience Level and Currency',
       x = 'Experience Level',
       y = 'Salary Range (B2B)') +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_y_continuous(labels = scales::comma)

# Display the plot
print(p)


