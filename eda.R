library(arrow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(leaflet)
library(wordcloud)
library(tm)
library(scales)
library(treemapify)


# Load the Dataset
jobs_data <- read_parquet("full_data.parquet")

# Data Cleaning and Transformation
# filter observation for only Poland
jobs_data <- jobs_data %>% filter(country_code == 'PL')

# Convert dates to proper Date format
jobs_data$published_date <- as.Date(jobs_data$published_date)

# Salary Data Transformation
jobs_salary <- jobs_data %>%
  mutate(
    b2b_salary_from = as.numeric(b2b_from),
    b2b_salary_to = as.numeric(b2b_to),
    permanent_salary_from = as.numeric(permanent_from),
    permanent_salary_to = as.numeric(permanent_to),
    mandate_from = as.numeric(mandate_from),
    mandate_to = as.numeric(mandate_to),
    skills_count = as.integer(skills_count),
    latitude = as.double(latitude),
    longitude = as.double(longitude),
    experience_level = as.factor(experience_level),
    company_size_category = as.factor(company_size_category),
    marker_icon = as.factor(marker_icon),
    workplace_type = as.factor(workplace_type),
    b2b_currency = as.factor(b2b_currency),
    permanent_currency = as.factor(permanent_currency),
    mandate_currency = as.factor(mandate_currency)
  )

# Extract year and month for temporal analysis
jobs_data <- jobs_data %>%
  mutate(year = year(published_date), month = month(published_date, label = TRUE))

# Convert the 'published_date' column to Date type if it isn't already
jobs_data$published_date <- as.Date(jobs_data$published_date)
# Filter the data for the year 2022
jobs_2022 <- jobs_data %>%
  filter(format(published_date, "%Y") == "2022")

head(jobs_2022)

# Check column names
colnames(jobs_2022)


# 1. Job Offers by City
# Get top 20 cities based on the count of job offers
top_cities <- jobs_2022 %>%
  mutate(city = tolower(city)) %>%
  count(city) %>%
  arrange(desc(n)) %>%
  head(20)

# Create bar plot for top 20 cities
ggplot(top_cities, aes(x = reorder(city, n), y = n, fill = city)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Top 20 Cities with Most Job Offers",
    x = "City",
    y = "Number of Job Offers"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none"
  )


# 2. Experience Levels
ggplot(jobs_2022, aes(x = experience_level, fill = experience_level)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Job Offers by Experience Level",
    x = "Experience Level",
    y = "Count",
    fill = "Experience Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )


# 3. Distribution of Job Titles
# Get top 20 cities based on the count of job offers
top_titles <- jobs_2022 %>%
  count(title) %>%
  arrange(desc(n)) %>%
  head(20)

# Create a bar plot for top 20 in-demand job titles
ggplot(top_titles, aes(x = reorder(title, n), y = n, fill = title)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Top 20 In-Demand Job Titles",
    x = "Job Title",
    y = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none"
  )


# 4. Salary Distribution by Employment Type
jobs_salary_long <- jobs_salary %>%
  pivot_longer(cols = starts_with("b2b_salary_") | starts_with("permanent_salary_"), 
               names_to = "salary_type", values_to = "salary") %>%
  filter(!is.na(salary))

ggplot(jobs_salary_long, aes(x = salary_type, y = salary)) +
  geom_boxplot(fill = "coral", width=0.5) +
  scale_y_continuous(labels = scales::comma, trans = "log10") +
  # facet_wrap(~salary_type, scales = "free_x") +
  theme_minimal() +
  labs(title = "Salary Distribution by Employment Type", x = "Salary Type", y = "Salary (PLN)")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )


# 5. Remote vs Onsite Jobs
ggplot(jobs_2022, aes(x = workplace_type, fill = workplace_type)) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Remote vs Onsite Jobs", x = "Workplace Type", y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )


# 6. Temporal Analysis of Job Postings
ggplot(jobs_2022, aes(x = year, fill = as.factor(month))) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Job Postings Over Time", x = "Year", y = "Count of Job Postings", fill = "Month")

# Aggregate data by year and month for 2022
monthly_data <- jobs_2022 %>%
  group_by(year, month) %>%
  summarize(job_postings = n(), .groups = "drop")

# Create bar plot with a line showing the trend, and text labels on top of each bar
p <- ggplot(monthly_data, aes(x = month, y = job_postings, fill = month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar plot without legend for fill
  geom_line(aes(group = 1), color = "blue", linewidth = 1, linetype = "dashed") +  # Add trend line
  # geom_text(aes(label = scales::comma(job_postings)), vjust = -0.5, size = 3) +
  geom_text(aes(label = scales::comma(job_postings)), 
            fontface = "bold",  # Make the text bold
            vjust = -1.5,         # Move the text a little above the bar
            size = 3.5) +         # Set text size
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  scale_x_discrete(limits = month.abb) +  # Order the months correctly
  theme_minimal() +
  labs(
    title = "Monthly Job Postings in 2022 with Trend Line",
    x = "Month",
    y = "Number of Job Postings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggsave("monthly_posting_with_trend_line.png", plot = p, width = 10, height = 14, dpi = 300)


# 7. Company Size Distribution
ggplot(jobs_2022, aes(x = company_size_category, fill = company_size_category)) +
  geom_bar() +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Company Size Distribution", x = "Company Size Category", y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

# 8. Pie chart
# Prepare data to calculate percentages
company_size_data <- jobs_2022 %>%
  count(company_size_category) %>%
  mutate(percentage = n / sum(n) * 100)

# Pie chart with percentage labels

p2 <- ggplot(company_size_data, aes(x = "", y = n, fill = company_size_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(
    title = "Company Size Distribution",
    fill = "Company Size Category",
    caption = "Company size definitions:\nSmall: ≤ 50 employees\nMedium: ≤ 250 employees\nLarge: ≤ 1000 employees\nEnterprise: > 1000 employees"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_blank(),  # Hide axis text
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    plot.caption = element_text(hjust = 0, size = 10, color = "black")
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_brewer(palette = "Set3")


plot(p2)
ggsave("pie_chart_of_company_size.png", plot = p2, width = 10, height = 10, dpi = 300)


# 9. Remote Interviews Availability
ggplot(jobs_2022, aes(x = remote_interview, fill = remote_interview)) +
  geom_bar() +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Jobs with Remote Interview Options", x = "Remote Interview Available", y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )


# 10. Job Market Trends Over Time by Experience Level
# Convert published_date to Date format
jobs_2022$published_date <- as.Date(jobs_2022$published_date)

# Aggregate job postings by published_date and experience_level
trend_data <- jobs_2022 %>%
  group_by(published_date, experience_level) %>%
  summarise(total_jobs = n(), avg_skills_count = mean(skills_count, na.rm = TRUE)) %>%
  ungroup()

# Line Chart: Trends over Time by Experience Level
ggplot(trend_data, aes(x = published_date, y = total_jobs, color = experience_level)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Job Market Trends Over Time",
    x = "Date",
    y = "Number of Job Postings",
    color = "Experience Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom"
  )


# 11. B2B salary distribution by experience level
# Filter and clean salary data
salary_data <- jobs_2022 %>%
  filter(!is.na(b2b_from) & !is.na(b2b_to)) %>%
  mutate(
    b2b_from = as.numeric(b2b_from),
    b2b_to = as.numeric(b2b_to),
    avg_b2b_salary = (b2b_from + b2b_to) / 2
  )

# Box Plot: Salary Distribution by Experience Level
p4 <- ggplot(salary_data, aes(x = experience_level, y = avg_b2b_salary, fill = experience_level)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  scale_y_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma, trans = "log10") +
  theme_minimal() +
  labs(
    title = "B2B Salary Distribution by Experience Level",
    x = "Experience Level",
    y = "Average B2B Salary (PLN)",
    fill = "Experience Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

plot(p4)

ggsave("b2b_salary_range_by_exp.png", plot = p4, width = 10, height = 10, dpi = 300)


# 12. B2B salary distribution by experience level
# Filter and clean salary data
salary_data <- jobs_2022 %>%
  filter(!is.na(b2b_from) & !is.na(b2b_to)) %>%
  mutate(
    b2b_from = as.numeric(b2b_from),
    b2b_to = as.numeric(b2b_to),
    avg_b2b_salary = (b2b_from + b2b_to) / 2
  )

# Create Violin Plot with log scale for better visualization
p5 <- ggplot(salary_data, aes(x = experience_level, y = avg_b2b_salary, fill = experience_level)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  scale_y_continuous(labels = scales::comma, trans = "log10") +  # Log-scale transformation
  theme_minimal() +
  labs(
    title = "Salary Distribution by Experience Level (Log-Scale)",
    x = "Experience Level",
    y = "Average B2B Salary (PLN)",
    fill = "Experience Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

plot(p5)

ggsave("b2b_salary_range_by_exp_log_scale.png", plot = p5, width = 10, height = 10, dpi = 300)



# Skills treemap by job level

library(treemapify)
library(stringr)

skills_data <- jobs_data %>%
  pivot_longer(
    cols = c("skill_1", "skill_2", "skill_3"),
    names_to = "skill_type", 
    values_to = "skill"
  ) %>%
  filter(!is.na(skill)) %>% # Remove NA skills
  mutate(
    skill = str_replace_all(skill, "[^[:alnum:]\\s]", ""), 
    skill = str_squish(skill) 
  ) %>%
  group_by(experience_level, skill) %>% 
  summarise(skill_count = n(), .groups = 'drop') %>% 
  arrange(experience_level, desc(skill_count))

skills_data_1 <- skills_data %>% 
  filter(skill_count > 1000, !is.na(skill), !is.na(experience_level)) %>%
  group_by(experience_level) %>%
  slice_max(skill_count, n = 50) %>%
  ungroup()


# treemap
p7 <- ggplot(skills_data_1, aes(
  area = skill_count,
  fill = skill_count,
  label = skill
)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 15
  ) +
  scale_fill_viridis_c(
    labels = label_number(scale = 1e-6, suffix = " mln") # Convert to millions
  ) + 
  facet_wrap(~experience_level) + 
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold",  
      size = 16       
    )
  ) +
  labs(
    title = "Demanding Skills by Experience Level",
    subtitle = "Treemap displaying most demanding skills for each level of experience",
    fill = "Skill Count"
  )


plot(p7)

ggsave("skills_treemap.png", plot = p7, width = 20, height = 13, dpi = 300)
