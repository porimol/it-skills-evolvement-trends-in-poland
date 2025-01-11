# IT skills evolvement trends in Poland

#### 1. **Geographical Analysis**
   **Chart Type**: Interactive Map (e.g., Leaflet or ggplot2 with plotly for interactivity)  
   **Columns**: `country_code`, `city`, `latitude`, `longitude`, `marker_icon`  
   **Reason**: To show the geographical distribution of job postings, providing an interactive way to explore job locations and their associated domains.

#### 2. **Job Market Trends**
   **Chart Type**: Animated Line Chart (e.g., gganimate)  
   **Columns**: `published_date`, `skills_count`, `experience_level`  
   **Reason**: To represent trends in job postings over time, segmented by experience levels or number of skills required. Animation adds a temporal storytelling element.

#### 3. **Salary Analysis**
   **Chart Type**: Box Plot with Jitter (e.g., ggplot2)  
   **Columns**: `b2b_currency`, `b2b_from`, `b2b_to`, `experience_level`  
   **Reason**: To display the distribution of salaries across experience levels and currencies, highlighting outliers and median ranges.

#### 4. **Skill Demand Visualization**
   **Chart Type**: Heatmap or Circular Heatmap  
   **Columns**: `skill_1`, `skill_1_level`, `skill_2`, `skill_2_level`, `skill_3`, `skill_3_level`  
   **Reason**: To show the intensity of demand for different skills and their levels in a visually engaging manner.

#### 5. **Company Size Insights**
   **Chart Type**: Treemap or Sunburst Chart  
   **Columns**: `company_size_category`, `company_name`, `country_code`  
   **Reason**: To depict the proportion of job postings by company size category, further broken down by companies and regions.

#### 6. **Employment Types**
   **Chart Type**: Radial Bar Chart  
   **Columns**: `emp_type_count`, `workplace_type`, `experience_level`  
   **Reason**: To emphasize the distribution of employment types (e.g., remote, partly remote, office-based) across different experience levels.

#### 7. **Correlation Matrix**
   **Chart Type**: Correlation Heatmap  
   **Columns**: Numerical columns like `skills_count`, `emp_type_count`, `b2b_from`, `b2b_to`, `multi_loc_count`  
   **Reason**: To explore relationships between various quantitative metrics, helping in data-driven insights.

#### 8. **Multilocation Job Analysis**
   **Chart Type**: Chord Diagram  
   **Columns**: `multilocation`, `multi_loc_count`, `city`  
   **Reason**: To visualize connections between cities for jobs that are available in multiple locations, emphasizing regional interconnectedness.

#### 9. **Job Titles and Skills Word Cloud**
   **Chart Type**: Interactive Word Cloud  
   **Columns**: `title`, `skill_1`, `skill_2`, `skill_3`  
   **Reason**: To highlight the most common job titles and skills in a visually compelling way.

#### 10. **Experience Level Analysis**
   **Chart Type**: Violin Plot  
   **Columns**: `experience_level`, `b2b_from`, `b2b_to`  
   **Reason**: To show the distribution of salary ranges across experience levels, focusing on density and variations.



---
### Additional 
----
#### 11. **Job Posting Dynamics Across Time**
   **Chart Type**: Stream Graph (e.g., streamgraph or ggplot2 with geom_area)  
   **Columns**: `published_date`, `experience_level`, `skills_count`  
   **Reason**: To visualize the ebb and flow of job postings over time for different experience levels or required skills. This dynamic, flowing visualization is visually appealing and conveys patterns effectively.

#### 12. **Remote Work Evolution**
   **Chart Type**: Sankey Diagram  
   **Columns**: `workplace_type`, `remote_interview`, `country_code`, `experience_level`  
   **Reason**: To show the transition and relationship between workplace types (remote, partly remote, office-based), remote interview availability, and regions. This visually conveys the connections and trends in work preferences.

#### 13. **Job Postings by Company Size and Region**
   **Chart Type**: Bubble Chart (e.g., ggplot2 with plotly for interactivity)  
   **Columns**: `company_size_category`, `country_code`, `b2b_from`, `b2b_to`  
   **Reason**: To represent the volume of job postings by company size and region, with bubble sizes indicating salary ranges or counts. It combines categorical and numerical data effectively.

#### 14. **Temporal Job Posting Heatmap**
   **Chart Type**: Calendar Heatmap  
   **Columns**: `published_date`, `skills_count`, `experience_level`  
   **Reason**: To map the intensity of job postings over a calendar year. This visually highlights peak periods for hiring and areas of lesser activity.
