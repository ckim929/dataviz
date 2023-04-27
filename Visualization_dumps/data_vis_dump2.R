# Joe Biden Approval in 2022

library(ggplot2)
library(tidyverse)
library(patchwork)
library(janitor)
library(dplyr)
library(readxl)
library(lubridate)
#install.packages("hrbrthemes")
library(hrbrthemes)

# Read in data of Biden's approval ratings
data <- read_excel('data/president_approval_polls_2022.xlsx')

# select data that only includes the columns: start_date, end_date, sample_size, yes, and no
selected_data <- data %>%
  select(start_date, end_date, sample_size, yes, no)

summary(selected_data)

# Extract the average approval rating for President Biden in 2022
biden_mean_approval <- selected_data %>%
  select(start_date, yes) %>%
  summarize(mean_approval = mean(yes))

# Calculate the average approval rating grouped by month
by_month <- selected_data %>%
  group_by(month = lubridate::floor_date(start_date, 'month')) %>%
  summarize(avg = mean(yes))

# Calculate the average standard deviation grouped by month
by_sd <- selected_data %>%
  group_by(month = lubridate::floor_date(start_date, 'month')) %>%
  summarize(std = sd(yes))

# Create the upper limit and lower limit for the error bars
upper_limit = by_month$avg + by_sd$std
lower_limit = by_month$avg - by_sd$std


ggplot(by_month,
       aes(x = month, y = avg)) +
  geom_bar(stat='identity', fill = "#125CB8") +
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit)) +
  scale_fill_brewer(palette = "Paired") +
  ylim(0,100) +
  labs(title = "President Biden's Approval Ratings (Jan 2022 - Oct 2022)",
       x = NULL, y = "Approval Ratings (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face="bold",size=20),
        axis.text = element_text(face="bold",size=12),
        axis.title = element_text(size=15))



# Visualization 2

# Data cleaning and manipulation, taken from short form blog

# Read in maternal mortality data
data_csv <- read_csv('data/maternalmortality.csv')

# Read in WASH mortality data
wash_csv <- read_csv('data/WASH_data.csv')

# Filter wash_csv data by Female
filtered_by_female <- filter(wash_csv, Dim1 == "Female")

# Select the country and mortality rate and rename mortality rate to Wash_value
selected_wash_csv <- filtered_by_female %>%
  select(Location, FactValueNumeric) %>%
  rename(Wash_value = FactValueNumeric)

# Filter maternal mortality dataset by the maternal mortality ratio (per 100,000 live births), and by year 2017
data_csv_2016 <- filter(data_csv, Indicator == "Maternal mortality ratio (per 100 000 live births)", Period == "2016")

# Combine the two datasets with function merge
wash_combined <- merge(selected_wash_csv, data_csv_2016, by = "Location")

# Select the Location, WASH mortality, and maternal mortality rates
wash_combined_selected <- wash_combined %>%
  select(Location, Wash_value, Value)



ggplot(wash_combined_selected, aes(x=as.numeric(Value), y=as.numeric(Wash_value))) + 
  geom_point(size = 2.5, color = "#085735") + 
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE)+
  theme_ipsum() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,1200)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.text = element_text(face="bold",size=12),
    axis.title = element_text(size=15)) +
  ggtitle("Correlation between Maternal Mortality and Unsafe Water, Sanitation, and Hygiene Mortality\n(with 95% Confidence Interval)") +
  xlab("Maternal Mortality Rate (per 100,000 deaths)") + ylab("Unsafe WASH mortality (per 100,000 deaths)") 

