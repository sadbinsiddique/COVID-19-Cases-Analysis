# COVID-19 Cases Analysis
# Load library 
library(tidyverse)  
library(lubridate)  
library(scales)     
library(zoo)        

# Load data
raw_data <- read.csv("data.csv", header = TRUE, sep = ',')

# Answer Of Question One
df <- raw_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "cases") %>%
  mutate(date = mdy(gsub("\\.", "/", sub("^X", "", date)))) %>%
  filter(date >= as.Date("2020-01-22") & date <= as.Date("2020-10-31"))

head(df)

# Answer Of Question Two
new_df<- df %>%
  mutate(cases = as.numeric(cases)) %>%
  group_by(Country.Region, date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  rename(country_name = Country.Region)

head(new_df)

# Answer Of Question Three
selected_countries <- c("China", "Colombia", "Germany", "Nigeria", "US")

selected_countries_df <- new_df %>%
  filter(country_name %in% selected_countries) %>%
  filter(!is.na(total_cases) & total_cases > 0)

p <- ggplot(selected_countries_df, aes(x = date, y = total_cases, color = country_name)) +
  geom_line(linewidth = 1) + 
  scale_y_continuous(labels = comma) + 
  labs(title = "COVID-19 Cases", subtitle = "Analysis: Jan 2020 - Oct 2020", x = "Timeline", y = "Confirmed Cases", color = "Country") + theme_minimal()

print(p)

# Answer Of Question Four
p_log <- ggplot(selected_countries_df, aes(x = date, y = total_cases, color = country_name)) +
  geom_line(linewidth = 1) + 
  scale_y_log10(labels = comma) + 
  labs(
    title = "COVID-19 Cases (Log Scale)",
    subtitle = "Analysis: Jan 2020 - Oct 2020",
    x = "Timeline",
    y = "Total Confirmed Cases",
    color = "Country"
  ) + 
  theme_minimal()

print(p_log)

# Answer Of Question Five
selected_countries_df <- selected_countries_df %>%
  arrange(country_name, date) %>%
  group_by(country_name) %>%
  mutate(
    total_cases = cummax(total_cases),
    new_cases = total_cases - lag(total_cases),
    new_cases = ifelse(is.na(new_cases), total_cases, new_cases)
  ) %>%
  ungroup()

head(selected_countries_df)

# Answer Of Question Six
selected_countries_df <- selected_countries_df %>%
  group_by(country_name) %>%
  arrange(date) %>%
  mutate(seven_day_avg = rollmean(new_cases, k = 7, fill = NA, align = "center")) %>%
  ungroup()

p_rolling <- ggplot(selected_countries_df, aes(x = date, y = seven_day_avg, color = country_name)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "7 Day Rolling Average",
    x = "Date",
    y = "7 day average",
    color = "Country"
  ) +
  theme_minimal()

print(p_rolling)

# Answer Of Question Seven
selected_countries_df %>%
  filter(!is.na(seven_day_avg)) %>%
  ggplot(aes(x = date, y = seven_day_avg, color = country_name)) +
  geom_line()

