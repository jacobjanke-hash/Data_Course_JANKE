# ============================================================================
# BIOL3100 Practice Exam 1 - Solution
# Student: JANKE
# Date: March 2026
# ============================================================================
# This script analyzes COVID-19 data for US states, focusing on states
# beginning with the letter "A" (Alabama, Alaska, Arizona, Arkansas).
# ============================================================================

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For plotting
library(stringr)    # For string operations
library(scales)     # For scale formatting

# ============================================================================
# TASK I: Read the CSV file using relative paths (20 points)
# ============================================================================
# Read the cleaned COVID data from the CSV file
# Using relative path - the CSV file is in the same directory as this script

covid_data <- read.csv("cleaned_covid_data.csv")

# Verify the data was read correctly
cat("=== Task I: Data Import ===\n")
cat("Dimensions of data:", dim(covid_data)[1], "rows,", dim(covid_data)[2], "columns\n")
cat("Column names:", paste(names(covid_data), collapse = ", "), "\n")
cat("Number of unique states:", length(unique(covid_data$state)), "\n\n")

# Convert date column to Date type for proper plotting
covid_data$date <- as.Date(covid_data$date)

# Preview the data structure
str(covid_data)
head(covid_data)

# ============================================================================
# TASK II: Subset to states beginning with "A" (20 points)
# ============================================================================
# Create a subset containing only states that start with the letter "A"
# Expected states: Alabama, Alaska, Arizona, Arkansas

A_states <- covid_data %>%
  filter(str_starts(state, "A"))

# Alternatively, using base R:
# A_states <- covid_data[grepl("^A", covid_data$state), ]

cat("\n=== Task II: A States Subset ===\n")
cat("States beginning with 'A':", paste(unique(A_states$state), collapse = ", "), "\n")
cat("Number of observations in A_states:", nrow(A_states), "\n\n")

# ============================================================================
# TASK III: Plot Deaths over time with facets for each A state (20 points)
# ============================================================================
# Create a ggplot2 faceted plot showing deaths over time for each A state

cat("=== Task III: Creating Faceted Deaths Plot ===\n")

deaths_plot <- ggplot(A_states, aes(x = date, y = deaths, color = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.8, alpha = 0.6) +
  facet_wrap(~state, scales = "free_y", ncol = 2) +
  labs(
    title = "Cumulative COVID-19 Deaths Over Time",
    subtitle = "US States Beginning with 'A' (March 2020 - December 2021)",
    x = "Date",
    y = "Cumulative Deaths",
    caption = "Source: COVID-19 Data Analysis - BIOL3100 Exam"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # Remove legend since we have facet labels
    strip.background = element_rect(fill = "steelblue"),
    strip.text = element_text(color = "white", face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_color_brewer(palette = "Set1")

# Save the plot
ggsave("A_states_deaths_over_time.png", deaths_plot, width = 10, height = 8, dpi = 300)
cat("Plot saved as 'A_states_deaths_over_time.png'\n\n")

# Display the plot
print(deaths_plot)

# ============================================================================
# TASK IV: Find peak Case_Fatality_Ratio for each state (20 points)
# ============================================================================
# Use dplyr to find the maximum Case_Fatality_Ratio for each state
# Save the result as state_max_fatality_rate

cat("=== Task IV: Peak Case Fatality Ratio by State ===\n")

state_max_fatality_rate <- covid_data %>%
  group_by(state) %>%
  summarize(
    max_fatality_rate = max(Case_Fatality_Ratio, na.rm = TRUE),
    date_of_max = date[which.max(Case_Fatality_Ratio)],
    .groups = "drop"
  ) %>%
  arrange(desc(max_fatality_rate))

# Display results
cat("Top 10 states by peak Case Fatality Ratio:\n")
print(head(state_max_fatality_rate, 10))

cat("\nBottom 10 states by peak Case Fatality Ratio:\n")
print(tail(state_max_fatality_rate, 10))

cat("\nSummary statistics for max fatality rates:\n")
summary(state_max_fatality_rate$max_fatality_rate)

# ============================================================================
# TASK V: Create a meaningful plot using state_max_fatality_rate (20 points)
# ============================================================================
# Create a bar plot showing the peak Case Fatality Ratio for each state

cat("\n=== Task V: Creating State Max Fatality Rate Plot ===\n")

# Get top 20 states for cleaner visualization
top_20_states <- state_max_fatality_rate %>%
  slice_head(n = 20)

fatality_plot <- ggplot(top_20_states, 
                        aes(x = reorder(state, max_fatality_rate), 
                            y = max_fatality_rate, 
                            fill = max_fatality_rate)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", max_fatality_rate)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Peak COVID-19 Case Fatality Ratios by State",
    subtitle = "Top 20 US States (March 2020 - December 2021)",
    x = "State",
    y = "Peak Case Fatality Ratio (%)",
    caption = "Source: COVID-19 Data Analysis - BIOL3100 Exam"
  ) +
  scale_fill_gradient(low = "steelblue", high = "darkred", name = "CFR (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 9),
    legend.position = "right"
  ) +
  ylim(0, max(top_20_states$max_fatality_rate) * 1.15)

# Save the plot
ggsave("state_max_fatality_rate_plot.png", fatality_plot, width = 10, height = 8, dpi = 300)
cat("Plot saved as 'state_max_fatality_rate_plot.png'\n\n")

# Display the plot
print(fatality_plot)

# ============================================================================
# TASK VI (BONUS): Plot cumulative deaths for entire US over time (10 points)
# ============================================================================
# Calculate total US deaths by summing across all states for each date
# Then plot cumulative deaths over time

cat("=== Task VI (BONUS): US Cumulative Deaths Over Time ===\n")

# Calculate total US deaths for each date
us_total_deaths <- covid_data %>%
  group_by(date) %>%
  summarize(
    total_deaths = sum(deaths, na.rm = TRUE),
    total_cases = sum(cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

cat("US total deaths summary:\n")
cat("  First date:", as.character(min(us_total_deaths$date)), 
    "- Deaths:", min(us_total_deaths$total_deaths), "\n")
cat("  Last date:", as.character(max(us_total_deaths$date)), 
    "- Deaths:", max(us_total_deaths$total_deaths), "\n\n")

# Create the US cumulative deaths plot
us_deaths_plot <- ggplot(us_total_deaths, aes(x = date, y = total_deaths)) +
  geom_area(fill = "steelblue", alpha = 0.4) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  labs(
    title = "Cumulative COVID-19 Deaths in the United States",
    subtitle = "March 2020 - December 2021",
    x = "Date",
    y = "Cumulative Deaths",
    caption = "Source: COVID-19 Data Analysis - BIOL3100 Exam"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = scales::comma)

# Save the plot
ggsave("US_cumulative_deaths.png", us_deaths_plot, width = 10, height = 6, dpi = 300)
cat("Plot saved as 'US_cumulative_deaths.png'\n\n")

# Display the plot
print(us_deaths_plot)

# ============================================================================
# Summary
# ============================================================================
cat("\n")
cat("============================================================================\n")
cat("                        EXAM COMPLETION SUMMARY\n")
cat("============================================================================\n")
cat("\n")
cat("Task I:   COVID data successfully loaded from 'cleaned_covid_data.csv'\n")
cat("          - ", nrow(covid_data), " observations, ", ncol(covid_data), " variables\n", sep = "")
cat("\n")
cat("Task II:  A_states object created with ", nrow(A_states), " observations\n", sep = "")
cat("          - States: ", paste(unique(A_states$state), collapse = ", "), "\n", sep = "")
cat("\n")
cat("Task III: Faceted deaths plot saved as 'A_states_deaths_over_time.png'\n")
cat("\n")
cat("Task IV:  state_max_fatality_rate object created with ", nrow(state_max_fatality_rate), " states\n", sep = "")
cat("\n")
cat("Task V:   Max fatality rate plot saved as 'state_max_fatality_rate_plot.png'\n")
cat("\n")
cat("BONUS:    US cumulative deaths plot saved as 'US_cumulative_deaths.png'\n")
cat("\n")
cat("============================================================================\n")
cat("All tasks completed successfully!\n")
cat("============================================================================\n")
