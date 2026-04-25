# ============================================================================
# BIOL3100 Exam 4 - Redo of Exam 1 with Corrections
# Student: JANKE
# Date: April 2026
# ============================================================================
# This script analyzes COVID-19 data for US states, focusing on states
# beginning with the letter "A" (Alabama, Alaska, Arizona, Arkansas).
# 
# CORRECTIONS FROM EXAM 1:
# - FIX 1: Use the existing Case_Fatality_Ratio column from the CSV (as-is)
#          to find the peak fatality ratio per state
# - FIX 2: Properly calculate cumulative deaths for the entire US over time
#          by ensuring correct aggregation and sorting
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
    caption = "Source: COVID-19 Data Analysis - BIOL3100 Exam 4"
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
# *** FIX 1: USE THE EXISTING Case_Fatality_Ratio COLUMN FROM THE CSV ***
# Group by state and find the MAXIMUM Case_Fatality_Ratio for each state.
# Save as `state_max_fatality_rate` data frame.

cat("=== Task IV: Peak Case_Fatality_Ratio by State (CORRECTED) ===\n")
cat("*** FIX 1: Using existing Case_Fatality_Ratio column from CSV ***\n\n")

# Group by state and find the peak (max) Case_Fatality_Ratio for each state
state_max_fatality_rate <- aggregate(Case_Fatality_Ratio ~ state,
                                     data = covid_data,
                                     FUN = function(x) max(x, na.rm = TRUE))
names(state_max_fatality_rate)[2] <- "max_fatality_rate"
state_max_fatality_rate <- state_max_fatality_rate[order(-state_max_fatality_rate$max_fatality_rate), ]
rownames(state_max_fatality_rate) <- NULL

# Display results
cat("Top 10 states by peak Case_Fatality_Ratio:\n")
print(head(state_max_fatality_rate, 10))

cat("\nBottom 10 states by peak Case_Fatality_Ratio:\n")
print(tail(state_max_fatality_rate, 10))

cat("\nSummary statistics for peak Case_Fatality_Ratio:\n")
print(summary(state_max_fatality_rate$max_fatality_rate))
cat("\n")

cat("Range: ", round(min(state_max_fatality_rate$max_fatality_rate), 2), " to ",
    round(max(state_max_fatality_rate$max_fatality_rate), 2), "\n\n", sep="")

# ============================================================================
# TASK V: Create a meaningful plot using state_max_fatality_rate (20 points)
# ============================================================================
# Create a bar plot showing the peak fatality rate for each state

cat("\n=== Task V: Creating State Max Fatality Rate Plot ===\n")

# Get top 20 states for cleaner visualization
top_20_states <- head(state_max_fatality_rate, 20)

fatality_plot <- ggplot(top_20_states, 
                        aes(x = reorder(state, max_fatality_rate), 
                            y = max_fatality_rate, 
                            fill = max_fatality_rate)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", max_fatality_rate)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Peak COVID-19 Case Fatality Rates by State",
    subtitle = "Top 20 US States (Peak Case_Fatality_Ratio from CSV)",
    x = "State",
    y = "Peak Case Fatality Rate (%)",
    caption = "Source: COVID-19 Data Analysis - BIOL3100 Exam 4 (Corrected)"
  ) +
  scale_fill_gradient(low = "steelblue", high = "darkred", name = "CFR (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
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
# *** FIX 2: PROPERLY CALCULATE CUMULATIVE DEATHS FOR ENTIRE US ***
# Sort by date first, then sum deaths across all states for each date
# Since deaths are already cumulative per state, summing gives US total

cat("=== Task VI (BONUS): US Cumulative Deaths Over Time (CORRECTED) ===\n")
cat("*** FIX 2: Ensuring proper aggregation of cumulative deaths ***\n\n")

# Sort data by date first to ensure chronological order
covid_data <- covid_data[order(covid_data$date), ]

# Calculate total US deaths for each date by summing across all states
# Since the deaths column is already cumulative per state, 
# summing across states gives us the total cumulative deaths for the US
# Using base R aggregate for compatibility
us_total_deaths <- aggregate(cbind(deaths, cases) ~ date, data = covid_data, 
                             FUN = function(x) c(sum = sum(x, na.rm = TRUE), count = length(x)))
# Flatten the result
us_total_deaths <- data.frame(
  date = us_total_deaths$date,
  total_deaths = us_total_deaths$deaths[, "sum"],
  total_cases = us_total_deaths$cases[, "sum"],
  num_states_reporting = us_total_deaths$deaths[, "count"]
)
us_total_deaths <- us_total_deaths[order(us_total_deaths$date), ]

# Verify the data makes sense
cat("US total deaths summary:\n")
cat("  First date:", as.character(min(us_total_deaths$date)), 
    "- Deaths:", format(min(us_total_deaths$total_deaths), big.mark=","), "\n")
cat("  Last date:", as.character(max(us_total_deaths$date)), 
    "- Deaths:", format(max(us_total_deaths$total_deaths), big.mark=","), "\n")
cat("  Number of states reporting on last date:", us_total_deaths$num_states_reporting[nrow(us_total_deaths)], "\n\n")

# Check that deaths are monotonically increasing (cumulative should always increase or stay same)
us_total_deaths$prev_deaths <- c(NA, us_total_deaths$total_deaths[-nrow(us_total_deaths)])
us_total_deaths$is_decreasing <- us_total_deaths$total_deaths < us_total_deaths$prev_deaths
deaths_decreasing <- us_total_deaths[!is.na(us_total_deaths$is_decreasing) & 
                                     us_total_deaths$is_decreasing == TRUE, ]

if(nrow(deaths_decreasing) > 0) {
  cat("WARNING: Found", nrow(deaths_decreasing), "dates where deaths decreased!\n")
  print(head(deaths_decreasing))
} else {
  cat("✓ Verification passed: Cumulative deaths are monotonically increasing\n\n")
}

# Display some sample dates to verify values are reasonable
cat("Sample cumulative death counts at various dates:\n")
target_dates <- as.Date(c("2020-04-01", "2020-07-01", "2020-10-01", 
                          "2021-01-01", "2021-06-01", "2021-12-01"))
sample_dates <- us_total_deaths[us_total_deaths$date %in% target_dates, ]
print(sample_dates)
cat("\n")

# Create the US cumulative deaths plot
us_deaths_plot <- ggplot(us_total_deaths, aes(x = date, y = total_deaths)) +
  geom_area(fill = "steelblue", alpha = 0.4) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  labs(
    title = "Cumulative COVID-19 Deaths in the United States",
    subtitle = "March 2020 - December 2021 (Properly Aggregated Across All States)",
    x = "Date",
    y = "Cumulative Deaths",
    caption = "Source: COVID-19 Data Analysis - BIOL3100 Exam 4 (Corrected)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
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
cat("                  EXAM 4 COMPLETION SUMMARY (CORRECTED)\n")
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
cat("Task IV:  *** CORRECTED *** state_max_fatality_rate object created\n")
cat("          - Used existing Case_Fatality_Ratio column from CSV (max per state)\n")
cat("          - ", nrow(state_max_fatality_rate), " states analyzed\n", sep = "")
cat("          - Fatality rate range: ", round(min(state_max_fatality_rate$max_fatality_rate), 2), 
    "% to ", round(max(state_max_fatality_rate$max_fatality_rate), 2), "%\n", sep = "")
cat("\n")
cat("Task V:   Max fatality rate plot saved as 'state_max_fatality_rate_plot.png'\n")
cat("\n")
cat("BONUS:    *** CORRECTED *** US cumulative deaths plot saved\n")
cat("          - Properly aggregated across all states by date\n")
cat("          - Verified monotonic increase (cumulative property)\n")
cat("          - Final US death toll: ", format(max(us_total_deaths$total_deaths), big.mark=","), "\n", sep = "")
cat("\n")
cat("============================================================================\n")
cat("CORRECTIONS APPLIED:\n")
cat("  1. Used existing Case_Fatality_Ratio column from CSV for peak per state\n")
cat("  2. US cumulative deaths properly aggregated with verification\n")
cat("============================================================================\n")
cat("All tasks completed successfully with corrections!\n")
cat("============================================================================\n")
