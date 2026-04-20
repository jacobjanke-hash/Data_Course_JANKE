# BIOL3100 Exam II - Jacob JANKE
# Analysis of UNICEF Under-5 Mortality Rate (U5MR) Data

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(modelr)

# ============================================================================
# 1. READ IN THE DATA
# ============================================================================

# Read unicef data using relative path
df <- read.csv("../Data/unicef-u5mr.csv", stringsAsFactors = FALSE)

# Check the structure
cat("Original data dimensions:", dim(df), "\n")
cat("Column names (first 10):", colnames(df)[1:10], "\n\n")

# ============================================================================
# 2. CONVERT TO TIDY FORMAT
# ============================================================================

# The data has columns for each year (1950.5, 1951.5, ..., 2024.5)
# We need to convert this to long format with columns: Country, Year, U5MR, Continent, etc.

# First, filter to only get the median estimates (not upper/lower bounds)
# The "Uncertainty.Bounds." column contains "Median", "Lower", or "Upper"
df_median <- df %>%
  filter(Uncertainty.Bounds. == "Median")

cat("After filtering for median estimates:", dim(df_median), "\n")

# Convert to tidy format (long format)
# Gather all year columns into two columns: Year and U5MR
df_tidy <- df_median %>%
  pivot_longer(
    cols = matches("^X[0-9]+\\.[0-9]+$"),  # Select columns that are years (e.g., "X1950.5")
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  # Convert Year to numeric (remove the X prefix and .5 suffix)
  mutate(Year = as.numeric(gsub("X", "", Year))) %>%
  # Select and rename key columns
  select(
    Country = Country.Name,
    ISO = ISO.Code,
    Continent = UNICEF.Region,
    Region = SDG.Region,
    Year,
    U5MR
  ) %>%
  # Remove rows with missing U5MR values
  filter(!is.na(U5MR))

cat("Tidy data dimensions:", dim(df_tidy), "\n")
cat("Sample of tidy data:\n")
print(head(df_tidy, 10))
cat("\n")

# ============================================================================
# 3. PLOT 1: Each country's U5MR over time
# ============================================================================

cat("Creating Plot 1: Country U5MR over time...\n")

plot1 <- ggplot(df_tidy, aes(x = Year, y = U5MR, group = Country, color = Continent)) +
  geom_line(alpha = 0.3, linewidth = 0.5) +
  labs(
    title = "Under-5 Mortality Rate (U5MR) by Country Over Time",
    subtitle = "Deaths per 1,000 live births",
    x = "Year",
    y = "U5MR (deaths per 1,000 live births)",
    color = "Continent"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

# Save plot
ggsave("JANKE_Plot_1.png", plot1, width = 12, height = 8, dpi = 300)
cat("✓ Plot 1 saved as JANKE_Plot_1.png\n\n")

# ============================================================================
# 4. PLOT 2: Mean U5MR by continent and year
# ============================================================================

cat("Creating Plot 2: Mean U5MR by continent and year...\n")

# Calculate mean U5MR for each continent at each year
df_continent_mean <- df_tidy %>%
  group_by(Continent, Year) %>%
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

plot2 <- ggplot(df_continent_mean, aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 0.5, alpha = 0.6) +
  labs(
    title = "Mean Under-5 Mortality Rate by Continent Over Time",
    subtitle = "Average deaths per 1,000 live births across countries in each continent",
    x = "Year",
    y = "Mean U5MR (deaths per 1,000 live births)",
    color = "Continent"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

# Save plot
ggsave("JANKE_Plot_2.png", plot2, width = 12, height = 8, dpi = 300)
cat("✓ Plot 2 saved as JANKE_Plot_2.png\n\n")

# ============================================================================
# 5. BUILD AND COMPARE THREE MODELS
# ============================================================================

cat("Building and comparing three models of U5MR...\n\n")

# Prepare data for modeling - filter for years with complete data
# Let's use data from 1990 onwards for better model performance
df_model <- df_tidy %>%
  filter(Year >= 1990) %>%
  mutate(
    # Create additional features
    Years_Since_1990 = Year - 1990,
    Decade = floor(Year / 10) * 10,
    Continent_Factor = as.factor(Continent)
  )

cat("Model data dimensions:", dim(df_model), "\n\n")

# Model 1: Simple linear model with Year only
cat("Model 1: U5MR ~ Year\n")
model1 <- lm(U5MR ~ Year, data = df_model)
cat("Model 1 Summary:\n")
print(summary(model1))
cat("\n")

# Model 2: Linear model with Year and Continent
cat("Model 2: U5MR ~ Year + Continent\n")
model2 <- lm(U5MR ~ Year + Continent_Factor, data = df_model)
cat("Model 2 Summary:\n")
print(summary(model2))
cat("\n")

# Model 3: Linear model with Year, Continent, and interaction
cat("Model 3: U5MR ~ Year * Continent (with interaction)\n")
model3 <- lm(U5MR ~ Year * Continent_Factor, data = df_model)
cat("Model 3 Summary:\n")
print(summary(model3))
cat("\n")

# Compare models using RMSE (Root Mean Square Error)
df_model <- df_model %>%
  add_predictions(model1, var = "pred1") %>%
  add_predictions(model2, var = "pred2") %>%
  add_predictions(model3, var = "pred3")

# Calculate RMSE for each model
rmse1 <- sqrt(mean((df_model$U5MR - df_model$pred1)^2))
rmse2 <- sqrt(mean((df_model$U5MR - df_model$pred2)^2))
rmse3 <- sqrt(mean((df_model$U5MR - df_model$pred3)^2))

# Calculate R-squared for each model
r2_1 <- summary(model1)$r.squared
r2_2 <- summary(model2)$r.squared
r2_3 <- summary(model3)$r.squared

# Create comparison table
model_comparison <- data.frame(
  Model = c("Model 1: U5MR ~ Year",
            "Model 2: U5MR ~ Year + Continent",
            "Model 3: U5MR ~ Year * Continent"),
  RMSE = c(rmse1, rmse2, rmse3),
  R_squared = c(r2_1, r2_2, r2_3),
  AIC = c(AIC(model1), AIC(model2), AIC(model3))
)

cat("\n=== MODEL COMPARISON ===\n")
print(model_comparison)
cat("\nBest model (lowest RMSE):", model_comparison$Model[which.min(model_comparison$RMSE)], "\n")
cat("Best model (highest R²):", model_comparison$Model[which.max(model_comparison$R_squared)], "\n")
cat("Best model (lowest AIC):", model_comparison$Model[which.min(model_comparison$AIC)], "\n\n")

# ============================================================================
# 6. PLOT MODEL PREDICTIONS
# ============================================================================

cat("Creating plot of model predictions...\n")

# Sample some countries for clearer visualization
sample_countries <- c("Ecuador", "United States", "Nigeria", "India", "China", "Brazil")
df_plot_models <- df_model %>%
  filter(Country %in% sample_countries)

plot_predictions <- ggplot(df_plot_models, aes(x = Year, y = U5MR)) +
  geom_point(aes(color = "Actual"), alpha = 0.6, size = 2) +
  geom_line(aes(y = pred1, color = "Model 1: Year only"), linewidth = 1) +
  geom_line(aes(y = pred2, color = "Model 2: Year + Continent"), linewidth = 1) +
  geom_line(aes(y = pred3, color = "Model 3: Year * Continent"), linewidth = 1) +
  facet_wrap(~Country, scales = "free_y", ncol = 3) +
  labs(
    title = "Model Predictions vs. Actual U5MR",
    subtitle = "Comparison of three models for selected countries",
    x = "Year",
    y = "U5MR (deaths per 1,000 live births)",
    color = "Legend"
  ) +
  scale_color_manual(
    values = c(
      "Actual" = "black",
      "Model 1: Year only" = "blue",
      "Model 2: Year + Continent" = "red",
      "Model 3: Year * Continent" = "green"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

# Save plot
ggsave("JANKE_Model_Predictions.png", plot_predictions, width = 14, height = 10, dpi = 300)
cat("✓ Model predictions plot saved as JANKE_Model_Predictions.png\n\n")

# ============================================================================
# 7. BONUS: Predict Ecuador 2020 U5MR
# ============================================================================

cat("=== BONUS: Predicting Ecuador 2020 U5MR ===\n\n")

# The actual value is 13 deaths per 1000 live births
actual_ecuador_2020 <- 13

# Get Ecuador's continent
ecuador_continent <- df_tidy %>%
  filter(Country == "Ecuador") %>%
  pull(Continent) %>%
  unique() %>%
  first()

# Create prediction data frame
ecuador_2020 <- data.frame(
  Year = 2020,
  Country = "Ecuador",
  Continent_Factor = factor(ecuador_continent, levels = levels(df_model$Continent_Factor)),
  Years_Since_1990 = 2020 - 1990
)

# Make predictions using all three models
pred1_ecuador <- predict(model1, newdata = ecuador_2020)
pred2_ecuador <- predict(model2, newdata = ecuador_2020)
pred3_ecuador <- predict(model3, newdata = ecuador_2020)

# Create comparison table
ecuador_predictions <- data.frame(
  Model = c("Model 1: U5MR ~ Year",
            "Model 2: U5MR ~ Year + Continent",
            "Model 3: U5MR ~ Year * Continent",
            "ACTUAL VALUE"),
  Predicted_U5MR = c(pred1_ecuador, pred2_ecuador, pred3_ecuador, actual_ecuador_2020),
  Error = c(
    pred1_ecuador - actual_ecuador_2020,
    pred2_ecuador - actual_ecuador_2020,
    pred3_ecuador - actual_ecuador_2020,
    0
  ),
  Absolute_Error = c(
    abs(pred1_ecuador - actual_ecuador_2020),
    abs(pred2_ecuador - actual_ecuador_2020),
    abs(pred3_ecuador - actual_ecuador_2020),
    0
  )
)

cat("Ecuador 2020 U5MR Predictions:\n")
print(ecuador_predictions)
cat("\n")

cat("Most accurate model:", 
    ecuador_predictions$Model[which.min(ecuador_predictions$Absolute_Error[-4])], "\n")
cat("with absolute error of", 
    min(ecuador_predictions$Absolute_Error[-4]), "deaths per 1,000 live births\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== SUMMARY STATISTICS ===\n\n")

cat("Global U5MR trends:\n")
global_summary <- df_tidy %>%
  group_by(Year) %>%
  summarise(
    Mean_U5MR = mean(U5MR, na.rm = TRUE),
    Median_U5MR = median(U5MR, na.rm = TRUE),
    Min_U5MR = min(U5MR, na.rm = TRUE),
    Max_U5MR = max(U5MR, na.rm = TRUE)
  )

cat("Year 1990:\n")
print(filter(global_summary, Year == 1990))
cat("\nYear 2024:\n")
print(filter(global_summary, Year == 2024))
cat("\n")

cat("Percentage reduction in mean U5MR from 1990 to 2024:\n")
u5mr_1990 <- global_summary %>% filter(Year == 1990) %>% pull(Mean_U5MR)
u5mr_2024 <- global_summary %>% filter(Year == 2024) %>% pull(Mean_U5MR)
percent_reduction <- ((u5mr_1990 - u5mr_2024) / u5mr_1990) * 100
cat(sprintf("%.1f%%\n\n", percent_reduction))

cat("=== EXAM COMPLETE ===\n")
cat("All plots and analyses have been generated successfully!\n")
cat("Files created:\n")
cat("  - JANKE_Plot_1.png\n")
cat("  - JANKE_Plot_2.png\n")
cat("  - JANKE_Model_Predictions.png\n")
cat("  - This script: JANKE_Exam_2_Script.R\n")
