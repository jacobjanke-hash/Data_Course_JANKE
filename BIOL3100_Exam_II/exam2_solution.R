# ============================================================================
# BIOL3100 Exam 2 - Solution
# Student: JANKE
# Date: April 2026
# ============================================================================
# Analysis of UNICEF Under-5 Mortality Rate (U5MR) data
# U5MR = deaths before age 5 per 1000 live births
# ============================================================================

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# ============================================================================
# TASK 1: Read in the unicef data (10 pts)
# ============================================================================

dat <- read.csv("unicef-u5mr.csv")

cat("=== Task 1: Data Import ===\n")
cat("Dimensions:", dim(dat)[1], "rows,", dim(dat)[2], "columns\n")
str(dat)

# ============================================================================
# TASK 2: Get it into tidy format (10 pts)
# ============================================================================

# Pivot the wide U5MR year columns into long (tidy) format
tidy <- dat %>%
  pivot_longer(
    cols = starts_with("U5MR"),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(Year = as.numeric(gsub("U5MR.", "", Year))) %>%
  filter(!is.na(U5MR))

cat("\n=== Task 2: Tidy Data ===\n")
cat("Tidy dimensions:", dim(tidy)[1], "rows,", dim(tidy)[2], "columns\n")
head(tidy)

# ============================================================================
# TASK 3 & 4: Plot each country's U5MR over time, faceted by continent (20 pts)
#             Save as JANKE_Plot_1.png (5 pts)
# ============================================================================

p1 <- ggplot(tidy, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~Continent) +
  labs(x = "Year", y = "U5MR", title = "U5MR by Country Over Time") +
  theme_bw()

ggsave("JANKE_Plot_1.png", plot = p1, width = 10, height = 8, dpi = 150)
cat("\n=== Plot 1 saved as JANKE_Plot_1.png ===\n")

# ============================================================================
# TASK 5 & 6: Mean U5MR by continent over time (20 pts)
#             Save as JANKE_Plot_2.png (5 pts)
# ============================================================================

mean_by_continent <- tidy %>%
  group_by(Continent, Year) %>%
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(mean_by_continent, aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line() +
  labs(x = "Year", y = "Mean_U5MR",
       title = "Mean U5MR by Continent Over Time") +
  theme_bw()

ggsave("JANKE_Plot_2.png", plot = p2, width = 10, height = 6, dpi = 150)
cat("\n=== Plot 2 saved as JANKE_Plot_2.png ===\n")

# ============================================================================
# TASK 7: Create three models of U5MR (20 pts)
# ============================================================================

# mod1: U5MR ~ Year only
mod1 <- lm(U5MR ~ Year, data = tidy)

# mod2: U5MR ~ Year + Continent
mod2 <- lm(U5MR ~ Year + Continent, data = tidy)

# mod3: U5MR ~ Year * Continent (Year, Continent, and their interaction)
mod3 <- lm(U5MR ~ Year * Continent, data = tidy)

# ============================================================================
# TASK 8: Compare the three models (performance)
# ============================================================================

cat("\n=== Task 8: Model Comparison ===\n")
cat("\n--- ANOVA comparison ---\n")
print(anova(mod1, mod2, mod3))

cat("\n--- R-squared values ---\n")
cat("mod1 R²:", summary(mod1)$r.squared, "\n")
cat("mod2 R²:", summary(mod2)$r.squared, "\n")
cat("mod3 R²:", summary(mod3)$r.squared, "\n")

cat("\n--- Adjusted R-squared values ---\n")
cat("mod1 Adj R²:", summary(mod1)$adj.r.squared, "\n")
cat("mod2 Adj R²:", summary(mod2)$adj.r.squared, "\n")
cat("mod3 Adj R²:", summary(mod3)$adj.r.squared, "\n")

# mod3 is the best model because it has the highest R-squared and adjusted R-squared,
# and the ANOVA shows significant improvement with each additional term.
# The interaction between Year and Continent makes sense because mortality
# rates declined at different rates across continents.

# ============================================================================
# TASK 9: Plot the 3 models' predictions (10 pts)
# ============================================================================

tidy$pred_mod1 <- predict(mod1, newdata = tidy)
tidy$pred_mod2 <- predict(mod2, newdata = tidy)
tidy$pred_mod3 <- predict(mod3, newdata = tidy)

# Reshape predictions to long format for faceted plotting
pred_long <- tidy %>%
  select(Continent, Year, CountryName, pred_mod1, pred_mod2, pred_mod3) %>%
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "Model",
    values_to = "Predicted_U5MR"
  ) %>%
  mutate(Model = gsub("pred_", "", Model))

p3 <- ggplot(pred_long, aes(x = Year, y = Predicted_U5MR, color = Continent)) +
  geom_line() +
  facet_wrap(~Model) +
  labs(x = "Year", y = "Predicted U5MR",
       title = "Model predictions") +
  theme_bw()

ggsave("JANKE_Plot_3.png", plot = p3, width = 12, height = 6, dpi = 150)
cat("\n=== Plot 3 saved as JANKE_Plot_3.png ===\n")

# ============================================================================
# TASK 10: BONUS - Predict Ecuador 2020 U5MR
# ============================================================================

cat("\n=== BONUS: Ecuador 2020 Prediction ===\n")

# Using mod3 (preferred model) to predict Ecuador 2020
ecuador_data <- data.frame(
  CountryName = "Ecuador",
  Continent = "Americas",
  Year = 2020
)

# Prediction from mod3
pred_mod3_ecuador <- predict(mod3, newdata = ecuador_data)
cat("mod3 prediction for Ecuador 2020:", pred_mod3_ecuador, "\n")

actual_value <- 13
difference <- pred_mod3_ecuador - actual_value
cat("Actual value:", actual_value, "\n")
cat("Difference (pred - actual):", difference, "\n")

# Show as a nice data frame like the example
result_mod3 <- data.frame(
  Continent = "Americas",
  Year = 2020,
  CountryName = "Ecuador",
  pred = pred_mod3_ecuador
)
cat("\nmod3 Ecuador prediction:\n")
print(result_mod3)

# ---- Improved model (mod4) using log-transformed U5MR ----
# By transforming U5MR with log, we can better capture the exponential decay
# of mortality rates over time
tidy$log_U5MR <- log(tidy$U5MR)
mod4 <- lm(log_U5MR ~ Year * Continent, data = tidy)

# Predict on log scale, then back-transform
pred_mod4_log <- predict(mod4, newdata = ecuador_data)
pred_mod4 <- exp(pred_mod4_log)

cat("\nmod4 (log-transformed) prediction for Ecuador 2020:", pred_mod4, "\n")
cat("Actual value:", actual_value, "\n")
cat("Difference (pred - actual):", pred_mod4 - actual_value, "\n")

bonus_result <- data.frame(
  Model = "mod4",
  Prediction = pred_mod4,
  Reality = actual_value
)
cat("\nBonus result:\n")
print(bonus_result)

cat("\n=== Exam 2 Complete ===\n")
