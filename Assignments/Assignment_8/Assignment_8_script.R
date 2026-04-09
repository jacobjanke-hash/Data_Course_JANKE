# Assignment 8 - Modeling and Predictions
# Author: Jacob Janke
# This script loads mushroom growth data, explores relationships,
# builds multiple models, compares them, and makes predictions.

# ---- Load Libraries ----
library(modelr)
library(easystats)
library(broom)
library(tidyverse)

# ---- 1. Load the Data ----
# Using relative path from Assignment_8 directory
mushroom <- read.csv("../../Data/mushroom_growth.csv")
glimpse(mushroom)
summary(mushroom)

# Convert categorical variables to factors
mushroom$Species <- as.factor(mushroom$Species)
mushroom$Humidity <- as.factor(mushroom$Humidity)

# ---- 2. Exploratory Plots ----

# Plot 1: GrowthRate vs Light, colored by Species
p1 <- ggplot(mushroom, aes(x = Light, y = GrowthRate, color = Species)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Growth Rate vs Light by Species",
       x = "Light (arbitrary units)", y = "Growth Rate") +
  theme_minimal()
ggsave("plot1_GrowthRate_vs_Light.png", p1, width = 8, height = 5)

# Plot 2: GrowthRate vs Nitrogen, colored by Humidity
p2 <- ggplot(mushroom, aes(x = Nitrogen, y = GrowthRate, color = Humidity)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Growth Rate vs Nitrogen by Humidity",
       x = "Nitrogen Concentration", y = "Growth Rate") +
  theme_minimal()
ggsave("plot2_GrowthRate_vs_Nitrogen.png", p2, width = 8, height = 5)

# Plot 3: GrowthRate vs Temperature, faceted by Species and Humidity
p3 <- ggplot(mushroom, aes(x = Temperature, y = GrowthRate, color = Species)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Humidity) +
  labs(title = "Growth Rate vs Temperature by Species and Humidity",
       x = "Temperature (C)", y = "Growth Rate") +
  theme_minimal()
ggsave("plot3_GrowthRate_vs_Temp_facet.png", p3, width = 10, height = 5)

# Plot 4: Boxplot of GrowthRate by Species and Humidity
p4 <- ggplot(mushroom, aes(x = Species, y = GrowthRate, fill = Humidity)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Growth Rate by Species and Humidity",
       x = "Species", y = "Growth Rate") +
  theme_minimal()
ggsave("plot4_Boxplot_Species_Humidity.png", p4, width = 8, height = 5)

# Plot 5: GrowthRate vs Nitrogen, colored by Species, faceted by Light
p5 <- ggplot(mushroom, aes(x = Nitrogen, y = GrowthRate, color = Species)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Light) +
  labs(title = "Growth Rate vs Nitrogen by Light Level",
       x = "Nitrogen", y = "Growth Rate") +
  theme_minimal()
ggsave("plot5_GrowthRate_vs_Nitrogen_by_Light.png", p5, width = 10, height = 5)

# ---- 3. Define at Least 4 Models ----

# Model 1: Simple - just Nitrogen
mod1 <- lm(GrowthRate ~ Nitrogen, data = mushroom)

# Model 2: Nitrogen + Humidity + Species (main effects)
mod2 <- lm(GrowthRate ~ Nitrogen + Humidity + Species, data = mushroom)

# Model 3: All predictors (main effects)
mod3 <- lm(GrowthRate ~ Light + Nitrogen + Humidity + Temperature + Species, data = mushroom)

# Model 4: All predictors with key interactions
mod4 <- lm(GrowthRate ~ Light * Humidity + Nitrogen * Humidity + Temperature + Species, data = mushroom)

# Model 5: Full interaction model
mod5 <- lm(GrowthRate ~ Light * Species * Humidity + Nitrogen * Humidity + Temperature, data = mushroom)

# Print model summaries
cat("\n===== Model 1: GrowthRate ~ Nitrogen =====\n")
summary(mod1)

cat("\n===== Model 2: GrowthRate ~ Nitrogen + Humidity + Species =====\n")
summary(mod2)

cat("\n===== Model 3: GrowthRate ~ All Main Effects =====\n")
summary(mod3)

cat("\n===== Model 4: GrowthRate ~ Main + Key Interactions =====\n")
summary(mod4)

cat("\n===== Model 5: GrowthRate ~ Full Interactions =====\n")
summary(mod5)

# ---- 4. Calculate Mean Squared Error of Each Model ----

mse1 <- mean(mod1$residuals^2)
mse2 <- mean(mod2$residuals^2)
mse3 <- mean(mod3$residuals^2)
mse4 <- mean(mod4$residuals^2)
mse5 <- mean(mod5$residuals^2)

cat("\n===== Mean Squared Errors =====\n")
cat("Model 1 (Nitrogen only)         MSE:", mse1, "\n")
cat("Model 2 (N + Hum + Sp)          MSE:", mse2, "\n")
cat("Model 3 (All main effects)      MSE:", mse3, "\n")
cat("Model 4 (Main + key interact.)  MSE:", mse4, "\n")
cat("Model 5 (Full interactions)     MSE:", mse5, "\n")

# ---- Model Performance Comparison ----
mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)
perf <- map(mods, performance) %>% reduce(full_join)
print(perf)

# Residual boxplot comparison
resid_plot <- mushroom %>%
  gather_residuals(mod1, mod2, mod3, mod4, mod5) %>%
  ggplot(aes(x = model, y = resid, fill = model)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.3) +
  labs(title = "Residuals Comparison Across Models",
       x = "Model", y = "Residual") +
  theme_minimal()
ggsave("plot6_residuals_comparison.png", resid_plot, width = 10, height = 6)

# ---- 5. Select the Best Model ----
cat("\n===== Best Model Selection =====\n")
best_idx <- which.min(c(mse1, mse2, mse3, mse4, mse5))
cat("Best model by MSE: Model", best_idx, "\n")
cat("Using mod5 as best model (lowest MSE and highest R-squared)\n\n")

best_mod <- mod5
summary(best_mod)
cat("\n")
report(best_mod)

# ---- 6. Add Predictions Based on New Hypothetical Values ----

# Create hypothetical data covering a range of conditions
hyp_data <- expand.grid(
  Light = c(0, 10, 20),
  Nitrogen = c(0, 10, 20, 30, 40, 50, 60),
  Humidity = c("Low", "High"),
  Temperature = c(20, 25),
  Species = c("P.ostreotus", "P.cornucopiae")
)

# Generate predictions
hyp_data$pred <- predict(best_mod, newdata = hyp_data)

# Mark real vs hypothetical
real_data <- mushroom %>% add_predictions(best_mod)
real_data$DataType <- "Real"
hyp_data$DataType <- "Hypothetical"
hyp_data$GrowthRate <- NA

# Combine
combined <- bind_rows(
  real_data %>% dplyr::select(Light, Nitrogen, Humidity, Temperature, Species, GrowthRate, pred, DataType),
  hyp_data %>% dplyr::select(Light, Nitrogen, Humidity, Temperature, Species, GrowthRate, pred, DataType)
)

# ---- 7. Plot Predictions Alongside Real Data ----

# Plot: Predictions vs Nitrogen, with real data points
pred_plot1 <- ggplot(combined, aes(x = Nitrogen, y = pred, color = DataType)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_point(aes(y = GrowthRate), color = "black", alpha = 0.4, size = 1) +
  facet_grid(Species ~ Humidity) +
  labs(title = "Model Predictions vs Real Data (by Nitrogen)",
       subtitle = "Black points = real data, colored = predictions",
       x = "Nitrogen", y = "Predicted Growth Rate") +
  theme_minimal()
ggsave("plot7_predictions_vs_real.png", pred_plot1, width = 10, height = 7)

# Plot: Show hypothetical extrapolations clearly
pred_plot2 <- ggplot(combined %>% filter(Light == 10, Temperature == 20),
                     aes(x = Nitrogen, y = pred, color = DataType, shape = Species)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_point(aes(y = GrowthRate), color = "black", alpha = 0.3, size = 2) +
  facet_wrap(~Humidity) +
  geom_vline(xintercept = 45, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = 52, y = 50, label = "Extrapolation\nbeyond data", color = "red", size = 3) +
  labs(title = "Predictions: Real vs Hypothetical (Light=10, Temp=20)",
       x = "Nitrogen", y = "Predicted Growth Rate") +
  theme_minimal()
ggsave("plot8_predictions_extrapolation.png", pred_plot2, width = 10, height = 6)

cat("\n===== Hypothetical Predictions (sample) =====\n")
print(head(hyp_data %>% dplyr::select(Species, Light, Nitrogen, Humidity, Temperature, pred), 20))

# Check for scientifically meaningless predictions
cat("\n===== Checking for Negative Predictions =====\n")
neg_preds <- hyp_data %>% filter(pred < 0)
if(nrow(neg_preds) > 0) {
  cat("WARNING: Some predictions are negative (scientifically meaningless):\n")
  print(neg_preds %>% dplyr::select(Species, Light, Nitrogen, Humidity, Temperature, pred))
} else {
  cat("No negative predictions found in hypothetical data.\n")
}

cat("\nScript complete! All plots saved.\n")
