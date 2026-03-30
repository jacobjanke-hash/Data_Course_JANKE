# Assignment 6 - BioLog Plate Data Analysis
# Tidy data, create plots, and generate animated visualization

# Load libraries
library(tidyverse)
library(gganimate)
library(gifski)

# Load data using relative path from Assignment_6 directory
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

# ---- Step 1: Clean data into tidy (long) form ----
# Pivot the Hr_24, Hr_48, Hr_144 columns into long format
dat_tidy <- dat %>%
  pivot_longer(cols = starts_with("Hr_"),
               names_to = "Time",
               values_to = "Absorbance") %>%
  mutate(Time = as.numeric(str_remove(Time, "Hr_")))

# ---- Step 2: Create new column specifying soil or water ----
dat_tidy <- dat_tidy %>%
  mutate(Type = case_when(
    `Sample ID` %in% c("Soil_1", "Soil_2") ~ "Soil",
    `Sample ID` %in% c("Clear_Creek", "Waste_Water") ~ "Water"
  ))

# ---- Step 3: Generate faceted plot for dilution == 0.1 ----
p1 <- dat_tidy %>%
  filter(Dilution == 0.1) %>%
  ggplot(aes(x = Time, y = Absorbance, color = Type)) +
  geom_line(aes(group = interaction(`Sample ID`, Rep))) +
  facet_wrap(~Substrate) +
  labs(title = "Just dilution 0.1",
       x = "Time",
       y = "Absorbance") +
  theme_minimal()

ggsave("facet_plot.png", plot = p1, width = 14, height = 10, dpi = 150)

# ---- Step 4: Generate animated plot for Itaconic Acid ----
# Calculate mean absorbance across all 3 replicates for each group
itaconic <- dat_tidy %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(`Sample ID`, Dilution, Time) %>%
  summarize(Mean_absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop") %>%
  mutate(Dilution = as.factor(Dilution))

p2 <- itaconic %>%
  ggplot(aes(x = Time, y = Mean_absorbance, color = `Sample ID`)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Dilution) +
  labs(title = "Itaconic Acid",
       x = "Time",
       y = "Mean_absorbance") +
  theme_minimal() +
  transition_reveal(Time)

anim <- animate(p2, renderer = gifski_renderer(), width = 600, height = 400, nframes = 50)
anim_save("animated_plot.gif", animation = anim)
