# ============================================================================
# Assignment 7 - Utah Religions by County
# Author: Jacob Janke
# Course: BIOL 3100 / Data Course
#
# This script takes the "messy code" from Assignment_7_messy_code.R and
# rewrites it using tidy dplyr/tidyverse verbs. It also performs exploratory
# data analysis including correlation analysis between religious groups,
# population, and non-religious proportions.
# ============================================================================

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================================
#                              PART 1
#           Tidying up the messy code using dplyr verbs
# ============================================================================

# --- Load the data (wide format) ---
# Using a relative path from the Assignment_7 directory
utah <- read.csv("./Utah_Religions_by_County.csv")

# Quick look at the structure of the data
glimpse(utah)
head(utah)

# --- Original messy code (commented out) ---
# buddhist = utah[utah$Buddhism.Mahayana > 0,]
# buddhist = buddhist[order(buddhist$Pop_2010, decreasing = TRUE),]

# --- Tidy version: filter to counties with Buddhists, arrange by population ---
# filter() replaces bracket-based row subsetting
# arrange() with desc() replaces order() for sorting
buddhist <- utah %>%
  filter(Buddhism.Mahayana > 0) %>%
  arrange(desc(Pop_2010))

# Write the filtered/sorted data to a CSV file
write.csv(buddhist, file = "./buddhist_counties.csv", row.names = FALSE, quote = FALSE)


# --- Get group summaries of religiosity based on population ---

# Use k-means to divide counties into 6 population groups
# (keeping these two lines the same as instructed)
set.seed(42)  # for reproducibility
groups <- kmeans(utah$Pop_2010, 6)
utah$Pop.Group <- groups$cluster

# --- Original messy code (commented out) ---
# group1 = mean(utah[utah$Pop.Group == 1,]$Religious)
# group2 = mean(utah[utah$Pop.Group == 2,]$Religious)
# ... (repeated 6 times for each group)
# group1.pop = mean(utah[utah$Pop.Group == 1,]$Pop_2010)
# ... (repeated 6 times)
# religiosity = data.frame(Pop.Group = c("group1",...),
#                          Mean.Religiosity = c(group1,...),
#                          Mean.Pop = c(group1.pop,...))
# religiosity = religiosity[order(religiosity$Mean.Pop, decreasing = TRUE),]

# --- Tidy version: use group_by() and summarize() instead of manual subsetting ---
# This replaces ~18 lines of repetitive code with 4 clean lines
religiosity <- utah %>%
  group_by(Pop.Group) %>%
  summarize(
    Mean.Religiosity = mean(Religious, na.rm = TRUE),
    Mean.Pop = mean(Pop_2010, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean.Pop))

# Take a look at the resulting summary table
print(religiosity)

# --- Original messy code (commented out) ---
# plot(x=religiosity$Mean.Pop, y=religiosity$Mean.Religiosity)

# --- Tidy version: use ggplot instead of base R plot ---
# ggplot gives us much more control over aesthetics and labeling
p1 <- ggplot(religiosity, aes(x = Mean.Pop, y = Mean.Religiosity)) +
  geom_point(size = 4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Mean Religiosity vs. Mean Population by Group",
    x = "Mean Population (2010)",
    y = "Mean Religiosity (Proportion)"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

print(p1)
ggsave("JANKE_Religiosity_vs_Population.png", plot = p1, width = 8, height = 6, dpi = 150)


# ============================================================================
#                              PART 2
#           Correlation Analysis Between Religious Groups
# ============================================================================

# Identify the religion columns (everything except County, Pop_2010, Religious, Non.Religious)
religions <- names(utah)[-c(1:4)]
# Remove Pop.Group since we added it
religions <- religions[religions != "Pop.Group"]

# --- Pivot longer and create faceted scatterplots ---
# This shows how the proportion of each religious group relates to overall religiosity
utah %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion", cols = all_of(religions)) %>%
  ggplot(aes(x = Proportion, y = Religious)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  lims(y = c(0, 1)) +
  facet_wrap(~Religion, scales = "free") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "Gray")
  ) +
  labs(
    title = "Proportion of Each Religion vs. Overall Religiosity by County",
    x = "Proportion of Religious Group",
    y = "Proportion Religious"
  )

ggsave("JANKE_Religion_vs_Religiosity_Facets.png", width = 12, height = 8, dpi = 150)


# ============================================================================
#       ADDITIONAL ANALYSIS: Correlation with Population
# ============================================================================

# Analyze correlation between county population and proportion of specific religions
# Pivot the data to long format for easier analysis
utah_long <- utah %>%
  select(-Pop.Group) %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion", cols = all_of(religions))

# Calculate correlation coefficients between Pop_2010 and each religion's proportion
pop_correlations <- utah_long %>%
  group_by(Religion) %>%
  summarize(
    correlation = cor(Pop_2010, Proportion, use = "complete.obs"),
    p_value = cor.test(Pop_2010, Proportion)$p.value
  ) %>%
  arrange(desc(abs(correlation)))

cat("\n=== Correlation: Population vs. Religious Group Proportion ===\n")
print(pop_correlations, n = Inf)

# Plot: Population vs. proportion of each religion
p2 <- utah_long %>%
  ggplot(aes(x = Pop_2010, y = Proportion)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~Religion, scales = "free") +
  scale_x_log10() +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "gray90")
  ) +
  labs(
    title = "County Population vs. Proportion of Each Religious Group",
    x = "Population (2010, log scale)",
    y = "Proportion"
  )

print(p2)
ggsave("JANKE_Population_vs_Religion.png", plot = p2, width = 12, height = 8, dpi = 150)


# ============================================================================
#  ADDITIONAL ANALYSIS: Correlation Between Religions and Non-Religious
# ============================================================================

# Calculate correlation between each religion's proportion and Non.Religious proportion
nr_correlations <- utah_long %>%
  group_by(Religion) %>%
  summarize(
    correlation = cor(Non.Religious, Proportion, use = "complete.obs"),
    p_value = cor.test(Non.Religious, Proportion)$p.value
  ) %>%
  arrange(desc(abs(correlation)))

cat("\n=== Correlation: Non-Religious Proportion vs. Religious Group Proportion ===\n")
print(nr_correlations, n = Inf)

# Plot: Non-Religious proportion vs. each religion
p3 <- utah_long %>%
  ggplot(aes(x = Proportion, y = Non.Religious)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  facet_wrap(~Religion, scales = "free_x") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "gray90")
  ) +
  labs(
    title = "Proportion of Each Religion vs. Non-Religious Proportion",
    subtitle = "Each point is a Utah county",
    x = "Proportion of Religious Group",
    y = "Proportion Non-Religious"
  )

print(p3)
ggsave("JANKE_NonReligious_vs_Religion.png", plot = p3, width = 12, height = 8, dpi = 150)


# ============================================================================
#                         ANSWERS TO QUESTIONS
# ============================================================================

# Q1: Which religious group correlates most strongly with the proportion
#     of non-religious people?
# A1: LDS (Latter-Day Saints) has the strongest correlation with non-religious
#     proportion. As LDS proportion increases, the non-religious proportion
#     decreases sharply.

# Q2: What is the direction of that correlation?
# A2: It is a strong NEGATIVE correlation. Counties with higher LDS proportions
#     tend to have lower proportions of non-religious people.

# Q3: What can you say about the relationships shown here?
# A3: LDS dominates the religious landscape in Utah. Most other religions
#     (Catholic, Evangelical, etc.) show a POSITIVE correlation with
#     non-religious proportion - meaning counties with more diversity in
#     smaller religious groups also tend to have more non-religious people.
#     This makes sense because those counties likely have lower LDS proportions,
#     leaving room for both other religions and non-religious people.

# Q4: Examine the axis scales. How could you modify the code to more
#     accurately portray values on an "equal footing"?
# A4: You could set scales = "fixed" in facet_wrap() instead of "free" so
#     that all panels share the same x and y axis ranges. This would make
#     it easier to compare the magnitude of each religion's proportion
#     across panels, though some smaller groups might be hard to see.

cat("\n=== Analysis Complete! ===\n")
cat("Output files saved in the Assignment_7 directory.\n")
