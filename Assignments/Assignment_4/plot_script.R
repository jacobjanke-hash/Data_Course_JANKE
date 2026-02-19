# Assignment 4 - Plot Script
# Generates a plot showing relationship between study hours and GPA

library(ggplot2)

# Read fake student data using relative path
student_data <- read.csv("fake_student_data.csv")

# Create a scatter plot with trend line showing study hours vs GPA
# Colored by whether students use active recall techniques
plot1 <- ggplot(student_data, aes(x = study_hours, y = gpa, color = active_recall)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
  labs(
    title = "Relationship Between Study Hours and GPA",
    subtitle = "Comparing students who use active recall vs those who don't",
    x = "Weekly Study Hours",
    y = "GPA",
    color = "Uses Active Recall"
  ) +
  scale_color_manual(values = c("no" = "coral", "yes" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

# Save the plot
ggsave("study_hours_vs_gpa.png", plot = plot1, width = 8, height = 6, dpi = 150)

print("Plot saved as study_hours_vs_gpa.png")
