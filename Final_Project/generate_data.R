# Generate realistic student performance dataset
# Jacob Janke - Final Project

set.seed(42)
n <- 250

# Base variables
study_hours <- round(pmin(pmax(rnorm(n, 15, 8), 0), 40), 1)
sleep_hours <- round(pmin(pmax(rnorm(n, 7, 1.2), 4), 10), 1)
attendance_rate <- round(pmin(pmax(rnorm(n, 82, 12), 50), 100), 1)
stress_level <- round(pmin(pmax(rnorm(n, 5.5, 2), 1), 10))
extracurricular_hours <- round(pmin(pmax(rnorm(n, 8, 5), 0), 20), 1)
active_recall <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.4, 0.6))
study_environment <- sample(c("Library", "Dorm", "Coffee_Shop", "Home"), n, replace = TRUE, prob = c(0.3, 0.25, 0.15, 0.3))
major <- sample(c("Biology", "English", "Computer_Science", "Psychology", "Business"), n, replace = TRUE)
year <- sample(c("Freshman", "Sophomore", "Junior", "Senior"), n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2))

# GPA as function of predictors + noise
gpa_raw <- 1.5 +
  0.035 * study_hours +
  0.12 * sleep_hours +
  0.008 * attendance_rate +
  -0.04 * stress_level +
  0.25 * (active_recall == "Yes") +
  0.05 * (study_environment == "Library") +
  -0.02 * (study_environment == "Dorm") +
  0.03 * (year == "Senior") + 0.02 * (year == "Junior") +
  rnorm(n, 0, 0.3)

gpa <- round(pmin(pmax(gpa_raw, 1.5), 4.0), 2)

# Add a few outliers
outlier_idx <- sample(1:n, 8)
gpa[outlier_idx[1:4]] <- round(runif(4, 1.5, 2.2), 2)
study_hours[outlier_idx[1:4]] <- round(runif(4, 25, 38), 1)
gpa[outlier_idx[5:8]] <- round(runif(4, 3.7, 4.0), 2)
study_hours[outlier_idx[5:8]] <- round(runif(4, 1, 6), 1)

student_id <- sprintf("STU%03d", 1:n)

df <- data.frame(
  student_id, study_hours_weekly = study_hours, sleep_hours, gpa,
  study_environment, active_recall, major, year,
  extracurricular_hours, stress_level, attendance_rate,
  stringsAsFactors = FALSE
)

write.csv(df, "student_performance_data.csv", row.names = FALSE)
cat("Generated", nrow(df), "student records\n")
