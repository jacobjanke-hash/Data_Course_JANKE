# Assignment 2 - File paths and R basics
# Author: Jacob Janke
# Date: January 27, 2026

# Task 4: List all .csv files in Data/ directory and store in "csv_files"
csv_files <- list.files("Data/", pattern = "\\.csv$", full.names = TRUE)

# Task 5: Find how many files match that description using length()
length(csv_files)

# Task 6: Open wingspan_vs_mass.csv and store as "df"
df <- read.csv("Data/wingspan_vs_mass.csv")

# Task 7: Inspect the first 5 lines using head()
head(df, 5)

# Task 8: Find files recursively in Data/ that begin with "b" (lowercase)
b_files <- list.files("Data/", pattern = "^b", recursive = TRUE, full.names = TRUE)

# Task 9: Display the first line of each "b" file using a for-loop
for (file in b_files) {
  cat("\nFirst line of", file, ":\n")
  first_line <- readLines(file, n = 1)
  cat(first_line, "\n")
}

# Task 10: Display first line of all .csv files
for (file in csv_files) {
  cat("\nFirst line of", file, ":\n")
  first_line <- readLines(file, n = 1)
  cat(first_line, "\n")
}
