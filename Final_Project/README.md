# Final Project: Study Habits and Academic Performance

**Author:** Jacob Janke  
**Course:** BIOL 3100 - Data Science  
**Date:** Spring 2026

## Overview

This project investigates the relationship between study habits, lifestyle factors, and academic performance (GPA) in college students. Using a simulated dataset of 250 undergraduate students, we explore which behaviors most strongly predict GPA using both traditional statistical methods and modern machine learning techniques.

## Research Questions

1. Which study habits and lifestyle factors most strongly predict GPA?
2. Can we build a reliable predictive model for academic performance?

## Files

| File | Description |
|------|-------------|
| `Final_Report.Rmd` | R Markdown source for the full report |
| `Final_Report.html` | Knitted HTML report (self-contained) |
| `student_performance_data.csv` | Simulated dataset (250 students, 11 variables) |
| `generate_data.R` | Script used to generate the dataset |
| `Final_Project.Rproj` | RStudio project file |

## Methods

- **Exploratory Data Analysis**: Distribution plots, scatter plots, boxplots, correlation heatmap
- **Multiple Linear Regression**: GPA modeled as a function of study hours, sleep, active recall, attendance, and stress
- **ANOVA**: Model comparison testing
- **Random Forest** (NEW SKILL): Ensemble machine learning model for GPA prediction with variable importance analysis
- **Interactive Plotly Visualizations** (NEW SKILL): Hover-enabled scatter plots, boxplots, and a 3D scatter plot

## Key Findings

- Study hours, sleep, and active recall usage are the strongest predictors of GPA
- Active recall techniques provide a measurable GPA boost beyond study time alone
- Random forest and linear regression yield comparable results, suggesting largely linear relationships
- Attendance and stress management also play significant roles

## How to Reproduce

1. Open `Final_Project.Rproj` in RStudio
2. Install required packages: `tidyverse`, `plotly`, `randomForest`, `corrplot`, `knitr`, `kableExtra`
3. Knit `Final_Report.Rmd` to HTML
