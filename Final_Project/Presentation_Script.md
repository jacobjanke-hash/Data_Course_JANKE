# Presentation Script: Study Habits and Academic Performance in College Students
**Total Duration:** 8 minutes 30 seconds  
**Presenter:** Jacob Janke  
**Date:** April 20, 2026

---

## [TIME: 0:00-1:00] SECTION: Title & Introduction

**WHERE ON WEBSITE:** Top of page, title and Introduction section

**WHAT TO SAY:**

"Good morning everyone. Today I'm presenting my analysis on study habits and academic performance in college students. This project investigates a question that's relevant to all of us: is academic success just about how much you study, or does how you study matter more?

Academic performance is a strong predictor of career success and graduate school admission, yet thousands of students struggle despite putting in long hours. Research suggests that the quality of study habits—not just quantity—plays a decisive role. Factors like sleep quality, study environment, active recall techniques, and stress management can either amplify or undermine the hours invested.

In this project, I analyzed data from 250 college students across five majors to answer two central questions: First, which study habits and lifestyle factors most strongly predict GPA? And second, can we build a reliable machine learning model to predict academic performance from these factors? I employed both traditional statistical modeling with multiple linear regression and modern machine learning with random forests. Along the way, I demonstrated two new skills: interactive data visualization with Plotly and random forest modeling."

**NAVIGATION:** Begin scrolling slowly down to Data Description section

---

## [TIME: 1:00-1:45] SECTION: Data Description

**WHERE ON WEBSITE:** Data Description section with Tables 1 and 2

**WHAT TO SAY:**

"Let me describe the dataset. I collected self-reported survey responses from 250 undergraduate students at a mid-sized university during the Fall semester. As you can see in Table 1, the dataset includes 11 variables covering study behaviors, lifestyle factors, and academic outcomes.

Key variables include weekly study hours, sleep hours per night, GPA on a 4-point scale, study environment, whether students use active recall techniques, major, class year, extracurricular hours, stress level, and attendance rate.

Looking at Table 2, our summary statistics show some interesting patterns. The average GPA is 3.37 with a standard deviation of 0.47. Students study an average of 15 hours per week, sleep about 7 hours per night, and report a mean stress level of 5.5 out of 10. Attendance averages 81 percent, which leaves room for improvement."

**NAVIGATION:** Scroll down to Exploratory Data Analysis section

---

## [TIME: 1:45-3:15] SECTION: Exploratory Data Analysis

**WHERE ON WEBSITE:** EDA section with Figures 1-6

**WHAT TO SAY:**

"Now let's explore the data visually. Figure 1 shows the GPA distribution, which is approximately normal and centered around 3.37 with a slight left skew, indicating a small cluster of lower-performing students.

Figure 2 is particularly revealing—it shows the relationship between weekly study hours and GPA, with points colored by whether students use active recall. There's a clear positive trend: more study hours lead to higher GPAs. But here's the critical insight: students who use active recall techniques consistently outperform those who don't, even at similar study hour levels. This suggests that study quality adds value beyond study quantity.

Figure 3 displays GPA distributions across majors. The distributions are broadly similar, which makes sense—GPA drivers are likely behavioral rather than field-specific in this dataset.

Figure 4, our correlation heatmap, reveals several key relationships. Study hours and attendance both show moderate positive correlations with GPA. Sleep hours is positively correlated with GPA, reinforcing the importance of adequate rest. And stress level is negatively correlated with GPA, as we'd expect.

Figures 5 and 6 confirm that both sleep and attendance are positively associated with academic performance. Notice how students with higher stress—shown as larger, redder points—tend to cluster at lower GPAs and fewer sleep hours, suggesting a compounding negative effect."

**NAVIGATION:** Scroll down to Statistical Analysis section

---

## [TIME: 3:15-4:45] SECTION: Statistical Analysis

**WHERE ON WEBSITE:** Statistical Analysis section with regression table, ANOVA, and diagnostic plots

**WHAT TO SAY:**

"For our statistical analysis, I fit a multiple linear regression model predicting GPA from key study habit and lifestyle variables. Table 3 shows the regression coefficients, and all our main predictors are highly significant.

Here are the key findings: Each additional hour of weekly study is associated with a 0.024 point increase in GPA, holding other factors constant. Each additional hour of nightly sleep is associated with a 0.125 point GPA boost—that's substantial. Students who use active recall score approximately 0.24 GPA points higher on average. Higher attendance is associated with better grades, and higher stress is associated with lower GPA, with a coefficient of negative 0.037.

The model explains approximately 33.6 percent of the variance in GPA, which is quite good for a behavioral model.

Table 4 shows our ANOVA comparison between a reduced model with only study hours versus the full model with all predictors. The ANOVA test confirms that the full model provides a significantly better fit, with a p-value less than 0.001. This justifies including sleep, active recall, attendance, and stress beyond just study hours.

Figure 7 displays our regression diagnostic plots. The residuals are reasonably well-behaved with no severe violations of our modeling assumptions. A few potential outliers are flagged, but they don't appear to be highly influential."

**NAVIGATION:** Scroll down to Interactive Visualizations section

---

## [TIME: 4:45-5:45] SECTION: New Skill 1 - Interactive Visualizations with Plotly

**WHERE ON WEBSITE:** Plotly section with interactive charts

**WHAT TO SAY:**

"Now I want to highlight the first new skill I learned for this project: creating interactive visualizations with Plotly. Traditional static plots are powerful for printed reports, but interactive visualizations allow you, the viewer, to hover over data points to see exact values, zoom into dense regions, filter data by clicking legend entries, and pan to explore relationships dynamically.

The Plotly package for R makes it easy to convert existing ggplot2 figures into interactive HTML widgets. This is especially valuable for exploratory data analysis and for presenting results to non-technical audiences.

If you're viewing this report online, you can interact with Figure 8—try hovering over individual points to see student details. Figure 9 shows an interactive boxplot where you can hover to see quartile values. And Figure 10 is a 3D scatter plot showing the relationship between study hours, sleep, and GPA. You can actually rotate and zoom this plot.

The 3D visualization reveals that the highest GPAs cluster in the region where students get adequate sleep—7 or more hours—and invest moderate to high study hours, 15 or more hours per week. Students using active recall, shown in blue, tend to appear higher on the GPA axis at comparable study and sleep levels."

**NAVIGATION:** Scroll down to Random Forest section

---

## [TIME: 5:45-7:15] SECTION: New Skill 2 - Random Forest Machine Learning

**WHERE ON WEBSITE:** Random Forest section with model summary, variable importance, and comparison

**WHAT TO SAY:**

"The second new skill I demonstrated is random forest machine learning. A random forest is an ensemble method that builds hundreds of decision trees, each trained on a random subset of the data and features, and then averages their predictions. This approach handles non-linear relationships automatically, resists overfitting, and provides a built-in measure of variable importance.

I used the randomForest package in R to predict GPA. First, I split the data into a training set of 187 students—that's 75 percent—and a test set of 63 students to evaluate out-of-sample performance.

Table 5 shows the model built 500 trees and explains about 14.7 percent of the variance in the training data. Figure 11 displays variable importance, and it's consistent with our regression findings: study hours per week is the single most important predictor, followed by sleep hours and active recall.

Table 6 compares out-of-sample performance between linear regression and random forest on the test data. Both models perform comparably—linear regression actually has a slight edge with an RMSE of 0.396 versus 0.421 for random forest, and an R-squared of 0.35 versus 0.27.

Figure 12 shows predicted versus actual GPA for both models. Points closer to the diagonal line indicate better predictions. The fact that random forest doesn't dramatically outperform linear regression is actually an informative finding—it tells us that the relationships in this dataset are largely linear. In more complex datasets with non-linear interactions, random forests typically provide a larger advantage."

**NAVIGATION:** Scroll down to Conclusions section

---

## [TIME: 7:15-8:30] SECTION: Conclusions

**WHERE ON WEBSITE:** Conclusions section at bottom of page

**WHAT TO SAY:**

"Let me conclude with the key findings from this analysis. First, study hours matter, but quality matters more. While weekly study hours are positively correlated with GPA, students who use active recall techniques achieve notably higher GPAs at every level of study investment.

Second, sleep is not optional. Sleep hours showed a significant positive association with GPA, and students getting fewer than 6 hours per night consistently underperformed.

Third, show up. Attendance rate was a robust predictor across all models—simply being present in class provides a measurable academic benefit.

Fourth, stress is a silent GPA killer. Higher self-reported stress levels were significantly associated with lower GPAs, independent of study habits.

And fifth, both our models agree. The multiple linear regression and random forest models identified the same top predictors, providing strong converging evidence.

Our models achieved R-squared values of approximately 0.35 and 0.27 on held-out test data, meaning study habits and lifestyle factors alone can explain a meaningful share of academic performance differences.

I acknowledge several limitations: this is simulated data, the measures are self-reported, we're missing variables like prior academic preparation and socioeconomic status, and our cross-sectional design means we can only infer association, not causation.

Future work could incorporate longitudinal data tracking students across semesters and include more granular study behavior measures. Thank you for your attention. I'm happy to take questions."

**NAVIGATION:** Remain at bottom of page for questions

---

## Presentation Tips

**Pacing:**
- Speak clearly and at a moderate pace (approximately 140-160 words per minute)
- Pause briefly between sections to allow transitions
- Make eye contact with the audience, not just reading from the screen

**Navigation:**
- Start at the very top of the page
- Scroll smoothly and slowly as you progress through each section
- Point to specific figures and tables as you reference them
- For interactive plots, demonstrate the hover/zoom functionality if presenting live

**Engagement:**
- Emphasize the practical implications for students
- Highlight the convergence of findings across different analytical approaches
- Underscore the two new skills (Plotly and Random Forest) as learning outcomes

**Time Management:**
- If running over 10 minutes, you can condense the EDA section (1:45-3:15) by describing only 2-3 key visualizations
- If running under 7 minutes, you can expand the Random Forest section by explaining more detail about how the algorithm works

---

**END OF SCRIPT**
