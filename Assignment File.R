lecture_data <- data.frame(
  Visual_Aids = factor(rep(c("No", "Yes"), each = 17)),  # Replicating "No" and "Yes" each 30 times
  Quality_Score = c(
    # Quality scores for lectures without visual aids
    50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61,
    # Quality scores for lectures with visual aids
    58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70
  )  # Quality score out of 100
)

# Get the summary of the data frame
summary(lecture_data)




# Load necessary libraries
library(ggplot2)

# Assuming the data frame is named 'lecture_data' with columns 'Quality_Score' and 'Visual_Aids'

lecture_data <- data.frame(
  Visual_Aids = factor(rep(c("No", "Yes"), each = 17)),  # Replicating "No" and "Yes" each 30 times
  Quality_Score = c(
    # Quality scores for lectures without visual aids
    50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61,
    # Quality scores for lectures with visual aids
    58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70
  )  # Quality score out of 100
)


# Create boxplots
ggplot(lecture_data, aes(x = Visual_Aids, y = Quality_Score, fill = Visual_Aids)) +
  geom_boxplot() +
  labs(title = "Boxplot of Lecture Quality Scores by Visual Aids",
       x = "Visual Aids",
       y = "Quality Score") +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "lightgreen")) +
  theme_minimal()

# Observation 1: Difference in Median
median_quality_no_aids <- median(lecture_data$Quality_Score[lecture_data$Visual_Aids == "No"])
median_quality_with_aids <- median(lecture_data$Quality_Score[lecture_data$Visual_Aids == "Yes"])
median_difference <- median_quality_with_aids - median_quality_no_aids
cat("Median Quality Score without Visual Aids:", median_quality_no_aids, "\n")
cat("Median Quality Score with Visual Aids:", median_quality_with_aids, "\n")
cat("Difference in Median Quality Score:", median_difference, "\n")

# Observation 2: Spread and Outliers
boxplot_stats_no_aids <- boxplot.stats(lecture_data$Quality_Score[lecture_data$Visual_Aids == "No"])
boxplot_stats_with_aids <- boxplot.stats(lecture_data$Quality_Score[lecture_data$Visual_Aids == "Yes"])
cat("\nInterquartile Range (No Visual Aids):", boxplot_stats_no_aids$stats[4] - boxplot_stats_no_aids$stats[2], "\n")
cat("Interquartile Range (With Visual Aids):", boxplot_stats_with_aids$stats[4] - boxplot_stats_with_aids$stats[2], "\n")




rm(list=ls())
student=seq(1:17)
no_vis=c(50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61)
with_vis=c(58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70)
df=data.frame(student,no_vis,with_vis)
plot=boxplot(student~no_vis,df)
aa=shapiro.test(no_vis)
ab=shapiro.test(with_vis)







# Assuming the data frame is named 'lecture_data' with columns 'Visual_Aids' and 'Quality_Score'
lecture_data <- data.frame(
  Visual_Aids = factor(rep(c("No", "Yes"), each = 17)),  # Replicating "No" and "Yes" each 30 times
  Quality_Score = c(
    # Quality scores for lectures without visual aids
    50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61,
    # Quality scores for lectures with visual aids
    58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70
  )  # Quality score out of 100
)
# Load necessary libraries
library(dplyr)

# Calculate mean, standard deviation, and confidence interval for lectures without visual aids
summary_no_aids <- summarise(lecture_data %>% filter(Visual_Aids == "No"),
                             Mean = mean(Quality_Score),
                             SD = sd(Quality_Score),
                             Lower_CI = Mean - qt(0.975, df = n() - 1) * (SD / sqrt(n())),
                             Upper_CI = Mean + qt(0.975, df = n() - 1) * (SD / sqrt(n())))

# Calculate mean, standard deviation, and confidence interval for lectures with visual aids
summary_with_aids <- summarise(lecture_data %>% filter(Visual_Aids == "Yes"),
                               Mean = mean(Quality_Score),
                               SD = sd(Quality_Score),
                               Lower_CI = Mean - qt(0.975, df = n() - 1) * (SD / sqrt(n())),
                               Upper_CI = Mean + qt(0.975, df = n() - 1) * (SD / sqrt(n())))

# Print the summary statistics for lectures without visual aids
cat("Summary Statistics for Lectures without Visual Aids:\n")
print(summary_no_aids)

# Print the summary statistics for lectures with visual aids
cat("\nSummary Statistics for Lectures with Visual Aids:\n")
print(summary_with_aids)











# Assuming the data frame is named 'lecture_data' with columns 'Visual_Aids' and 'Quality_Score'
lecture_data <- data.frame(
  Visual_Aids = factor(rep(c("No", "Yes"), each = 17)),  # Replicating "No" and "Yes" each 30 times
  Quality_Score = c(
    # Quality scores for lectures without visual aids
    50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61,
    # Quality scores for lectures with visual aids
    58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70
  )  # Quality score out of 100
)
# Load necessary libraries
library(dplyr)

# Calculate mean, standard deviation, and confidence interval for lectures without visual aids
summary_no_aids <- summarise(lecture_data %>% filter(Visual_Aids == "No"),
                             Mean = mean(Quality_Score),
                             SD = sd(Quality_Score),
                             Lower_CI = Mean - qt(0.975, df = n() - 1) * (SD / sqrt(n())),
                             Upper_CI = Mean + qt(0.975, df = n() - 1) * (SD / sqrt(n())))

# Calculate mean, standard deviation, and confidence interval for lectures with visual aids
summary_with_aids <- summarise(lecture_data %>% filter(Visual_Aids == "Yes"),
                               Mean = mean(Quality_Score),
                               SD = sd(Quality_Score),
                               Lower_CI = Mean - qt(0.975, df = n() - 1) * (SD / sqrt(n())),
                               Upper_CI = Mean + qt(0.975, df = n() - 1) * (SD / sqrt(n())))

# Print the comparison of statistics
cat("Comparison of Summary Statistics:\n")
cat("\nLectures without Visual Aids:\n")
print(summary_no_aids)
cat("\nLectures with Visual Aids:\n")
print(summary_with_aids)

# Comment on each comparison
cat("\nComments:\n")

# Mean comparison
mean_difference <- summary_with_aids$Mean - summary_no_aids$Mean
cat("\nMean Difference:", mean_difference)
if (mean_difference > 0) {
  cat("\nLectures with visual aids have a higher mean quality score compared to lectures without visual aids.")
} else if (mean_difference < 0) {
  cat("\nLectures with visual aids have a lower mean quality score compared to lectures without visual aids.")
} else {
  cat("\nThere is no difference in the mean quality score between lectures with and without visual aids.")
}

# Standard deviation comparison
sd_difference <- summary_with_aids$SD - summary_no_aids$SD
cat("\n\nStandard Deviation Difference:", sd_difference)
if (sd_difference > 0) {
  cat("\nLectures with visual aids have a higher standard deviation, indicating greater variability in quality scores compared to lectures without visual aids.")
} else if (sd_difference < 0) {
  cat("\nLectures with visual aids have a lower standard deviation, indicating less variability in quality scores compared to lectures without visual aids.")
} else {
  cat("\nThere is no difference in the variability of quality scores between lectures with and without visual aids.")
}

# Confidence interval comparison
ci_difference_lower <- summary_with_aids$Lower_CI - summary_no_aids$Lower_CI
ci_difference_upper <- summary_with_aids$Upper_CI - summary_no_aids$Upper_CI
cat("\n\nConfidence Interval Difference (Lower Bound):", ci_difference_lower)
cat("\nConfidence Interval Difference (Upper Bound):", ci_difference_upper)
if (ci_difference_lower > 0 && ci_difference_upper > 0) {
  cat("\nThe confidence interval for lectures with visual aids is shifted to the right compared to lectures without visual aids, indicating a higher range of plausible mean quality scores.")
} else if (ci_difference_lower < 0 && ci_difference_upper < 0) {
  cat("\nThe confidence interval for lectures with visual aids is shifted to the left compared to lectures without visual aids, indicating a lower range of plausible mean quality scores.")
} else {
  cat("\nThere is no consistent difference in the range of plausible mean quality scores between lectures with and without visual aids.")
}




