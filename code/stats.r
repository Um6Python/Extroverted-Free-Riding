# Load required libraries
library(ggplot2)
library(dplyr)
library(car)

# Load the dataset (replace 'free_riding.csv' with actual filename)
data <- read.csv("free_riding.csv")

# Open output file to save the results
output_file <- "important.txt"
sink(output_file)

cat("===== Library and Dataset Load =====\n\n")
head(data)

cat("===== Basic statistics summary =====\n\n")
summary(data)

cat("\n\n=== Testing Assumptions about the Data ===\n\n")

# Normality Test using Shapiro-Wilk
cat("\n\n--- Shapiro: Normality Check for extroversion_scores and average_peer_score ---\n\n")
shapiro_extroversion <- shapiro.test(data$extroversion_scores)
shapiro_peer <- shapiro.test(data$average_peer_score)

# Function to check normality
check_normality <- function(shapiro_test) {
  if (shapiro_test$p.value > 0.05) {
    return("Data is normally distributed.")
  } else {
    return("Data is NOT normally distributed.")
  }
}

cat("--- Normality Check Results ---\n")
cat("Extraversion Scores: ", check_normality(shapiro_extroversion), "\n")
cat("Peer Scores: ", check_normality(shapiro_peer), "\n")

# Levene's test for equal variances
levene_test <- leveneTest(data$average_peer_score ~ as.factor(data$extroversion_scores > 65), data = data)

cat("\n--- Equal Variances Assumption Results (Levene's Test) ---\n")
print(levene_test)

# Evaluate variance equality and assign the result to a variable
equal_var <- levene_test$`Pr(>F)`[1] > 0.05

if (equal_var) {
  cat("Equal variances assumed (p =", levene_test$`Pr(>F)`[1], ")\n")
} else {
  cat("Equal variances NOT assumed (p =", levene_test$`Pr(>F)`[1], ")\n")
}

# Pearson Correlation Test
cor_test <- cor.test(data$extroversion_scores, data$average_peer_score, method = "pearson")

cat("\n\n--- Pearson Correlation Test ---\n\n")
print(cor_test)

cat("Pearson Correlation Coefficient:", cor_test$estimate, "\n")
cat("p-value for correlation:", cor_test$p.value, "\n")

if (cor_test$p.value < 0.05) {
  cat("There is a significant correlation between extraversion and peer scores (p =", cor_test$p.value, ")\n")
} else {
  cat("No significant correlation between extraversion and peer scores (p =", cor_test$p.value, ")\n")
}

# Independent T-test based on variance assumption
data$extroversion_group <- ifelse(data$extroversion_scores > 65, "High", "Low")
t_test_result <- t.test(average_peer_score ~ extroversion_group, data = data, var.equal = equal_var)

cat("\n\n--- T-Test Results ---\n\n")
print(t_test_result)

if (t_test_result$p.value > 0.05) {
  cat("No significant difference in peer scores between high and low extraversion groups (p =", t_test_result$p.value, ")\n")
} else {
  cat("There is a significant difference in peer scores between high and low extraversion groups (p =", t_test_result$p.value, ")\n")
}

# Linear Regression
model <- lm(average_peer_score ~ extroversion_scores, data = data)
summary_model <- summary(model)

cat("\n\n--- Linear Regression Results ---\n\n")
print(summary_model)

r_squared <- summary_model$r.squared
p_value <- summary_model$coefficients["extroversion_scores", "Pr(>|t|)"]

cat("R-squared value:", r_squared, "\n")
cat("P-value for Extraversion Predictor:", p_value, "\n")

# Scatter Plot with Regression Line
cat("\n\n--- Scatter plot with regression line (R^2) ---\n\n")
ggplot(data, aes(x = extroversion_scores, y = average_peer_score)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title = paste("Linear Regression: Extraversion vs Peer Score (R² =", round(r_squared, 3), ")"),
       x = "Extraversion Scores",
       y = "Average Peer Score") +
  theme_minimal()

# Boxplot comparing peer scores across extraversion levels
cat("\n\n--- Boxplot comparing peer scores across extraversion levels (Threshold = 65) ---\n\n")
ggplot(data, aes(x = factor(extroversion_scores > 65, labels = c("Low", "High")), 
                 y = average_peer_score, fill = factor(extroversion_scores > 65))) +
  geom_boxplot() +
  labs(title = "Peer Score Comparison by Extraversion Level",
       x = "Extraversion Level",
       y = "Average Peer Score",
       fill = "Group") +
  theme_minimal()

# Bar chart of extraversion scores
cat("\n\n--- Bar Chart of Extraversion Scores ---\n\n")
ggplot(data, aes(x = factor(extroversion_scores))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Bar Chart of Extraversion Scores",
       x = "Extraversion Scores",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\nAll results have been saved to", output_file, "\n")

# Close the sink to stop writing to the file
sink()
cat("Assumption checks completed and saved to 'assumption_checks_output.txt'.\n")
