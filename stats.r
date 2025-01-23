output_file <- "console_log.txt"
sink(output_file, append = TRUE, split = TRUE)

cat("===== Library and Dataset Load =====\n\n")
library(ggplot2)
library(dplyr)
data <- read.csv("free_riding.csv")
head(data)

cat("===== Basic statistics summary =====\n\n")
summary(data)

cat("\n\n--- Dataset Summary ---\n\n")
summary(data)

cor_test <- cor.test(data$extroversion_scores, data$average_peer_score, method = "pearson")

cat("\n\n--- Pearson Correlation Test ---\n\n")
print(cor_test)


data$extroversion_group <- ifelse(data$extroversion_scores > 65, "High", "Low")
t_test_result <- t.test(average_peer_score ~ extroversion_group, data = data)
cat("\n\n--- T-Test Results ---\n\n")
print(t_test_result)

model <- lm(average_peer_score ~ extroversion_scores, data = data)

summary_model <- summary(model)
cat("R-squared value:", summary_model$r.squared, "\n")

r_squared <- summary_model$r.squared
cat("\n\n--- Scatter plot with regression line (R^2) ---\n\n")
ggplot(data, aes(x = extroversion_scores, y = average_peer_score)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title = paste("Linear Regression: Extraversion vs Peer Score (R² =", round(r_squared, 3), ")"),
       x = "Extraversion Scores",
       y = "Average Peer Score") +
  theme_minimal()

cat("\n\n--- boxplot comparing peer scores across extraversion levels(Threshold = 65) ---\n\n")
threshold <- 65
high_extroversion <- data %>% filter(extroversion_scores > threshold) %>% pull(average_peer_score)
low_extroversion <- data %>% filter(extroversion_scores <= threshold) %>% pull(average_peer_score)

ggplot(data, aes(x = factor(extroversion_scores > threshold, labels = c("Low", "High")), 
                 y = average_peer_score, fill = factor(extroversion_scores > threshold))) +
  geom_boxplot() +
  labs(title = "Peer Score Comparison by Extraversion Level",
       x = "Extraversion Level",
       y = "Average Peer Score",
       fill = "Group") +
  theme_minimal()

correlation_pearson <- cor(data$extroversion_scores, data$average_peer_score, use = "complete.obs", method = "pearson")
cat("Pearson Correlation Coefficient:", correlation_pearson, "\n")


cat("\n\n--- Independent t-test to compare peer scores for high and low extroversion groups ---\n\n")
t_test_result <- t.test(high_extroversion, low_extroversion, var.equal = FALSE)
print(t_test_result)

cat("\n\n--- linear regression, P value for our Null Hypothesis ---\n\n")
model <- lm(average_peer_score ~ extroversion_scores, data = data)
summary_model <- summary(model)
p_value <- summary_model$coefficients["extroversion_scores", "Pr(>|t|)"]
cat("P-value for Extraversion Predictor:", p_value, "\n")

cat("\n\n--- Bar Chart of Extraversion Scores ---\n\n")
ggplot(data, aes(x = factor(extroversion_scores))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Bar Chart of Extraversion Scores",
       x = "Extraversion Scores",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sink()

cat("All results have been saved to", output_file, "\n")

