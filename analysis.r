# Load necessary libraries


# Read the data (replace 'your_data.csv' with the actual filename)




# Calculate mean and standard deviation for extroversion and self-score
mean_extroversion <- mean(data$extroversion_scores, na.rm = TRUE)
sd_extroversion <- sd(data$extroversion_scores, na.rm = TRUE)

mean_self_score <- mean(data$self_score, na.rm = TRUE)
sd_self_score <- sd(data$self_score, na.rm = TRUE)


cat("Mean Extraversion:", mean_extroversion, "SD:", sd_extroversion, "\n")
cat("Mean Self Score:", mean_self_score, "SD:", sd_self_score, "\n")

# Linear regression (Average peer scores X Extroversion Scores)
model <- lm(average_peer_score ~ extroversion_scores, data = data)
summary_model <- summary(model)
p_value <- summary_model$coefficients["extroversion_scores", "Pr(>|t|)"]
cat("P-value for Extraversion Predictor:", p_value, "\n")

# Perform a t-test comparing high vs low extroversion based on a threshold of 65
high_extroversion <- data[data$extroversion_scores > 65, "self_score"]
low_extroversion <- data[data$extroversion_scores <= 65, "self_score"]

t_test_result <- t.test(high_extroversion, low_extroversion)
print(t_test_result)

# Count free riders (flagged as 1)
num_free_riders <- sum(data$free_rider_flag)
cat("Number of Free Riders:", num_free_riders, "\n")

# Group-wise analysis: Calculate average self_score and peer_score per team
team_summary <- data %>%
  group_by(team_id) %>%
  summarise(
    avg_extroversion = mean(extroversion_scores, na.rm = TRUE),
    avg_self_score = mean(self_score, na.rm = TRUE),
    avg_peer_score = mean(average_peer_score, na.rm = TRUE),
    total_free_riders = sum(free_rider_flag)
  )

print(team_summary)

# Visualization: Scatter plot of extroversion vs self-score with free-rider flag
ggplot(data, aes(x = extroversion_scores, y = self_score, color = as.factor(free_rider_flag))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot: Extraversion vs Self-Score",
       x = "Extraversion Scores",
       y = "Self Score",
       color = "Free Rider Flag") +
  theme_minimal()

# Histogram of extroversion scores
ggplot(data, aes(x = extroversion_scores)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Extraversion Scores",
       x = "Extraversion Scores",
       y = "Count")

# Boxplot to compare self-score by free-rider status
ggplot(data, aes(x = as.factor(free_rider_flag), y = self_score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Self-Score Distribution by Free-Rider Status",
       x = "Free Rider (0 = No, 1 = Yes)",
       y = "Self Score") +
  theme_minimal()



# Correlation between extraversion and peer evaluation
correlation_result <- cor(data$extroversion_scores, data$average_peer_score, use = "complete.obs")
cat("Correlation between Extraversion and Peer Score:", correlation_result, "\n")


correlation_pearson <- cor(data$extroversion_scores, data$average_peer_score, use = "complete.obs", method = "pearson")
cat("Pearson Correlation: ", correlation_pearson, "\n")

# Calculate Spearman correlation (for non-linear relationships)
correlation_spearman <- cor(data$extroversion_scores, data$average_peer_score, use = "complete.obs", method = "spearman")
cat("Spearman Correlation: ", correlation_spearman, "\n")


# Scatter plot with regression line
ggplot(data, aes(x = extroversion_scores, y = average_peer_score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title = "Correlation between Extraversion Scores and Average Peer Score",
       x = "Extraversion Scores",
       y = "Average Peer Score") +
  theme_minimal()


#######################



# Check for normality of residuals using Shapiro-Wilk test
shapiro_test <- shapiro.test(model$residuals)
cat("Shapiro-Wilk test p-value:", shapiro_test$p.value, "\n")

# Residuals plot to check for linearity
plot(model$fitted.values, resid(model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     col = "blue", pch = 20)
abline(h = 0, col = "red")

# Compute Pearson correlation coefficient
correlation_pearson <- cor(data$extroversion_scores, data$average_peer_score, use = "complete.obs", method = "pearson")
cat("Pearson Correlation Coefficient:", correlation_pearson, "\n")

# Perform an independent t-test to compare peer scores for high and low extroversion groups

t_test_result <- t.test(high_extroversion, low_extroversion, var.equal = FALSE)
print(t_test_result)



