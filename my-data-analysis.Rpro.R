library(ggplot2)
library(dplyr)

students <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "Diana", "Eric", "Faith", "George", "Hannah"),
  Math = c(78, 85, NA, 92, 66, 88, NA, 90),
  English = c(82, 79, 75, NA, 70, 91, 68, 85),
  Science = c(80, NA, 78, 89, 72, 94, 70, NA)
)

print("Original Data")
print(students)

print("Data Summary")
summary(students)

students_clean <- students %>%
  mutate(
    Math = ifelse(is.na(Math), mean(Math, na.rm = TRUE), Math),
    English = ifelse(is.na(English), mean(English, na.rm = TRUE), English),
    Science = ifelse(is.na(Science), mean(Science, na.rm = TRUE), Science)
  )

print("Cleaned Data")
print(students_clean)

students_clean$Average <- rowMeans(students_clean[, 2:4])

print("Data with Average Score")
print(students_clean)

students_sorted <- students_clean %>% arrange(desc(Average))
print("Sorted Data by Average Score")
print(students_sorted)

bar_plot <- ggplot(students_clean, aes(x = Name, y = Average)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Scores per Student") +
  theme_minimal()
print(bar_plot)

pie_data <- students_clean %>%
  group_by(Name) %>%
  summarise(Total = sum(Math + English + Science))

pie_plot <- ggplot(pie_data, aes(x = "", y = Total, fill = Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Total Score Distribution") +
  theme_void()
print(pie_plot)

line_plot <- ggplot(students_clean, aes(x = Name, y = Average, group = 1)) +
  geom_line() +
  geom_point() +
  ggtitle("Average Score Trend") +
  theme_minimal()
print(line_plot)

hist_plot <- ggplot(students_clean, aes(x = Average)) +
  geom_histogram(bins = 6) +
  ggtitle("Distribution of Average Scores") +
  theme_minimal()
print(hist_plot)

model <- lm(Science ~ Math + English, data = students_clean)
print("Linear Model Summary")
summary(model)

cat("\n--- REPORT ---\n")
cat("Total Students:", nrow(students_clean), "\n")
cat("Highest Average Score:", max(students_clean$Average), "\n")
cat("Lowest Average Score:", min(students_clean$Average), "\n")
cat("Overall Mean Score:", mean(students_clean$Average), "\n")

cat("\nTop Performing Student:\n")
print(students_sorted[1, ])

cat("\nModel Coefficients:\n")
print(coef(model))