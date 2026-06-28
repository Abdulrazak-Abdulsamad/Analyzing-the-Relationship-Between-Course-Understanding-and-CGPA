# Load required libraries
library(tidyverse)
library(ggplot2)

# Read and clean the data
df <- read.csv("~/Documents/NCAIR/DSB/Student Metadata.csv", stringsAsFactors = FALSE)

# Clean and prepare the data
df_clean <- df %>%
  janitor::clean_names() %>%
  mutate(
    # Clean CGPA - extract numeric values
    cgpa_clean = as.numeric(str_extract(cgpa, "^[0-9]+\\.[0-9]+")),
    # Clean level of understanding
    level = as.numeric(level_of_understanding_of_current_course),
    # Clean gender
    gender = str_trim(gender),
    gender = ifelse(str_to_lower(gender) %in% c("male", "m", "make"), "Male", 
                    ifelse(str_to_lower(gender) %in% c("female", "f"), "Female", NA))
  ) %>%
  # Keep only valid data
  filter(!is.na(level), !is.na(cgpa_clean), level %in% 1:5) %>%
  mutate(level = factor(level, levels = 1:5, 
                        labels = c("Very Low", "Low", "Medium", "High", "Very High")))


# METHOD 1: Boxplot (Simple and Clear)

p1 <- ggplot(df_clean, aes(x = level, y = cgpa_clean, fill = level)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), 
               vjust = -1, size = 3) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "CGPA Distribution by Course Understanding Level",
       x = "Level of Understanding", y = "CGPA") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)


# METHOD 2: Bar Chart of Average CGPA

avg_cgpa <- df_clean %>%
  group_by(level) %>%
  summarise(
    avg_cgpa = mean(cgpa_clean, na.rm = TRUE),
    count = n()
  )

p2 <- ggplot(avg_cgpa, aes(x = level, y = avg_cgpa, fill = level)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste("CGPA:", round(avg_cgpa, 2), "\n(n=", count, ")")), 
            vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Average CGPA by Understanding Level",
       x = "Level of Understanding", y = "Average CGPA") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 5)

print(p2)


# METHOD 3: Simple Statistical Test (ANOVA)

# Perform ANOVA to test if differences are significant
anova_result <- aov(cgpa_clean ~ level, data = df_clean)
summary(anova_result)

# Post-hoc test to see which groups are different
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)


# METHOD 4: Summary Table

summary_table <- df_clean %>%
  group_by(level) %>%
  summarise(
    `Number of Students` = n(),
    `Average CGPA` = round(mean(cgpa_clean), 2),
    `Median CGPA` = round(median(cgpa_clean), 2),
    `Min CGPA` = round(min(cgpa_clean), 2),
    `Max CGPA` = round(max(cgpa_clean), 2)
  )

print(summary_table)


# SIMPLE CONCLUSION

# Calculate overall average for comparison
overall_avg <- mean(df_clean$cgpa_clean, na.rm = TRUE)

cat("\n FINDINGS \n")
cat("Overall average CGPA:", round(overall_avg, 2), "\n\n")
cat("Average CGPA by understanding level:\n")
for(i in 1:nrow(avg_cgpa)) {
  diff <- avg_cgpa$avg_cgpa[i] - overall_avg
  cat("  ", avg_cgpa$level[i], ":", round(avg_cgpa$avg_cgpa[i], 2), 
      ifelse(diff > 0, paste("(+", round(diff, 2), "above average)"), 
             paste("(", round(diff, 2), "below average)")), "\n")
}

# ANOVA conclusion
p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
if(p_value < 0.05) {
  cat("\n CONCLUSION: There IS a significant relationship between course understanding and CGPA (p < 0.05)")
  cat("\n  Students with higher understanding tend to have higher CGPAs")
} else {
  cat("\n CONCLUSION: No significant relationship found between course understanding and CGPA")
}
