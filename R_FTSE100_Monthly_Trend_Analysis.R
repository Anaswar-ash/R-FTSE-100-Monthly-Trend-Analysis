# Load required libraries using pacman for clean installation and loading
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

# --- Load and Prepare Data ---
tryCatch({
  # Load the dataset
  df <- read_csv("chart (1).csv", show_col_types = FALSE) %>%
    # Rename columns for easier access
    rename(Date_str = 1, FTSE_100 = 2) %>%
    # Convert 'Date' column to datetime objects.
    # Add a dummy day "01" to make the format compatible with lubridate.
    mutate(Date = myd(paste0(Date_str, "/01"), quiet = TRUE)) %>%
    # Arrange data chronologically
    arrange(Date)

}, error = function(e) {
  stop("Error: 'chart (1).csv' not found or could not be processed. Make sure it's in the same directory.") # nolint
})

# --- Monthly Analysis ---

df <- df %>%
  # Calculate monthly percentage returns
  mutate(monthly_return = (FTSE_100 / lag(FTSE_100) - 1) * 100) %>%
  # Add a 'Month' column for grouping, ensuring it's an ordered factor
  mutate(Month = month(Date, label = TRUE, abbr = FALSE))

# --- Visualization & Summary ---

# 1. Box Plot of Monthly Returns
ggplot(df, aes(x = Month, y = monthly_return)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) + # nolint: line_length_linter.
  labs(
    title = "Distribution of FTSE 100 Monthly Returns (2000-2025)",
    subtitle = "Showing the median, interquartile range, and outliers for each month", # nolint: line_length_linter.
    x = "Month",
    y = "Monthly Return (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("ftse_100_monthly_distribution_r.png", width = 14, height = 7)
cat("Saved monthly return distribution plot.\n")


# 2. Bar Chart of Average Monthly Returns
# Calculate summary statistics
monthly_stats <- df %>%
  group_by(Month) %>%
  summarise(
    mean_return = mean(monthly_return, na.rm = TRUE),
    median_return = median(monthly_return, na.rm = TRUE),
    positive_prob = mean(monthly_return > 0, na.rm = TRUE)
  )

ggplot(monthly_stats, aes(x = Month, y = mean_return, fill = mean_return > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none") + # nolint
  labs(
    title = "Average FTSE 100 Monthly Return (2000-2025)",
    x = "Month",
    y = "Average Monthly Return (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("ftse_100_average_monthly_return_r.png", width = 14, height = 7)
cat("Saved average monthly return plot.\n")


# --- Print a summary to the console ---
cat("\n--- Monthly Trend Analysis Summary (R Version) ---\n")
cat("This analysis looks at historical data to see if certain months have, on average, performed better than others.\n") # nolint

cat("\nMonthly Performance Statistics (2000-2025):\n")
print(monthly_stats %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

best_month <- monthly_stats %>% filter(mean_return == max(mean_return))
worst_month <- monthly_stats %>% filter(mean_return == min(mean_return))

cat(paste0("\nHistorically, the best performing month on average has been ", best_month$Month, ".\n")) # nolint # nolint: line_length_linter.
cat(paste0("Historically, the worst performing month on average has been ", worst_month$Month, ".\n")) # nolint: line_length_linter.

cat("\nIMPORTANT DISCLAIMER:\n")
cat("This analysis is based on historical data and is for informational purposes only. Past performance is not an indicator of future results. This is not financial advice.\n") # nolint
