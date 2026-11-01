# Question 2: within-hour price allocation after the 15-minute reform
#
# The requested model is
#   P_qh - mean(P_h) = beta * (RL_qh - mean(RL_h)) + error_qh.
#
# model_1_hourly.csv contains the hourly means but not the four underlying
# quarter-hour observations. Therefore, this script uses that file as the
# authoritative table of post-reform hourly means and joins those means to the
# corresponding quarter-hours in final_electricity_dataset.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(readr)
})

# Make all paths relative to this script, so it can be run from any directory.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 1L) {
  script_path <- sub("^--file=", "", script_arg)
  script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
} else {
  script_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

hourly_path <- file.path(script_dir, "model_1_hourly.csv")
quarter_hour_path <- file.path(script_dir, "final_electricity_dataset.csv")
output_path <- file.path(script_dir, "regression_2.txt")

stopifnot(
  file.exists(hourly_path),
  file.exists(quarter_hour_path)
)

# Hourly averages already created from the four quarter-hours in Question 1.
hourly <- read_csv(hourly_path, show_col_types = FALSE) |>
  mutate(
    delivery_date = as.Date(delivery_date),
    delivery_hour = as.integer(delivery_hour),
    repeated_interval = as.integer(repeated_interval),
    Post = as.integer(Post)
  )

required_hourly_columns <- c(
  "delivery_date",
  "delivery_hour",
  "repeated_interval",
  "Post",
  "price_de_eur_mwh",
  "residual_load_gw",
  "quarters_in_hour"
)
stopifnot(all(required_hourly_columns %in% names(hourly)))

# Omit all observations before the reform and retain the hourly means needed
# for demeaning the quarter-hour observations.
post_hours <- hourly |>
  filter(Post == 1L) |>
  transmute(
    delivery_date,
    delivery_hour,
    repeated_interval,
    price_hour_mean = price_de_eur_mwh,
    residual_load_hour_mean = residual_load_gw,
    quarters_in_hour
  )

# Read the underlying quarter-hour observations. Stable column positions are
# used because some source headers contain locale-dependent symbols.
quarter_hour <- read_csv(
  quarter_hour_path,
  show_col_types = FALSE,
  name_repair = "minimal"
)

stopifnot(ncol(quarter_hour) >= 22L)
names(quarter_hour)[c(1, 3, 20, 22)] <- c(
  "start_time",
  "price_de_eur_mwh",
  "repeated_interval",
  "residual_load_mwh"
)

quarter_hour <- quarter_hour |>
  transmute(
    delivery_date = as.Date(start_time, tz = "UTC"),
    delivery_hour = as.integer(format(start_time, "%H", tz = "UTC")),
    quarter_in_hour = as.integer(format(start_time, "%M", tz = "UTC")) %/% 15L + 1L,
    repeated_interval = as.integer(repeated_interval),
    price_de_eur_mwh = as.numeric(price_de_eur_mwh),
    # MWh supplied during 15 minutes -> average GW.
    residual_load_gw = as.numeric(residual_load_mwh) / 250
  )

# Join each post-reform hour's two means to its four quarter-hour observations,
# then construct the left- and right-hand sides of the requested equation.
q_post <- quarter_hour |>
  inner_join(
    post_hours,
    by = c("delivery_date", "delivery_hour", "repeated_interval")
  ) |>
  mutate(
    price_deviation = price_de_eur_mwh - price_hour_mean,
    residual_load_deviation = residual_load_gw - residual_load_hour_mean,
    # Each cluster contains the four quarter-hours that share the two means.
    hour_cluster = interaction(
      delivery_date,
      delivery_hour,
      repeated_interval,
      drop = TRUE
    )
  ) |>
  arrange(delivery_date, delivery_hour, repeated_interval, quarter_in_hour)

# Data-integrity checks for the within-hour design.
group_check <- q_post |>
  group_by(delivery_date, delivery_hour, repeated_interval) |>
  summarise(
    quarter_hours = n(),
    mean_price_deviation = mean(price_deviation),
    mean_residual_load_deviation = mean(residual_load_deviation),
    .groups = "drop"
  )

stopifnot(
  nrow(post_hours) == 8760L,
  nrow(q_post) == 35040L,
  all(post_hours$quarters_in_hour == 4L),
  all(group_check$quarter_hours == 4L),
  !anyNA(q_post$price_deviation),
  !anyNA(q_post$residual_load_deviation),
  max(abs(group_check$mean_price_deviation)) < 1e-10,
  max(abs(group_check$mean_residual_load_deviation)) < 1e-10
)

# No intercept: both variables are deviations from their hourly means.
# Standard errors are clustered by delivery hour, allowing arbitrary
# dependence among the four quarter-hour observations within each hour.
model_2 <- feols(
  price_deviation ~ 0 + residual_load_deviation,
  data = q_post,
  cluster = ~hour_cluster
)

coefficient_table <- coeftable(model_2)

sink(output_path)
cat("QUESTION 2: WITHIN-HOUR PRICE ALLOCATION AFTER THE REFORM\n")
cat("=========================================================\n\n")
cat("Specification:\n")
cat("  price_qh - mean(price_h) = beta * [residual_load_qh - mean(residual_load_h)] + error_qh\n\n")
cat("Data construction:\n")
cat("  Reform period:              2025-10-01 through 2026-09-30\n")
cat("  Post-reform delivery hours:", format(nrow(post_hours), big.mark = ","), "\n")
cat("  Quarter-hour observations: ", format(nrow(q_post), big.mark = ","), "\n")
cat("  Quarter-hours per hour:     4\n")
cat("  Intercept:                  omitted (within-hour deviations sum to zero)\n")
cat("  Standard errors:            clustered by delivery hour\n")
cat("  Number of clusters:        ", format(nlevels(q_post$hour_cluster), big.mark = ","), "\n\n")

cat("Regression results:\n")
print(summary(model_2))

cat("\nKey result:\n")
cat(sprintf(
  "  beta = %.6f (clustered SE = %.6f, t = %.3f, p %s)\n",
  coefficient_table[1L, "Estimate"],
  coefficient_table[1L, "Std. Error"],
  coefficient_table[1L, "t value"],
  format.pval(
    coefficient_table[1L, "Pr(>|t|)"],
    digits = 4L,
    eps = 2.2e-16
  )
))
cat(sprintf("  R-squared = %.6f\n", r2(model_2, "r2")))
sink()

message(
  "Saved Question 2 regression results to: ",
  normalizePath(output_path, winslash = "/", mustWork = TRUE)
)
