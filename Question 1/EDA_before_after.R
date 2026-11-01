# Exploratory data analysis: before versus after the 15-minute reform
# Run this script with the working directory set to the "Question 1" folder.
# One-time setup: install.packages(c("dplyr", "readr", "ggplot2"))

library(dplyr)
library(readr)
library(ggplot2)

reform_date <- as.Date("2025-10-01")

electricity_data <- read_csv(
  "model_1_hourly.csv",
  show_col_types = FALSE,
  col_types = cols(
    delivery_date = col_date()
  )
) |>
  mutate(
    period = factor(
      Post,
      levels = c(0, 1),
      labels = c("Before reform", "After reform")
    )
  )

stopifnot(
  nrow(electricity_data) == 17520L,
  !anyNA(electricity_data$delivery_date),
  !anyNA(electricity_data$price_de_eur_mwh),
  !anyNA(electricity_data$residual_load_gw),
  all(electricity_data$quarters_in_hour == 4L)
)

# ---------------------------------------------------------------------------
# 1. Summary statistics
# ---------------------------------------------------------------------------

eda_summary <- electricity_data |>
  group_by(period) |>
  summarise(
    observations = n(),
    first_date = min(delivery_date),
    last_date = max(delivery_date),
    mean_price = mean(price_de_eur_mwh),
    median_price = median(price_de_eur_mwh),
    price_sd = sd(price_de_eur_mwh),
    minimum_price = min(price_de_eur_mwh),
    price_p05 = quantile(price_de_eur_mwh, 0.05),
    price_p95 = quantile(price_de_eur_mwh, 0.95),
    maximum_price = max(price_de_eur_mwh),
    negative_price_share = mean(price_de_eur_mwh < 0),
    mean_residual_load_gw = mean(residual_load_gw),
    residual_load_sd_gw = sd(residual_load_gw),
    minimum_residual_load_gw = min(residual_load_gw),
    maximum_residual_load_gw = max(residual_load_gw),
    price_load_correlation = cor(price_de_eur_mwh, residual_load_gw),
    .groups = "drop"
  )

print(eda_summary, width = Inf)

# ---------------------------------------------------------------------------
# 2. Daily mean price time series
# ---------------------------------------------------------------------------

daily_data <- electricity_data |>
  group_by(delivery_date, period) |>
  summarise(
    mean_price = mean(price_de_eur_mwh),
    mean_residual_load_gw = mean(residual_load_gw),
    .groups = "drop"
  )

daily_price_plot <- ggplot(
  daily_data,
  aes(x = delivery_date, y = mean_price)
) +
  geom_line(colour = "#1F4E79", linewidth = 0.45) +
  geom_vline(
    xintercept = reform_date,
    colour = "#C00000",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = reform_date,
    y = Inf,
    label = "Reform: 1 Oct 2025",
    colour = "#C00000",
    hjust = -0.05,
    vjust = 1.5,
    size = 3.5
  ) +
  labs(
    title = "Daily mean German day-ahead price",
    subtitle = "Dashed line marks the introduction of 15-minute day-ahead pricing",
    x = NULL,
    y = "Daily mean price (EUR/MWh)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  "eda_daily_mean_price.png",
  daily_price_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# ---------------------------------------------------------------------------
# 3. Price distributions before and after the reform
# ---------------------------------------------------------------------------

price_limits <- quantile(
  electricity_data$price_de_eur_mwh,
  probs = c(0.01, 0.99)
)

price_distribution_plot <- ggplot(
  electricity_data,
  aes(x = price_de_eur_mwh, fill = period, colour = period)
) +
  geom_density(alpha = 0.20, linewidth = 0.8) +
  coord_cartesian(xlim = price_limits) +
  scale_fill_manual(
    values = c("Before reform" = "#4C78A8", "After reform" = "#F58518")
  ) +
  scale_colour_manual(
    values = c("Before reform" = "#4C78A8", "After reform" = "#F58518")
  ) +
  labs(
    title = "Distribution of German day-ahead prices",
    subtitle = "Display is zoomed to the 1st-99th percentiles; no observations are removed",
    x = "Hourly-equivalent price (EUR/MWh)",
    y = "Density",
    fill = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(
  "eda_price_distribution.png",
  price_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ---------------------------------------------------------------------------
# 4. Average hour-of-day profiles
# ---------------------------------------------------------------------------

hourly_profile <- electricity_data |>
  group_by(period, delivery_hour) |>
  summarise(
    mean_price = mean(price_de_eur_mwh),
    mean_residual_load_gw = mean(residual_load_gw),
    .groups = "drop"
  )

hourly_price_plot <- ggplot(
  hourly_profile,
  aes(x = delivery_hour, y = mean_price, colour = period)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_colour_manual(
    values = c("Before reform" = "#4C78A8", "After reform" = "#F58518")
  ) +
  labs(
    title = "Average day-ahead price by delivery hour",
    x = "Delivery hour",
    y = "Mean price (EUR/MWh)",
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(
  "eda_hourly_price_profile.png",
  hourly_price_plot,
  width = 8,
  height = 5,
  dpi = 300
)

hourly_load_plot <- ggplot(
  hourly_profile,
  aes(x = delivery_hour, y = mean_residual_load_gw, colour = period)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_colour_manual(
    values = c("Before reform" = "#4C78A8", "After reform" = "#F58518")
  ) +
  labs(
    title = "Average forecast residual load by delivery hour",
    x = "Delivery hour",
    y = "Mean residual load (GW)",
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(
  "eda_hourly_residual_load_profile.png",
  hourly_load_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ---------------------------------------------------------------------------
# 5. Descriptive price-residual-load relationship
# ---------------------------------------------------------------------------

load_price_bins <- electricity_data |>
  group_by(period) |>
  mutate(load_bin = ntile(residual_load_gw, 30)) |>
  group_by(period, load_bin) |>
  summarise(
    mean_residual_load_gw = mean(residual_load_gw),
    mean_price = mean(price_de_eur_mwh),
    observations = n(),
    .groups = "drop"
  )

load_price_plot <- ggplot(
  load_price_bins,
  aes(
    x = mean_residual_load_gw,
    y = mean_price,
    colour = period
  )
) +
  geom_point(size = 2.2) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = c("Before reform" = "#4C78A8", "After reform" = "#F58518")
  ) +
  labs(
    title = "Day-ahead price and forecast residual load",
    subtitle = "Thirty equally sized residual-load bins within each period",
    x = "Mean residual load within bin (GW)",
    y = "Mean price within bin (EUR/MWh)",
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(
  "eda_residual_load_price_bins.png",
  load_price_plot,
  width = 8,
  height = 5,
  dpi = 300
)

message("EDA complete. Summary statistics were printed and five PNG files were saved.")
