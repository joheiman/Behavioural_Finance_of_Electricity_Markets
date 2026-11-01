# Faster Markets, Better Prices? - Questions 1 and 2
# Data: Electricity_regression_dataset.xlsx (sheets "Hourly" and "QuarterHour")
#
# One-time setup:
#   install.packages(c("readxl", "fixest"))
library(readxl)
library(fixest)

file <- "~/Desktop/Grad/Behavioral/Group project/Data/DataSet.xlsx"

#----------------------------------------------------------
# Question 1
#----------------------------------------------------------
h <- read_excel(file, sheet = "Hourly")
h$date <- as.Date(h$date)
nrow(h)    # check: 17520 - only AFTER h is created
head(h)

# Date and hour fixed effects, SE clustered by date.
# post itself is absorbed by the date fixed effects.
m1 <- feols(price_de ~ rl_fc_gw + rl_post | date + hour,
            data = h, cluster = ~date)
summary(m1)

# After-reform slope (beta1 + beta2), like Stata's lincom
b  <- coef(m1)
V  <- vcov(m1)
post_slope <- b["rl_fc_gw"] + b["rl_post"]
post_se    <- sqrt(V["rl_fc_gw", "rl_fc_gw"] + V["rl_post", "rl_post"] +
                   2 * V["rl_fc_gw", "rl_post"])
cat(sprintf("After-reform slope: %.3f EUR/MWh per GW (SE %.3f)\n",
            post_slope, post_se))

#----------------------------------------------------------
# Question 2: within-hour allocation (post-reform only)
#----------------------------------------------------------
q <- read_excel(file, sheet = "QuarterHour")
q$date <- as.Date(q$date)
q_post <- subset(q, post == 1)

# No constant: both variables are deviations from their hourly mean
m2 <- feols(price_dev ~ -1 + rl_dev, data = q_post, cluster = ~date)
summary(m2)
r2(m2, "r2")

#----------------------------------------------------------
# Side-by-side table
#----------------------------------------------------------
etable(m1, m2, headers = c("Q1: hourly", "Q2: within-hour"))

