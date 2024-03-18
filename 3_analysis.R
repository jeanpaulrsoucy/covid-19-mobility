#### 3: Analysis ####

# SETUP #

# create directories for output (if they don't already exist)
dir.create("fig", showWarnings = FALSE)
dir.create("tab", showWarnings = FALSE)

# load libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(ggeffects)
library(glmmTMB)
library(EpiEstim)
library(broom.mixed)
library(performance)
library(DHARMa)

# set random seed number
set.seed(as.integer(as.Date("2020-04-07")))

# source functions and theme
source("3_analysis_funs.R")
source("theme.R")

# LOAD AND PROCESS DATA #

# load data
cases <- read_csv("data/cases_clean.csv") # case data
cases_country <- read_csv("data/cases_country_clean.csv") # country-level case data
cmi <- read_csv("data/cmi_clean.csv") # CMI data
pdist <- read_csv("data/locations_pdist_dates.csv") # physical distancing data

# exclude Monaco (does not have 100 cases by end of study period)
cases <- cases %>% filter(city != "Monaco")
cases_country <- cases_country %>% filter(country != "Monaco")
cmi <- cmi %>% filter(city != "Monaco")
pdist <- pdist %>% filter(city != "Monaco")

# process case and CMI data for analysis
dat <- process_case_cmi_data(cases, cases_country, cmi)

# censor problematic data from the week beginning March 16
dat[dat$city == "Istanbul", c("weekly_gr_3", "log_weekly_gr_3", "R_week_3")] <- NA
dat[dat$city == "Montréal", c("weekly_gr_3", "log_weekly_gr_3", "R_week_3")] <- NA

# process physical distancing data
pdist <- process_pdist_data(pdist, cmi)

# DESCRIPTIVE STATISTICS FOR CMI #

# report median and IQR of CMI on March 2 (first Monday)
cmi %>%
  ungroup %>%
  filter(date == "2020-03-02") %>%
  select(cmi) %>%
  summarize(
    median = round(quantile(cmi, 0.5), 0),
    iqr = paste0(
      round(quantile(cmi, 0.25), 0), "–", round(quantile(cmi, 0.75), 0)))

# report median and IQR of CMI on March 29 (final Sunday)
cmi %>%
  ungroup %>%
  filter(date == "2020-03-29") %>%
  select(cmi) %>%
  summarize(
    median = round(quantile(cmi, 0.5), 0),
    iqr = paste0(
      round(quantile(cmi, 0.25), 0), "–", round(quantile(cmi, 0.75), 0)))

# FIGURE 1: CMI BEFORE AND AFTER PHYSICAL DISTANCING MEASURES ANNOUNCED #

# plot
p_cmi_pdist <- plot_cmi_pdist(pdist)
p_cmi_pdist # view plot

# save plot
ggsave("fig/fig_1.png", p_cmi_pdist, width = 7.5, height = 7, dpi = 300)

# ANALYSIS: LOG WEEKLY GROWTH RATE VS CMI #
# NOTE: "WEEKLY GROWTH RATE" IS REFERRED TO AS "WEEKLY CASE RATIO" IN THE MANUSCRIPT

# model: gr / 2-week lag, unadjusted
mod_gr_2 <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE)
summary(mod_gr_2)
# mod_gr_2_res <- simulateResiduals(mod_gr_2) # simulate residuals with DHARMa
# plotQQunif(mod_gr_2_res); plotResiduals(mod_gr_2_res, quantreg = TRUE) # plot simulated residuals
rep_est(mod_gr_2, "mean_cmi")
r2_nakagawa(mod_gr_2)

# model: gr / 2-week lag, unadjusted / interaction between CMI and week
mod_gr_2_cmi_week_int <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE, cmi_week_int = TRUE)
# summary(mod_gr_2_cmi_week_int)

# LRT to compare models with and without interaction between CMI and week
rep_lrt(anova(mod_gr_2, mod_gr_2_cmi_week_int)) # model without interactions is superior

# model: gr / 2-week lag, adjusted
mod_gr_2_adj <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE)
summary(mod_gr_2_adj)
# mod_gr_2_adj_res <- simulateResiduals(mod_gr_2_adj) # simulate residuals with DHARMa
# plotQQunif(mod_gr_2_adj_res); plotResiduals(mod_gr_2_adj_res, quantreg = TRUE) # plot simulated residuals
rep_est(mod_gr_2_adj, "mean_cmi")
rep_est(mod_gr_2_adj, "days_since_100")
r2_nakagawa(mod_gr_2_adj)

# model: gr / 2-week lag, adjusted / interaction between CMI and week
mod_gr_2_adj_cmi_week_int <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE, cmi_week_int = TRUE)
# summary(mod_gr_2_adj_cmi_week_int)

# LRT to compare models with and without interaction between CMI and week
rep_lrt(anova(mod_gr_2_adj, mod_gr_2_adj_cmi_week_int)) # model without interactions is superior

# model: gr / 3-week lag, unadjusted
mod_gr_3 <- mod_gr(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = FALSE)
summary(mod_gr_3)
# mod_gr_3_res <- simulateResiduals(mod_gr_3) # simulate residuals with DHARMa
# plotQQunif(mod_gr_3_res); plotResiduals(mod_gr_3_res, quantreg = TRUE) # plot simulated residuals
rep_est(mod_gr_3, "mean_cmi")
r2_nakagawa(mod_gr_3)

# model: gr / 3-week lag, adjusted
mod_gr_3_adj <- mod_gr(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = TRUE)
summary(mod_gr_3_adj)
# mod_gr_3_adj_res <- simulateResiduals(mod_gr_3_adj) # simulate residuals with DHARMa
# plotQQunif(mod_gr_3_adj_res); plotResiduals(mod_gr_3_adj_res, quantreg = TRUE) # plot simulated residuals
rep_est(mod_gr_3_adj, "mean_cmi")
rep_est(mod_gr_3_adj, "days_since_100")
r2_nakagawa(mod_gr_3_adj)

## LRT to compare unadjusted and adjusted models for 2-week lag
lrt_gr_2 <- anova(mod_gr_2, mod_gr_2_adj)
rep_lrt(lrt_gr_2)

## LRT to compare unadjusted and adjusted models for 3-week lag
lrt_gr_3 <- anova(mod_gr_3, mod_gr_3_adj)
rep_lrt(lrt_gr_3)

# FIGURE 2: MEAN GROWTH RATE VS CMI WITH LAG OF 2 WEEKS #

# plot
p_gr_cmi_2 <- plot_gr_cmi(dat, mod = mod_gr_2, cmi_lag = 2, outcome_weeks = 3:6)
p_gr_cmi_2 # view plot

# save plot
ggsave("fig/fig_2.png", p_gr_cmi_2, width = 8, height = 5, dpi = 300)

# SUPPLEMENTARY FIGURE 2: MEAN GROWTH RATE VS CMI WITH LAG OF 2 WEEKS (alternative) #

# plot
p_gr_cmi_2_alt <- plot_gr_cmi_2(dat, mod = mod_gr_2, cmi_lag = 2, outcome_weeks = 3:6)
p_gr_cmi_2_alt # view plot

# save plot
ggsave("fig/fig_s2.png", p_gr_cmi_2_alt, width = 8, height = 5, dpi = 300)

# SUPPLEMENTARY FIGURE 3: MEAN GROWTH RATE VS CMI WITH LAG OF 3 WEEKS #

# plot
p_gr_cmi_3 <- plot_gr_cmi(
  dat, mod = mod_gr_3, cmi_lag = 3, outcome_weeks = 4:6,
  x_breaks = c(0, 25, 50, 75, 100, 125),
  x_labs = c(0, 25, 50, 75, 100, 125),
  x_lims = c(0, 140),
  x_pred = "[0:140 by = 1]",
  x_labs_size = 8)
p_gr_cmi_3

# save plot
ggsave("fig/fig_s3.png", p_gr_cmi_3, width = 8, height = 5, dpi = 300)

# ANAYLSIS: EFFECTIVE REPRODUCTION NUMBER VS CMI #

# model: Rt / 2-week lag, unadjusted
mod_R_2 <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE)
summary(mod_R_2)
# mod_R_2_res <- simulateResiduals(mod_R_2) # simulate residuals with DHARMa
# plotQQunif(mod_R_2_res); plotResiduals(mod_R_2_res, quantreg = TRUE) # plot simulated residuals
rep_est_R(mod_R_2, "mean_cmi")
r2_nakagawa(mod_R_2)

# model: Rt / 2-week lag, unadjusted / interaction between CMI and week
mod_R_2_cmi_week_int <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE, cmi_week_int = TRUE)
# summary(mod_R_2_cmi_week_int)

# LRT to compare models with and without interaction between CMI and week
# model without interactions is superior
rep_lrt(anova(mod_R_2, mod_R_2_cmi_week_int))

## model: Rt / 2-week lag, adjusted
mod_R_2_adj <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE)
summary(mod_R_2_adj)
# mod_R_2_adj_res <- simulateResiduals(mod_R_2_adj) # simulate residuals with DHARMa
# plotQQunif(mod_R_2_adj_res); plotResiduals(mod_R_2_adj_res, quantreg = TRUE) # plot simulated residuals
rep_est_R(mod_R_2_adj, "mean_cmi")
rep_est_R(mod_R_2_adj, "days_since_100")
r2_nakagawa(mod_R_2_adj)

## model: Rt / 2-week lag, adjusted
mod_R_2_adj_cmi_week_int <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE, cmi_week_int = TRUE)
# summary(mod_R_2_adj_cmi_week_int)

# LRT to compare models with and without interaction between CMI and week
# model without interactions is superior
rep_lrt(anova(mod_R_2_adj, mod_R_2_adj_cmi_week_int))

## model: Rt / 3-week lag, unadjusted
mod_R_3 <- mod_R(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = FALSE)
summary(mod_R_3)
# mod_R_3_res <- simulateResiduals(mod_R_3) # simulate residuals with DHARMa
# plotQQunif(mod_R_3_res); plotResiduals(mod_R_3_res, quantreg = TRUE) # plot simulated residuals
rep_est_R(mod_R_3, "mean_cmi")
r2_nakagawa(mod_R_3)

## model: Rt / 3-week lag, adjusted
mod_R_3_adj <- mod_R(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = TRUE)
summary(mod_R_3_adj)
# mod_R_3_adj_res <- simulateResiduals(mod_R_3_adj) # simulate residuals with DHARMa
# plotQQunif(mod_R_3_adj_res); plotResiduals(mod_R_3_adj_res, quantreg = TRUE) # plot simulated residuals
rep_est_R(mod_R_3_adj, "mean_cmi")
rep_est_R(mod_R_3_adj, "days_since_100")
r2_nakagawa(mod_R_3_adj)

## LRT to compare unadjusted and adjusted models for 2-week lag
lrt_R_2 <- anova(mod_R_2, mod_R_2_adj)
rep_lrt(lrt_R_2)

## LRT to compare unadjusted and adjusted models for 3-week lag
lrt_R_3 <- anova(mod_R_3, mod_R_3_adj)
rep_lrt(lrt_R_3)

# FIGURE 3: EFFECTIVE REPRODUCTION NUMBER VS CMI WITH LAG OF 2 WEEKS #

# plot
p_R_cmi_2 <- plot_R_cmi(dat, mod = mod_R_2, cmi_lag = 2, outcome_weeks = 3:6)
p_R_cmi_2 # view plot

# save plot
ggsave("fig/fig_3.png", p_R_cmi_2, width = 8, height = 5, dpi = 300)

## SUPPLEMENTARY FIGURE 4: EFFECTIVE REPRODUCTION NUMBER VS CMI WITH LAG OF 2 WEEKS (alternative) ##

# plot
p_R_cmi_2_alt <- plot_R_cmi_2(dat, mod = mod_R_2, cmi_lag = 2, outcome_weeks = 3:6)
p_R_cmi_2_alt # view plot

# save plot
ggsave("fig/fig_s4.png", p_R_cmi_2_alt, width = 8, height = 5, dpi = 300)

# SUPPLEMENTARY FIGURE 5: EFFECTIVE REPRODUCTION NUMBER VS CMI WITH LAG OF 3 WEEKS #

# plot
p_R_cmi_3 <- plot_R_cmi(
  dat, mod = mod_R_3, cmi_lag = 3, outcome_weeks = 4:6,
  x_breaks = c(0, 25, 50, 75, 100, 125),
  x_labs = c(0, 25, 50, 75, 100, 125),
  x_lims = c(0, 140),
  x_pred = "[0:140 by = 1]",
  x_labs_size = 8)
p_R_cmi_3

# save plot
ggsave("fig/fig_s5.png", p_R_cmi_3, width = 8, height = 5, dpi = 300)

# TABLE 1: PRIMARY RESULTS #

# create table with results
tab_1 <- matrix(
  c(
    "", "2-week unadjusted", "2-week adjusted", "3-week unadjusted", "3-week adjusted",
    "Mobility", rep_est(mod_gr_2, "mean_cmi"), rep_est(mod_gr_2_adj, "mean_cmi"), rep_est(mod_gr_3, "mean_cmi"), rep_est(mod_gr_3_adj, "mean_cmi"),
    "Days since 100th case", "", rep_est(mod_gr_2_adj, "days_since_100"), "", rep_est(mod_gr_3_adj, "days_since_100"),
    "Marginal R2", rep_R2(mod_gr_2, "marginal"), rep_R2(mod_gr_2_adj, "marginal"), rep_R2(mod_gr_3, "marginal"), rep_R2(mod_gr_3_adj, "marginal"),
    "Conditional R2", rep_R2(mod_gr_2, "conditional"), rep_R2(mod_gr_2_adj, "conditional"), rep_R2(mod_gr_3, "conditional"), rep_R2(mod_gr_3_adj, "conditional"),
    "Likelihood ratio test", "", rep_lrt(lrt_gr_2), "", rep_lrt(lrt_gr_3),
    "", "2-week unadjusted", "2-week adjusted", "3-week unadjusted", "3-week adjusted",
    "Mobility", rep_est_R(mod_R_2, "mean_cmi"), rep_est_R(mod_R_2_adj, "mean_cmi"), rep_est_R(mod_R_3, "mean_cmi"), rep_est_R(mod_R_3_adj, "mean_cmi"),
    "Days since 100th case", "", rep_est_R(mod_R_2_adj, "days_since_100"), "", rep_est_R(mod_R_3_adj, "days_since_100"),
    "Marginal R2", rep_R2(mod_R_2, "marginal"), rep_R2(mod_R_2_adj, "marginal"), rep_R2(mod_R_3, "marginal"), rep_R2(mod_R_3_adj, "marginal"),
    "Conditional R2", rep_R2(mod_R_2, "conditional"), rep_R2(mod_R_2_adj, "conditional"), rep_R2(mod_R_3, "conditional"), rep_R2(mod_R_3_adj, "conditional"),
    "Likelihood ratio test", "", rep_lrt(lrt_R_2), "", rep_lrt(lrt_R_3)
  ),
  nrow = 12,
  ncol = 5,
  byrow = TRUE
)

# save table
write.table(tab_1, "tab/tab_1.csv", sep = ",", row.names = FALSE, col.names = FALSE)

# SUPPLEMENTARY TABLE 3 #

# re-run primary models using outcome data excluding weeks 3 and 4 (2-week lag)

## model: gr / 2-week lag, unadjusted
mod_gr_2_noweek34 <- mod_gr(dat, cmi_lag = 2, adj = FALSE, outcome_weeks = 5:6)
summary(mod_gr_2_noweek34)
rep_est(mod_gr_2_noweek34, "mean_cmi")

## model: gr / 2-week lag, adjusted
mod_gr_2_adj_noweek34 <- mod_gr(dat, cmi_lag = 2, adj = TRUE, outcome_weeks = 5:6)
summary(mod_gr_2_adj_noweek34)
rep_est(mod_gr_2_adj_noweek34, "mean_cmi")
rep_est(mod_gr_2_adj_noweek34, "days_since_100")

## model: Rt / 2-week lag, unadjusted
mod_R_2_noweek34 <- mod_R(dat, cmi_lag = 2, adj = FALSE, outcome_weeks = 5:6)
summary(mod_R_2_noweek34)
rep_est_R(mod_R_2_noweek34, "mean_cmi")

## model: Rt / 2-week lag, adjusted
mod_R_2_adj_noweek34 <- mod_R(dat, cmi_lag = 2, adj = TRUE, outcome_weeks = 5:6)
summary(mod_R_2_adj_noweek34)
rep_est_R(mod_R_2_adj_noweek34, "mean_cmi")
rep_est_R(mod_R_2_adj_noweek34, "days_since_100")

# re-run primary models using outcome data excluding week 4 (3-week lag)

## model: gr / 3-week lag, unadjusted
mod_gr_3_noweek34 <- mod_gr(dat, cmi_lag = 3, adj = FALSE, outcome_weeks = 5:6)
summary(mod_gr_3_noweek34)
rep_est(mod_gr_3_noweek34, "mean_cmi")

## model: gr / 3-week lag, adjusted
mod_gr_3_adj_noweek34 <- mod_gr(dat, cmi_lag = 3, adj = TRUE, outcome_weeks = 5:6)
summary(mod_gr_3_adj_noweek34)
rep_est(mod_gr_3_adj_noweek34, "mean_cmi")
rep_est(mod_gr_3_adj_noweek34, "days_since_100")

## model: Rt / 3-week lag, unadjusted
mod_R_3_noweek34 <- mod_R(dat, cmi_lag = 3, adj = FALSE, outcome_weeks = 5:6)
summary(mod_R_3_noweek34)
rep_est_R(mod_R_3_noweek34, "mean_cmi")

## model: Rt / 3-week lag, adjusted
mod_R_3_adj_noweek34 <- mod_R(dat, cmi_lag = 3, adj = TRUE, outcome_weeks = 5:6)
summary(mod_R_3_adj_noweek34)
rep_est_R(mod_R_3_adj_noweek34, "mean_cmi")
rep_est_R(mod_R_3_adj_noweek34, "days_since_100")

## LRT to compare unadjusted and adjusted models for 2-week lag
lrt_gr_2_noweek34 <- anova(mod_gr_2_noweek34, mod_gr_2_adj_noweek34)
rep_lrt(lrt_gr_2_noweek34)
lrt_R_2_noweek34 <- anova(mod_R_2_noweek34, mod_R_2_adj_noweek34)
rep_lrt(lrt_R_2_noweek34)

## LRT to compare unadjusted and adjusted models for 3-week lag
lrt_gr_3_noweek34 <- anova(mod_gr_3_noweek34, mod_gr_3_adj_noweek34)
rep_lrt(lrt_gr_3_noweek34)
lrt_R_3_noweek34 <- anova(mod_R_3_noweek34, mod_R_3_adj_noweek34)
rep_lrt(lrt_R_3_noweek34)

# create table with results
tab_s3 <- matrix(
  c(
    "", "2-week unadjusted", "2-week adjusted", "3-week unadjusted", "3-week adjusted",
    "Mobility", rep_est(mod_gr_2_noweek34, "mean_cmi"), rep_est(mod_gr_2_adj_noweek34, "mean_cmi"), rep_est(mod_gr_3_noweek34, "mean_cmi"), rep_est(mod_gr_3_adj_noweek34, "mean_cmi"),
    "Days since 100th case", "", rep_est(mod_gr_2_adj_noweek34, "days_since_100"), "", rep_est(mod_gr_3_adj_noweek34, "days_since_100"),
    "Marginal R2", rep_R2(mod_gr_2_noweek34, "marginal"), rep_R2(mod_gr_2_adj_noweek34, "marginal"), rep_R2(mod_gr_3_noweek34, "marginal"), rep_R2(mod_gr_3_adj_noweek34, "marginal"),
    "Conditional R2", rep_R2(mod_gr_2_noweek34, "conditional"), rep_R2(mod_gr_2_adj_noweek34, "conditional"), rep_R2(mod_gr_3_noweek34, "conditional"), rep_R2(mod_gr_3_adj_noweek34, "conditional"),
    "Likelihood ratio test", "", rep_lrt(lrt_gr_2_noweek34), "", rep_lrt(lrt_gr_3_noweek34),
    "", "2-week unadjusted", "2-week adjusted", "3-week unadjusted", "3-week adjusted",
    "Mobility", rep_est_R(mod_R_2_noweek34, "mean_cmi"), rep_est_R(mod_R_2_adj_noweek34, "mean_cmi"), rep_est_R(mod_R_3_noweek34, "mean_cmi"), rep_est_R(mod_R_3_adj_noweek34, "mean_cmi"),
    "Days since 100th case", "", rep_est_R(mod_R_2_adj_noweek34, "days_since_100"), "", rep_est_R(mod_R_3_adj_noweek34, "days_since_100"),
    "Marginal R2", rep_R2(mod_R_2_noweek34, "marginal"), rep_R2(mod_R_2_adj_noweek34, "marginal"), rep_R2(mod_R_3_noweek34, "marginal"), rep_R2(mod_R_3_adj_noweek34, "marginal"),
    "Conditional R2", rep_R2(mod_R_2_noweek34, "conditional"), rep_R2(mod_R_2_adj_noweek34, "conditional"), rep_R2(mod_R_3_noweek34, "conditional"), rep_R2(mod_R_3_adj_noweek34, "conditional"),
    "Likelihood ratio test", "", rep_lrt(lrt_R_2_noweek34), "", rep_lrt(lrt_R_3_noweek34)
  ),
  nrow = 12,
  ncol = 5,
  byrow = TRUE
)

# save table
write.table(tab_s3, "tab/tab_s3.csv", sep = ",", row.names = FALSE, col.names = FALSE)
