#### 3: Analysis ####

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
library(rstanarm); options(mc.cores = parallel::detectCores())
library(broom.mixed)
library(EpiEstim)
library(flextable)

# set random seed number
seed <- as.integer(as.Date("2020-04-07"))

# load ggplot theme and colour palette for figures
source("theme.R")

# load data

## load cmi data
cmi <- read_csv("data/cmi_clean.csv")

## load case data
cases <- read_csv("data/cases_clean.csv")

## load location data
locs <- read_csv("data/locations.csv")

## load physical distancing data
pdist <- read_csv("data/locations_pdist_dates.csv")

## load country-level case data
cases_country <- read_csv("data/cases_country_clean.csv")

# process case data

## calculate growth rates for unique cities in the dataset

### add ISO week
cases_weekly <- cases %>%
  mutate(week = isoweek(date))

### exclude incomplete weeks
cases_completeness <- cases_weekly %>%
  count(city, week)
cases_weekly <- cases_weekly %>%
  left_join(
    cases_completeness,
    by = c("city", "week")
  ) %>%
  filter(n == 7) %>%
  select(-n)

### aggregate cases by week
cases_weekly <- cases_weekly %>%
  group_by(city, country, continent, week) %>%
  summarize(
    cases = sum(cases),
    cases_cumulative = sum(cases_cumulative),
    .groups = "drop_last"
  ) %>%
  ### calculate weekly growth rate of cases by city
  mutate(
    cases_lag = lag(cases, 1),
    weekly_growth_rate = cases / lag(cases, 1),
  ) %>%
  ungroup()

## function to extract weekly growth rate
gr_week <- function(week, iso_week) {
  assign(
    paste0("gr_week_", week),
    cases_weekly %>%
      filter(week == iso_week) %>%
      transmute(
        city, country, continent,
        !!as.name(paste0("weekly_gr_", week)) := weekly_growth_rate,
        !!as.name(paste0("log_weekly_gr_", week)) := log(weekly_growth_rate)),
    envir = parent.frame()
  )
}

## weekly growth rate in week 3 of March
gr_week(3, 12)

## weekly growth rate in week 4 of March
gr_week(4, 13)

## weekly growth rate in week 5
gr_week(5, 14)

## weekly growth rate in week 6
gr_week(6, 15)

# process cmi data

## report median and IQR of CMI on March 2
cmi %>%
  ungroup %>%
  filter(date == "2020-03-02") %>%
  select(cmi) %>%
  summarize(
    median = quantile(cmi, 0.5),
    iqr = paste0(quantile(cmi, 0.25), "–", quantile(cmi, 0.75)))

## report median and IQR of CMI on March 29
cmi %>%
  ungroup %>%
  filter(date == "2020-03-29") %>%
  select(cmi) %>%
  summarize(
    median = quantile(cmi, 0.5),
    iqr = paste0(quantile(cmi, 0.25), "–", quantile(cmi, 0.75)))

## function to calculate mean cmi for a week
agg_cmi_week <- function(week, date_start, date_end) {
  assign(
    paste0("cmi_week_", week),
      cmi %>%
        filter(date >= date_start & date <= date_end) %>%
        group_by(city, country, continent) %>%
        summarize(!!as.name(paste0("mean_cmi_", week)) := mean(cmi),
                  .groups = "drop"),
    envir = parent.frame()
    )
}

## calculate average cmi in week 1 of March
agg_cmi_week(1, "2020-03-02", "2020-03-08")

## calculate average cmi in week 2
agg_cmi_week(2, "2020-03-09", "2020-03-15")

## calculate average cmi in week 3
agg_cmi_week(3, "2020-03-16", "2020-03-22")

## calculate average cmi in week 4
agg_cmi_week(4, "2020-03-23", "2020-03-29")

# process physical distancing data

## process data
pdist <- pdist %>%
  mutate(pdist_date = as.Date(pdist_date)) %>%
  select(city, country, continent, pdist_date) %>%
  ### join with basic cmi data
  left_join(read_csv("data/cmi_clean.csv"), by = c("city", "country", "continent")) %>%
  ### keep weeks 1-4 of march
  filter(date >= as.Date("2020-03-02") & date <= as.Date("2020-03-29")) %>%
  mutate(
    pdist = factor(
      ifelse(date >= pdist_date, 1, 0),
      levels = c(0, 1),
      labels = c("Before", "After")
    ),
    time = as.integer(date - as.Date("2020-03-02")),
    ### group Americas for the purpose of plotting
    continent = ifelse(
      continent %in% c("North America", "South America"),
      "Americas",
      continent
    )
  )

# additional processing

## join cmi and case data
dat <- inner_join(
  gr_week_3, gr_week_4, by = c("city", "country", "continent")) %>%
  inner_join(gr_week_5, by =  c("city", "country", "continent")) %>%
  inner_join(gr_week_6, by =  c("city", "country", "continent")) %>%
  inner_join(cmi_week_1, by = c("city", "country", "continent")) %>%
  inner_join(cmi_week_2, by = c("city", "country", "continent")) %>%
  inner_join(cmi_week_3, by = c("city", "country", "continent")) %>%
  inner_join(cmi_week_4, by = c("city", "country", "continent"))

## estimate Rt using EpiEstim

### create incidence time series
inci <- cases %>%
  mutate(I = cases) %>%
  select(city, date, I, country, continent)

### trim initial run of 0s
first_cases <- inci %>%
  group_by(city) %>%
  filter(I > 0) %>%
  slice_head(n = 1) %>%
  select(city, first_case = date)
inci <- inci %>%
  left_join(
    first_cases,
    by = c("city")
  ) %>%
  filter(date >= first_case)

### key dates for each incidence time series
inci_dates <- inci %>%
  group_by(city) %>%
  summarize(
    t_3_start = which(date == "2020-03-16"),
    t_3_end = which(date == "2020-03-22"),
    t_4_start = which(date == "2020-03-23"),
    t_4_end = which(date == "2020-03-29"),
    t_5_start = which(date == "2020-03-30"),
    t_5_end = which(date == "2020-04-05"),
    t_6_start = which(date == "2020-04-06"),
    t_6_end = which(date == "2020-04-12"),
    t_max = which(date == "2020-04-12")
    )

### estimate R and extract results
cities <- unique(inci$city)
R <- data.frame(
  city = cities,
  R_week_3 = NA_real_,
  R_week_4 = NA_real_,
  R_week_5 = NA_real_,
  R_week_6 = NA_real_
)
for (i in 1:length(cities)) {
  ### extract dates for city
  t_max <- inci_dates[inci_dates$city == cities[i], "t_max", drop = TRUE]
  t_3_start <- inci_dates[inci_dates$city == cities[i], "t_3_start", drop = TRUE]
  t_3_end <- inci_dates[inci_dates$city == cities[i], "t_3_end", drop = TRUE]
  t_4_start <- inci_dates[inci_dates$city == cities[i], "t_4_start", drop = TRUE]
  t_4_end <- inci_dates[inci_dates$city == cities[i], "t_4_end", drop = TRUE]
  t_5_start <- inci_dates[inci_dates$city == cities[i], "t_5_start", drop = TRUE]
  t_5_end <- inci_dates[inci_dates$city == cities[i], "t_5_end", drop = TRUE]
  t_6_start <- inci_dates[inci_dates$city == cities[i], "t_6_start", drop = TRUE]
  t_6_end <- inci_dates[inci_dates$city == cities[i], "t_6_end", drop = TRUE]
  ### defaults to weekly sliding window
  R_est <- estimate_R(
    inci %>% filter(city == cities[i]),
    method = "parametric_si",
    config = make_config(
      list(t_start = 2:(t_max - 6),
           t_end = 8:t_max,
           mean_si = 3.96, std_si = 4.75)))$R
  ### extract estimates for weeks 3, 4, 5, 6
  R[R$city == cities[i], "R_week_3"] <- R_est[R_est$t_start == t_3_start & R_est$t_end == t_3_end, "Mean(R)"]
  R[R$city == cities[i], "R_week_4"] <- R_est[R_est$t_start == t_4_start & R_est$t_end == t_4_end, "Mean(R)"]
  R[R$city == cities[i], "R_week_5"] <- R_est[R_est$t_start == t_5_start & R_est$t_end == t_5_end, "Mean(R)"]
  R[R$city == cities[i], "R_week_6"] <- R_est[R_est$t_start == t_6_start & R_est$t_end == t_6_end, "Mean(R)"]
}

## join to Rt estimates to data
dat <- dat %>%
  inner_join(R, by = "city")

## calculate days since 100th case for each country
## Monaco is NA because it does not even have even 50 cases by 2020-03-29 (or 100 cases by 2020-04-12)

### days since 100th cases in country
country_100_daily <- cases_country %>%
  group_by(country) %>%
  mutate(cases_cumulative = cumsum(cases)) %>%
  filter(cases_cumulative >= 100) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(country, date_100 = date) %>%
  mutate(
    days_since_100_3 = as.integer(as.Date("2020-03-16") - date_100),
    days_since_100_4 = as.integer(as.Date("2020-03-23") - date_100),
    days_since_100_5 = as.integer(as.Date("2020-03-30") - date_100),
    days_since_100_6 = as.integer(as.Date("2020-04-06") - date_100)
  )

### join to data
dat <- dat %>%
  left_join(
    country_100_daily,
    by = "country")

## define country groups for plotting
dat <- dat %>%
  mutate(
    colour_groups = factor(
      case_when(
        country == "United Kingdom" ~ "UK",
        country == "United States" ~ "US",
        country == "Australia" ~ "AU",
        country == "Canada" ~ "CA",
        country == "Germany" ~ "DE",
        country == "Italy" ~ "IT",
        country == "Spain" ~ "ES",
        country %in% c("Mexico", "Brazil") ~ "Latin America",
        continent == "Asia" ~ "Asia",
        TRUE ~ "Europe (other)"
      ),
      levels = c("AU", "CA", "DE", "ES", "IT", "UK", "US", "Asia", "Europe (other)", "Latin America")
    ))

## censor suspicious data in the week of March 16
dat[dat$city == "Istanbul", c("weekly_gr_3", "log_weekly_gr_3", "R_week_3")] <- NA
dat[dat$city == "Montréal", c("weekly_gr_3", "log_weekly_gr_3", "R_week_3")] <- NA

# figure 1: cmi before and after physical distancing measures announced

## hack for plot to ensure no gap between lines when linetype changes
pdist <- pdist %>%
  bind_rows(
    inner_join(
      pdist %>%
        filter(pdist == "Before") %>%
        group_by(city, country, continent) %>%
        top_n(1, date) %>%
        mutate(date = date + 1) %>%
        select(city, country, continent, date),
      pdist %>%
        filter(pdist == "After") %>%
        group_by(city, country, continent) %>%
        top_n(-1, date) %>%
        select(city, country, continent, cmi) %>%
        mutate(pdist = factor(0, levels = c(0, 1), labels = c("Before", "After"))),
      by = c("city",  "country", "continent")
    )
  )

## plot
plot_cmi_pdist <- ggplot(pdist, aes(x = date, y = cmi, group = city, color = continent)) +
  geom_line(data = filter(pdist, pdist == "Before"), linetype = "solid", alpha = 0.35, linewidth = 0.8) +
  geom_line(data = filter(pdist, pdist == "After"), linetype = "dashed", alpha = 0.35, linewidth = 0.8) +
  ### hack to make linetype aes show up in legend
  geom_line(data = filter(pdist, city == "Toronto") %>% group_by(pdist) %>% top_n(2, date), aes(group = pdist, linetype = pdist), alpha = 0) +
  scale_x_date(breaks = pretty_breaks()) +
  scale_linetype_manual(values = c("Before" = "solid", "After" = "dashed"), guide = guide_legend(
    title.position = "top", override.aes = list(alpha = 1))) +
  scale_color_manual(values = c(
    "Americas" = "#A000FF", "Asia" = "red", "Australia" = "#FF9400","Europe" = "#659EC7"),
                     guide = guide_legend(
                       title.position = "top", override.aes = list(linetype = "solid"))) +
  labs(x = "Date", y = "Mobility index (%)", colour = "Continent", linetype = "Gov't intervention") +
  theme_mobility +
  theme(
    legend.position = c(0.74, 0.87),
    legend.background = element_blank(),
    legend.box = "horizontal",
    legend.box.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
### view plot
plot_cmi_pdist
### save plot
ggsave("fig/fig_1.png", plot_cmi_pdist, width = 7.5, height = 7, dpi = 300)

# analysis: log weekly growth rate vs cmi

## format data for log weekly growth rate analysis
dat_gr <- function(dat, cmi_lag, outcome_weeks = c(3, 4, 5, 6)) {
  
  ### process data
  d <- vector(mode = "list", length = length(outcome_weeks))
  for (week in outcome_weeks) {
    dd <- dat %>%
      select(city, country, colour_groups,
             matches(paste0("^weekly_gr_", week)),
             matches(paste0("^log_weekly_gr_", week)),
             matches(paste0("mean_cmi_", week - cmi_lag)),
             matches(paste0("days_since_100_", week))
      ) %>%
      mutate(
        week_gr = paste("Week", week),
        week_cmi = paste("Week", week - cmi_lag)
      )
    names(dd) <- c("city", "country", "colour_groups", "weekly_gr", "log_weekly_gr", "mean_cmi", "days_since_100", "week_gr", "week_cmi")
    d[[match(week, outcome_weeks)]] <- dd
  }
  bind_rows(d)
}

## function: model for log weekly growth rate with specified lag of cmi
mod_gr <- function(dat, cmi_lag, outcome_weeks, adj = FALSE, cmi_week_int = FALSE) {
  
  ### set seed
  set.seed(seed)
  
  ### create dataset
  d <- dat_gr(dat, cmi_lag, outcome_weeks) %>%
    arrange(city)
  
  ### fit model
  if (!adj) {
    if (!cmi_week_int) {
      stan_lmer(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi, data = d)
    } else {
      stan_lmer(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi, data = d) 
    }
  } else {
    if (!cmi_week_int) {
      stan_lmer(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi + days_since_100, data = d)
    } else {
      stan_lmer(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi + days_since_100, data = d) 
    }
  }
}

## function: report point estimate and 95% confidence inteval for a 10-unit change (10% decrease of CMI, 10-unit increase for other variables)
rep_est <- function(mod, x = "mean_cmi") {
  out <- mod %>% tidy(conf.int = TRUE) %>% filter(term == x)
  if (x == "mean_cmi") {
    print(paste0(sprintf("%.3f", exp(out$estimate * -10)), " (", sprintf("%.3f", exp(out$conf.high * -10)), ", ", sprintf("%.3f", exp(out$conf.low * -10)), ")"))
  } else {
    print(paste0(sprintf("%.3f", exp(out$estimate * 10)), " (", sprintf("%.3f", exp(out$conf.low * 10)), ", ", sprintf("%.3f", exp(out$conf.high * 10)), ")"))
  }
}

## model: gr / 2-week lag, unadjusted
mod_gr_2 <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE)
summary(mod_gr_2)
rep_est(mod_gr_2)

## model: gr / 2-week lag, unadjusted / interaction between CMI and week
mod_gr_2_cmi_week_int <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE, cmi_week_int = TRUE)
fixef(mod_gr_2_cmi_week_int)

## compare models with and without interaction between CMI and week
# model without interactions is superior
loo_compare(loo(mod_gr_2, k_threshold = 0.7), loo(mod_gr_2_cmi_week_int, k_threshold = 0.7))

## model: gr / 2-week lag, adjusted
mod_gr_2_adj <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE)
summary(mod_gr_2_adj)
rep_est(mod_gr_2_adj) # mean_cmi
rep_est(mod_gr_2_adj, "days_since_100" ) # days_since_100

## model: gr / 2-week lag, adjusted / interaction between CMI and week
mod_gr_2_adj_cmi_week_int <- mod_gr(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE, cmi_week_int = TRUE)
fixef(mod_gr_2_adj_cmi_week_int)

## compare models with and without interaction between CMI and week
# model without interactions is superior
loo_compare(loo(mod_gr_2_adj, k_threshold = 0.7), loo(mod_gr_2_adj_cmi_week_int, k_threshold = 0.7))

## model: gr / 3-week lag, unadjusted
mod_gr_3 <- mod_gr(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = FALSE)
summary(mod_gr_3)
rep_est(mod_gr_3)

## model: gr / 3-week lag, adjusted
mod_gr_3_adj <- mod_gr(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = TRUE)
summary(mod_gr_3_adj)
rep_est(mod_gr_3_adj) # mean_cmi
rep_est(mod_gr_3_adj, "days_since_100" ) # days_since_100

# figure 2: log mean growth rate vs cmi

## function to plot mean growth rate vs cmi
fig_gr_cmi <- function(dat, mod, cmi_lag, outcome_weeks, path,
                       x_breaks = c(0, 25, 50, 75, 100),
                       x_labs = c(0, 25, 50, 75, 100),
                       x_lims = c(0, 120),
                       x_pred = "[0:120 by = 20]",
                       x_labs_size = NULL) {
  ### match args
  if (!cmi_lag %in% c(2, 3)) stop("cmi_lag must be 2 or 3.")
  
  ### create dataset
  d <- dat_gr(dat, cmi_lag, outcome_weeks)
  
  ### predicted values for fixed effect of cmi on response scale (growth rate)
  pred_mod <- ggpredict(mod, paste("mean_cmi", x_pred), type = "fe", ppd = TRUE)
  pred_mod$predicted <- exp(pred_mod$predicted) # predicted values: growth rate
  pred_mod$conf.low <- exp(pred_mod$conf.low)
  pred_mod$conf.high <- exp(pred_mod$conf.high)
  
  ### plot
  plot_gr_cmi <- plot(pred_mod) +
    geom_point(data = d, aes(x = mean_cmi,
                             y = exp(log_weekly_gr),
                             colour = colour_groups),
               size = 2) +
    labs(x = paste0("Mean mobility index ", ifelse(cmi_lag == 2, "two", "three"), " weeks prior (%)"),
         y = "Weekly growth rate",
         title = NULL,
         colour = NULL) +
    scale_x_continuous(breaks = x_breaks, limits = x_lims, labels = x_labs) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_color_manual(values = palette_country_groups) +
    theme_mobility +
    guides(
      ### no shading or border in legend
      color = guide_legend(nrow = 2, override.aes = list(fill = NA, linetype = 0))
    ) +
    theme(
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.position = "bottom"
    ) +
    facet_wrap(~ week_gr, ncol = 4, labeller = as_labeller(
      c(
        "Week 3" = "Week of March 16",
        "Week 4" = "Week of March 23",
        "Week 5" = "Week of March 30",
        "Week 6" = "Week of April 6"
      )
    ))
  
  ### override x-axis label size
  if (!is.null(x_labs_size)) {
    plot_gr_cmi <- plot_gr_cmi + theme(
      axis.text.x = element_text(size = x_labs_size)
    )
  }
  
  ### assign plot
  assign("plot_gr_cmi", plot_gr_cmi, envir = parent.frame())
  
  ### view plot
  plot(plot_gr_cmi)
  
  ### save plot
  ggsave(path, plot_gr_cmi, width = 8, height = 5, dpi = 300)
  
}

## fig 2: log growth rate vs cmi with lag of 2 weeks
fig_gr_cmi(dat, mod = mod_gr_2, cmi_lag = 2, outcome_weeks = 3:6, path = "fig/fig_2.png")

# analysis: effective reproduction number vs cmi

## format data for log mean growth rate analysis
dat_R <- function(dat, cmi_lag, outcome_weeks) {
  
  ### process data
  d <- vector(mode = "list", length = length(outcome_weeks))
  for (week in outcome_weeks) {
    
    dd <- dat %>%
      select(city, country, colour_groups,
             matches(paste0("R_week_", week)),
             matches(paste0("mean_cmi_", week - cmi_lag)),
             matches(paste0("days_since_100_", week))
      ) %>%
      mutate(
        week_R = paste("Week", week),
        week_cmi = paste("Week", week - cmi_lag)
      )
    names(dd) <- c("city", "country", "colour_groups", "R", "mean_cmi", "days_since_100", "week_R", "week_cmi")
    d[[match(week, outcome_weeks)]] <- dd
    
  }
  bind_rows(d)
  
}

## function: model for Rt with specified lag of cmi
mod_R <- function(dat, cmi_lag, outcome_weeks, adj = FALSE, cmi_week_int = FALSE) {
  
  ### set seed
  set.seed(seed)
  
  ### create dataset
  d <- dat_R(dat, cmi_lag, outcome_weeks)
  
  ### fit model
  if (!adj) {
    if (!cmi_week_int) {
      stan_lmer(R ~ (1 | country) + (1 | country:city) + mean_cmi, data = d)
    } else {
      stan_lmer(R ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi, data = d)
    }
    } else {
      if (!cmi_week_int) {
        stan_lmer(R ~ (1 | country) + (1 | country:city) + mean_cmi + days_since_100, data = d)
      } else {
        stan_lmer(R ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi + days_since_100, data = d)
      }
    }
  
}

## function: report point estimate and 95% confidence inteval for a 10-unit change (10% decrease of CMI, 10-unit increase for other variables)
rep_est_R <- function(mod, x = "mean_cmi") {
  out <- mod %>% tidy(conf.int = TRUE) %>% filter(term == x)
  if (x == "mean_cmi") {
    print(paste0(sprintf("%.3f", out$estimate * -10), " (", sprintf("%.3f", out$conf.high * -10), ", ", sprintf("%.3f", out$conf.low * -10), ")"))
  } else {
    print(paste0(sprintf("%.3f", out$estimate * 10), " (", sprintf("%.3f", out$conf.low * 10), ", ", sprintf("%.3f", out$conf.high * 10), ")"))
  }
}

## model: Rt / 2-week lag, unadjusted
mod_R_2 <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE)
summary(mod_R_2)
rep_est_R(mod_R_2)

## model: Rt / 2-week lag, unadjusted / interaction between CMI and week
mod_R_2_cmi_week_int <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = FALSE, cmi_week_int = TRUE)
fixef(mod_R_2_cmi_week_int)

## compare models with and without interaction between CMI and week
# model without interactions is superior
loo_compare(loo(mod_R_2, k_threshold = 0.7), loo(mod_R_2_cmi_week_int, k_threshold = 0.7))

## model: Rt / 2-week lag, adjusted
mod_R_2_adj <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE)
summary(mod_R_2_adj)
rep_est_R(mod_R_2_adj) # mean_cmi
rep_est_R(mod_R_2_adj, "days_since_100") # days_since_100

## model: Rt / 2-week lag, adjusted
mod_R_2_adj_cmi_week_int <- mod_R(dat, cmi_lag = 2, outcome_weeks = 3:6, adj = TRUE, cmi_week_int = TRUE)
fixef(mod_R_2_adj_cmi_week_int)

## compare models with and without interaction between CMI and week
# model without interactions is superior
loo_compare(loo(mod_R_2_adj, k_threshold = 0.7), loo(mod_R_2_adj_cmi_week_int, k_threshold = 0.7))

## model: Rt / 3-week lag, unadjusted
mod_R_3 <- mod_R(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = FALSE)
summary(mod_R_3)
rep_est_R(mod_R_3) # mean_cmi

## model: Rt / 3-week lag, adjusted
mod_R_3_adj <- mod_R(dat, cmi_lag = 3, outcome_weeks = 4:6, adj = TRUE)
summary(mod_R_3_adj)
rep_est_R(mod_R_3_adj) # mean_cmi
rep_est_R(mod_R_3_adj, "days_since_100") # days_since_100

# figure 3: effective reproduction number vs cmi

## function to generate figure for effective reproduction number vs cmi
fig_R_cmi <- function(dat, mod, cmi_lag, outcome_weeks = c(3, 4, 5, 6), path,
                      x_breaks = c(0, 25, 50, 75, 100),
                      x_labs = c(0, 25, 50, 75, 100),
                      x_lims = c(0, 120),
                      x_pred = "[0:120 by = 20]",
                      x_labs_size = NULL) {
  ### match args
  if (!cmi_lag %in% c(2, 3)) stop("cmi_lag must be 2 or 3.")
  
  ### create dataset
  d <- dat_R(dat, cmi_lag, outcome_weeks)
  
  ### predicted values for fixed effect of cmi on response scale (growth rate)
  pred_mod <- ggpredict(mod, paste("mean_cmi", x_pred), type = "fe", ppd = TRUE) # predicted values: Rt
  
  ### plot
  plot_R_cmi <- plot(pred_mod) +
    geom_point(data = d, aes(x = mean_cmi,
                             y = R,
                             colour = colour_groups),
               size = 2) +
    labs(x = paste0("Mean mobility index ", ifelse(cmi_lag == 2, "two", "three"), " weeks prior (%)"),
         y = paste0("Effective reproduction number"),
         title = NULL,
         colour = NULL) +
    scale_x_continuous(breaks = x_breaks, limits = x_lims, labels = x_labs) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_color_manual(values = palette_country_groups) +
    theme_mobility +
    guides(
      color = guide_legend(nrow = 2, override.aes = list(fill = NA, linetype = 0)) # no shading or border in legend
    ) +
    theme(
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.position = "bottom"
    ) +
    facet_wrap(~ week_R, ncol = 4, labeller = as_labeller(
      c(
        "Week 3" = "Week of March 16",
        "Week 4" = "Week of March 23",
        "Week 5" = "Week of March 30",
        "Week 6" = "Week of April 6"
      )
    ))
  
  ### override x-axis label size
  if (!is.null(x_labs_size)) {
    plot_R_cmi <- plot_R_cmi + theme(
      axis.text.x = element_text(size = x_labs_size)
    )
  }
  
  ### assign plot
  assign("plot_R_cmi", plot_R_cmi, envir = parent.frame())
  
  ### view plot
  plot(plot_R_cmi)
  
  ### save plot
  ggsave(path, plot_R_cmi, width = 8, height = 5, dpi = 300)

}

## fig 3: Rt vs cmi with lag of 2 weeks
fig_R_cmi(dat, mod = mod_R_2, cmi_lag = 2, outcome_weeks = 3:6, path = "fig/fig_3.png")

# table 1: primary results

## save results
tab_1 <- data.frame(
  lag = c("2-week lag", "3-week lag"), 
  matrix(
    c(
      rep_est(mod_gr_2), rep_est(mod_gr_2_adj), rep_est(mod_gr_2_adj, "days_since_100"), rep_est_R(mod_R_2), rep_est_R(mod_R_2_adj), rep_est_R(mod_R_2_adj, "days_since_100"),
      rep_est(mod_gr_3), rep_est(mod_gr_3_adj), rep_est(mod_gr_3_adj, "days_since_100"), rep_est_R(mod_R_3), rep_est_R(mod_R_3_adj), rep_est_R(mod_R_3_adj, "days_since_100")
      ),
    nrow = 2,
    ncol = 6,
    byrow = TRUE
  ))
names(tab_1) <- c("Mobility index", "GR/Unadjusted/CMI", "GR/Adjusted/CMI", "GR/Adjusted/Days ", "Rt/Unadjusted/CMI", "Rt/Adjusted/CMI", "Rt/Adjusted/Days")

## format table (not publication-ready)
tab_1 <- tab_1 %>%
  flextable %>%
  bold(part = "header", bold = TRUE) %>%
  fontsize(size = 7, part = "header") %>%
  fontsize(size = 7, part = "body") %>%
  autofit

## save table (not publication-ready)
save_as_docx(tab_1, path = "tab/tab_1.docx")

# supplementary analyses

# supplementary table 3

## re-run primary models using outcome data excluding weeks 3 and 4 (2-week lag)

### growth rate - unadjusted
mod_gr_2_noweek34 <- mod_gr(dat, cmi_lag = 2, adj = FALSE, outcome_weeks = 5:6)
summary(mod_gr_2_noweek34)
rep_est(mod_gr_2_noweek34)

### growth_rate - adjusted
mod_gr_2_adj_noweek34 <- mod_gr(dat, cmi_lag = 2, adj = TRUE, outcome_weeks = 5:6)
summary(mod_gr_2_adj_noweek34)
rep_est(mod_gr_2_adj_noweek34) # mean_cmi
rep_est(mod_gr_2_adj_noweek34, "days_since_100")

### R - unadjusted
mod_R_2_noweek34 <- mod_R(dat, cmi_lag = 2, adj = FALSE, outcome_weeks = 5:6)
summary(mod_R_2_noweek34)
rep_est_R(mod_R_2_noweek34)

### R - adjusted
mod_R_2_adj_noweek34 <- mod_R(dat, cmi_lag = 2, adj = TRUE, outcome_weeks = 5:6)
summary(mod_R_2_adj_noweek34)
rep_est_R(mod_R_2_adj_noweek34) # mean_cmi
rep_est_R(mod_R_2_adj_noweek34, "days_since_100")

## re-run primary models using outcome data excluding week 4 (3-week lag)

### growth rate - unadjusted
mod_gr_3_noweek34 <- mod_gr(dat, cmi_lag = 3, adj = FALSE, outcome_weeks = 5:6)
summary(mod_gr_3_noweek34)
rep_est(mod_gr_3_noweek34)

### growth_rate - adjusted
mod_gr_3_adj_noweek34 <- mod_gr(dat, cmi_lag = 3, adj = TRUE, outcome_weeks = 5:6)
summary(mod_gr_3_adj_noweek34)
rep_est(mod_gr_3_adj_noweek34) # mean_cmi
rep_est(mod_gr_3_adj_noweek34, "days_since_100")

### R - unadjusted
mod_R_3_noweek34 <- mod_R(dat, cmi_lag = 3, adj = FALSE, outcome_weeks = 5:6)
summary(mod_R_3_noweek34)
rep_est_R(mod_R_3_noweek34)

### R - adjusted
mod_R_3_adj_noweek34 <- mod_R(dat, cmi_lag = 3, adj = TRUE, outcome_weeks = 5:6)
summary(mod_R_3_adj_noweek34)
rep_est_R(mod_R_3_adj_noweek34) # mean_cmi
rep_est_R(mod_R_3_adj_noweek34, "days_since_100")

### save results
tab_s3 <- data.frame(
  lag = c("2-week lag", "3-week lag"), 
  matrix(
    c(
      rep_est(mod_gr_2_noweek34), rep_est(mod_gr_2_adj_noweek34), rep_est(mod_gr_2_adj_noweek34, "days_since_100"), rep_est_R(mod_R_2_noweek34), rep_est_R(mod_R_2_adj_noweek34), rep_est_R(mod_R_2_adj_noweek34, "days_since_100"),
      rep_est(mod_gr_3_noweek34), rep_est(mod_gr_3_adj_noweek34), rep_est(mod_gr_3_adj_noweek34, "days_since_100"), rep_est_R(mod_R_3_noweek34), rep_est_R(mod_R_3_adj_noweek34), rep_est_R(mod_R_3_adj_noweek34, "days_since_100")
    ),
    nrow = 2,
    ncol = 6,
    byrow = TRUE
  ))
names(tab_s3) <- c("Mobility index", "GR/Unadjusted/CMI", "GR/Adjusted/CMI", "GR/Adjusted/Days ", "Rt/Unadjusted/CMI", "Rt/Adjusted/CMI", "Rt/Adjusted/Days")

## format table (not publication-ready)
tab_s3 <- tab_s3 %>%
  flextable %>%
  bold(part = "header", bold = TRUE) %>%
  fontsize(size = 7, part = "header") %>%
  fontsize(size = 7, part = "body") %>%
  autofit

## save table (not publication-ready)
save_as_docx(tab_s3, path = "tab/tab_s3.docx")

# supplementary figures 1 and 2 - outcomes with 3-week lag

## fig s1: growth rate vs cmi with lag of 3 weeks
fig_gr_cmi(dat, mod = mod_gr_3, cmi_lag = 3, outcome_weeks = 4:6, path = "fig/fig_s1.png",
           x_breaks = c(0, 25, 50, 75, 100, 125),
           x_labs = c(0, 25, 50, 75, 100, 125),
           x_lims = c(0, 140),
           x_pred = "[0:140 by = 20]",
           x_labs_size = 8)

## fig s2: Rt vs cmi with lag of 3 weeks
fig_R_cmi(dat, mod = mod_R_3, cmi_lag = 3, outcome_weeks = 4:6, path = "fig/fig_s2.png",
          x_breaks = c(0, 25, 50, 75, 100, 125),
          x_labs = c(0, 25, 50, 75, 100, 125),
          x_lims = c(0, 140),
          x_pred = "[0:140 by = 20]",
          x_labs_size = 8)