#### 3: Analysis ####

# create directories for output (if they don't already exist)
dir.create("fig", showWarnings = FALSE)
dir.create("tab", showWarnings = FALSE)

# load libraries
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(ggplot2) # visualization
library(scales) # ggplot add-on
library(ggeffects) # ggplot add-on
library(rstanarm); options(mc.cores = parallel::detectCores()) # bayesian models (version 2.19.3)
library(broom.mixed) # tidy output from mixed models
library(EpiEstim) # estimate Rt
library(flextable) # generate Word tables

# set random seed number
seed <- as.integer(as.Date("2020-04-07"))

# load ggplot theme and colour palette for figures
source("theme.R")

# load data

## load cmi data
cmi <- read.csv("data/cmi_clean.csv",
                stringsAsFactors = FALSE,
                colClasses = c("date" = "Date"))

## load cumulative case data
cases <- read.csv("data/cases_clean.csv",
                  stringsAsFactors = FALSE,
                  colClasses = c("date" = "Date"))

## load metro population data for cities
pop_metro <- read.csv("data/locations_pop_metro.csv",
                      stringsAsFactors = FALSE,
                      colClasses = c("state" = "character"))

## load physical distancing data
pdist <- read.csv("data/locations_pdist_dates.csv",
                  stringsAsFactors = FALSE)

# process case data

## calculate growth rates for unique locations in the dataset
cases <- cases %>%
  ### create unique location identifier based on country + state
  mutate(location = trimws(paste(country, state), "right")) %>%
  ### calculate daily growth rate of cases by location
  group_by(location) %>%
  mutate(
    daily_diff = cases - lag(cases, 1),
    daily_growth_rate = daily_diff / lag(cases, 1) * 100) %>%
  ### replace NaN, Inf, NA values with 0
  mutate(
    daily_diff = ifelse(is.finite(daily_diff), daily_diff, 0),
    daily_growth_rate = ifelse(is.finite(daily_growth_rate), daily_growth_rate, 0)
  )

## function to calculate mean and median daily growth rate for a week
agg_gr_week <- function(week, date_start, date_end) {
  assign(
    paste0("gr_week_", week),
    cases %>%
      filter(date >= date_start & date <= date_end) %>%
      group_by(location, country, continent) %>%
      summarise(!!as.name(paste0("mean_gr_", week)) := mean(daily_growth_rate),
                !!as.name(paste0("log_mean_gr_", week)) := log(!!as.name(paste0("mean_gr_", week))),
                !!as.name(paste0("median_gr_", week)) := median(daily_growth_rate),
                !!as.name(paste0("log_median_gr_", week)) := log(!!as.name(paste0("median_gr_", week)))),
    envir = parent.frame()
  )
}

## calculate mean and median daily growth rate in week 4 of March
agg_gr_week(4, "2020-03-23", "2020-03-29")

## calculate mean and median daily growth rate in week 5
agg_gr_week(5, "2020-03-30", "2020-04-05")

## calculate mean and median daily growth rate in week 6
agg_gr_week(6, "2020-04-06", "2020-04-12")

# process cmi data

## report average (sd) cmi on March 2
cmi %>%
  ungroup %>%
  filter(date == "2020-03-02") %>%
  select(cmi) %>%
  summarise(mean = mean(cmi), sd = sd(cmi))

## report average (sd) cmi on March 29
cmi %>%
  ungroup %>%
  filter(date == "2020-03-29") %>%
  select(cmi) %>%
  summarise(mean = mean(cmi), sd = sd(cmi))

## weighted average of cmi when >1 cmi time series for one outcome (i.e. France - two cities but no regional data)
cmi <- cmi %>%
  mutate(location = trimws(paste(country, state), "right")) %>%
  left_join(pop_metro, by = c("city", "state", "country", "continent", "location")) %>%
  replace_na(list(pop_metro = 1)) %>%
  group_by(location, country, continent, date) %>%
  summarise(cmi = as.integer(round(weighted.mean(cmi, pop_metro), 0)))

## function to calculate mean cmi for a week
agg_cmi_week <- function(week, date_start, date_end) {
  assign(
    paste0("cmi_week_", week),
      cmi %>%
        filter(date >= date_start & date <= date_end) %>%
        group_by(location, country, continent) %>%
        summarise(!!as.name(paste0("mean_cmi_", week)) := mean(cmi)),
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
  select(city, state, country, continent, pdist_date) %>%
  ### join with basic cmi data
  left_join(read.csv("data/cmi_clean.csv",
                     stringsAsFactors = FALSE,
                     colClasses = c("date" = "Date")), by = c("city", "state", "country", "continent")) %>%
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
  gr_week_4, cmi_week_2, by = c("location", "country", "continent")) %>%
  inner_join(gr_week_5, by =  c("location", "country", "continent")) %>%
  inner_join(gr_week_6, by =  c("location", "country", "continent")) %>%
  inner_join(cmi_week_1, by = c("location", "country", "continent")) %>%
  inner_join(cmi_week_3, by = c("location", "country", "continent")) %>%
  inner_join(cmi_week_4, by = c("location", "country", "continent"))

## add variable for days since 100th case
## Monaco is NA because it does not even have even 50 cases by 2020-03-29 (or 100 cases by 2020-04-12)
dat <- dat %>%
left_join(
  cases %>%
    group_by(location) %>%
    filter(cases >= 100, date <= "2020-03-29") %>%
    top_n(-1, date) %>%
    ### time since 100th case - end of each week
    mutate(days_since_100_4 = as.integer(as.Date("2020-03-29") - date),
           days_since_100_5 = days_since_100_4 + 7 * 1,
           days_since_100_6 = days_since_100_4 + 7 * 2) %>%
    select(location, days_since_100_4, days_since_100_5, days_since_100_6),
  by = "location"
)

## estimate Rt using EpiEstim
inci <- cases %>%
  filter(date >= "2020-03-08", date <= "2020-04-12") %>%
  mutate(I = daily_diff) %>%
  select(location, date, I) %>%
  filter(location %in% dat$location)
locs <- unique(inci$location)
R <- data.frame(
  location = locs,
  R_week_4 = numeric(length = length(locs)),
  R_week_5 = numeric(length = length(locs)),
  R_week_6 = numeric(length = length(locs)),
  stringsAsFactors = FALSE
)
for (i in 1:length(locs)) {
  ### defaults to weekly sliding window
  R_est <- estimate_R(inci %>% filter(location == locs[i]), method = "parametric_si",
                      config = make_config(list(t_start = 2:30, t_end = 8:36, mean_si = 3.96, std_si = 4.75)))$R
  ### extract estimates for weeks 4, 5, 6
  R[R$location == locs[i], "R_week_4"] <- R_est[R_est$t_start == 16 & R_est$t_end == 22, "Mean(R)"]
  R[R$location == locs[i], "R_week_5"] <- R_est[R_est$t_start == 23 & R_est$t_end == 29, "Mean(R)"]
  R[R$location == locs[i], "R_week_6"] <- R_est[R_est$t_start == 30 & R_est$t_end == 36, "Mean(R)"]
}

## join to Rt estimation to data
dat <- dat %>%
  inner_join(R, by = "location")

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

# figure 1: cmi before and after physical distancing measures announced

## hack for plot to ensure no gap between lines when linetype changes
pdist <- pdist %>%
  bind_rows(
    inner_join(
      pdist %>%
        filter(pdist == "Before") %>%
        group_by(city, state, country, continent) %>%
        top_n(1, date) %>%
        mutate(date = date + 1) %>%
        select(city, state, country, continent, date),
      pdist %>%
        filter(pdist == "After") %>%
        group_by(city, state, country, continent) %>%
        top_n(-1, date) %>%
        select(city, state, country, continent, cmi) %>%
        mutate(pdist = factor(0, levels = c(0, 1), labels = c("Before", "After"))),
      by = c("city", "state", "country", "continent")
    )
  )

## plot
plot_cmi_pdist <- ggplot(pdist, aes(x = date, y = cmi, group = city, color = continent)) +
  geom_line(data = filter(pdist, pdist == "Before"), linetype = "solid", alpha = 0.35, size = 0.8) +
  geom_line(data = filter(pdist, pdist == "After"), linetype = "dashed", alpha = 0.35, size = 0.8) +
  ### hack to make linetype aes show up in legend
  geom_line(data = filter(pdist, city == "Toronto") %>% group_by(pdist) %>% top_n(2, date), aes(group = pdist, linetype = pdist), alpha = 0) +
  scale_x_date(breaks = pretty_breaks()) +
  scale_linetype_manual(values = c("Before" = "solid", "After" = "dashed"), guide = guide_legend(
    title.position = "top", override.aes = list(alpha = 1))) +
  scale_color_manual(values = c(
    "Americas" = "#A000FF", "Asia" = "red", "Australia" = "#FF9400","Europe" = "#659EC7"),
                     guide = guide_legend(
                       title.position = "top", override.aes = list(linetype = "solid"))) +
  labs(x = "Date", y = "Mobility index (%)", colour = "Continent", linetype = "Phys. distancing") +
  theme_mobility +
  theme(
    legend.position = c(0.78, 0.87),
    legend.background = element_blank(),
    legend.box = "horizontal",
    legend.box.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
### view plot
plot_cmi_pdist
### save plot
ggsave("fig/fig_1.png", plot_cmi_pdist, width = 7.5, height = 7, dpi = 300)

# analysis: log mean growth rate vs cmi

## format data for log mean growth rate analysis
dat_gr <- function(dat, cmi_lag, outcome_weeks = c(4, 5, 6), stat = "mean") {
  
  ### verify stat is valid
  match.arg(stat, c("mean", "median"))
  
  ### process data
  if (stat == "mean") {
    d <- vector(mode = "list", length = length(outcome_weeks))
    for (week in outcome_weeks) {
      dd <- dat %>%
        select(location, country, colour_groups,
               matches(paste0("log_mean_gr_", week)),
               matches(paste0("mean_cmi_", week - cmi_lag)),
               matches(paste0("days_since_100_", week))
        ) %>%
        mutate(
          week_gr = paste("Week", week),
          week_cmi = paste("Week", week - cmi_lag)
        )
      names(dd) <- c("location", "country", "colour_groups", "log_mean_gr", "mean_cmi", "days_since_100", "week_gr", "week_cmi")
      d[[match(week, outcome_weeks)]] <- dd
    }
    bind_rows(d)
  } else if (stat == "median") {
    d <- vector(mode = "list", length = length(outcome_weeks))
    for (week in outcome_weeks) {
      dd <- dat %>%
        select(location, country, colour_groups,
               matches(paste0("log_median_gr_", week)),
               matches(paste0("mean_cmi_", week - cmi_lag)),
               matches(paste0("days_since_100_", week))
        ) %>%
        mutate(
          week_gr = paste("Week", week),
          week_cmi = paste("Week", week - cmi_lag)
        )
      names(dd) <- c("location", "country", "colour_groups", "log_median_gr", "mean_cmi", "days_since_100", "week_gr", "week_cmi")
      d[[match(week, outcome_weeks)]] <- dd
    }
    bind_rows(d)
  }
}

## function: model for log mean growth rate with specified lag of cmi
mod_gr <- function(dat, cmi_lag, outcome_weeks = c(4, 5, 6), stat = "mean", adj = FALSE) {
  
  ### set seed
  set.seed(seed)
  
  ### create dataset
  d <- dat_gr(dat, cmi_lag, outcome_weeks, stat) %>%
    arrange(location)
  
  ### fit model
  if (stat == "mean") {
    if (!adj) {
      stan_lmer(log_mean_gr ~ (1 | country) + (1 | country:location) + mean_cmi, data = d)
    } else {
      stan_lmer(log_mean_gr ~ (1 | country) + (1 | country:location) + mean_cmi + days_since_100, data = d)
    }
  } else if (stat == "median") {
    if (!adj) {
      stan_lmer(log_median_gr ~ (1 | country) + (1 | country:location) + mean_cmi, data = d)
    } else {
      stan_lmer(log_median_gr ~ (1 | country) + (1 | country:location) + mean_cmi + days_since_100, data = d)
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
mod_gr_2 <- mod_gr(dat, cmi_lag = 2, adj = FALSE)
summary(mod_gr_2)
rep_est(mod_gr_2)

## model: gr / 2-week lag, adjusted
mod_gr_2_adj <- mod_gr(dat, cmi_lag = 2, adj = TRUE)
summary(mod_gr_2_adj)
rep_est(mod_gr_2_adj) # mean_cmi
rep_est(mod_gr_2_adj, "days_since_100" ) # days_since_100

## model: gr / 3-week lag, unadjusted
mod_gr_3 <- mod_gr(dat, cmi_lag = 3, adj = FALSE)
summary(mod_gr_3)
rep_est(mod_gr_3)

## model: gr / 3-week lag, adjusted
mod_gr_3_adj <- mod_gr(dat, cmi_lag = 3, adj = TRUE)
summary(mod_gr_3_adj)
rep_est(mod_gr_3_adj) # mean_cmi
rep_est(mod_gr_3_adj, "days_since_100" ) # days_since_100

# figure 2: log mean growth rate vs cmi

## function to plot mean growth rate vs cmi
fig_gr_cmi <- function(dat, mod, cmi_lag, outcome_weeks = c(4, 5, 6), path,
                       x_breaks = c(0, 25, 50, 75, 100),
                       x_labs = c(0, 25, 50, 75, 100),
                       x_lims = c(0, 120),
                       x_pred = "[0:120 by = 20]",
                       x_labs_size = NULL) {
  
  ### create dataset
  d <- dat_gr(dat, cmi_lag, outcome_weeks)
  
  ### predicted values for fixed effect of cmi on response scale (growth rate)
  pred_mod <- ggpredict(mod, paste("mean_cmi", x_pred), type = "fe", ppd = TRUE) # predicted values: log growth rate
  pred_mod$predicted <- exp(pred_mod$predicted) # predicted values: growth rate
  pred_mod$conf.low <- exp(pred_mod$conf.low)
  pred_mod$conf.high <- exp(pred_mod$conf.high)
  
  ### plot
  plot_gr_cmi <- plot(pred_mod) +
    geom_point(data = d, aes(x = mean_cmi,
                             y = exp(log_mean_gr),
                             colour = colour_groups),
               size = 2) +
    labs(x = "Mean mobility index (%)",
         y = "Mean daily growth rate (%)",
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
  ggsave(path, plot_gr_cmi, width = 5, height = 5, dpi = 300)
  
}

## fig 2: log growth rate vs cmi with lag of 2 weeks
fig_gr_cmi(dat, mod = mod_gr_2, cmi_lag = 2, path = "fig/fig_2.png")

# analysis: instantaneous reproductive number vs cmi

## format data for log mean growth rate analysis
dat_R <- function(dat, cmi_lag, outcome_weeks = c(4, 5, 6)) {
  
  ### process data
  d <- vector(mode = "list", length = length(outcome_weeks))
  for (week in outcome_weeks) {
    
    dd <- dat %>%
      select(location, country, colour_groups,
             matches(paste0("R_week_", week)),
             matches(paste0("mean_cmi_", week - cmi_lag)),
             matches(paste0("days_since_100_", week))
      ) %>%
      mutate(
        week_R = paste("Week", week),
        week_cmi = paste("Week", week - cmi_lag)
      )
    names(dd) <- c("location", "country", "colour_groups", "R", "mean_cmi", "days_since_100", "week_R", "week_cmi")
    d[[match(week, outcome_weeks)]] <- dd
    
  }
  bind_rows(d)
  
}

## function: model for Rt with specified lag of cmi
mod_R <- function(dat, cmi_lag, outcome_weeks = c(4, 5, 6), adj = FALSE) {
  
  ### set seed
  set.seed(seed)
  
  ### create dataset
  d <- dat_R(dat, cmi_lag, outcome_weeks)
  
  ### fit model
  if (!adj) {
      stan_lmer(R ~ (1 | country) + (1 | country:location) + mean_cmi, data = d)
    } else {
      stan_lmer(R ~ (1 | country) + (1 | country:location) + mean_cmi + days_since_100, data = d)
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
mod_R_2 <- mod_R(dat, cmi_lag = 2, adj = FALSE)
summary(mod_R_2)
rep_est_R(mod_R_2)

## model: Rt / 2-week lag, adjusted
mod_R_2_adj <- mod_R(dat, cmi_lag = 2, adj = TRUE)
summary(mod_R_2_adj)
rep_est_R(mod_R_2_adj) # mean_cmi
rep_est_R(mod_R_2_adj, "days_since_100") # days_since_100

## model: Rt / 3-week lag, unadjusted
mod_R_3 <- mod_R(dat, cmi_lag = 3, adj = FALSE)
summary(mod_R_3)
rep_est_R(mod_R_3) # mean_cmi

## model: Rt / 3-week lag, adjusted
mod_R_3_adj <- mod_R(dat, cmi_lag = 3, adj = TRUE)
summary(mod_R_3_adj)
rep_est_R(mod_R_3_adj) # mean_cmi
rep_est_R(mod_R_3_adj, "days_since_100") # days_since_100

# figure 3: instantaneous reproductive number vs cmi

## function to generate figure for instantaneous reproductive number vs cmi
fig_R_cmi <- function(dat, mod, cmi_lag, outcome_weeks = c(4, 5, 6), path,
                      x_breaks = c(0, 25, 50, 75, 100),
                      x_labs = c(0, 25, 50, 75, 100),
                      x_lims = c(0, 120),
                      x_pred = "[0:120 by = 20]",
                      x_labs_size = NULL) {
  
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
    labs(x = paste0("Mean mobility index (%)"),
         y = paste0("Mean reproductive number"),
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
  ggsave(path, plot_R_cmi, width = 5, height = 5, dpi = 300)

}

## fig 3: Rt vs cmi with lag of 2 weeks
fig_R_cmi(dat, mod = mod_R_2, cmi_lag = 2, path = "fig/fig_3.png")

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

## re-run primary growth rate model using median growth rate

### unadjusted
mod_gr_2_median <- mod_gr(dat, cmi_lag = 2, stat = "median", adj = FALSE)
summary(mod_gr_2_median)
rep_est(mod_gr_2_median)

# supplementary table 1

## re-run primary models using outcome data excluding week 4 (2-week lag)

### growth rate - unadjusted
mod_gr_2_noweek4 <- mod_gr(dat, cmi_lag = 2, adj = FALSE, outcome_weeks = c(5, 6))
summary(mod_gr_2_noweek4)
rep_est(mod_gr_2_noweek4)

### growth_rate - adjusted
mod_gr_2_adj_noweek4 <- mod_gr(dat, cmi_lag = 2, adj = TRUE, outcome_weeks = c(5, 6))
summary(mod_gr_2_adj_noweek4)
rep_est(mod_gr_2_adj_noweek4) # mean_cmi
rep_est(mod_gr_2_adj_noweek4, "days_since_100")

### R - unadjusted
mod_R_2_noweek4 <- mod_R(dat, cmi_lag = 2, adj = FALSE, outcome_weeks = c(5, 6))
summary(mod_R_2_noweek4)
rep_est_R(mod_R_2_noweek4)

### R - adjusted
mod_R_2_adj_noweek4 <- mod_R(dat, cmi_lag = 2, adj = TRUE, outcome_weeks = c(5, 6))
summary(mod_R_2_adj_noweek4)
rep_est_R(mod_R_2_adj_noweek4) # mean_cmi
rep_est_R(mod_R_2_adj_noweek4, "days_since_100")

## re-run primary models using only outcome data excluding week 4 (3-week lag)

### growth rate - unadjusted
mod_gr_3_noweek4 <- mod_gr(dat, cmi_lag = 3, adj = FALSE, outcome_weeks = c(5, 6))
summary(mod_gr_3_noweek4)
rep_est(mod_gr_3_noweek4)

### growth_rate - adjusted
mod_gr_3_adj_noweek4 <- mod_gr(dat, cmi_lag = 3, adj = TRUE, outcome_weeks = c(5, 6))
summary(mod_gr_3_adj_noweek4)
rep_est(mod_gr_3_adj_noweek4) # mean_cmi
rep_est(mod_gr_3_adj_noweek4, "days_since_100")

### R - unadjusted
mod_R_3_noweek4 <- mod_R(dat, cmi_lag = 3, adj = FALSE, outcome_weeks = c(5, 6))
summary(mod_R_3_noweek4)
rep_est_R(mod_R_3_noweek4)

### R - adjusted
mod_R_3_adj_noweek4 <- mod_R(dat, cmi_lag = 3, adj = TRUE, outcome_weeks = c(5, 6))
summary(mod_R_3_adj_noweek4)
rep_est_R(mod_R_3_adj_noweek4) # mean_cmi
rep_est_R(mod_R_3_adj_noweek4, "days_since_100")

### save results
tab_s1 <- data.frame(
  lag = c("2-week lag", "3-week lag"), 
  matrix(
    c(
      rep_est(mod_gr_2_noweek4), rep_est(mod_gr_2_adj_noweek4), rep_est(mod_gr_2_adj_noweek4, "days_since_100"), rep_est_R(mod_R_2_noweek4), rep_est_R(mod_R_2_adj_noweek4), rep_est_R(mod_R_2_adj_noweek4, "days_since_100"),
      rep_est(mod_gr_3_noweek4), rep_est(mod_gr_3_adj_noweek4), rep_est(mod_gr_3_adj_noweek4, "days_since_100"), rep_est_R(mod_R_3_noweek4), rep_est_R(mod_R_3_adj_noweek4), rep_est_R(mod_R_3_adj_noweek4, "days_since_100")
    ),
    nrow = 2,
    ncol = 6,
    byrow = TRUE
  ))
names(tab_s1) <- c("Mobility index", "GR/Unadjusted/CMI", "GR/Adjusted/CMI", "GR/Adjusted/Days ", "Rt/Unadjusted/CMI", "Rt/Adjusted/CMI", "Rt/Adjusted/Days")

## format table (not publication-ready)
tab_s1 <- tab_s1 %>%
  flextable %>%
  bold(part = "header", bold = TRUE) %>%
  fontsize(size = 7, part = "header") %>%
  fontsize(size = 7, part = "body") %>%
  autofit

## save table (not publication-ready)
save_as_docx(tab_s1, path = "tab/tab_s1.docx")

# supplementary figures 1 and 2 - outcomes with 3-week lag

## fig s1: growth rate vs cmi with lag of 3 weeks
fig_gr_cmi(dat, mod = mod_gr_3, cmi_lag = 3, path = "fig/fig_s1.png",
           x_breaks = c(0, 25, 50, 75, 100, 125),
           x_labs = c(0, 25, 50, 75, 100, 125),
           x_lims = c(0, 140),
           x_pred = "[0:140 by = 20]",
           x_labs_size = 8)

## fig s2: Rt vs cmi with lag of 3 weeks
fig_R_cmi(dat, mod = mod_R_3, cmi_lag = 3, path = "fig/fig_s2.png",
          x_breaks = c(0, 25, 50, 75, 100, 125),
          x_labs = c(0, 25, 50, 75, 100, 125),
          x_lims = c(0, 140),
          x_pred = "[0:140 by = 20]",
          x_labs_size = 8)