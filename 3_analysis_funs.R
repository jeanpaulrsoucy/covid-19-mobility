#### Functions for 3: Analysis ####

# process case and CMI data for analysis
process_case_cmi_data <- function(cases, cases_country, cmi) {
  
  # PROCESS CASE DATA #
  
  # calculate growth rates for unique cities in the dataset
  
  ## add ISO week
  cases_weekly <- cases %>%
    mutate(week = isoweek(date))
  
  ## exclude incomplete weeks
  cases_completeness <- cases_weekly %>%
    count(city, week)
  cases_weekly <- cases_weekly %>%
    left_join(
      cases_completeness,
      by = c("city", "week")
    ) %>%
    filter(n == 7) %>%
    select(-n)
  
  ## aggregate cases by week
  cases_weekly <- cases_weekly %>%
    group_by(city, country, continent, week) %>%
    summarize(
      cases = sum(cases),
      cases_cumulative = sum(cases_cumulative),
      .groups = "drop_last"
    ) %>%
    # calculate weekly growth rate of cases by city
    mutate(
      cases_lag = lag(cases, 1),
      weekly_growth_rate = cases / lag(cases, 1),
    ) %>%
    ungroup()
  
  # function to extract weekly growth rate
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
  
  # calculate growth rates for each week, beginning with week 3 of March
  gr_week(3, 12)
  gr_week(4, 13)
  gr_week(5, 14)
  gr_week(6, 15)
  
  # PROCESS CMI DATA #
  
  # function to calculate mean cmi for a week
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
  
  # calculate average CMI for each week, beginning with week 1 of March
  agg_cmi_week(1, "2020-03-02", "2020-03-08")
  agg_cmi_week(2, "2020-03-09", "2020-03-15")
  agg_cmi_week(3, "2020-03-16", "2020-03-22")
  agg_cmi_week(4, "2020-03-23", "2020-03-29")
  
  # join cmi and case data
  dat <- inner_join(
    gr_week_3, gr_week_4, by = c("city", "country", "continent")) %>%
    inner_join(gr_week_5, by =  c("city", "country", "continent")) %>%
    inner_join(gr_week_6, by =  c("city", "country", "continent")) %>%
    inner_join(cmi_week_1, by = c("city", "country", "continent")) %>%
    inner_join(cmi_week_2, by = c("city", "country", "continent")) %>%
    inner_join(cmi_week_3, by = c("city", "country", "continent")) %>%
    inner_join(cmi_week_4, by = c("city", "country", "continent"))
  
  # ADD ESTIMATES OF Rt USING EpiEstim #
  
  # create incidence time series
  inci <- cases %>%
    mutate(I = cases) %>%
    select(city, date, I, country, continent)
  
  # trim initial run of 0s
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
  
  # key dates for each incidence time series
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
  
  # estimate R and extract results
  cities <- unique(inci$city)
  R <- data.frame(
    city = cities,
    R_week_3 = NA_real_,
    R_week_4 = NA_real_,
    R_week_5 = NA_real_,
    R_week_6 = NA_real_
  )
  for (i in 1:length(cities)) {
    # extract dates for city
    t_max <- inci_dates[inci_dates$city == cities[i], "t_max", drop = TRUE]
    t_3_start <- inci_dates[inci_dates$city == cities[i], "t_3_start", drop = TRUE]
    t_3_end <- inci_dates[inci_dates$city == cities[i], "t_3_end", drop = TRUE]
    t_4_start <- inci_dates[inci_dates$city == cities[i], "t_4_start", drop = TRUE]
    t_4_end <- inci_dates[inci_dates$city == cities[i], "t_4_end", drop = TRUE]
    t_5_start <- inci_dates[inci_dates$city == cities[i], "t_5_start", drop = TRUE]
    t_5_end <- inci_dates[inci_dates$city == cities[i], "t_5_end", drop = TRUE]
    t_6_start <- inci_dates[inci_dates$city == cities[i], "t_6_start", drop = TRUE]
    t_6_end <- inci_dates[inci_dates$city == cities[i], "t_6_end", drop = TRUE]
    # defaults to weekly sliding window
    R_est <- estimate_R(
      inci %>% filter(city == cities[i]),
      method = "parametric_si",
      config = make_config(
        list(t_start = 2:(t_max - 6),
             t_end = 8:t_max,
             mean_si = 3.96, std_si = 4.75)))$R
    # extract estimates for weeks 3, 4, 5, 6
    R[R$city == cities[i], "R_week_3"] <- R_est[R_est$t_start == t_3_start & R_est$t_end == t_3_end, "Mean(R)"]
    R[R$city == cities[i], "R_week_4"] <- R_est[R_est$t_start == t_4_start & R_est$t_end == t_4_end, "Mean(R)"]
    R[R$city == cities[i], "R_week_5"] <- R_est[R_est$t_start == t_5_start & R_est$t_end == t_5_end, "Mean(R)"]
    R[R$city == cities[i], "R_week_6"] <- R_est[R_est$t_start == t_6_start & R_est$t_end == t_6_end, "Mean(R)"]
  }
  
  # join to Rt estimates to data
  dat <- dat %>%
    inner_join(R, by = "city")
  
  # ADDITIONAL PROCESSING #
  
  # calculate days since 100th case for each country
  # Monaco is NA because it does not even have even 50 cases by 2020-03-29 (or 100 cases by 2020-04-12)
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
  
  # join to main dataset
  dat <- dat %>%
    left_join(
      country_100_daily,
      by = "country")
  
  # define country groups for plotting
  dat <- dat %>%
    mutate(
      country_groups = factor(
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
          continent == "Europe" ~ "Europe (other)"
        ),
        levels = c("AU", "CA", "DE", "ES", "IT", "UK", "US", "Asia", "Europe (other)", "Latin America")
      ),
      country_groups_2 = factor(
        case_when(
          country == "United Kingdom" ~ "United Kingdom",
          country == "United States" ~ "United States",
          country == "Australia" ~ "Australia",
          country == "Canada" ~ "Canada",
          country == "Germany" ~ "Germany",
          country == "Italy" ~ "Italy",
          country == "Spain" ~ "Spain",
          country %in% c("Mexico", "Brazil") ~ "Latin America",
          continent == "Asia" ~ "Asia",
          continent == "Europe" ~ "Europe (other)"
        ),
        )
    )
  
  # check country groups were assigned correctly
  if (sum(is.na(dat$country_groups)) > 0) stop("Some countries were not assigned to a country group")
  if (sum(is.na(dat$country_groups_2)) > 0) stop("Some countries were not assigned to a country group")
  
  # return data
  dat
}

# process physical distancing data
process_pdist_data <- function(pdist, cmi) {
  
  # process physical distancing data
  pdist <- pdist %>%
    mutate(pdist_date = as.Date(pdist_date)) %>%
    select(city, country, continent, pdist_date) %>%
    # join with basic cmi data
    left_join(cmi, by = c("city", "country", "continent")) %>%
    # keep weeks 1-4 of march
    filter(date >= as.Date("2020-03-02") & date <= as.Date("2020-03-29")) %>%
    mutate(
      pdist = factor(
        ifelse(date >= pdist_date, 1, 0),
        levels = c(0, 1),
        labels = c("Before", "After")
      ),
      time = as.integer(date - as.Date("2020-03-02")),
      # group Americas for the purpose of plotting
      continent = ifelse(
        continent %in% c("North America", "South America"),
        "Americas",
        continent
      )
    )
  
  # hack for physical distancing plot to ensure no gap between lines when linetype changes
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
  
  # return data
  pdist
}

# plot: CMI before and after physical distancing measures announced
plot_cmi_pdist <- function(pdist) {
  ggplot(pdist, aes(x = date, y = cmi, group = city)) +
    geom_line(data = filter(pdist, pdist == "Before"), linetype = "solid", alpha = 0.5, linewidth = 0.6) +
    geom_line(data = filter(pdist, pdist == "After"), linetype = "dashed", alpha = 0.5, linewidth = 0.6) +
    # hack to make linetype aes show up in legend
    geom_line(data = filter(pdist, city == "Toronto") %>% group_by(pdist) %>% top_n(2, date), aes(group = pdist, linetype = pdist), alpha = 0) +
    scale_x_date(breaks = pretty_breaks()) +
    scale_linetype_manual(values = c("Before" = "solid", "After" = "dashed"), guide = guide_legend(
      title.position = "left", override.aes = list(alpha = 1))) +
    facet_wrap(~ continent, ncol = 2) +
    labs(x = "Date", y = "Mobility index (%)", linetype = "Intervention announced") +
    theme_mobility +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# format data for log weekly growth rate analysis
dat_gr <- function(dat, cmi_lag, outcome_weeks = c(3, 4, 5, 6)) {
  d <- vector(mode = "list", length = length(outcome_weeks))
  for (week in outcome_weeks) {
    dd <- dat %>%
      select(city, country, continent, country_groups, country_groups_2,
             matches(paste0("^weekly_gr_", week)),
             matches(paste0("^log_weekly_gr_", week)),
             matches(paste0("mean_cmi_", week - cmi_lag)),
             matches(paste0("days_since_100_", week))
      ) %>%
      mutate(
        week_gr = paste("Week", week),
        week_cmi = paste("Week", week - cmi_lag)
      )
    names(dd) <- c("city", "country", "continent", "country_groups", "country_groups_2",
                   "weekly_gr", "log_weekly_gr", "mean_cmi", "days_since_100", "week_gr", "week_cmi")
    d[[match(week, outcome_weeks)]] <- dd
  }
  bind_rows(d)
}

# model for log weekly growth rate with specified lag of CMI
mod_gr <- function(dat, cmi_lag, outcome_weeks, adj = FALSE, cmi_week_int = FALSE) {
  # create dataset
  d <- dat_gr(dat, cmi_lag, outcome_weeks) %>%
    arrange(city)
  # fit model
  # suppress irrelevant warnings
  suppressWarnings(
    if (!adj) {
      if (!cmi_week_int) {
        glmmTMB(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi, data = d)
      } else {
        glmmTMB(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi, data = d) 
      }
    } else {
      if (!cmi_week_int) {
        glmmTMB(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi + days_since_100, data = d)
      } else {
        glmmTMB(log_weekly_gr ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi + days_since_100, data = d) 
      }
    }
  )
}

# GR model: report point estimate and 95% confidence interval for a 10 unit change in x (10% decrease for CMI, 10 unit increase for other variables)
rep_est <- function(mod, x = "mean_cmi") {
  out <- mod %>% tidy(effects = "fixed", conf.int = TRUE) %>% filter(term == x)
  if (x == "mean_cmi") {
    paste0(sprintf("%.3f", exp(out$estimate * -10)), " (", sprintf("%.3f", exp(out$conf.high * -10)), ", ", sprintf("%.3f", exp(out$conf.low * -10)), ")")
  } else {
    paste0(sprintf("%.3f", exp(out$estimate * 10)), " (", sprintf("%.3f", exp(out$conf.low * 10)), ", ", sprintf("%.3f", exp(out$conf.high * 10)), ")")
  }
}

# plot: mean growth rate vs CMI
plot_gr_cmi <- function(
    dat, mod, cmi_lag, outcome_weeks,
    x_breaks = c(0, 25, 50, 75, 100),
    x_labs = c(0, 25, 50, 75, 100),
    x_lims = c(0, 120),
    x_pred = "[0:120 by = 1]",
    x_labs_size = NULL) {
  # match args
  if (!cmi_lag %in% c(2, 3)) stop("cmi_lag must be 2 or 3.")
  # create dataset
  d <- dat_gr(dat, cmi_lag, outcome_weeks)
  # remove missing data
  d <- d[complete.cases(d), ]
  # predicted values for fixed effect of CMI
  pred_mod <- ggpredict(
    mod, paste("mean_cmi", x_pred), type = "fe",
    allow.new.levels = TRUE) # suppress false warning
  pred_mod$predicted <- exp(pred_mod$predicted)
  pred_mod$conf.low <- exp(pred_mod$conf.low)
  pred_mod$conf.high <- exp(pred_mod$conf.high)
  # plot
  plot_gr_cmi <- plot(pred_mod) +
    geom_point(
      data = d, aes(x = mean_cmi, y = weekly_gr, colour = country_groups), size = 1.75) +
    labs(x = paste0("Mean mobility index ", ifelse(cmi_lag == 2, "two", "three"), " weeks prior (%)"),
         y = "Weekly COVID-19 case ratio",
         title = NULL,
         colour = NULL) +
    scale_x_continuous(breaks = x_breaks, limits = x_lims, labels = x_labs) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_color_brewer(palette = "Paired") +
    theme_mobility +
    guides(color = guide_legend(nrow = 2, override.aes = list(fill = NA, linetype = 0))) + # no shading or border in legend
    theme(
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.position = "bottom"
    ) +
    facet_wrap(~week_gr, ncol = 4, labeller = as_labeller(
      c(
        "Week 3" = "Week beginning March 16",
        "Week 4" = "Week beginning March 23",
        "Week 5" = "Week beginning March 30",
        "Week 6" = "Week beginning April 6"
      )
    ))
  # override x-axis label size
  if (!is.null(x_labs_size)) {
    plot_gr_cmi <- plot_gr_cmi + theme(
      axis.text.x = element_text(size = x_labs_size)
    )
  }
  # return plot
  plot_gr_cmi
}

# plot: mean growth rate vs CMI (alternative format)
plot_gr_cmi_2 <- function(
    dat, mod, cmi_lag, outcome_weeks,
    x_breaks = c(0, 25, 50, 75, 100),
    x_labs = c(0, 25, 50, 75, 100),
    x_lims = c(0, 120),
    x_pred = "[0:120 by = 1]",
    x_labs_size = NULL) {
  # match args
  if (!cmi_lag %in% c(2, 3)) stop("cmi_lag must be 2 or 3.")
  # create dataset
  d <- dat_gr(dat, cmi_lag, outcome_weeks)
  # remove missing data
  d <- d[complete.cases(d), ]
  # predicted values for fixed effect of CMI
  pred_mod <- ggpredict(
    mod, paste("mean_cmi", x_pred), type = "fe",
    allow.new.levels = TRUE) # suppress false warning
  pred_mod$predicted <- exp(pred_mod$predicted)
  pred_mod$conf.low <- exp(pred_mod$conf.low)
  pred_mod$conf.high <- exp(pred_mod$conf.high)
  # plot
  plot_gr_cmi <- plot(pred_mod) +
    geom_point(
      data = d, aes(x = mean_cmi, y = weekly_gr, shape = week_gr), size = 1.75, alpha = 0.8) +
    geom_path(
      data = d, aes(x = mean_cmi, y = weekly_gr, group = city), alpha = 0.5) +
    labs(x = paste0("Mean mobility index ", ifelse(cmi_lag == 2, "two", "three"), " weeks prior (%)"),
         y = "Weekly COVID-19 case ratio",
         title = NULL,
         colour = NULL) +
    scale_x_continuous(breaks = x_breaks, limits = x_lims, labels = x_labs) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_shape_discrete(labels = as_labeller(
      c(
        "Week 3" = "March 16",
        "Week 4" = "March 23",
        "Week 5" = "March 30",
        "Week 6" = "April 6"
      ))) +
    theme_mobility +
    guides(shape = guide_legend(title = "Week beginning", nrow = 1)) +
    theme(
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.position = "bottom"
    ) +
    facet_wrap(~country_groups_2, nrow = 2)
  # override x-axis label size
  if (!is.null(x_labs_size)) {
    plot_gr_cmi <- plot_gr_cmi + theme(
      axis.text.x = element_text(size = x_labs_size)
    )
  }
  # return plot
  plot_gr_cmi
}

# format data for Rt analysis
dat_R <- function(dat, cmi_lag, outcome_weeks) {
  
  ### process data
  d <- vector(mode = "list", length = length(outcome_weeks))
  for (week in outcome_weeks) {
    
    dd <- dat %>%
      select(city, country, continent, country_groups, country_groups_2,
             matches(paste0("R_week_", week)),
             matches(paste0("mean_cmi_", week - cmi_lag)),
             matches(paste0("days_since_100_", week))
      ) %>%
      mutate(
        week_R = paste("Week", week),
        week_cmi = paste("Week", week - cmi_lag)
      )
    names(dd) <- c("city", "country", "continent", "country_groups", "country_groups_2",
                   "R", "mean_cmi", "days_since_100", "week_R", "week_cmi")
    d[[match(week, outcome_weeks)]] <- dd
    
  }
  bind_rows(d)
  
}

# model for Rt with specified lag of CMI
mod_R <- function(dat, cmi_lag, outcome_weeks, adj = FALSE, cmi_week_int = FALSE) {
  # create dataset
  d <- dat_R(dat, cmi_lag, outcome_weeks)
  # fit model
  if (!adj) {
    if (!cmi_week_int) {
      glmmTMB(R ~ (1 | country) + (1 | country:city) + mean_cmi, data = d)
    } else {
      glmmTMB(R ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi, data = d)
    }
  } else {
    if (!cmi_week_int) {
      glmmTMB(R ~ (1 | country) + (1 | country:city) + mean_cmi + days_since_100, data = d)
    } else {
      glmmTMB(R ~ (1 | country) + (1 | country:city) + mean_cmi + mean_cmi:week_cmi + days_since_100, data = d)
    }
  }
}

# Rt model: report point estimate and 95% confidence inteval for a 10 unit change (10% decrease of CMI, 10 unit increase for other variables)
rep_est_R <- function(mod, x = "mean_cmi") {
  out <- mod %>% tidy(effects = "fixed", conf.int = TRUE) %>% filter(term == x)
  if (x == "mean_cmi") {
    paste0(sprintf("%.3f", out$estimate * -10), " (", sprintf("%.3f", out$conf.high * -10), ", ", sprintf("%.3f", out$conf.low * -10), ")")
  } else {
    paste0(sprintf("%.3f", out$estimate * 10), " (", sprintf("%.3f", out$conf.low * 10), ", ", sprintf("%.3f", out$conf.high * 10), ")")
  }
}

# plot: effective reproduction number vs cmi with lag of 2 weeks
plot_R_cmi <- function(
    dat, mod, cmi_lag, outcome_weeks = c(3, 4, 5, 6),
    x_breaks = c(0, 25, 50, 75, 100),
    x_labs = c(0, 25, 50, 75, 100),
    x_lims = c(0, 120),
    x_pred = "[0:120 by = 1]",
    x_labs_size = NULL) {
  # match args
  if (!cmi_lag %in% c(2, 3)) stop("cmi_lag must be 2 or 3.")
  # create dataset
  d <- dat_R(dat, cmi_lag, outcome_weeks)
  # predicted values for fixed effect of CMI
  pred_mod <- ggpredict(
    mod, paste("mean_cmi", x_pred), type = "fe",
    allow.new.levels = TRUE) # suppress false warning
  # plot
  plot_R_cmi <- plot(pred_mod) +
    geom_point(data = d, aes(x = mean_cmi,
                             y = R,
                             colour = country_groups),
               size = 1.75) +
    labs(x = paste0("Mean mobility index ", ifelse(cmi_lag == 2, "two", "three"), " weeks prior (%)"),
         y = expression(paste("Weekly COVID-19 ", italic(R[t]))),
         title = NULL,
         colour = NULL) +
    scale_x_continuous(breaks = x_breaks, limits = x_lims, labels = x_labs) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_color_brewer(palette = "Paired") +
    theme_mobility +
    guides(color = guide_legend(nrow = 2, override.aes = list(fill = NA, linetype = 0))) + # no shading or border in legend
    theme(
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.position = "bottom"
    ) +
    facet_wrap(~week_R, ncol = 4, labeller = as_labeller(
      c(
        "Week 3" = "Week starting March 16",
        "Week 4" = "Week starting March 23",
        "Week 5" = "Week starting March 30",
        "Week 6" = "Week starting April 6"
      )
    ))
  # override x-axis label size
  if (!is.null(x_labs_size)) {
    plot_R_cmi <- plot_R_cmi + theme(
      axis.text.x = element_text(size = x_labs_size)
    )
  }
  # return plot
  plot_R_cmi
}

# plot: effective reproduction number vs cmi with lag of 2 weeks (alternative format)
plot_R_cmi_2 <- function(
    dat, mod, cmi_lag, outcome_weeks = c(3, 4, 5, 6),
    x_breaks = c(0, 25, 50, 75, 100),
    x_labs = c(0, 25, 50, 75, 100),
    x_lims = c(0, 120),
    x_pred = "[0:120 by = 1]",
    x_labs_size = NULL) {
  # match args
  if (!cmi_lag %in% c(2, 3)) stop("cmi_lag must be 2 or 3.")
  # create dataset
  d <- dat_R(dat, cmi_lag, outcome_weeks)
  # predicted values for fixed effect of CMI
  pred_mod <- ggpredict(
    mod, paste("mean_cmi", x_pred), type = "fe",
    allow.new.levels = TRUE) # suppress false warning
  # plot
  plot_R_cmi <- plot(pred_mod) +
    geom_point(
      data = d, aes(x = mean_cmi, y = R, shape = week_R), size = 1.75, alpha = 0.8) +
    geom_path(
      data = d, aes(x = mean_cmi, y = R, group = city), alpha = 0.5) +
    labs(x = paste0("Mean mobility index ", ifelse(cmi_lag == 2, "two", "three"), " weeks prior (%)"),
         y = expression(paste("Weekly COVID-19 ", italic(R[t]))),
         title = NULL,
         colour = NULL) +
    scale_x_continuous(breaks = x_breaks, limits = x_lims, labels = x_labs) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_shape_discrete(labels = as_labeller(
      c(
        "Week 3" = "March 16",
        "Week 4" = "March 23",
        "Week 5" = "March 30",
        "Week 6" = "April 6"
      ))) +
    theme_mobility +
    guides(shape = guide_legend(title = "Week beginning", nrow = 1)) +
    theme(
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.position = "bottom"
    ) +
    facet_wrap(~country_groups_2, nrow = 2)
  # override x-axis label size
  if (!is.null(x_labs_size)) {
    plot_R_cmi <- plot_R_cmi + theme(
      axis.text.x = element_text(size = x_labs_size)
    )
  }
  # return plot
  plot_R_cmi
}

# model: report R2 (marginal or conditional)
rep_R2 <- function(mod, type = c("marginal", "conditional")) {
  match.arg(type, c("marginal", "conditional"))
  if (type == "marginal") {
    sprintf("%.3f", r2_nakagawa(mod)$R2_marginal)
  } else if (type == "conditional") {
    sprintf("%.3f", r2_nakagawa(mod)$R2_conditional)
  }
}

# model comparison: report LRT statistics
rep_lrt <- function(x) {
  paste0("Chi^2(", x$`Chi Df`[2], ") = ", sprintf("%.3f", x$Chisq[2]), ", p = ", sprintf("%.3f", x$`Pr(>Chisq)`[2]))
}
