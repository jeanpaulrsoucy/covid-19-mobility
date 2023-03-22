#### 2: Clean and process CMI and cumulative case data ####

# load libraries
library(dplyr)
library(tidyr)
library(readr)

# load and clean data

## define maximum date for case data
date_final <- as.Date("2020-04-12")

## load location info
locs <- read_csv("data/locations.csv") %>%
  select(city, country, continent)

## load cmi data and join with location data
cmi <- read_csv("data/cmi/cmi.csv") %>%
  inner_join(locs, by = "city")

## load case data and join with location data
cases <-
  lapply(
    setdiff(
      list.files("data/cases", full.names = TRUE),
      list.dirs("data/cases", recursive = FALSE)
    ),
    read_csv
  ) %>%
  bind_rows() %>%
  inner_join(locs, by = "city") %>%
  arrange(city, date)

## clean case data
cases <- cases %>%
  # filter to maximum date
  filter(date <= date_final) %>%
  # fill empty values with 0
  replace_na(list(cases = 0))
# fix handful of negative values
cases[cases$city == "Amsterdam" & cases$date == "2020-03-20", "cases"] <- cases[cases$city == "Amsterdam" & cases$date == "2020-03-20", "cases"] +
  cases[cases$city == "Amsterdam" & cases$date == "2020-03-21", "cases"]
cases[cases$city == "Amsterdam" & cases$date == "2020-03-21", "cases"] <- 0
cases[cases$city == "Rome" & cases$date == "2020-03-11", "cases"] <- cases[cases$city == "Rome" & cases$date == "2020-03-11", "cases"] +
  cases[cases$city == "Rome" & cases$date == "2020-03-10", "cases"]
cases[cases$city == "Rome" & cases$date == "2020-03-10", "cases"] <- 0
sum(cases$cases < 0) == 0 # should be TRUE
# pad true zeroes to complete weeks
# Philadelphia: https://www.phila.gov/2020-03-10-philadelphia-announces-first-case-of-covid-19-coronavirus/
cases <- bind_rows(
  cases,
  data.frame(
    city = "Philadelphia",
    date = as.Date("2020-03-09"),
    cases = 0,
    country = "United States",
    continent = "North America"
  )) %>% arrange(city, date)
# calculate cumulative cases
cases <- cases %>%
  group_by(city) %>%
  mutate(cases_cumulative = cumsum(cases)) %>%
  ungroup()

## load and process country-level case data
cases_country <- bind_rows(
  read_csv("data/cases/country/google_country.csv"),
  read_csv("data/cases/country/br_country.csv"),
  read_csv("data/cases/country/ca.csv")
) %>%
  # fill empty values
  replace_na(list(cases = 0)) %>%
  # filter to maximum date
  filter(date <= date_final)

# format clean datasets
cmi_clean <- cmi %>%
  select(city, date, cmi, country, continent) %>%
  arrange(city, date)
cases_clean <- cases %>%
  select(city, date, cases, cases_cumulative, country, continent) %>%
  arrange(city, date)
cases_country_clean <- cases_country %>%
  arrange(country, date)

# write clean datasets
write.csv(cmi_clean, "data/cmi_clean.csv", row.names = FALSE)
write.csv(cases_clean, "data/cases_clean.csv", row.names = FALSE)
write.csv(cases_country_clean, "data/cases_country_clean.csv", row.names = FALSE)