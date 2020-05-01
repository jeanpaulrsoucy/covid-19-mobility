#### 2: Clean and process CMI and cumulative case data ####

# load libraries
library(dplyr) # data manipulation

# load data

## load location info (city, state, country, continent)
locations <- read.csv(
  "data/locations.csv",
  stringsAsFactors = FALSE,
  colClasses = c("state" = "character")
)

## load cmi data and join with location data
cmi <- read.csv(
  "data/cmi/cmi.csv",
  stringsAsFactors = FALSE,
  colClasses = c("date" = "Date")
) %>%
  inner_join(locations, by = "city")

## load case data and join with location data
### read all files in data/cases
cases <-
  lapply(
    setdiff(
      list.files("data/cases", full.names = TRUE),
      list.dirs("data/cases", recursive = FALSE)
    ),
    read.csv,
    stringsAsFactors = FALSE,
    colClasses = c("date" = "Date", "state" = "character")
  ) %>%
  ### bind results together
  bind_rows() %>%
  ### keep only case data for locations with cmi data
  inner_join(distinct(select(locations, -city)), by = c("state", "country"))

# create clean versions of cmi and cases datasets
cmi_clean <-
  select(cmi, city, state, country, continent, date, cmi) %>%
  arrange(city, date)
cases_clean <-
  select(cases, state, country, continent, date, cases) %>%
  arrange(country, state, date)

# write data
write.csv(cmi_clean, "data/cmi_clean.csv", row.names = FALSE)
write.csv(cases_clean, "data/cases_clean.csv", row.names = FALSE)