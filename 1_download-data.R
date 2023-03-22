#### 1: Download CMI and case data ####

# create directories for data (if they don't already exist)
dir.create("data/cmi", showWarnings = FALSE)
dir.create("data/cases", showWarnings = FALSE)
dir.create("data/cases/raw", showWarnings = FALSE)
dir.create("data/cases/country", showWarnings = FALSE)

# load libraries
library(readr)
library(dplyr)
library(tidyr)

# download Citymapper Mobility Index (CMI) data
# https://citymapper.com/cmi
# download code has been removed since the original data source is no longer available
# original download code: https://github.com/jeanpaulrsoucy/covid-19-mobility/blob/2e38f6584e5e31d6c68ee4b765469bb412855c6f/1_download-data.R
# previously downloaded CMI dataset is available in data/cmi

# download and process case data

# Google COVID-19 Open Data Repository (global)
# https://health.google.com/covid-19/open-data/

## read locations.csv, which has google_key column to subset relevant data
locs <- read_csv("data/locations.csv") %>%
  # split data for Rhine-Ruhr (one row per google_key)
  # identified the 13 regions comprising Rhine-Ruhr using Wikipedia: https://en.wikipedia.org/wiki/Rhine-Ruhr
  # "Dortmund", "Essen", "Duisburg", "Bochum", "Gelsenkirchen", "Oberhausen", "Düsseldorf", "Rhein-Kreis Neuss", "Mönchengladbach", "Wuppertal", "Cologne", "Bonn", "Leverkusen"
  separate_rows(google_key, sep = ";")

## download location name index
g_locations <- read_csv("https://storage.googleapis.com/covid19-open-data/v3/index.csv")

# join info from location name index to ensure correct google_key is used
locs <- locs %>%
  left_join(
    g_locations %>%
      select(location_key, country_name, subregion1_name, subregion2_name, locality_name),
    by = c("google_key" = "location_key")
)

## download epidemiology file to temporary location
g_epi_path <- tempfile()
options(timeout = 60 * 10) # longer timeout for big download
download.file("https://storage.googleapis.com/covid19-open-data/v3/epidemiology.csv", g_epi_path)

## load epidemiology file and filter to relevant locations and dates
g_epi <- read_csv(g_epi_path) %>%
  # join location data
  left_join(locs %>% select(city, google_key), by = c("location_key" = "google_key")) %>%
  # filter to cities in dataset
  filter(!is.na(city)) %>%
  # filter out data where superior alternatives exist to Google dataset
  filter(!city %in% c(
    "Melbourne",
    "Montréal",
    "Moscow",
    "São Paulo",
    "St. Petersburg",
    "Sydney",
    "Tokyo",
    "Toronto",
    "Vancouver"
  )) %>%
  select(
    city,
    date,
    cases = new_confirmed) %>%
  # aggregate Rhine-Ruhr
  group_by(city, date) %>%
  summarize(cases = sum(cases)) %>%
  ungroup()

## write csv
write.csv(
  g_epi, "data/cases/google.csv", row.names = FALSE)

## create separate country-level epidemiology dataset
g_epi_country <- read_csv(g_epi_path) %>%
  # join location data
  left_join(
    locs %>%
      select(country, google_key_country) %>%
      distinct(),
    by = c("location_key" = "google_key_country")) %>%
  # filter to countries in dataset
  filter(!is.na(country)) %>%
  # filter out country with bad data
  filter(!country %in% c("Brazil", "Canada")) %>%
  select(
    country,
    date,
    cases = new_confirmed)

## write csv
write.csv(
  g_epi_country, "data/cases/country/google_country.csv", row.names = FALSE)

# Australia

## download raw data for Melbourne (Victoria)
# https://www.coronavirus.vic.gov.au/victorian-coronavirus-covid-19-data
download.file(
  "https://www.dhhs.vic.gov.au/ncov-covid-cases-by-lga-csv",
  "data/cases/raw/au_vic_raw.csv",
  headers = c("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64; rv:108.0) Gecko/20100101 Firefox/108.0"))

## download LGA to PHN correspondence file and save to data/cases/raw/au_lga_phn.csv (table 3 of spreadsheet)
# https://www.health.gov.au/resources/publications/primary-health-networks-phn-concordance-files-local-government-areas-2017?language=en

## download raw data for Sydney (New South Wales)
# https://data.nsw.gov.au/search/dataset/ds-nsw-ckan-aefcde60-3b0c-4bc0-9af1-6fe652944ec2/details?q=
download.file(
  "https://data.nsw.gov.au/data/dataset/aefcde60-3b0c-4bc0-9af1-6fe652944ec2/resource/5d63b527-e2b8-4c42-ad6f-677f14433520/download/confirmed_cases_table1_location_agg.csv",
  "data/cases/raw/au_nsw_raw.csv")

## read and process data
au_lga_phn <- read_csv("data/cases/raw/au_lga_phn.csv") %>%
  filter(PHN_NAME_2017 %in% c(
    "Eastern Melbourne", "North Western Melbourne", "South Eastern Melbourne"
  ))
au_melbourne_lga <- unique(au_lga_phn$LGA_NAME_2017)

au_melbourne <- read_csv("data/cases/raw/au_vic_raw.csv") %>%
  filter(Localgovernmentarea %in% au_melbourne_lga) %>%
  count(diagnosis_date) %>%
  transmute(
    date = diagnosis_date,
    cases = n
  )
au_melbourne <- au_melbourne %>%
  right_join(
    data.frame(
      city = "Melbourne",
      date = seq.Date(
        from = min(au_melbourne$date), to = max(au_melbourne$date), by = "day")
    ),
    by = "date"
  ) %>%
  replace_na(list(cases = 0)) %>%
  select(city, date, cases) %>%
  arrange(date)

au_sydney <- read_csv("data/cases/raw/au_nsw_raw.csv") %>%
  # filter to Sydney-related Local Health Districts
  filter(lhd_2010_name %in% c(
    "Northern Sydney", "South Eastern Sydney", "South Western Sydney",
    "Sydney", "Western Sydney")) %>%
  count(notification_date) %>%
  transmute(
    date = notification_date,
    cases = n
  )
au_sydney <- au_sydney %>%
  right_join(
    data.frame(
      city = "Sydney",
      date = seq.Date(
        from = min(au_sydney$date), to = max(au_sydney$date), by = "day")
    ),
    by = "date"
  ) %>%
  replace_na(list(cases = 0)) %>%
  select(city, date, cases) %>%
  arrange(date)

au <- bind_rows(au_melbourne, au_sydney)

## write CSV
write.csv(
  au, "data/cases/au.csv", row.names = FALSE
)

# Brazil

## download file
download.file(
  "https://github.com/wcota/covid19br/raw/master/cases-brazil-cities-time_2020.csv.gz",
  "data/cases/raw/br.csv.gz"
)

## read and process data for São Paulo
br <- read_csv("data/cases/raw/br.csv.gz") %>%
  filter(city == "São Paulo/SP") %>%
  transmute(
    city = "São Paulo",
    date = date,
    cases = newCases
  )

## write CSV
write.csv(
  br, "data/cases/br.csv", row.names = FALSE)

## read and process data for Brazil
br_country <- read_csv("data/cases/raw/br.csv.gz") %>%
  filter(city == "TOTAL") %>%
  transmute(
    country = "Brazil",
    date = date,
    cases = newCases
  )

## write CSV
write.csv(
  br_country, "data/cases/country/br_country.csv", row.names = FALSE)

# Canada

## download file for health regions
download.file(
  "https://api.opencovid.ca/timeseries?stat=cases&geo=hr&before=2020-04-12&hr_names=short&loc=593&loc=3595&loc=2406&fmt=csv",
  "data/cases/raw/ca_raw.csv"
)

## download file for Canada
download.file(
  "https://api.opencovid.ca/timeseries?stat=cases&geo=can&before=2020-04-12&fmt=csv",
  "data/cases/raw/ca_country_raw.csv"
)

## read and process data for health regions
ca <- read_csv("data/cases/raw/ca_raw.csv") %>%
  transmute(
    city = case_when(
      sub_region_1 == "Vancouver Coastal" ~ "Vancouver",
      sub_region_1 == "Toronto" ~ "Toronto",
      sub_region_1 == "Montréal" ~ "Montréal"
    ),
    date,
    cases = value_daily
  )

## write CSV
write.csv(
  ca, "data/cases/ca.csv", row.names = FALSE)

## read and process data for Canada
ca_country <- read_csv("data/cases/raw/ca_country_raw.csv") %>%
  transmute(
    country = "Canada",
    date,
    cases = value_daily
  )

## write CSV
write.csv(
  ca_country, "data/cases/country/ca.csv", row.names = FALSE)

# Japan
# https://catalog.data.metro.tokyo.lg.jp/dataset/t000010d0000000068

## download data
download.file(
  "https://translate.google.com/website?sl=auto&tl=en&hl=en&client=webapp&u=https://data.stopcovid19.metro.tokyo.lg.jp/130001_tokyo_covid19_details_testing_positive_cases.csv",
  "data/cases/raw/jp.csv", row.names = FALSE
)

## process data
jp <- read_csv("data/cases/raw/jp.csv") %>%
  transmute(
    city = "Tokyo",
    date = 公表_年月日,
    cases = `陽性者数（累計）`,
    cases = c(cases[1], diff(cases)) # reverse cumulative sum (note that Tokyo starts at 37, but this doesn't really matter)
  )

## write CSV
write.csv(jp, "data/cases/jp.csv", row.names = FALSE)

# Russia
# https://yandex.ru/web-maps/covid19?ll=37.646921%2C55.725146&z=9 (Moscow)
# https://yandex.ru/web-maps/covid19?ll=30.424830%2C59.939314&z=9 (St. Petersburg)
# transcribe and save data as data/cases/raw/ru_raw.csv
# manually transcribed dataset exactly matches the Kaggle version of the Yandex dataset
# (mirror of Kaggle dataset: https://raw.githubusercontent.com/Kapral42/covid-19-yandex-dataset-copy-kaggle/master/RS.csv)

## read and process data
ru <- read_csv("data/cases/raw/ru_raw.csv") %>%
  select(city, date, cases) %>%
  arrange(city, date) %>%
  group_by(city) %>%
  mutate(cases = c(cases[1], diff(cases))) %>% # get daily cases
  ungroup()

## write csv
write.csv(
  ru, "data/cases/ru.csv", row.names = FALSE)
