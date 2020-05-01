#### 1: Download CMI and cumulative case data ####

# create directories for data (if they don't already exist)
dir.create("data/cmi", showWarnings = FALSE)
dir.create("data/cases", showWarnings = FALSE)
dir.create("data/cases/raw", showWarnings = FALSE)

# load libraries
library(httr) # downloading from URLs
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(xlsx) # read xlsx

# define recent dates
# some download urls are based on date
today <- Sys.Date()
today_1 <- today - 1
today_2 <- today - 2

# download citymapper mobility index (CMI) data
# https://citymapper.com/cmi

## download data (try today and past two days)
cmi_url <- paste0(
  "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_",
  format.Date(today, "%Y"),
  format.Date(today, "%m"),
  format.Date(today, "%d"),
  ".csv"
)
if (status_code(GET(cmi_url)) == 200) {
  cmi <- read.csv(
    cmi_url,
    stringsAsFactors = FALSE,
    skip = 3,
    colClasses = c("Date" = "Date")
  )
} else {
  cmi_url <- paste0(
    "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_",
    format.Date(today_1, "%Y"),
    format.Date(today_1, "%m"),
    format.Date(today_1, "%d"),
    ".csv"
  )
}
if (status_code(GET(cmi_url)) == 200) {
  cmi <- read.csv(
    cmi_url,
    stringsAsFactors = FALSE,
    skip = 3,
    colClasses = c("Date" = "Date")
  )
} else {
  cmi_url <- paste0(
    "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_",
    format.Date(today_2, "%Y"),
    format.Date(today_2, "%m"),
    format.Date(today_2, "%d"),
    ".csv"
  )
}
if (status_code(GET(cmi_url)) == 200) {
  cmi <- read.csv(
    cmi_url,
    stringsAsFactors = FALSE,
    skip = 3,
    colClasses = c("Date" = "Date")
  )
} else {
  stop("Error downloading CMI. Check if the data source has been altered.")
}

## process data
names(cmi) <-
  gsub("\\.(?=[A-Z])", " ", names(cmi), perl = TRUE) # replace . with spaces in city names
names(cmi)[names(cmi) == "Rhine Ruhr"] <-
  "Rhine-Ruhr" # fix Rhine-Ruhr
cmi <- cmi %>%
  pivot_longer(
    cols = !matches("Date"),
    names_to = "city",
    values_to = "cmi"
  ) %>%
  ### express CMI as percentage
  mutate(cmi = as.integer(cmi * 100)) %>%
  rename(date = Date) %>%
  select(city, date, cmi) %>%
  arrange(city, date)

## write csv
write.csv(cmi,
          "data/cmi/cmi.csv",
          row.names = FALSE)

# download and process cumulative case data

# JHU CSSE (global)
# https://github.com/CSSEGISandData/COVID-19

## download and process data
jhu <-
  read.csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
    stringsAsFactors = FALSE
  ) %>%
  select(-Lat, -Long)
### rename columns
names(jhu)[1:2] <- c("state", "country")
names(jhu)[3:ncol(jhu)] <-
  as.character(seq.Date(
    from = as.Date("2020-01-22"),
    by = "day",
    length.out = ncol(jhu) - 2
  ))
### rename S.K.
jhu[jhu$country == "Korea, South", "country"] <- "South Korea"
### process data
jhu <- jhu %>%
  pivot_longer(cols = starts_with("202"),
               names_to = "date",
               values_to = "cases") %>%
  ### keep only main countries, not separate islands (for countries not filtered below)
  filter(!(country %in% c("Netherlands", "Denmark") &
             state != "")) %>%
  select(state, country, date, cases) %>%
  ### delete countries with region-level data downloaded later in the script
  filter(
    !country %in% c(
      "United Kingdom",
      "France",
      "Spain",
      "Italy",
      "Germany",
      "Canada",
      "Brazil",
      "Belgium",
      "Austria",
      "South Korea",
      "Japan",
      "Portugal",
      "Russia",
      "Sweden"
    )
  ) %>%
  arrange(state, country, date)

## save
write.csv(jhu,
          "data/cases/jhu.csv",
          row.names = FALSE)

# NYT (US states)
# https://github.com/nytimes/covid-19-data

## download and process data
nyt_states <-
  read.csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
    stringsAsFactors = FALSE,
    colClasses = c("date" = "Date")
  ) %>%
  mutate(country = "United States") %>%
  select(state, country, date, cases) %>%
  ### fill US case time series with 0s before first cases
  complete(state, country, date, fill = list(cases = 0)) %>%
  ### will use county-level data for California
  filter(state != "California") %>%
  arrange(state, country, date)

## write csv
write.csv(nyt_states,
          "data/cases/nyt_states.csv",
          row.names = FALSE)

# NYT (US counties)
# California - because two cities are represented (LA and SF)
# https://github.com/nytimes/covid-19-data

## download and process data
nyt_counties <-
  read.csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
    stringsAsFactors = FALSE,
    colClasses = c("date" = "Date")
  ) %>%
  mutate(country = "United States") %>%
  select(county, country, date, cases) %>%
  filter(county %in% c("Los Angeles", "San Francisco")) %>%
  rename(state = county) %>%
  arrange(state, country, date)

## write csv
write.csv(nyt_counties,
          "data/cases/nyt_counties.csv",
          row.names = FALSE)

# UK (regions)
# https://coronavirus.data.gov.uk/#regions
# download region-level cases csv and save as data/cases/raw/uk_raw.csv

## download and process data
uk <- read.csv(
  "data/cases/raw/uk_raw.csv",
  stringsAsFactors = FALSE,
  colClasses = c("Specimen.date" = "Date")
) %>%
  select(Area.name, Specimen.date, Cumulative.lab.confirmed.cases) %>%
  rename(state = Area.name,
         date = Specimen.date,
         cases = Cumulative.lab.confirmed.cases) %>%
  filter(state %in% c("London", "West Midlands", "North West")) %>%
  mutate(country = "United Kingdom") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(uk,
          "data/cases/uk.csv",
          row.names = FALSE)

# France (country-level)
# https://github.com/opencovid19-fr

## download and process data
fr <-
  read.csv(
    "https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv",
    stringsAsFactors = FALSE,
    colClasses = c("date" = "Date")
  ) %>%
  ### select country-level data (region-level data ends in late March)
  filter(granularite == "pays",!is.na(cas_confirmes)) %>%
  select(date, cas_confirmes) %>%
  rename(cases = cas_confirmes) %>%
  ### only want final report from each day
  group_by(date) %>%
  top_n(1, cases) %>%
  distinct %>%
  ungroup %>%
  ### ensure time series is complete
  right_join(data.frame(date = seq.Date(
    from = min(.$date),
    to = max(.$date),
    by = "day"
  )), by = "date") %>%
  fill(cases, .direction = "down") %>%
  mutate(state = "",
         country = "France") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(fr,
          "data/cases/fr.csv",
          row.names = FALSE)

# Spain (regions)
# https://github.com/datadista/datasets/tree/master/COVID%2019

## download and process data
es <-
  read.csv(
    "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv",
    stringsAsFactors = FALSE,
    colClasses = c("fecha" = "Date")
  ) %>%
  select(fecha, CCAA, total) %>%
  rename(date = fecha,
         state = CCAA,
         cases = total) %>%
  filter(state %in% c("Madrid", "Cataluña")) %>%
  mutate(country = "Spain") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(es,
          "data/cases/es.csv",
          row.names = FALSE)

# Italy
# https://github.com/pcm-dpc/COVID-19

## download and process data
it <-
  read.csv(
    "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
    stringsAsFactors = FALSE,
    colClasses = c("data" = "Date")
  ) %>%
  select(data, denominazione_regione, totale_casi) %>%
  rename(date = data,
         state = denominazione_regione,
         cases = totale_casi) %>%
  filter(state %in% c("Lazio", "Lombardia")) %>%
  mutate(state = as.character(state),
         country = "Italy") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(it,
          "data/cases/it.csv",
          row.names = FALSE)

# Germany
# https://github.com/jgehrcke/covid-19-germany-gae/

## download and process data
de <-
  read.csv(
    "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rki-by-state.csv",
    stringsAsFactors = FALSE
  ) %>%
  select(time_iso8601, DE.BE, DE.HH, DE.NW) %>%
  rename(
    date = time_iso8601,
    Berlin = DE.BE,
    Hamburg = DE.HH,
    `North Rhine-Westphalia` = DE.NW
  ) %>%
  pivot_longer(
    cols = c("Berlin", "Hamburg", "North Rhine-Westphalia"),
    names_to = "state",
    values_to = "cases"
  ) %>%
  mutate(country = "Germany",
         date = as.Date(date)) %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(de,
          "data/cases/de.csv",
          row.names = FALSE)

# Canada
# https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html

## download and process data
ca <-
  read.csv(
    "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
    stringsAsFactors = FALSE,
    colClasses = c("date" = "Date")
  ) %>%
  select(prname, date, numtotal) %>%
  rename(state = prname, cases = numtotal) %>%
  filter(state %in% c("British Columbia", "Ontario", "Quebec")) %>%
  mutate(date = as.Date(as.character(date), "%d-%m-%y")) %>%
  ### fill in gaps in time series
  right_join(data.frame(
    state = rep(c("British Columbia", "Ontario", "Quebec"),
                each = length(seq.Date(
                  from = min(.$date),
                  to = max(.$date),
                  by = "day"
                ))),
    date = rep(seq.Date(
      from = min(.$date),
      to = max(.$date),
      by = "day"
    ), times = 3),
    stringsAsFactors = FALSE
  ),
  by = c("date", "state")) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  fill(cases, .direction = "down") %>%
  ungroup %>%
  replace_na(list(cases = 0)) %>%
  mutate(country = "Canada") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(ca,
          "data/cases/ca.csv",
          row.names = FALSE)

# Belgium
# https://epistat.wiv-isp.be/covid/

## download and process data
be <-
  read.csv(
    "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv",
    stringsAsFactors = FALSE,
    colClasses = c("DATE" = "Date")
  ) %>%
  filter(REGION == "Brussels" & !is.na(DATE)) %>%
  select(DATE, CASES) %>%
  rename(date = DATE, cases_daily = CASES) %>%
  group_by(date) %>%
  summarise(cases_daily = sum(cases_daily)) %>%
  right_join(data.frame(date = seq.Date(
    from = min(.$date),
    to = max(.$date),
    by = "day"
  )), by = "date") %>%
  replace_na(list("cases_daily" = 0)) %>%
  mutate(cases = cumsum(cases_daily)) %>%
  select(date, cases) %>%
  mutate(state = "Brussels",
         country = "Belgium") %>%
  ungroup()  %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(be,
          "data/cases/be.csv",
          row.names = FALSE)

# Austria
# https://covid19.spiessknafl.at/d/2fa2-Y_Wz222/covid-19-2
# export provincial case data manually using YYYY-MM-DD date format for up to 2020-04-13
# scrape rest of Wien data manually from archive of gov page: www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html
# save as data/cases/raw/at_raw.csv

## read and process data
at <- read.csv(
  "data/cases/raw/at_raw.csv",
  stringsAsFactors = FALSE,
  colClasses = c("Time" = "Date"),
  sep = ";"
) %>%
  mutate(Value = as.integer(ifelse(Value == "null", NA, Value))) %>%
  rename(state = Series,
         date = Time,
         cases = Value) %>%
  filter(state == "Wien") %>%
  arrange(date) %>%
  ### only want final report from each day
  group_by(date) %>%
  top_n(1, cases) %>%
  distinct() %>%
  ungroup() %>%
  ### currently no missing values, so next two steps are not actually needed
  right_join(., data.frame(date = seq.Date(
    from = min(.$date),
    to = max(.$date),
    by = "day"
  )), by = "date") %>%
  fill(cases, .direction = "down") %>%
  mutate(state = "Vienna",
         country = "Austria") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(at,
          "data/cases/at.csv",
          row.names = FALSE)

# South Korea
# https://github.com/jihoo-kim/Data-Science-for-COVID-19

## download and process data
kr <-
  read.csv(
    "https://raw.githubusercontent.com/jihoo-kim/Data-Science-for-COVID-19/master/dataset/Time/TimeProvince.csv",
    stringsAsFactors = FALSE,
    colClasses = c("date" = "Date")
  ) %>%
  select(date, province, confirmed) %>%
  rename(state = province, cases = confirmed) %>%
  filter(state == "Seoul") %>%
  mutate(country = "South Korea") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(kr,
          "data/cases/kr.csv",
          row.names = FALSE)

# Brazil
# https://covid.saude.gov.br/
# download manually at top of page (Arquivo CSV)
# save as data/cases/raw/br_raw.csv

## read and process data
br <- read.csv(
  "data/cases/raw/br_raw.csv",
  stringsAsFactors = FALSE,
  sep = ";",
  fileEncoding = "Latin1"
) %>%
  mutate(data = as.Date(data, "%d/%m/%Y")) %>%
  select(estado, data, casosAcumulados) %>%
  rename(state = estado,
         date = data,
         cases = casosAcumulados) %>%
  filter(state == "SP") %>%
  mutate(country = "Brazil",
         state = "São Paulo")  %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

write.csv(br,
          "data/cases/br.csv",
          row.names = FALSE)

# Japan
# https://catalog.data.metro.tokyo.lg.jp/dataset/t000010d0000000068
# save data as data/cases/raw/jp_raw.csv

## read and process data
jp <- read.csv("data/cases/raw/jp_raw.csv",
               stringsAsFactors = FALSE) %>%
  rename(date =  公表_年月日) %>%
  select(date) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(cases = n()) %>%
  right_join(data.frame(date = seq.Date(
    from = min(.$date),
    to = max(.$date),
    by = "day"
  )), by = "date") %>%
  replace_na(list(cases = 0)) %>%
  mutate(cases = cumsum(cases),
         country = "Japan",
         state = "Tokyo") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(jp,
          "data/cases/jp.csv",
          row.names = FALSE)

# Portugal
# https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Portugal#Cases_timeline
# transcribe and save data as data/cases/raw/pt_raw.csv

## read and process data
pt <- read.csv(
  "data/cases/raw/pt_raw.csv",
  stringsAsFactors = FALSE,
  colClasses = c("date" = "Date")
) %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(pt,
          "data/cases/pt.csv",
          row.names = FALSE)

# Russia
# https://yandex.ru/web-maps/covid19?ll=37.646921%2C55.725146&z=9 (Moscow)
# https://yandex.ru/web-maps/covid19?ll=30.424830%2C59.939314&z=9 (St. Petersburg)
# transcribe and save data as data/cases/raw/ru_raw.csv

## read and process data
ru <- read.csv(
  "data/cases/raw/ru_raw.csv",
  stringsAsFactors = FALSE,
  colClasses = c("date" = "Date")
) %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(ru,
          "data/cases/ru.csv",
          row.names = FALSE)

# Sweden
# https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/bekraftade-fall-i-sverige/

## read and process data
se_tmp <- tempfile(fileext = ".xlsx")
download.file(url = "https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
              destfile = se_tmp,
              mode = "wb")
se <- read.xlsx(se_tmp, sheetName = "Antal per dag region") %>%
  select(Statistikdatum, Stockholm) %>%
  rename(date = Statistikdatum) %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  pivot_longer(Stockholm,
               names_to = "state",
               values_to = "cases") %>%
  mutate(cases = cumsum(cases),
         country = "Sweden") %>%
  select(state, country, date, cases) %>%
  arrange(state, country, date)

## write csv
write.csv(se,
          "data/cases/se.csv",
          row.names = FALSE)