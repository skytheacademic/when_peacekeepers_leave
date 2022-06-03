#!/usr/bin/env Rscript
# clean_data.R
# Merges all data and cleans it for analysis
# Zach Warner
# 3 June 2022

##### SET UP #####

### load packages
library(janitor); library(lubridate); library(sf); library(tidyverse)

### set working directory ###
setwd("/Users/zmwarner/github/when_peacekeepers_leave")

### set seed
set.seed(8675309) # hey jenny

##### CLEAN PRIO DATA #####

### load data
prio_static <- read_csv("./data/prio/PRIO-GRID Static Variables - 2022-06-03.csv")
prio_yearly <- read_csv("./data/prio/PRIO-GRID Yearly Variables for 1999-2014 - 2022-06-03.csv")

### expand static data to cover years when "yearly" data isn't available
year <- seq(1999, 2022, 1)
prio_static <- expand_grid(prio_static, year)

### merge on grid ID and year, then clean
prio <- full_join(prio_static, prio_yearly, by = c("gid", "year")) %>% 
  # move year variable to just after GID
  relocate(year, .after = "gid")

### expand into monthly data
month <- seq(1, 12, 1)
prio <- expand_grid(prio, month) %>% 
  relocate(month, .after = "year") %>% 
  arrange(gid, year, month)

### clean up
rm(month, prio_static, prio_yearly, year)

##### CLEAN RADPKO DATA #####

### load data
radpko <- read_csv("./data/radpko/radpko_grid.csv") %>% 
  # make the date variable a date type
  mutate(date = ymd(date),
         month = month(date),
         year = year(date)) %>% 
  # rename variable for ease of merging
  rename(gid = prio.grid)

##### CLEAN ACLED DATA #####

### load data and clean it
acled <- read_csv("./data/acled/1999-01-01-2015-01-01.csv") %>% 
  # recode Abyei to match RADPKO data
  mutate(country = case_when(admin1 == "Abyei" ~ "Abyei",
                             TRUE ~ country)) %>% 
  # make the date variable a date type then subset to post-1999
  mutate(event_date = dmy(event_date)) %>% 
  filter(event_date >= "1999-01-01")

#### MERGE ACLED DATA WITH PRIO GRID IDS #####

### get geographic data for prio grids using the shapefiles
prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", 
                    stringsAsFactors = F)

### save the CRS
proj_crs <- st_crs(prio_shp)

### convert acled to an sf object with a shared CRS
acled <- st_as_sf(acled, coords = c("longitude", "latitude"), crs = proj_crs)

### join acled events to prio grid info
acled <- st_join(acled, prio_shp)

### reshape ACLED long to wide, to aggregate deaths by type
acled <- acled %>% 
  # get month info
  mutate(month = month(event_date)) %>% 
  # remove extra spatial info, now unneeded
  as_tibble() %>% 
  # trim to just variables needed
  select(gid, year, month, event_type, fatalities) %>% 
  # aggregate
  group_by(gid, year, month, event_type) %>% 
  summarize(deaths = sum(fatalities)) %>% 
  ungroup() %>% 
  # reshape
  pivot_wider(id_cols = c("gid", "year", "month"), 
              names_from = "event_type",
              values_from = "deaths")

### clean names and expand grid to include all GID-month-years
acled <- acled %>% 
  clean_names() %>% 
  rename(fatalities_protests = protests, fatalities_riots = riots,
         fatalities_violence_against_civilians = violence_against_civilians,
         fatalities_explosions_remote_violence = explosions_remote_violence,
         fatalities_battles = battles,
         talities_strategic_developments = strategic_developments)

##### MERGE EVERYTHING TOGETHER #####

### create a full grid of gid-month-years
all_gids <- sort(unique(c(acled$gid, prio$gid, radpko$gid)))
df <- expand_grid(gid = all_gids, year = seq(1999, 2021, 1), month = seq(1, 12, 1))

### merge the acled data to the main df
df <- full_join(df, acled, by = c("gid", "year", "month"))

### merge the radpko data to the main df
df <- full_join(df, radpko, by = c("gid", "year", "month"))

### merge the PRIO data to the main df
df <- full_join(df, prio, by = c("gid", "year", "month"))

### clean up
rm(acled, all_gids, prio, prio_shp, proj_crs, radpko)


##### VERSION CONTROL #####
sessionInfo()

