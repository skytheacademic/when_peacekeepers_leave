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

### create a function to compute queen contiguity
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

##### CLEAN PRIO DATA #####

### load data
prio_static <- read_csv("./data/prio/PRIO-GRID Static Variables - 2022-06-03.csv")
prio_yearly <- read_csv("./data/prio/PRIO-GRID Yearly Variables for 1999-2014 - 2022-06-03.csv")

### expand static data to cover years when "yearly" data isn't available
year <- seq(1999, 2022, 1)
prio_static <- expand_grid(prio_static, year)

### merge on grid ID and year, then reorder variables
prio <- full_join(prio_static, prio_yearly, by = c("gid", "year")) %>% 
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

### there are duplicate gid-month-years because of different missions, so we
### need to aggregate everything by those variables. all vars are sums/counts,
### so we can just sum them all.
radpko <- radpko %>% 
  select(-c(country, mission, date)) %>% 
  relocate(c(year, month), .after = gid) %>% 
  group_by(gid, year, month) %>% 
  summarise(across(units_deployed:afr_unmob, sum)) %>% 
  ungroup()

##### CLEAN ACLED DATA #####

### load data and clean it
acled <- read_csv("./data/acled/1999-01-01-2021-12-31.csv") %>% 
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

### clean names
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
df <- expand_grid(gid = all_gids, 
                  year = seq(1999, 2021, 1), 
                  month = seq(1, 12, 1))

### merge the acled data to the main df
df <- full_join(df, acled, by = c("gid", "year", "month"))

### merge the radpko data to the main df
df <- full_join(df, radpko, by = c("gid", "year", "month"))

### merge the PRIO data to the main df
df <- full_join(df, prio, by = c("gid", "year", "month"))

### clean up
rm(acled, all_gids, prio, proj_crs, radpko)

### drop unneeded data
df <- df %>% 
  # some acled events have grids outside terrestrial range; drop these
  filter(gid >= 49182 & gid <= 249344) %>% 
  # some years are outside the range we care about; drop these
  filter(year >= 1999 & year <= 2021) %>% 
  # some ACLED data is outside the PRIO static table. No PKO ops here, so drop
  filter(!is.na(row) & !is.na(col))

### reorganize and rename
df <- df %>% 
  relocate(c(row, col), .after = month) %>% 
  select(-c(xcoord, ycoord)) %>% 
  rename_at(vars(fatalities_protests:fatalities_explosions_remote_violence), 
            function(x) paste0("acled_", x)) %>% 
  rename_at(vars(units_deployed:afr_unmob), function(x) paste0("radpko_", x)) %>% 
  rename_at(vars(agri_gc:water_ih), function(x) paste0("prio_", x)) 

### subset to Africa for the purpose of our analysis
df <- df %>% 
  filter(col < 500 & col > 300 & row < 260 & row > 80)

### carry forward country IDs from Gleditsch and Ward, so we don't drop later
### years. No boundary changes since 2014 -- South Sudan are already in the data 
df <- df %>% 
  group_by(gid) %>% 
  fill(prio_gwno) %>% 
  ungroup()

### drop any countries that we've clipped which aren't in Africa. This clips out
### all the water too. It does mean we lose some territories and microstates, eg
### Western Sahara.
df <- df %>% 
  filter(prio_gwno %in% c(402, 404, 411, 420, 432, 433, 434, 435, 436, 437, 438, 
                          439, 450, 451, 452, 461, 471, 475, 481, 482, 483, 484, 
                          490, 500, 501, 510, 516, 517, 520, 522, 530, 531, 540, 
                          541, 551, 552, 553, 560, 565, 570, 571, 572, 580, 581, 
                          590, 600, 615, 616, 620, 625, 626, 651))

##### RECODE VARIABLES #####

### RADPKO data are complete 1999-2021 for Africa, so we recode NA to 0
df <- df %>% 
  mutate(across(radpko_units_deployed:radpko_afr_unmob, ~replace_na(.x, 0)))

### create an "any peacekeepers" variable for RADPKO
df <- df %>% 
  mutate(radpko_pko_deployed_any = case_when(radpko_pko_deployed > 0 ~ 1,
                                             TRUE ~ 0))

### ACLED are also complete over this time/area, so recode NA to 0 here too
df <- df %>% 
  mutate(across(acled_fatalities_protests:acled_fatalities_explosions_remote_violence, 
                ~replace_na(.x, 0)))

### create an "any fatalities" variable for ACLED
df <- df %>% 
  mutate(acled_fatalities_any = sum(c_across(
    acled_fatalities_protests:acled_fatalities_explosions_remote_violence)))

##### CREATE SPATIAL MEASURES #####

### merge shapefile here (faster than if we do it above), then set it as spatial
df <- left_join(df, prio_shp, by = c("gid", "col", "row")) %>%
  st_as_sf(sf_column_name = "geometry")

### get neighbors -- fastest/most robust way is to split it by month/year and 
### compute each. Adjacency doesn't change over time but spatial properties 
### prevent a simple copy-paste. At the same time, this is faster than iterating.
z <- Sys.time()
nb <- df %>% 
  group_by(year, month) %>% 
  mutate(neighbor = st_queen(.)) %>% 
  ungroup()
Sys.time() - z

nb <- split(df, f = list(df$year, df$month))
nb <- lapply(nb, FUN = function(x){
  x <- st_queen(x)
})

### bind back together
df2 <- do.call(rbind, nb) %>% as_tibble()
class(df2)


##### VERSION CONTROL #####
sessionInfo()



##### OLD TO DELETE #####

# ### plotting which cells we're using
# xx <- df[which(df$year == 2014 & df$month == 1),]
# ggplot(df, aes(x = col, y = row)) + 
#   geom_tile(aes(fill = prio_temp)) +
#   coord_fixed()





# p1 <- df %>%
#   filter(year == 2005 & month == 12) %>%
#   ggplot() +
#   geom_sf(aes(fill = prio_forest_gc), lwd = 0)
# ggsave("/users/zmwarner/desktop/p1.pdf", height = 8, width = 8)







df2 %>% 
  filter(year == 2000 & month == 1) %>% 
  ggplot() + geom_sf() + 
  # random place in canada
  geom_sf(data = df2[1000,], fill = "red") + 
  # neighbors from nb object
  geom_sf(data = df2[unlist(df2$neighbor[1000]),], fill = "blue") 

