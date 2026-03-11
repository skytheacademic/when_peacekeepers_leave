#!/usr/bin/env Rscript
# clean_data.R
# Merges all data and cleans it for analysis
# Zach Warner, Sky Kunkel
# 8 June 2022

##### SET UP #####

### load packages
library(doSNOW); library(foreach); library(lubridate)
library(parallel); library(sf); library(tidyverse)

### register the clusters
cl <- makeCluster(detectCores()-1)
registerDoSNOW(cl)

### set working directory ###
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../")

### create a function to compute queen contiguity
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

### set seed
set.seed(8675309) # hey jenny

##### CLEAN PRIO DATA #####
### load data
prio_gwno <-  read_csv("./data/prio/PRIO-GRID Yearly Variables for 1999-2014 - 2022-06-03.csv") %>% 
  distinct(gid, gwno)

##### CLEAN RADPKO DATA #####
### there are duplicate gid-month-years because of different missions, so we
### need to aggregate everything by those variables. all vars are sums/counts,
### so we can just create a binary based on the grid.

### load data
radpko <- read_csv("./data/radpko/radpko_grid.csv") %>% 
  # make the date variable a date type
  mutate(date = ymd(date),
         month = month(date),
         year = year(date)) %>% 
  filter(year > 1999 & year < 2018) %>% 
  rename(gid = prio.grid) %>% 
  # collapse to unique gid-month-years: if any row exists, PKO was present
  distinct(gid, year, month) %>% 
  ### create an "any peacekeepers" variable for RADPKO
  mutate(radpko_pko_deployed_any = 1L)

##### CLEAN ACLED DATA #####

### load data and clean it
acled <- read_csv("./data/acled/1999-01-01-2021-12-31.csv") %>% 
  # recode Abyei to match RADPKO data
  mutate(country = case_when(admin1 == "Abyei" ~ "Abyei",
                             TRUE ~ country)) %>% 
  # make the date variable a date type then subset to post-1999
  mutate(event_date = dmy(event_date)) %>% 
  filter(event_date >= "2000-01-01" & event_date < "2018-01-01") %>% 
  # VAC only
  filter(event_type == "Violence against civilians")

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

### add VAC events and deaths by actor
acled$event = 1
acled$vac_gov_death_all = 0
acled$vac_gov_death_all[acled$interaction == 17 | acled$interaction == 37] = 
  acled$fatalities[acled$interaction == 17 | acled$interaction == 37]
acled$vac_reb_death_all = 0
acled$vac_reb_death_all[acled$interaction == 27] = acled$fatalities[acled$interaction == 27]
acled$vac_gov_event_all = 0
acled$vac_gov_event_all[acled$interaction == 17 | acled$interaction == 37] = 
  acled$event[acled$interaction == 17 | acled$interaction == 37]
acled$vac_reb_event_all = 0
acled$vac_reb_event_all[acled$interaction == 27] = acled$event[acled$interaction == 27]

### reshape ACLED long to wide, to aggregate deaths by type
acled <- acled %>% 
  # get month info
  mutate(month = month(event_date)) %>% 
  # remove extra spatial info, now unneeded
  as_tibble() %>% 
  # remove un-needed variables
  select(-c(iso, data_id, event_id_cnty, event_id_no_cnty, time_precision, 
            sub_event_type, actor1, assoc_actor_1, actor2, assoc_actor_2,
            region, country, admin1, admin2, admin3, location, geo_precision, 
            source, source_scale, notes, timestamp, iso3,
            xcoord, ycoord, col, row, geometry)) %>% 
  group_by(gid, year, month) %>% 
  summarise(across(event:vac_reb_event_all, sum), .groups = "drop")

##### MERGE EVERYTHING TOGETHER #####

### create a full grid of gid-month-years
all_gids <- sort(unique(c(acled$gid, radpko$gid)))
df <- expand_grid(gid = all_gids, 
                  year = seq(2000, 2017, 1), 
                  month = seq(1, 12, 1))

### merge the acled data to the main df
df <- full_join(df, acled, by = c("gid", "year", "month"))

### merge the radpko data to the main df
df <- full_join(df, radpko, by = c("gid", "year", "month"))

### clean up
rm(acled, all_gids, proj_crs, radpko)

### add row/col from shapefile and gwno from static for filtering
df <- df %>% 
  left_join(prio_gwno, by = "gid") %>% 
  left_join(prio_shp %>% as_tibble() %>% select(gid, row, col), by = "gid")

### drop unneeded data
df <- df %>% 
  # some acled events have grids outside terrestrial range; drop these
  filter(gid >= 49182 & gid <= 249344) %>% 
  # some years are outside the range we care about; drop these
  filter(year >= 2000 & year <= 2017) %>% 
  # some ACLED data is outside the PRIO static table. No PKO ops here, so drop
  filter(!is.na(row) & !is.na(col))

### rename ACLED variables
df <- df %>% 
  relocate(c(row, col), .after = month) %>% 
  rename_at(vars(event:vac_reb_event_all), 
            function(x) paste0("acled_", x))

### subset to Africa for the purpose of our analysis
df <- df %>% 
  filter(col < 500 & col > 300 & row < 260 & row > 80)

### carry forward country IDs from Gleditsch and Ward, so we don't drop later
### years. No boundary changes since 2014 -- South Sudan are already in the data 
df <- df %>% 
  group_by(gid) %>% 
  fill(gwno) %>% 
  ungroup()

### drop any countries that we've clipped which aren't in Africa. This clips out
### all the water too. It does mean we lose some territories and microstates, eg
### Western Sahara.
african_gids <- prio_gwno %>%
  filter(gwno %in% c(402, 404, 411, 420, 432, 433, 434, 435, 436, 437, 438, 
                      439, 450, 451, 452, 461, 471, 475, 481, 482, 483, 484, 
                      490, 500, 501, 510, 516, 517, 520, 522, 530, 531, 540, 
                      541, 551, 552, 553, 560, 565, 570, 571, 572, 580, 581, 
                      590, 600, 615, 616, 620, 625, 626, 651)) %>%
  distinct(gid)

df <- semi_join(df, african_gids, by = "gid")

##### RECODE VARIABLES #####

### RADPKO data are complete 1999-2018 for Africa, so we recode NA to 0
df <- df %>% 
  mutate(radpko_pko_deployed_any = replace_na(radpko_pko_deployed_any, 0))

### ACLED are also complete over this time/area, so recode NA to 0 here too
df <- df %>% 
  mutate(across(acled_event:acled_vac_reb_event_all, ~replace_na(.x, 0)))

### derive binary _any variables from _all
df <- df %>% 
  mutate(
    acled_vac_gov_death_any = ifelse(acled_vac_gov_death_all > 0, 1, 0),
    acled_vac_gov_event_any = ifelse(acled_vac_gov_event_all > 0, 1, 0),
    acled_vac_reb_death_any = ifelse(acled_vac_reb_death_all > 0, 1, 0),
    acled_vac_reb_event_any = ifelse(acled_vac_reb_event_all > 0, 1, 0)
  )

##### CREATE SPATIAL MEASURES #####
### now we're going to loop through the entire df, computing neighboring violence
### run loop getting violence in adjacent grid cells. this is actually fastest
### given the panel structure of the data, once parallelized
# set up progress bar

### merge shapefile here (faster than if we do it above), then set it as spatial
df <- left_join(df, prio_shp, by = c("gid", "col", "row")) %>%
  st_as_sf(sf_column_name = "geometry")

### compute neighbors - we only need to do one month since grids don't change
nb <- df %>% 
  filter(year == 2000, month == 1) %>%  
  mutate(neighbor = st_queen(.)) %>% 
  select(gid, neighbor)

### convert the neighbor list from rows to gids -- loop due to odd structure.
### we're leaving the tidyverse for a bit, mea culpa
nb_gid <- vector("list", nrow(nb))
for(i in 1:nrow(nb)){
  ids <- nb$gid[unlist(nb$neighbor[i])]
  nb_gid[[i]] <- ids
}
names(nb_gid) <- nb$gid

### run loop getting violence in adjacent grid cells
pb <- txtProgressBar(max = nrow(df), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

out <- foreach(i = 1:nrow(df), .combine = rbind, .options.snow = opts) %dopar% {
  # get grid cell info
  yr <- df$year[i]
  mn <- df$month[i]
  gi <- as.character(df$gid[i])
  # subset to relevant observations based on queen adjacency
  tmp <- df[which(df$year == yr & df$month == mn &
                  df$gid %in% nb_gid[[which(names(nb_gid) == as.character(gi))]]),]
  # compute and store values
  c(sum(tmp$acled_vac_gov_death_all, na.rm = T),
    sum(tmp$acled_vac_reb_death_all, na.rm = T),
    sum(tmp$acled_vac_gov_event_all, na.rm = T),
    sum(tmp$acled_vac_reb_event_all, na.rm = T),
    sum(tmp$radpko_pko_deployed_any, na.rm = T))
}
# turn off progress bar and clusters
close(pb)
stopCluster(cl) 

### convert the output into a tibble and rename
out <- as_tibble(out) %>% 
  rename(neighbor_vac_gov_death_all = V1,
         neighbor_vac_reb_death_all = V2,
         neighbor_vac_gov_event_all = V3,
         neighbor_vac_reb_event_all = V4,
         neighbor_pko_all = V5)

### bind them together
df <- bind_cols(df, out)

### derive _any from _all
df <- df %>% 
  mutate(
    neighbor_vac_gov_death_any = ifelse(neighbor_vac_gov_death_all > 0, 1, 0),
    neighbor_vac_reb_death_any = ifelse(neighbor_vac_reb_death_all > 0, 1, 0),
    neighbor_vac_gov_event_any = ifelse(neighbor_vac_gov_event_all > 0, 1, 0),
    neighbor_vac_reb_event_any = ifelse(neighbor_vac_reb_event_all > 0, 1, 0),
    neighbor_pko_any           = ifelse(neighbor_pko_all > 0, 1, 0)
  )

### clean up
rm(cl, i, ids, nb, nb_gid, opts, out, pb, prio_shp, progress, st_queen)

##### CREATE VARIABLES FOR PACKAGE `DID` #####

### create a unified time variable. this needs to be a positive integer for `did`
df <- df %>% 
  mutate(time = (year-2000)*(12) + month)

### split by GID and make some variables
dd <- df %>% as.data.frame() %>% select(gid, time, radpko_pko_deployed_any)
dd <- split(dd, f = dd$gid)
dd <- lapply(dd, FUN = function(x){
  y <- x[which(x$radpko_pko_deployed_any == 1),]
  # create a "first treated" variable. needs to be 0 for untreated
  x$first_treated <- ifelse(nrow(y) == 0, 0, min(y$time))
  # create a "post treated" variable. needs to be 0 until treatment then 1
  x$post_treatment <- ifelse(x$first_treated != 0 & x$time >= x$first_treated, 
                             1, 0)
  # create a "treated" variable. needs to be 0 if control and 1 if treated
  x$treated <- ifelse(sum(x$radpko_pko_deployed_any, na.rm = T) > 0, 1, 0)
  x
})
dd <- do.call(rbind, dd)
dd <- dd[,c("gid", "time", "first_treated", "treated", "post_treatment")]
# merge back to main df
df <- left_join(df, dd, by = c("gid", "time"))

### now do the same thing but for when the treatment is "peacekeepers leave"
dd <- df %>% as.data.frame() %>% select(gid, time, radpko_pko_deployed_any)
dd <- split(dd, f = dd$gid)
dd <- lapply(dd, FUN = function(x){
  x$l_pko <- lag(x$radpko_pko_deployed_any, 1)
  y <- x[which(x$radpko_pko_deployed_any == 0 & x$l_pko == 1),]
  # create a "first treated" variable. needs to be 0 for untreated
  x$first_treated_leave <- ifelse(nrow(y) == 0, 0, min(y$time))
  # create a "post treated" variable. needs to be 0 until treatment then 1
  x$post_treatment_leave <- ifelse(x$first_treated_leave != 0 & 
                                     x$time >= x$first_treated_leave, 
                                   1, 0)
  # create a "treated" variable. needs to be 0 if control and 1 if treated
  x$treated_leave <- ifelse(sum(x$l_pko, na.rm = T) > 0, 1, 0)
  x
})
dd <- do.call(rbind, dd)
dd <- dd[,c("gid", "time", "first_treated_leave", "treated_leave", 
            "post_treatment_leave")]
# merge back to main df
df <- left_join(df, dd, by = c("gid", "time"))

### clean up
rm(dd)

##### FINAL CLEANING AND EXPORT #####

### reorder variables
df <- df %>% 
  relocate(c(time, first_treated, treated, post_treatment, first_treated_leave,
             treated_leave, post_treatment_leave), .after = month)

### save it
write_rds(df, "./data/Kunkel-Atkinson-Dudley-Warner-final.rds")

##### VERSION CONTROL (Sky) #####
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 11.7.1
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] forcats_0.5.1    stringr_1.4.0    dplyr_1.0.9      purrr_0.3.4      readr_2.1.2      tidyr_1.2.0      tibble_3.1.7    
# [8] ggplot2_3.3.6    tidyverse_1.3.1  sf_1.0-3         lubridate_1.8.0  janitor_2.1.0    doSNOW_1.0.20    snow_0.4-3      
# [15] iterators_1.0.13 foreach_1.5.1   
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.8.3       class_7.3-19       assertthat_0.2.1   utf8_1.2.2         R6_2.5.1           cellranger_1.1.0  
# [7] backports_1.4.1    reprex_2.0.1       e1071_1.7-9        httr_1.4.2         pillar_1.7.0       rlang_1.0.2       
# [13] readxl_1.4.0       rstudioapi_0.13    bit_4.0.4          munsell_0.5.0      proxy_0.4-26       broom_0.8.0       
# [19] compiler_4.1.0     modelr_0.1.8       pkgconfig_2.0.3    tidyselect_1.1.2   codetools_0.2-18   fansi_1.0.3       
# [25] crayon_1.5.1       tzdb_0.3.0         dbplyr_2.1.1       withr_2.5.0        wk_0.5.0           grid_4.1.0        
# [31] jsonlite_1.8.0     gtable_0.3.0       lifecycle_1.0.1    DBI_1.1.1          magrittr_2.0.3     units_0.7-2       
# [37] scales_1.2.0       KernSmooth_2.23-20 cli_3.3.0          stringi_1.7.6      vroom_1.5.7        fs_1.5.0          
# [43] snakecase_0.11.0   xml2_1.3.3         ellipsis_0.3.2     generics_0.1.2     vctrs_0.4.1        s2_1.0.7          
# [49] tools_4.1.0        bit64_4.0.5        glue_1.6.2         hms_1.1.1          colorspace_2.0-3   classInt_0.4-3    
# [55] rvest_1.0.1        haven_2.4.3   
