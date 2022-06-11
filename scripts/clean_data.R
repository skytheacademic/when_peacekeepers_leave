#!/usr/bin/env Rscript
# clean_data.R
# Merges all data and cleans it for analysis
# Zach Warner
# 8 June 2022

##### SET UP #####

### load packages
library(doSNOW); library(foreach); library(janitor); library(lubridate)
library(parallel); library(sf); library(tidyverse)

### register the clusters
cl <- makeCluster(detectCores()-1)
registerDoSNOW(cl)

### set working directory ###
setwd("/Users/zmwarner/github/when_peacekeepers_leave")

### create a function to compute queen contiguity
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

### set seed
set.seed(8675309) # hey jenny

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
         fatalities_strategic_developments = strategic_developments)

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
  mutate(acled_fatalities_any = case_when(rowSums(across(
    acled_fatalities_protests:acled_fatalities_explosions_remote_violence)) > 0 ~ 1,
    TRUE ~ 0))

### create a sum fatalaties variable for ACLED
df <- df %>% 
  mutate(acled_fatalities_all = rowSums(across(
    acled_fatalities_protests:acled_fatalities_explosions_remote_violence)))


##### CREATE SPATIAL MEASURES #####

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

### now we're going to loop through the entire df, computing neighboring violence
### run loop getting violence in adjacent grid cells. this is actually fastest
### given the panel structure of the data, once parallelized
# set up progress bar
pb <- txtProgressBar(max = nrow(df), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
# run loop
out <- foreach(i = 1:nrow(df), .combine = rbind, .options.snow = opts) %dopar% {
  # get grid cell info
  yr <- df$year[i]
  mn <- df$month[i]
  gi <- as.character(df$gid[i])
  # subset to relevant observations based on queen adjacency
  tmp <- df[which(df$year == yr & df$month == mn &
                  df$gid %in% nb_gid[[which(names(nb_gid) == as.character(gi))]]),]
  # compute and store values
  c(sum(tmp$acled_fatalities_protests, na.rm = T),
    sum(tmp$acled_fatalities_strategic_developments, na.rm = T),
    sum(tmp$acled_fatalities_riots, na.rm = T),
    sum(tmp$acled_fatalities_violence_against_civilians, na.rm = T),
    sum(tmp$acled_fatalities_battles, na.rm = T),
    sum(tmp$acled_fatalities_explosions_remote_violence, na.rm = T),
    ifelse(sum(tmp$acled_fatalities_any, na.rm = T) > 0, 1, 0),
    sum(tmp$acled_fatalities_all, na.rm = T))
}
# turn off progress bar and clusters
close(pb)
stopCluster(cl) 

### convert the output into a tibble and rename
out <- as_tibble(out) %>% 
  rename(neighbor_fatalities_protests = V1, 
         neighbor_fatalities_strategic_developments  = V2,
         neighbor_fatalities_riots = V3,
         neighbor_fatalities_violence_against_civilians = V4,
         neighbor_fatalities_battles = V5,
         neighbor_fatalities_explosions_remote_violence = V6,
         neighbor_fatalities_any = V7,
         neighbor_fatalities_all = V8)

### bind them together
df <- bind_cols(df, out)

### clean up
rm(cl, i, ids, nb, nb_gid, opts, out, pb, prio_shp, progress, st_queen)

##### CREATE VARIABLES FOR PACKAGE `DID` #####

### create a unified time variable. this needs to be a positive integer for `did`
df <- df %>% 
  mutate(time = (year-1999)*(12) + month)

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
  x$post_treatment_leave <- ifelse(x$first_treated != 0 & 
                                     x$time >= x$first_treated, 
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

##### FINAL CLEANING AND EXPORT #####

### reorder variables
df <- df %>% 
  rename(prio_xcoord = xcoord, prio_ycoord = ycoord, 
         prio_geometry = geometry) %>% 
  relocate(radpko_pko_deployed_any, .after = radpko_afr_unmob) %>% 
  relocate(c(acled_fatalities_any, acled_fatalities_all),
           .after = acled_fatalities_explosions_remote_violence) %>% 
  relocate(c(time, first_treated, treated, post_treatment, first_treated_leave,
             treated_leave, post_treatment_leave), .after = month)

### save it
write_rds(df, "./data/Kunkel-Atkinson-Warner-final.rds")

##### VERSION CONTROL #####
sessionInfo()
# R version 4.1.0 (2021-05-18)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Big Sur 11.6
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] parallel  stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
# [1] forcats_0.5.1    stringr_1.4.0    dplyr_1.0.7      purrr_0.3.4     
# [5] readr_2.0.1      tidyr_1.1.3      tibble_3.1.5     ggplot2_3.3.5   
# [9] tidyverse_1.3.1  sf_1.0-2         lubridate_1.7.10 janitor_2.1.0   
# [13] doSNOW_1.0.20    snow_0.4-3       iterators_1.0.13 foreach_1.5.1   
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.7         class_7.3-19       assertthat_0.2.1   utf8_1.2.2        
# [5] R6_2.5.1           cellranger_1.1.0   backports_1.2.1    reprex_2.0.1      
# [9] e1071_1.7-9        httr_1.4.2         pillar_1.6.4       rlang_0.4.12      
# [13] readxl_1.3.1       rstudioapi_0.13    bit_4.0.4          munsell_0.5.0     
# [17] proxy_0.4-26       broom_0.7.9        compiler_4.1.0     modelr_0.1.8      
# [21] pkgconfig_2.0.3    tidyselect_1.1.1   codetools_0.2-18   fansi_0.5.0       
# [25] crayon_1.4.1       tzdb_0.1.2         dbplyr_2.1.1       withr_2.4.2       
# [29] wk_0.5.0           grid_4.1.0         jsonlite_1.7.2     gtable_0.3.0      
# [33] lifecycle_1.0.1    DBI_1.1.1          magrittr_2.0.1     units_0.7-2       
# [37] scales_1.1.1       KernSmooth_2.23-20 vroom_1.5.5        cli_3.0.1         
# [41] stringi_1.7.5      fs_1.5.0           snakecase_0.11.0   xml2_1.3.2        
# [45] ellipsis_0.3.2     generics_0.1.1     vctrs_0.3.8        s2_1.0.6          
# [49] tools_4.1.0        bit64_4.0.5        glue_1.4.2         hms_1.1.0         
# [53] colorspace_2.0-2   classInt_0.4-3     rvest_1.0.1        haven_2.4.3


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

# ### create some empty variables for loop below
# df <- df %>% 
#   mutate(neighbor_fatalities_protests = NA,
#          neighbor_fatalities_strategic_developments = NA,
#          neighbor_fatalities_riots = NA,
#          neighbor_fatalities_violence_against_civilians = NA,
#          neighbor_fatalities_battles = NA, 
#          neighbor_fatalities_explosions_remote_violence = NA,
#          neighbor_fatalities_any = NA)

# ### run loop getting violence in adjacent grid cells. this is actually fastest
# ### given the panel structure of the data
# for(i in 1:nrow(df)){
#   # get grid cell info
#   yr <- df$year[i]
#   mn <- df$month[i]
#   co <- df$col[i]
#   ro <- df$row[i]
#   gi <- df$gid[i]
#   # subset to relevant observations based on queen adjacency
#   tmp <- df %>% 
#     filter(year == yr & month == mn) %>% 
#     filter(col >= co - 1 & col <= co + 1) %>% 
#     filter(row >= ro - 1 & row <= ro + 1) %>% 
#     filter(gid != gi)
#   # compute and store values
#   df$neighbor_fatalities_protests[i] <- 
#     sum(tmp$acled_fatalities_protests, na.rm = T)
#   df$neighbor_fatalities_strategic_developments[i] <- 
#     sum(tmp$acled_fatalities_strategic_developments, na.rm = T)
#   df$neighbor_fatalities_riots[i] <- 
#     sum(tmp$acled_fatalities_riots, na.rm = T)
#   df$neighbor_fatalities_violence_against_civilians[i] <- 
#     sum(tmp$acled_fatalities_violence_against_civilians, na.rm = T)
#   df$neighbor_fatalities_battles[i] <- 
#     sum(tmp$acled_fatalities_battles, na.rm = T)
#   df$neighbor_fatalities_explosions_remote_violence[i] <- 
#     sum(tmp$acled_fatalities_explosions_remote_violence, na.rm = T)
#   df$neighbor_fatalities_any[i] <- 
#     ifelse(sum(tmp$acled_fatalities_any, na.rm = T) > 0, 1, 0)
#   # update progress bar
#   print(i)
# }

# 
# 
# df2 %>% 
#   filter(year == 2000 & month == 1) %>% 
#   ggplot() + geom_sf() + 
#   # random place in canada
#   geom_sf(data = df2[1000,], fill = "red") + 
#   # neighbors from nb object
#   geom_sf(data = df2[unlist(df2$neighbor[1000]),], fill = "blue") 

# 
# 
# ### create a function to compute queen contiguity
# st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

