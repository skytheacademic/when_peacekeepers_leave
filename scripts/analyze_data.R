#!/usr/bin/env Rscript
# analyze_data.R
# Produces all analysis, tables, and figures
# Zach Warner
# 9 June 2022

##### SET UP #####

### load packages
library(did); library(sf); library(tidyverse)

### set working directory ###
setwd("/Users/zmwarner/github/when_peacekeepers_leave")

### set seed
set.seed(8675309) # hey jenny

##### READ IN DATA #####

### load it
df <- read_rds("./data/Kunkel-Atkinson-Warner-final.rds")

##### SUMMARY STATISTICS #####

### number of years
sort(unique(df$year))

### number of cells
length(unique(df$gid))

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE #####

gids <- sample(unique(df$gid), 1200)
dd <- df[which(df$gid %in% gids),]

z <- Sys.time()
out <- att_gt(yname = "acled_fatalities_any", tname = "time", idname = "gid", 
              gname = "first_treated", data = dd, pl = T, cores = 14)
Sys.time() - z
es <- aggte(out, type = "group")
summary(es)

##### OLD-SCHOOL DID FOR IF PKO REDUCE VIOLENCE #####

# time = post_treatment

# treated = treated
df <- df %>%
  group_by(gid) %>% 
  mutate(treated = case_when(sum(radpko_pko_deployed_any, na.rm = T) > 0 ~ 1,
                             TRUE ~ 0)) %>% 
  ungroup()

m1 <- glm(acled_fatalities_any ~ treated + post_treatment + 
            treated*post_treatment, data = df)
summary(m1)

