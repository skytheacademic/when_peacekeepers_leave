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

##### DIFF-IN-DIFF #####

gids <- sample(unique(df$gid), 1200)
dd <- df[which(df$gid %in% gids),]

z <- Sys.time()
out <- att_gt(yname = "acled_fatalities_any", tname = "time", idname = "gid", 
              gname = "first_treated", data = df, pl = T, cores = 14)
Sys.time() - z
es <- aggte(out, type = "group")
summary(es)



