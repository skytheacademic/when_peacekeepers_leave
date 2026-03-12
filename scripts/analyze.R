#!/usr/bin/env Rscript
# analyze_data.R
# Produces all analysis, tables, and figures
# Sky Kunkel, Zach Warner
# 9 June 2022

##### SET UP #####

### load packages
library(did); library(tidyverse); library(ggtext)

### set working directory ###
# setwd("/Users/zmwarner/github/when_peacekeepers_leave")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../")
### set seed
set.seed(8675309) # hey jenny

##### READ IN DATA #####

### load it
df <- read_rds("./data/Kunkel-Atkinson-Dudley-Warner-final.rds") %>%
  select(-geometry) %>%
  as.data.frame()
gc()

##### SUMMARY STATISTICS #####

### general look of the data
head(df)

### number of years
sort(unique(df$year))

### number of cells
length(unique(df$gid))
options(max.print = 20)

##################################### VIOLENT EVENTS #####################################
# make dataframe to save values
results = data.frame() %>%
  mutate(time = NA, cell = NA, actor = NA, dv = NA, dv_type = NA, att = NA, se = NA)

###### TOTAL #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es1 <- aggte(out1, type = "group", na.rm = T)
gc()
summary(es1)
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "GOV", dv = "Total", dv_type = "Event",
                                    att = es1$overall.att, se = es1$overall.se))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es2 <- aggte(out2, type = "group", na.rm = T)
gc()
summary(es2)
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "REB", dv = "Total", dv_type = "Event",
                                    att = es2$overall.att, se = es2$overall.se))
rm(out2, es2)
gc()
## Neighbor cell, enter ##
set.seed(8675309) # hey jenny

out3 <- att_gt(yname = "neighbor_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es3 <- aggte(out3, type = "group", na.rm = T)
gc()
summary(es3)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "GOV", dv = "Total", dv_type = "Event",
                                    att = es3$overall.att, se = es3$overall.se))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es4 <- aggte(out4, type = "group", na.rm = T)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "REB", dv = "Total", dv_type = "Event",
                                    att = es4$overall.att, se = es4$overall.se))
gc()
summary(es4)
rm(out4, es4)
gc()

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es5 <- aggte(out5, type = "group", na.rm = T)
gc()
summary(es5)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "GOV", dv = "Total", dv_type = "Event",
                                    att = es5$overall.att, se = es5$overall.se))
rm(out5, es5)
gc()

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es6 <- aggte(out6, type = "group", na.rm = T)
gc()
summary(es6)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "REB", dv = "Total", dv_type = "Event",
                                    att = es6$overall.att, se = es6$overall.se))
rm(out6, es6)
gc()

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es7 <- aggte(out7, type = "group", na.rm = T)
gc()
summary(es7)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "GOV", dv = "Total", dv_type = "Event",
                                    att = es7$overall.att, se = es7$overall.se))
rm(out7, es7)
gc()

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es8 <- aggte(out8, type = "group", na.rm = T)
gc()
summary(es8)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "REB", dv = "Total", dv_type = "Event",
                                    att = es8$overall.att, se = es8$overall.se))
rm(out8, es8)
gc()

###### Pr() #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es1 <- aggte(out1, type = "group", na.rm = T)
gc()
summary(es1)
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "GOV", dv = "Binary", dv_type = "Event",
                                    att = es1$overall.att, se = es1$overall.se))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es2 <- aggte(out2, type = "group", na.rm = T)
gc()
summary(es2)
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "REB", dv = "Binary", dv_type = "Event",
                                    att = es2$overall.att, se = es2$overall.se))
rm(out2, es2)
gc()

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es3 <- aggte(out3, type = "group", na.rm = T)
gc()
summary(es3)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "GOV", dv = "Binary", dv_type = "Event",
                                    att = es3$overall.att, se = es3$overall.se))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es4 <- aggte(out4, type = "group", na.rm = T)
gc()

summary(es4)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "REB", dv = "Binary", dv_type = "Event",
                                    att = es4$overall.att, se = es4$overall.se))
rm(out4, es4)
gc()

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es5 <- aggte(out5, type = "group", na.rm = T)
gc()
summary(es5)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "GOV", dv = "Binary", dv_type = "Event",
                                    att = es5$overall.att, se = es5$overall.se))
rm(out5, es5)
gc()

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es6 <- aggte(out6, type = "group", na.rm = T)
gc()
summary(es6)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "REB", dv = "Binary", dv_type = "Event",
                                    att = es6$overall.att, se = es6$overall.se))
rm(out6, es6)
gc()
## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es7 <- aggte(out7, type = "group", na.rm = T)
gc()
summary(es7)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "GOV", dv = "Binary", dv_type = "Event",
                                    att = es7$overall.att, se = es7$overall.se))
rm(out7, es7)
gc()

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es8 <- aggte(out8, type = "group", na.rm = T)
gc()
summary(es8)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "REB", dv = "Binary", dv_type = "Event",
                                    att = es8$overall.att, se = es8$overall.se))
rm(out8, es8)
gc()

##################################### FATALITIES ##################################### 
###### Total #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es1 <- aggte(out1, type = "group", na.rm = T)
gc()
summary(es1)
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "GOV", dv = "Total", dv_type = "Death",
                                    att = es1$overall.att, se = es1$overall.se))
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es2 <- aggte(out2, type = "group", na.rm = T)
gc()
summary(es2)
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "REB", dv = "Total", dv_type = "Death",
                                    att = es2$overall.att, se = es2$overall.se))
rm(out2, es2)
gc()

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es3 <- aggte(out3, type = "group", na.rm = T)
gc()
summary(es3)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "GOV", dv = "Total", dv_type = "Death",
                                    att = es3$overall.att, se = es3$overall.se))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es4 <- aggte(out4, type = "group", na.rm = T)
gc()
summary(es4)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "REB", dv = "Total", dv_type = "Death",
                                    att = es4$overall.att, se = es4$overall.se))
rm(out4, es4)
gc()

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es5 <- aggte(out5, type = "group", na.rm = T)
gc()
summary(es5)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "GOV", dv = "Total", dv_type = "Death",
                                    att = es5$overall.att, se = es5$overall.se))
rm(out5, es5)
gc()

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es6 <- aggte(out6, type = "group", na.rm = T)
gc()
summary(es6)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "REB", dv = "Total", dv_type = "Death",
                                    att = es6$overall.att, se = es6$overall.se))
rm(out6, es6)
gc()

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es7 <- aggte(out7, type = "group", na.rm = T)
gc()
summary(es7)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "GOV", dv = "Total", dv_type = "Death",
                                    att = es7$overall.att, se = es7$overall.se))
rm(out7, es7)
gc()

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es8 <- aggte(out8, type = "group", na.rm = T)
gc()
summary(es8)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "REB", dv = "Total", dv_type = "Death",
                                    att = es8$overall.att, se = es8$overall.se))
rm(out8, es8)
gc()

###### Pr() #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es1 <- aggte(out1, type = "group", na.rm = T)
gc()
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "GOV", dv = "Binary", dv_type = "Death",
                                    att = es1$overall.att, se = es1$overall.se))
summary(es1)
rm(out1, es1)
gc()

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es2 <- aggte(out2, type = "dynamic", na.rm = T)
gc()
summary(es2)
results = rbind(results, data.frame(time = "Enter", cell = "Same", actor = "REB", dv = "Binary", dv_type = "Death",
                                    att = es2$overall.att, se = es2$overall.se))
rm(out2, es2)
gc()

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es3 <- aggte(out3, type = "group", na.rm = T)
gc()
summary(es3)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "GOV", dv = "Binary", dv_type = "Death",
                                    att = es3$overall.att, se = es3$overall.se))
rm(out3, es3)
gc()

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1)
gc()
es4 <- aggte(out4, type = "group", na.rm = T)
gc()
summary(es4)
results = rbind(results, data.frame(time = "Enter", cell = "Neighbor", actor = "REB", dv = "Binary", dv_type = "Death",
                                    att = es4$overall.att, se = es4$overall.se))
rm(out4, es4)
gc()

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es5 <- aggte(out5, type = "group", na.rm = T)
gc()
summary(es5)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "GOV", dv = "Binary", dv_type = "Death",
                                    att = es5$overall.att, se = es5$overall.se))
rm(out5, es5)
gc()

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es6 <- aggte(out6, type = "group", na.rm = T)
gc()
summary(es6)
results = rbind(results, data.frame(time = "Leave", cell = "Same", actor = "REB", dv = "Binary", dv_type = "Death",
                                    att = es6$overall.att, se = es6$overall.se))
rm(out6, es6)
gc()
## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es7 <- aggte(out7, type = "group", na.rm = T)
gc()
summary(es7)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "GOV", dv = "Binary", dv_type = "Death",
                                    att = es7$overall.att, se = es7$overall.se))
rm(out7, es7)
gc()

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 1)
gc()
es8 <- aggte(out8, type = "group", na.rm = T)
gc()
summary(es8)
results = rbind(results, data.frame(time = "Leave", cell = "Neighbor", actor = "REB", dv = "Binary", dv_type = "Death",
                                    att = es8$overall.att, se = es8$overall.se))
rm(out8, es8)
gc()

saveRDS(results, "./results/main_models.RDS")

##### VERSION CONTROL #####
sessionInfo()