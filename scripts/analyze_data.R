#!/usr/bin/env Rscript
# analyze_data.R
# Produces all analysis, tables, and figures
# Zach Warner, Sky Kunkel
# 9 June 2022

##### SET UP #####

### load packages
library(did); library(sf); library(tidyverse); library(lubridate); library(ggtext)

### set working directory ###
setwd("/Users/zmwarner/github/when_peacekeepers_leave")

### set seed
set.seed(8675309) # hey jenny

##### READ IN DATA #####

### load it
df <- read_rds("./data/Kunkel-Atkinson-Warner-final.rds")

##### SUMMARY STATISTICS #####

### general look of the data
head(df)

### number of years
sort(unique(df$year))

### number of cells
length(unique(df$gid))
options(max.print = 20)
##################################### VIOLENT EVENTS #####################################
###### TOTAL #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
rm(out1, es1)

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
rm(out2, es2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
rm(out3, es3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
rm(out4, es4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "group", na.rm = T)
summary(es5)
rm(out5, es5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "group", na.rm = T)
summary(es6)
rm(out6, es6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "group", na.rm = T)
summary(es7)
rm(out7, es7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "group", na.rm = T)
summary(es8)
rm(out8, es8)

###### Pr() #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
rm(out1, es1)

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
rm(out2, es2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
rm(out3, es3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
rm(out4, es4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "group", na.rm = T)
summary(es5)
rm(out5, es5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "group", na.rm = T)
summary(es6)
rm(out6, es6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "group", na.rm = T)
summary(es7)
rm(out7, es7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "group", na.rm = T)
summary(es8)
rm(out8, es8)

##################################### FATALITIES ##################################### 
###### Total #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
rm(out1, es1)

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "group", na.rm = T)
summary(es2)
rm(out2, es2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
rm(out3, es3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
rm(out4, es4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "group", na.rm = T)
summary(es5)
rm(out5, es5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "group", na.rm = T)
summary(es6)
rm(out6, es6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "group", na.rm = T)
summary(es7)
rm(out7, es7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "group", na.rm = T)
summary(es8)
rm(out8, es8)

###### Pr() #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "group", na.rm = T)
summary(es1)
rm(out1, es1)

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "dynamic", na.rm = T)
summary(es2)
rm(out2, es2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "group", na.rm = T)
summary(es3)
rm(out3, es3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "group", na.rm = T)
summary(es4)
rm(out4, es4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "group", na.rm = T)
summary(es5)
rm(out5, es5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "group", na.rm = T)
summary(es6)
rm(out6, es6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "group", na.rm = T)
summary(es7)
rm(out7, es7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "group", na.rm = T)
summary(es8)
rm(out8, es8)



es1_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es1$egt, ")"),
  event.time= es1$egt,
  estimate  = es1$att.egt,
  std.error = es1$se.egt,
  conf.low  = es1$att.egt - es1$crit.val.egt * es1$se.egt,
  conf.high = es1$att.egt + es1$crit.val.egt  * es1$se.egt,
  point.conf.low  = es1$att.egt - stats::qnorm(1 - es1$DIDparams$alp/2) * es1$se.egt,
  point.conf.high = es1$att.egt + stats::qnorm(1 - es1$DIDparams$alp/2) * es1$se.egt
) %>%
  filter(event.time < 1)

ggplot(data = es1_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, size = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, size = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es1_plot$event.time), max(es1_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))

### plotting event study ###
es1_1_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es1_1$egt, ")"),
  event.time= es1_1$egt,
  estimate  = es1_1$att.egt,
  std.error = es1_1$se.egt,
  conf.low  = es1_1$att.egt - es1_1$crit.val.egt * es1_1$se.egt,
  conf.high = es1_1$att.egt + es1_1$crit.val.egt  * es1_1$se.egt,
  point.conf.low  = es1_1$att.egt - stats::qnorm(1 - es1_1$DIDparams$alp/2) * es1_1$se.egt,
  point.conf.high = es1_1$att.egt + stats::qnorm(1 - es1_1$DIDparams$alp/2) * es1_1$se.egt
)

ggplot(data = es1_1_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, size = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, size = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", size = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') + ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es1_1_plot$event.time), 0, by = 10)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
### end plotting



##### OLD-SCHOOL DID FOR IF PKO REDUCE VIOLENCE #####

df <- df %>%
  group_by(gid) %>% 
  mutate(treated = case_when(sum(radpko_pko_deployed_any, na.rm = T) > 0 ~ 1,
                             TRUE ~ 0)) %>% 
  ungroup()

m1 <- glm(acled_fatalities_any ~ treated + post_treatment + 
            treated*post_treatment, data = df)
summary(m1)


############ OLD, TO DELETE ############ 
# df <- as_tibble(df) %>% 
#   rename(neighbor_vac_gov_death_all = ...143,
#          neighbor_vac_gov_death_any = ...144,
#          neighbor_vac_reb_death_all  = ...145,
#          neighbor_vac_reb_death_any = ...146,
#          neighbor_vac_gov_event_all = ...147,
#          neighbor_vac_gov_event_any = ...148,
#          neighbor_vac_reb_event_all = ...149,
#          neighbor_vac_reb_event_any = ...150,
#          neighbor_gov_death_all = ...151,
#          neighbor_gov_death_any = ...152,
#          neighbor_reb_death_all = ...153,
#          neighbor_reb_death_any = ...154,
#          neighbor_gov_event_all = ...155,
#          neighbor_gov_event_any = ...156,
#          neighbor_reb_event_all = ...157,
#          neighbor_reb_event_any = ...158,
#          neighbor_bat_death_all = ...159,
#          neighbor_bat_death_any = ...160,
#          neighbor_bat_event_all = ...161,
#          neighbor_bat_event_any = ...162)
# 
# radpko <- read_csv("./data/radpko/radpko_grid.csv") %>% 
#   # make the date variable a date type
#   mutate(date = ymd(date),
#          month = month(date),
#          year = year(date)) %>% 
#   # rename variable for ease of merging
#   rename(gid = prio.grid) %>%
#   select(-c(country, mission, date)) %>% 
#   relocate(c(year, month), .after = gid) %>% 
#   group_by(gid, year, month) %>% 
#   summarise(across(units_deployed:afr_unmob, sum)) %>% 
#   select(c(gid, month, year)) %>%
#   ungroup()
# 
# df = left_join(radpko, df, by = c("gid", "year", "month"))
# rm(radpko)

##### VERSION CONTROL #####
sessionInfO()