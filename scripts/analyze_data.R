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

### model where the outcome is Pr(fatalties)
z <- Sys.time()
out <- att_gt(yname = "acled_fatalities_any", tname = "time", idname = "gid", 
              gname = "first_treated", data = df)
Sys.time() - z
es <- aggte(out, type = "group")
summary(es)
ggdid(es)

### model where the outcome is # violence against civilians
z <- Sys.time()
out <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
              tname = "time", idname = "gid", 
              gname = "first_treated", data = df)
Sys.time() - z
es <- aggte(out, type = "group")
summary(es)
ggdid(es)

### model where the outcome is # violence against civilians
z <- Sys.time()
out <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
              tname = "time", idname = "gid", 
              gname = "first_treated", data = df)
Sys.time() - z
es <- aggte(out, type = "group")
summary(es)
ggdid(es)


##### OLD-SCHOOL DID FOR IF PKO REDUCE VIOLENCE #####

df <- df %>%
  group_by(gid) %>% 
  mutate(treated = case_when(sum(radpko_pko_deployed_any, na.rm = T) > 0 ~ 1,
                             TRUE ~ 0)) %>% 
  ungroup()

m1 <- glm(acled_fatalities_any ~ treated + post_treatment + 
            treated*post_treatment, data = df)
summary(m1)




##### VERSION CONTROL #####
sessionInfO()



##### OLD TO DELETE

# gids <- sample(unique(df$gid), 2000)
# dd <- df[which(df$gid %in% gids),]
# 
# ### temp
# library(parallel)
# Sys.setenv("MC_CORES"=6)
# options(mc.cores=6)
# mc.cores <- options("mc.cores")
# ### end temp
