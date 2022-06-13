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

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out1 <- att_gt(yname = "acled_fatalities_any", tname = "time", idname = "gid", 
              gname = "first_treated", data = df)
es1 <- aggte(out, type = "group")
summary(es1)

### model where the outcome is # all fatalities
out2 <- att_gt(yname = "acled_fatalities_all", 
              tname = "time", idname = "gid", 
              gname = "first_treated", data = df)
es2 <- aggte(out, type = "group")
summary(es)

### model where the outcome is # violence against civilians
out3 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
              tname = "time", idname = "gid", 
              gname = "first_treated", data = df)
es3 <- aggte(out, type = "group")
summary(es3)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out4 <- att_gt(yname = "acled_fatalities_any", 
              tname = "time", idname = "gid", 
              gname = "first_treated_leave", data = df)
es4 <- aggte(out, type = "group")
summary(es4)

### model where the outcome is # all fatalities
out5 <- att_gt(yname = "acled_fatalities_all", 
              tname = "time", idname = "gid", 
              gname = "first_treated_leave", data = df)
es5 <- aggte(out, type = "group")
summary(es5)

### model where the outcome is # violence against civilians
out6 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
              tname = "time", idname = "gid", 
              gname = "first_treated_leave", data = df)
es6 <- aggte(out, type = "group")
summary(es6)

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out7 <- att_gt(yname = "neighbor_fatalities_any", tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es7 <- aggte(out, type = "group")
summary(es7)

### model where the outcome is # all fatalities
out8 <- att_gt(yname = "neighbor_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es8 <- aggte(out, type = "group")
summary(es8)

### model where the outcome is # violence against civilians
out9 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es9 <- aggte(out, type = "group")
summary(es9)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out10 <- att_gt(yname = "neighbor_fatalities_any", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df)
es10 <- aggte(out, type = "group")
summary(es10)

### model where the outcome is # all fatalities
out11 <- att_gt(yname = "neighbor_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df)
es11 <- aggte(out, type = "group")
summary(es11)

### model where the outcome is # violence against civilians
out12 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df)
es12 <- aggte(out, type = "group")
summary(es12)

##### SUBSET TO ONLY COUNTRIES THAT HAVE HAD A PKO #####
cry <- df$prio_gwno[which(df$radpko_pko_deployed_any == 1)]
df <- df %>% 
  filter(prio_gwno %in% cry)

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out13 <- att_gt(yname = "acled_fatalities_any", tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es13 <- aggte(out, type = "group")
summary(es13)

### model where the outcome is # all fatalities
out14 <- att_gt(yname = "acled_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es14 <- aggte(out, type = "group")
summary(es14)

### model where the outcome is # violence against civilians
out15 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es15 <- aggte(out, type = "group")
summary(es15)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out16 <- att_gt(yname = "acled_fatalities_any", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df)
es16 <- aggte(out, type = "group")
summary(es16)

### model where the outcome is # all fatalities
out17 <- att_gt(yname = "acled_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df)
es17 <- aggte(out, type = "group")
summary(es17)

### model where the outcome is # violence against civilians
out18 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df)
es18 <- aggte(out, type = "group")
summary(es18)

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out19 <- att_gt(yname = "neighbor_fatalities_any", tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es19 <- aggte(out, type = "group")
summary(es19)

### model where the outcome is # all fatalities
out20 <- att_gt(yname = "neighbor_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es20 <- aggte(out, type = "group")
summary(es20)

### model where the outcome is # violence against civilians
out21 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df)
es21 <- aggte(out, type = "group")
summary(es21)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out22 <- att_gt(yname = "neighbor_fatalities_any", 
                tname = "time", idname = "gid", 
                gname = "first_treated_leave", data = df)
es22 <- aggte(out, type = "group")
summary(es22)

### model where the outcome is # all fatalities
out23 <- att_gt(yname = "neighbor_fatalities_all", 
                tname = "time", idname = "gid", 
                gname = "first_treated_leave", data = df)
es23 <- aggte(out, type = "group")
summary(es23)

### model where the outcome is # violence against civilians
out24 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
                tname = "time", idname = "gid", 
                gname = "first_treated_leave", data = df)
es24 <- aggte(out, type = "group")
summary(es24)

#

















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
