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

### general look of the data
head(df)

### number of years
sort(unique(df$year))

### number of cells
length(unique(df$gid))

##### Diff-in-Diff for if PKOs Reduce VAC in the Same Cell ####
### DV - total fatalities from GOV
out1 <- att_gt(yname = "acled_vac_gov_death", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es1 <- aggte(out1, type = "group")
summary(es1)
#rm(out1, es1)

### DV - total fatalities from REB
out2 <- att_gt(yname = "acled_vac_reb_death", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es2 <- aggte(out2, type = "group")
summary(es2)
#rm(out2, es2)

### DV - total battle fatalities
out3 <- att_gt(yname = "acled_bat_death", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es3 <- aggte(out3, type = "group")
summary(es3)
#rm(out3, es3)

### DV - total battle events
out4 <- att_gt(yname = "acled_bat_event", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es4 <- aggte(out4, type = "dynamic")
summary(es4)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN SAME CELL #####

### DV - total fatalities from GOV
out5 <- att_gt(yname = "acled_vac_gov_death", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es5 <- aggte(out5, type = "group")
summary(es5)
#rm(out5, es5)

### DV - total fatalities from REB
out6 <- att_gt(yname = "acled_vac_reb_death", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6)
es6 <- aggte(out6, type = "group")
summary(es6)
#rm(out6, es6)

### DV - total battle fatalities
out7 <- att_gt(yname = "acled_bat_death", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6)
es7 <- aggte(out7, type = "group")
summary(es7)
#rm(out7, es7)

### DV - total battle events
out8 <- att_gt(yname = "acled_bat_event", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6)
es8 <- aggte(out8, type = "group")
summary(es8)

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN *NEIGHBORING* CELLS #####

### DV - total fatalities from GOV
out9 <- att_gt(yname = "neighbor_gov_death", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es9 <- aggte(out9, type = "group")
summary(es9)
#rm(out9, es9)

### DV - total fatalities from REB
out10 <- att_gt(yname = "neighbor_reb_death", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es10 <- aggte(out10, type = "group")
summary(es10)
#rm(out10, es10)

### DV - total battle fatalities
out11 <- att_gt(yname = "neighbor_bat_death", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es11 <- aggte(out11, type = "group")
summary(es11)
#rm(out11, es11)

### DV - total battle events
out12 <- att_gt(yname = "neighbor_bat_event", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6)
es12 <- aggte(out12, type = "group")
summary(es12)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN NEIGHBORING CELL #####

### DV - total fatalities from GOV
out13 <- att_gt(yname = "neighbor_gov_death", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es13 <- aggte(out13, type = "group")
summary(es13)

### DV - total fatalities from REB
out14 <- att_gt(yname = "neighbor_reb_death", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6)
es14 <- aggte(out14, type = "group")
summary(es14)
#rm(out14, es14)

### DV - total battle fatalities
out15 <- att_gt(yname = "neighbor_bat_death", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6)
es15 <- aggte(out15, type = "group")
summary(es15)
#rm(out15, es15)

### DV - total battle events
out16 <- att_gt(yname = "neighbor_bat_event", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6)
es16 <- aggte(out16, type = "group")
summary(es16)







########################## OLD, to be deleted ##################################

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out1 <- att_gt(yname = "acled_fatalities_any", tname = "time", idname = "gid", 
              gname = "first_treated", base_period = "universal",data = df, pl = T, cores = 6)
es1 <- aggte(out1, type = "group")

pdf("./results/test_plot.pdf", width = 100, height = 100)
ggdid(out1)
dev.off()
summary(es1)
rm(out1, es1)

### model where the outcome is # all fatalities
out2 <- att_gt(yname = "acled_fatalities_all", 
              tname = "time", idname = "gid", 
              gname = "first_treated", data = df, pl = T, cores = 6)
es2 <- aggte(out2, type = "group")
summary(es2)
rm(out2, es2)

### model where the outcome is # violence against civilians
out3 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
              tname = "time", idname = "gid", 
              gname = "first_treated", data = df, pl = T, cores = 6)
es3 <- aggte(out3, type = "group")
summary(es3)
rm(out3, es3)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out4 <- att_gt(yname = "acled_fatalities_any", 
              tname = "time", idname = "gid", 
              gname = "first_treated_leave", data = df, pl = T, cores = 6)
es4 <- aggte(out4, type = "group")
es4.2 = aggte(out4.1, type = "group")
es4.1 = aggte(out4, type = "dynamic")
ggdid(es4)
ggdid(es4.1)
summary(es4)
rm(out4, es4)

### model where the outcome is # all fatalities
out5 <- att_gt(yname = "acled_fatalities_all", 
              tname = "time", idname = "gid", 
              gname = "first_treated_leave", data = df, pl = T, cores = 6)
es5 <- aggte(out5, type = "group")
summary(es5)
rm(out5, es5)

### model where the outcome is # violence against civilians
out6 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
              tname = "time", idname = "gid", 
              gname = "first_treated_leave", data = df, pl = T, cores = 6)
es6 <- aggte(out6, type = "group")
summary(es6)
rm(out6, es6)

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out7 <- att_gt(yname = "neighbor_fatalities_any", tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es7 <- aggte(out7, type = "group")
summary(es7)
rm(out7, es7)

### model where the outcome is # all fatalities
out8 <- att_gt(yname = "neighbor_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es8 <- aggte(out8, type = "group")
summary(es8)
rm(out8, es8)

### model where the outcome is # violence against civilians
out9 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es9 <- aggte(out9, type = "group")
summary(es9)
rm(out9, es9)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out10 <- att_gt(yname = "neighbor_fatalities_any", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es10 <- aggte(out10, type = "group")
summary(es10)
rm(out10, es10)

### model where the outcome is # all fatalities
out11 <- att_gt(yname = "neighbor_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es11 <- aggte(out11, type = "group")
summary(es11)
rm(out11, es11)

### model where the outcome is # violence against civilians
out12 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es12 <- aggte(out12, type = "group")
summary(es12)
rm(out12, es12)

##### SUBSET TO ONLY COUNTRIES THAT HAVE HAD A PKO #####
cry <- df$prio_gwno[which(df$radpko_pko_deployed_any == 1)]
df <- df %>% 
  filter(prio_gwno %in% cry)

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out13 <- att_gt(yname = "acled_fatalities_any", tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es13 <- aggte(out13, type = "group")
summary(es13)
rm(out13, es13)

### model where the outcome is # all fatalities
out14 <- att_gt(yname = "acled_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es14 <- aggte(out14, type = "group")
summary(es14)
rm(out14, es14)

### model where the outcome is # violence against civilians
out15 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es15 <- aggte(out15, type = "group")
summary(es15)
rm(out15, es15)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN SAME CELL #####

### model where the outcome is Pr(fatalties)
out16 <- att_gt(yname = "acled_fatalities_any", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es16 <- aggte(out16, type = "group")
summary(es16)
rm(out16, es16)

### model where the outcome is # all fatalities
out17 <- att_gt(yname = "acled_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es17 <- aggte(out17, type = "group")
summary(es17)
rm(out17, es17)

### model where the outcome is # violence against civilians
out18 <- att_gt(yname = "acled_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave", data = df, pl = T, cores = 6)
es18 <- aggte(out18, type = "group")
summary(es18)
rm(out18, es18)

##### DIFF-IN-DIFF FOR IF PKO REDUCE VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out19 <- att_gt(yname = "neighbor_fatalities_any", tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es19 <- aggte(out19, type = "group")
summary(es19)
rm(out19, es19)

### model where the outcome is # all fatalities
out20 <- att_gt(yname = "neighbor_fatalities_all", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es20 <- aggte(out20, type = "group")
summary(es20)
rm(out20, es20)

### model where the outcome is # violence against civilians
out21 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es21 <- aggte(out21, type = "group")
summary(es21)
rm(out21, es21)

#### DIFF-IN-DIFF FOR IF PKO *LEAVING* REDUCES VIOLENCE IN *NEIGHBORING* CELLS #####

### model where the outcome is Pr(fatalties)
out22 <- att_gt(yname = "neighbor_fatalities_any", 
                tname = "time", idname = "gid", 
                gname = "first_treated_leave", data = df, pl = T, cores = 6)
es22 <- aggte(out22, type = "group")
summary(es22)
rm(out22, es22)

### model where the outcome is # all fatalities
out23 <- att_gt(yname = "neighbor_fatalities_all", 
                tname = "time", idname = "gid", 
                gname = "first_treated_leave", data = df, pl = T, cores = 6)
es23 <- aggte(out23, type = "group")
summary(es23)
rm(out23, es23)

### model where the outcome is # violence against civilians
out24 <- att_gt(yname = "neighbor_fatalities_violence_against_civilians", 
                tname = "time", idname = "gid", 
                gname = "first_treated_leave", data = df, pl = T, cores = 6)
es24 <- aggte(out24, type = "group")
summary(es24)
rm(out24, es24)

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
