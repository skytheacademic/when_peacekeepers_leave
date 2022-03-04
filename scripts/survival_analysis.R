# Survival Analysis #
# By: Adam Kunkel #

### load libraries ###
library(exactextractr); library(raster); library(rasterVis); library(rgdal)
library(rgeos); library(sf); library(sp); library(tidygeocoder)
library(tidyverse); library(tmap); library(viridis); library(lubridate)
# need to install Terra 1.5 or higher to install spatialeco, need to adjust image
library(spatialEco); library(gdata) 
library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer) # need to add these to dockerfile
library(spdep); library(MASS); library(lme4)
library(survival); library(survminer)


# turn off scientific notation
options(scipen = 999)
options(max.print = 10000)   
# reading in cleaned data
setwd("../data/")
a = readRDS("merged_data.rds")

# Create a "treatment" indicator telling us if PKs existed in a certain grid at a certain time 
a$t_ind = 0
a$t_ind[a$units_deployed >= 1] = 1
a$event.b = 0
a$event.b[a$event>0] = 1
a$death = 0
a$death[a$fatalities>0] = 1
a$pop.dens = a$pop_gpw_sum / a$landarea 
a$fate.5 = 0
a$fate.5[a$fatalities > 4] = 1

# replace NAs w/ 0
a$units_deployed[is.na(a$units_deployed)] <- 0
a$countries_deployed[is.na(a$countries_deployed)] <- 0
a$pko_deployed[is.na(a$pko_deployed)] <- 0
a$untrp[is.na(a$untrp)] <- 0
a$unpol[is.na(a$unpol)] <- 0
a$unmob[is.na(a$unmob)] <- 0
a$f_untrp[is.na(a$f_untrp)] <- 0
a$f_unpol[is.na(a$f_unpol)] <- 0
a$f_unmob[is.na(a$f_unmob)] <- 0
a$fatalities[is.na(a$fatalities)] <- 0
a$event[is.na(a$event)] <- 0
a$mountains_mean[is.na(a$mountains_mean)] <- 0

### add OSV by distinct actors variables ###
# violent events + binaries #
a$inter1[is.na(a$inter1)] <- 0
a$gov_event = 0
a$gov_event[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$event[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$gov_event.b = 0
a$gov_event.b[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$event.b[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$reb_event = 0
a$reb_event[a$inter1 == 2] = a$event[a$inter1 == 2]
a$reb_event.b = 0
a$reb_event.b[a$inter1 == 2] = a$event.b[a$inter1 == 2]

# fatalities + binaries #
a$gov_death = 0
a$gov_death[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$fatalities[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$gov_death.b = 0
a$gov_death.b[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$death[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$reb_death = 0
a$reb_death[a$inter1 == 2] = a$fatalities[a$inter1 == 2]
a$reb_death.b = 0
a$reb_death.b[a$inter1 == 2] = a$death[a$inter1 == 2]



##########################################
########### Survival Analysis ############
##########################################


# let's see the date range for each mission #
tapply(a$date, a$mission, range)
# $MINURCAT
# [1] "2007-09-01" "2010-12-01"
# $MINUSCA
# [1] "2014-04-01" "2017-12-01"
# $MINUSMA
# [1] "2013-04-01" "2017-12-01"
# $MONUC
# [1] "2000-01-01" "2010-07-01"
# $MONUSCO
# [1] "2010-08-01" "2017-12-01"
# $ONUB
# [1] "2004-05-01" "2006-12-01"
# $UNAMID
# [1] "2007-07-01" "2018-02-01"
# $UNAMSIL
# [1] "1999-10-01" "2005-12-01"
# $UNISFA
# [1] "2011-06-01" "2018-01-01"
# $UNMIL
# [1] "2003-09-01" "2017-12-01"
# $UNMIS
# [1] "2005-01-01" "2011-06-01"
# $UNMISS
# [1] "2011-07-01" "2017-12-01"
# $UNOCI
# [1] "2004-04-01" "2017-06-01"

# Discrete model: the state variables change only at a countable number of
  # points in time. These points in time are the ones at which the event 
  # occurs/change in state.

# mark the first time peacekeepers enter a grid #

tapply(a$date, a$prio.grid, range)
# need to add a 1 going up by prio grid and date
a = a[order(a$date, decreasing=FALSE), ] # first sort the grids by date
a = a[order(a$prio.grid, decreasing=FALSE), ] # not sure if this is needed, but leaving for now

a <- a %>%
  group_by(prio.grid) %>% # adding a time variable starting with 1
  mutate(time.var = 1:n())

# I think I have the t_ind in the wrong spot here,
# is treatment the violence?
# I think t_ind is an IV and violence is the DV

# do I need to subset for when peacekeepers enter a grid?

b = Surv(a$time.var, a$event.b)
c = Surv2(a$time.var, a$event.b, repeated = TRUE) # which of these is correct?
# my guess is the second one

# chances of violence ending after peacekeepers enter a grid:
# life table #
m1 <- survfit(Surv(time.var, event.b) ~ 1, data = a)
m1
summary(m1)



# transform the data

# "death" in surv model could be 5 deaths in a month, indicating death is clustering (need to define this)
# figure out how to code data: censoring and death (https://towardsdatascience.com/survival-analysis-part-a-70213df21c2e)
# should each grid be counted until "death"? or could I separate grids into time ranges where each unit of analysis
# is a grid month where it is in the data until experiencing "death", at which point it starts again?
# need to talk with Giancarlo or Shawn about this


# From Shawn re: survival analysis

# Here's a code snippet with a bit of explanation in case it's helpful.

### Identify age first not dual function
d5 <- d4 %>%
  group_by(hhidpn) %>%
  mutate(sndf = cumsum(ndf),
         age_ndf = case_when(
           ndf == 1 & sndf == 1 ~ age,
           ndf == 0 & sndf == 0 ~ age)) %>%
  filter(age_ndf == max(age_ndf, na.rm = T)) %>%
  select(-c(id, wave, age, df, sndf))

# The group_by statement is for the case ID (i.e., each case has a run of multiple observations by age). 
# NDF is the event indicator. I first find the cumulative sum within each case. And then assign the age 
    # when the event happened by identifying when the indicator is 1 and the cumulative sum is 1. 
# Finally, the filter statement gets rid of observations within each case for ages beyond the event.
# The select statement is just cleaning up a few variables used in the process that I don't need for analysis.


### general discrete-time model
m1 <- glm(event.b ~ as.factor(time.var) + units_deployed + untrp + unpol + unmob + f_untrp +
            f_unpol + f_unmob + mountains_mean + ttime_mean + 
            urban_gc + nlights_calib_mean, data = a, family = "binomial")
# getting rid of pop.dens and pop_gpw_sum because they have too many missing observations
# need to verify interpretation of these models
summary(m1)
BIC(m1)

# plot log odds, odds, and hazard
m1e       <- tidy(m1)
m1_logit  <- rbind(m1e[[1,2]], m1e[2:12,2] + m1e[[1,2]])
m1_odds   <- exp(m1_logit)
m1_hazard <- 1/(1 + exp(-1*m1_logit))
m1r       <- data.frame(13:24, m1_logit, m1_odds, m1_hazard)
colnames(m1r) <- c("month", "logit", "odds", "hazard")

fig1 <- ggplot(data = m1r, mapping = aes(x = month, y = hazard)) +
  geom_line() +
  geom_point() +
  labs(x = "month", y = "hazard") +
  scale_x_continuous(breaks = 13:24) +
  theme_light()
fig1


######
# this seems to be good, but now I need to reverse it to see if violence recurs when PKs leave
######




