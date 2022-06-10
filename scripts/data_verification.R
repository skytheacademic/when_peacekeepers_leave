# When PKs Leave #
# Sky Kunkel #

library(sfheaders)
library(sf)
library(tidyverse)
d <- readRDS("../data/Kunkel-Atkinson-Warner-final.rds")
a = unique(d$gid)

sample(a, 25)
# result: 
#  [1] 140771 124950 140091 132900  92556 154516 127846 139410 143680 172476 150117 146498 164489 115608 173189 153770 145878 164590 146525
# [20] 135812 126426 129293 126417 118499 148730

### Grid 62356 ### (this is the island that is just one prio grid)
d.62356 = subset(d, gid == 62356     | gid == 62356-1   | gid == 62356+1 | 
                    gid == 62356+720 | gid == 62356+719 | gid == 62356+721 | 
                    gid == 62356-720 | gid == 62356-719 | gid == 62356-721) %>%
  relocate(131:137, .after = 12)
d.62356 = d.62356[order(d.62356$year, d.62356$month, d.62356$gid),]
# no neighbor fatalities, so verified


### 140771 ###
d.140771 = subset(d, gid == 140771     | gid == 140771-1   | gid == 140771+1 | 
                     gid == 140771+720 | gid == 140771+719 | gid == 140771+721 | 
                     gid == 140771-720 | gid == 140771-719 | gid == 140771-721) %>%
  relocate(131:137, .after = 12)
d.140771 = d.140771[order(d.140771$year, d.140771$month, d.140771$gid),]
# manually verified as correct

# d.124950 
d.124950 = subset(d, gid == 124950     | gid == 124950-1   | gid == 124950+1 | 
                    gid == 124950+720 | gid == 124950+719 | gid == 124950+721 | 
                    gid == 124950-720 | gid == 124950-719 | gid == 124950-721)%>%
  relocate(131:137, .after = 12)
d.124950 = d.124950[order(d.124950$year, d.124950$month, d.124950$gid),]
# manually verified as correct

# d.140091 
d.140091 = subset(d, gid == 140091     | gid == 140091-1   | gid == 140091+1 | 
                    gid == 140091+720 | gid == 140091+719 | gid == 140091+721 | 
                    gid == 140091-720 | gid == 140091-719 | gid == 140091-721)%>%
  relocate(131:137, .after = 12)
d.140091 = d.140091[order(d.140091$year, d.140091$month, d.140091$gid),]
# manually verified as correct (no violence in grid of interest)

# d.132900  
d.132900 = subset(d, gid == 132900     | gid == 132900-1   | gid == 132900+1 | 
                    gid == 132900+720 | gid == 132900+719 | gid == 132900+721 | 
                    gid == 132900-720 | gid == 132900-719 | gid == 132900-721) %>%
  relocate(131:137, .after = 12)
d.132900 = d.132900[order(d.132900$year, d.132900$month, d.132900$gid),]
# manually verified as correct

# d.92556 
d.92556 = subset(d, gid == 92556     | gid == 92556-1   | gid == 92556+1 | 
                    gid == 92556+720 | gid == 92556+719 | gid == 92556+721 | 
                    gid == 92556-720 | gid == 92556-719 | gid == 92556-721) %>%
  relocate(131:137, .after = 12)
d.92556 = d.92556[order(d.92556$year, d.92556$month, d.92556$gid),]
# manually verified as correct (no violence in grid of interest)

# d.154516 
d.154516 = subset(d, gid == 154516     | gid == 154516-1   | gid == 154516+1 | 
                    gid == 154516+720 | gid == 154516+719 | gid == 154516+721 | 
                    gid == 154516-720 | gid == 154516-719 | gid == 154516-721) %>%
  relocate(131:137, .after = 12)
d.154516 = d.154516[order(d.154516$year, d.154516$month, d.154516$gid),]
# manually verified as correct (no violence in grid of interest)

# d.127846 
d.127846 = subset(d, gid == 127846     | gid == 127846-1   | gid == 127846+1 | 
                    gid == 127846+720 | gid == 127846+719 | gid == 127846+721 | 
                    gid == 127846-720 | gid == 127846-719 | gid == 127846-721) %>%
  relocate(131:137, .after = 12)
d.127846 = d.127846[order(d.127846$year, d.127846$month, d.127846$gid),]
# manually verified as correct (no violence in grid of interest)

# d.139410 
d.139410 = subset(d, gid == 139410     | gid == 139410-1   | gid == 139410+1 | 
                    gid == 139410+720 | gid == 139410+719 | gid == 139410+721 | 
                    gid == 139410-720 | gid == 139410-719 | gid == 139410-721) %>%
  relocate(131:137, .after = 12)
d.139410 = d.139410[order(d.139410$year, d.139410$month, d.139410$gid),]
# manually verified as correct (no violence in grid of interest)

# d.143680 
d.143680 = subset(d, gid == 143680     | gid == 143680-1   | gid == 143680+1 | 
                    gid == 143680+720 | gid == 143680+719 | gid == 143680+721 | 
                    gid == 143680-720 | gid == 143680-719 | gid == 143680-721) %>%
  relocate(131:137, .after = 12)
d.143680 = d.143680[order(d.143680$year, d.143680$month, d.143680$gid),]
# manually verified as correct (no violence in grid of interest)

# d.172476 
d.172476 = subset(d, gid == 172476     | gid == 172476-1   | gid == 172476+1 | 
                    gid == 172476+720 | gid == 172476+719 | gid == 172476+721 | 
                    gid == 172476-720 | gid == 172476-719 | gid == 172476-721) %>%
  relocate(131:137, .after = 12)
d.172476 = d.172476[order(d.172476$year, d.172476$month, d.172476$gid),]
# manually verified as correct (no violence in grid of interest)

# d.150117 
d.150117 = subset(d, gid == 150117     | gid == 150117-1   | gid == 150117+1 | 
                    gid == 150117+720 | gid == 150117+719 | gid == 150117+721 | 
                    gid == 150117-720 | gid == 150117-719 | gid == 150117-721) %>%
  relocate(131:137, .after = 12)
d.150117 = d.150117[order(d.150117$year, d.150117$month, d.150117$gid),]
# manually verified as correct

# d.146498 
d.146498 = subset(d, gid == 146498     | gid == 146498-1   | gid == 146498+1 | 
                    gid == 146498+720 | gid == 146498+719 | gid == 146498+721 | 
                    gid == 146498-720 | gid == 146498-719 | gid == 146498-721) %>%
  relocate(131:137, .after = 12)
d.146498 = d.146498[order(d.146498$year, d.146498$month, d.146498$gid),]
# manually verified as correct (no violence in grid of interest)

# d.164489 
d.164489 = subset(d, gid == 164489     | gid == 164489-1   | gid == 164489+1 | 
                    gid == 164489+720 | gid == 164489+719 | gid == 164489+721 | 
                    gid == 164489-720 | gid == 164489-719 | gid == 164489-721) %>%
  relocate(131:137, .after = 12)
d.164489 = d.164489[order(d.164489$year, d.164489$month, d.164489$gid),]
# manually verified as correct (no violence in grid of interest)

# d.115608 
d.115608 = subset(d, gid == 115608     | gid == 115608-1   | gid == 115608+1 | 
                    gid == 115608+720 | gid == 115608+719 | gid == 115608+721 | 
                    gid == 115608-720 | gid == 115608-719 | gid == 115608-721) %>%
  relocate(131:137, .after = 12)
d.115608 = d.115608[order(d.115608$year, d.115608$month, d.115608$gid),]
# manually verified as correct (no violence in grid of interest)

# d.173189 
d.173189 = subset(d, gid == 173189     | gid == 173189-1   | gid == 173189+1 | 
                    gid == 173189+720 | gid == 173189+719 | gid == 173189+721 | 
                    gid == 173189-720 | gid == 173189-719 | gid == 173189-721) %>%
  relocate(131:137, .after = 12)
d.173189 = d.173189[order(d.173189$year, d.173189$month, d.173189$gid),]
# manually verified as correct (no violence in grid of interest)

# d.153770 
d.153770 = subset(d, gid == 153770     | gid == 153770-1   | gid == 153770+1 | 
                    gid == 153770+720 | gid == 153770+719 | gid == 153770+721 | 
                    gid == 153770-720 | gid == 153770-719 | gid == 153770-721) %>%
  relocate(131:137, .after = 12)
d.153770 = d.153770[order(d.153770$year, d.153770$month, d.153770$gid),]
# manually verified as correct (no violence in grid of interest)

# d.145878 
d.145878 = subset(d, gid == 145878     | gid == 145878-1   | gid == 145878+1 | 
                    gid == 145878+720 | gid == 145878+719 | gid == 145878+721 | 
                    gid == 145878-720 | gid == 145878-719 | gid == 145878-721) %>%
  relocate(131:137, .after = 12)
d.145878 = d.145878[order(d.145878$year, d.145878$month, d.145878$gid),]
# manually verified as correct (no violence in grid of interest)

# d.164590 
d.164590 = subset(d, gid == 164590     | gid == 164590-1   | gid == 164590+1 | 
                    gid == 164590+720 | gid == 164590+719 | gid == 164590+721 | 
                    gid == 164590-720 | gid == 164590-719 | gid == 164590-721) %>%
  relocate(131:137, .after = 12)
d.164590 = d.164590[order(d.164590$year, d.164590$month, d.164590$gid),]
# manually verified as correct (no violence in grid of interest)

# d.146525
d.146525 = subset(d, gid == 146525     | gid == 146525-1   | gid == 146525+1 | 
                    gid == 146525+720 | gid == 146525+719 | gid == 146525+721 | 
                    gid == 146525-720 | gid == 146525-719 | gid == 146525-721) %>%
  relocate(131:137, .after = 12)
d.146525 = d.146525[order(d.146525$year, d.146525$month, d.146525$gid),]
# manually verified as correct

# d.135812 
d.135812 = subset(d, gid == 135812     | gid == 135812-1   | gid == 135812+1 | 
                    gid == 135812+720 | gid == 135812+719 | gid == 135812+721 | 
                    gid == 135812-720 | gid == 135812-719 | gid == 135812-721) %>%
  relocate(131:137, .after = 12)
d.135812 = d.135812[order(d.135812$year, d.135812$month, d.135812$gid),]
# manually verified as correct

# d.126426 
d.126426 = subset(d, gid == 126426     | gid == 126426-1   | gid == 126426+1 | 
                    gid == 126426+720 | gid == 126426+719 | gid == 126426+721 | 
                    gid == 126426-720 | gid == 126426-719 | gid == 126426-721) %>%
  relocate(131:137, .after = 12)
d.126426 = d.126426[order(d.126426$year, d.126426$month, d.126426$gid),]
# manually verified as correct (no violence in grid of interest)

# d.129293 
d.129293 = subset(d, gid == 129293     | gid == 129293-1   | gid == 129293+1 | 
                    gid == 129293+720 | gid == 129293+719 | gid == 129293+721 | 
                    gid == 129293-720 | gid == 129293-719 | gid == 129293-721) %>%
  relocate(131:137, .after = 12)
d.129293 = d.129293[order(d.129293$year, d.129293$month, d.129293$gid),]
# manually verified as correct (no violence in grid of interest)

# d.126417 
d.126417 = subset(d, gid == 126417     | gid == 126417-1   | gid == 126417+1 | 
                    gid == 126417+720 | gid == 126417+719 | gid == 126417+721 | 
                    gid == 126417-720 | gid == 126417-719 | gid == 126417-721) %>%
  relocate(131:137, .after = 12)
d.126417 = d.126417[order(d.126417$year, d.126417$month, d.126417$gid),]
# manually verified as correct

# d.118499 
d.118499 = subset(d, gid == 118499     | gid == 118499-1   | gid == 118499+1 | 
                    gid == 118499+720 | gid == 118499+719 | gid == 118499+721 | 
                    gid == 118499-720 | gid == 118499-719 | gid == 118499-721) %>%
  relocate(131:137, .after = 12)
d.118499 = d.118499[order(d.118499$year, d.118499$month, d.118499$gid),]
# manually verified as correct

# d.148730
d.148730 = subset(d, gid == 148730     | gid == 148730-1   | gid == 148730+1 | 
                    gid == 148730+720 | gid == 148730+719 | gid == 148730+721 | 
                    gid == 148730-720 | gid == 148730-719 | gid == 148730-721) %>%
  relocate(131:137, .after = 12)
d.148730 = d.148730[order(d.148730$year, d.148730$month, d.148730$gid),]
# manually verified as correct
