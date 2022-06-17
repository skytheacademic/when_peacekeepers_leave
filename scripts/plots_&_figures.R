# When Peacekeepers Leave: Figures and Plots #
# Sky Kunkel #
# reading in cleaned data
setwd("../")
a = readRDS("./data/Kunkel-Atkinson-Warner-final.rds")

# Search for violence data

sort(tapply(a$acled_fatalities_violence_against_civilians, a$gid, max))

### Grid 139387 ###
a.139387 = subset(a, gid == 139387     | gid == 139387-1   | gid == 139387+1 | 
                    gid == 139387+720 | gid == 139387+719 | gid == 139387+721 | 
                    gid == 139387-720 | gid == 139387-719 | gid == 139387-721) #%>%
relocate(131:137, .after = 12)
a.139387 = a.139387[order(a.139387$year, a.139387$month, a.139387$gid),]


# search for pk presence
sort(tapply(a$radpko_pko_deployed, a$gid, max))

### Grid 143697 ###
a.143697 = subset(a, gid == 143697     | gid == 143697-1   | gid == 143697+1 | 
                    gid == 143697+720 | gid == 143697+719 | gid == 143697+721 | 
                    gid == 143697-720 | gid == 143697-719 | gid == 143697-721) #%>%
relocate(131:137, .after = 12)
a.143697 = a.143697[order(a.143697$year, a.143697$month, a.143697$gid),]

### Grid 141454 ### (not enough violence)
a.141454 = subset(a, gid == 141454     | gid == 141454-1   | gid == 141454+1 | 
                    gid == 141454+720 | gid == 141454+719 | gid == 141454+721 | 
                    gid == 141454-720 | gid == 141454-719 | gid == 141454-721) #%>%
a.141454 = a.141454[order(a.141454$year, a.141454$month, a.141454$gid),]

### Grid 127139 ### (not enough violence)
a.127139 = subset(a, gid == 127139     | gid == 127139-1   | gid == 127139+1 | 
                    gid == 127139+720 | gid == 127139+719 | gid == 127139+721 | 
                    gid == 127139-720 | gid == 127139-719 | gid == 127139-721) #%>%
a.127139 = a.127139[order(a.127139$gid, a.127139$year, a.127139$month),]

### Grid 138579 ### (not enough violence)
a.138579 = subset(a, gid == 138579     | gid == 138579-1   | gid == 138579+1 | 
                    gid == 138579+720 | gid == 138579+719 | gid == 138579+721 | 
                    gid == 138579-720 | gid == 138579-719 | gid == 138579-721) #%>%
a.138579 = a.138579[order(a.138579$gid, a.138579$year, a.138579$month),]

### Grid 149451 ### (not enough violence)
a.149451 = subset(a, gid == 149451     | gid == 149451-1   | gid == 149451+1 | 
                    gid == 149451+720 | gid == 149451+719 | gid == 149451+721 | 
                    gid == 149451-720 | gid == 149451-719 | gid == 149451-721 &
                    time < 228) #%>%
a.149451 = a.149451[order(a.149451$gid, a.149451$year, a.149451$month),]

# potential grids for plotting: 
# 142978, 2014-5
# 127139, 2018-1


### Grid 132181 ### 
a.132181 = subset(a, gid == 132181     | gid == 132181-1   | gid == 132181+1 | gid == 132181+2 | gid == 132181-2| 
                    gid == 132181+720 | gid == 132181+719 | gid == 132181+721 | gid == 132181+718 | gid == 132181+722|
                    gid == 132181+1438| gid == 132181+1439| gid == 132181+1440| gid == 132181+1441| gid == 132181+1442|
                    gid == 132181-720 | gid == 132181-719 | gid == 132181-721 | gid == 132181-722 | gid == 132181-718 |
                    gid == 132181-1442| gid == 132181-1441| gid == 132181-1440| gid == 132181-1439| gid == 132181-1438) #%>%
a.132181 = a.132181[order(a.132181$gid, a.132181$year, a.132181$month),]
# summarize 6 months before PKO entrance, then all violent events during PK presence,
# then 6 months after PK exit

# PKs enter at time 2000-3 (16)
# PKs exit at time 2003-2 (50) [AKA, there were 0 PKs at this time in the data]

b = subset(a.132181, time < 56 & time > 9)
library(ggplot2)
library(tidyverse)
library(sf)

b = as.data.frame(b)
b$t_ind = 0
b$t_ind[b$time > 15 & b$time < 50] = 1
b$t_ind[b$time > 49] = 2


b.ag = b %>%
  group_by(t_ind, gid) %>%
  summarize(fatalities = sum(acled_fatalities_battles))

b.ag$fatalities[b.ag$fatalities == 0] <- NA

# now join geographic data so we can plot it

b.join = left_join(b.ag, b)

b.join.0 = subset(b.join, t_ind == 0)
b.join.0 = st_as_sf(b.join.0)

b.join.1 = subset(b.join, t_ind == 1)
b.join.1 = st_as_sf(b.join.1)

b.join.2 = subset(b.join, t_ind == 2)
b.join.2 = st_as_sf(b.join.2)

setwd("../606-Repository/final_project/data/country_sf")
drc.sf = readRDS("drc_sf.rds")

uga_shp <- st_read(dsn = "./UGA_shp", layer = "gadm40_UGA_2", 
                    stringsAsFactors = F)
st_crs(drc.sf) = st_crs(uga_shp)
st_crs(b.join.0) = st_crs(uga_shp)

plot_1 = ggplot() + geom_sf(aes(fill = b.join.0$fatalities, geometry = b.join.0$prio_geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events") + 
  ggtitle("Aggregate Battle Violence 6 months before PK entrance") +
  xlim(29,31.5) + ylim(0.5,3) +
  geom_sf(aes(geometry = drc.sf$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_shp$geometry), alpha = 0)
plot_1

plot_2 = ggplot() + geom_sf(aes(fill = b.join.1$fatalities, geometry = b.join.1$prio_geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events") + 
  ggtitle("Aggregate Battle Violence in 3 years of PK presence") +
  xlim(29,31.5) + ylim(0.5,3) +
  geom_sf(aes(geometry = drc.sf$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_shp$geometry), alpha = 0)
plot_2

plot_3 = ggplot() + geom_sf(aes(fill = b.join.2$fatalities, geometry = b.join.2$prio_geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events") + 
  ggtitle("Aggregate Battle Violence 6 months after PK exit") +
  xlim(29,31.5) + ylim(0.5,3) +
  geom_sf(aes(geometry = drc.sf$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_shp$geometry), alpha = 0)
plot_3


# make descriptive statistics plot #
library(ggplot2)
library(tidyverse)
library(sf)
library(janitor)
library(lubridate)


# pull up world map data #
wrld_shp <- st_read(dsn = "./data/world_shp", layer = "gadm404", 
                    stringsAsFactors = F)

# read in ACLED data #

acled <- read_csv("./data/acled/1999-01-01-2021-12-31.csv") %>% 
  # make the date variable a date type then subset to post-1999
  mutate(event_date = dmy(event_date)) %>% 
  filter(event_date >= "1999-01-01")

#### MERGE ACLED DATA WITH PRIO GRID IDS #####

### get geographic data for countries using the shapefiles
prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", 
                    stringsAsFactors = F)

### save the CRS
proj_crs <- st_crs(prio_shp)

### convert acled to an sf object with a shared CRS
acled <- st_as_sf(acled, coords = c("longitude", "latitude"), crs = proj_crs)


rm(a, a.132181, acled, b, b.ag, b.join, b.join.0, b.join.1, b.join.2, drc.sf, plot_1, plot_2,
   plot_3, prio_shp, prio.df, radpko, radpko1, uga_shp, wrld_shp)
gc()

df = left_join(df, radpko)

df = st_as_sf(df, sf_column_name = "geometry", crs = "world_shp")



# read in ACLED's data and subset to Africa #

acled <- read_csv("./data/acled/1999-01-01-2021-12-31.csv") %>% 
  # make the date variable a date type then subset to post-1999
  mutate(event_date = dmy(event_date)) %>% 
  filter(event_date >= "1999-01-01") 

acled = subset(acled, region == "Southern Africa" | region == "Eastern Africa" | region == "Middle Africa" |
           region == "Western Africa" | region == "Northern Africa")

# read in RADPKO's data #
# since RADPKO is already in main dataset and gridded w/ map data, let's just used the clean data
df = readRDS("./data/Kunkel-Atkinson-Warner-final.rds") %>%
  as.data.frame() %>%
  select(-c(2:15, 22:146))


#### MERGE ACLED DATA WITH PRIO GRID IDS #####

### get geographic data for countries using the shapefiles
prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", 
                    stringsAsFactors = F)

### save the CRS
proj_crs <- st_crs(prio_shp)

### convert acled to an sf object with a shared CRS
acled <- st_as_sf(acled, coords = c("longitude", "latitude"), crs = proj_crs)


### subset to Africa for the purpose of our analysis
df <- df %>% 
  filter(col < 500 & col > 300 & row < 260 & row > 80)

plot_dsc = ggplot(data = df) + geom_sf(aes(fill = fatalities_violence_against_civilians, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events")

pdf("../results/violence_pkos_Africa.pdf")
plot_dsc
dev.off()

plot_dsc

plot_bat = ggplot(data = df) + geom_sf(aes(fill = fatalities_battles, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events")
plot_bat

plot_pko = ggplot(data = df) + geom_sf(aes(fill = pko_deployed, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeeper deployment")
plot_pko

### Descriptive Statistics Plots and Graphs ###























# let's add a plot of Mali to check #
a.min = subset(a, country == "Mali")
table(a.min$fatalities)

a.min1 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min1, by = "prio.grid")
#aa$v[aa$v == 0] <- NA

plot_1 = ggplot(data = aa) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events")

pdf("../results/mali_violent_events_prio.pdf")
plot_1
dev.off()

a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa2 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
# aa2$v[aa2$v == 0] <- NA

plot_3 = ggplot(data = aa2) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/mali_pks_prio.pdf")
plot_3
dev.off()



### Aggregate PK & PKO locations w/ violence and violent events ###

a.min = subset(a, country == "Mali")
table(a.min$fatalities)

a.min1 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min1, by = "prio.grid")
aa$v[aa$v == 0] <- NA

plot_1 = ggplot(data = aa) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_1.pdf")
plot_1
dev.off()



table(a.min$t_ind)
a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(t_ind))

aa1 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
aa1$v[aa1$v == 0] <- NA

plot_2 = ggplot(data = aa1) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "PKO")

pdf("../results/plot_2.pdf")
plot_2
dev.off()


a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa2 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
aa2$v[aa2$v == 0] <- NA

plot_3 = ggplot(data = aa2) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_3.pdf")
plot_3
dev.off()


a.min3 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(event))

aa3 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min3, by = "prio.grid")
aa3$v[aa3$v == 0] <- NA

plot_4 = ggplot(data = aa3) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent Events")

pdf("../results/plot_4.pdf")
plot_4
dev.off()

# gg.all = ggarrange(plot_1, 
#                    ggarrange(plot_2, plot_3, labels = c("Presence of PKO (Binary Count)", "Presence of PKs (Total)")),
#                    nrow = 2, labels = "Fatalities")
# mali.plot = annotate_figure(
#   gg.all,
#   top = text_grob("Visualizing the Overlap of PKOs & Violence in Mali",
#                   color = "blue", face = "bold", size = 14),
#   bottom = text_grob("Data source: \n ACLED & RADPKO", color = "blue",
#                      hjust = 1, x = 1, face = "italic", size = 10),
#   left = text_grob("Fig arranged using ggpubr",
#                    color = "black", rot = 90),
#   fig.lab = "Figure 1", fig.lab.face = "bold"
# )

# mali.plot



### Time-series plots of MINUSMA ###
#subset
a.una = subset(a, country == "Democratic Republic of Congo") 


tapply(a.una$fatalities, a.una$date, sum)

# dates to subset for MINUSMA: 2017-06-01 2017-07-01 2017-08-01 2017-09-01
a.min.02 = subset(a.una, date == "2017-02-01")
a.min.03 = subset(a.una, date == "2017-03-01")
a.min.04 = subset(a.una, date == "2017-04-01")
a.min.05 = subset(a.una, date == "2017-05-01")


## 2017-02-01 ##
a.min.02.ft = a.min.02 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa4 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.02.ft, by = "prio.grid")
aa4$v[aa4$v == 0] <- NA

plot_5 = ggplot(data = aa4) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_5.pdf")
plot_5
dev.off()


a.min.02.pk = a.min.02 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa5 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.02.pk, by = "prio.grid")
aa5$v[aa5$v == 0] <- NA

plot_6 = ggplot(data = aa5) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_6.pdf")
plot_6
dev.off()


## 2017-03-01 ##

a.min.03.ft = a.min.03 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa6 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.03.ft, by = "prio.grid")
aa6$v[aa6$v == 0] <- NA

plot_7 = ggplot(data = aa6) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_7.pdf")
plot_7
dev.off()


a.min.03.pk = a.min.03 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa7 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.03.pk, by = "prio.grid")
aa7$v[aa7$v == 0] <- NA

plot_8 = ggplot(data = aa7) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_8.pdf")
plot_8
dev.off()


## 2017-04-01 ##

a.min.04.ft = a.min.04 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa8 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.04.ft, by = "prio.grid")
aa8$v[aa8$v == 0] <- NA

plot_9 = ggplot(data = aa8) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_9.pdf")
plot_9
dev.off()


a.min.04.pk = a.min.04 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa9 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.04.pk, by = "prio.grid")
aa9$v[aa9$v == 0] <- NA

plot_10 = ggplot(data = aa9) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_10.pdf")
plot_10
dev.off()


## 2017-05-01 ##

a.min.05.ft = a.min.05 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa10 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.05.ft, by = "prio.grid")
aa10$v[aa10$v == 0] <- NA

plot_11 = ggplot(data = aa10) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_11.pdf")
plot_11
dev.off()


a.min.05.pk = a.min.05 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa11 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.05.pk, by = "prio.grid")
aa11$v[aa11$v == 0] <- NA

plot_12 = ggplot(data = aa11) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_12.pdf")
plot_12
dev.off()



