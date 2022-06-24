# When Peacekeepers Leave: Figures and Plots #
# Sky Kunkel #
# reading in cleaned data
setwd("../")
a = readRDS("./data/Kunkel-Atkinson-Warner-final.rds")
a= as.data.frame(a)
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
rm(a)
gc()
# summarize 6 months before PKO entrance, then all violent events during PK presence,
# then 6 months after PK exit

# PKs enter at time 2000-3 (16)
# PKs exit at time 2003-2 (50) [AKA, there were 0 PKs at this time in the data]

b = subset(a.132181, time < 56 & time > 9)
library(ggplot2)
library(tidyverse)
library(sf)
library(ggpubr)
library(tmap)
library(tmaptools)
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
b.join.0 = subset(b.join, t_ind == 0) %>%
  st_as_sf()
b.join.1 = subset(b.join, t_ind == 1)%>%
  st_as_sf()
b.join.2 = subset(b.join, t_ind == 2)%>%
  st_as_sf()

# read in Uganda shapefiles
uga_00 <- st_read(dsn = "./data/gadm/uganda", layer = "gadm40_UGA_0", 
                    stringsAsFactors = F)
uga_01 <- st_read(dsn = "./data/gadm/uganda", layer = "gadm40_UGA_1", 
                  stringsAsFactors = F)

# read in DRC shapefiles
drc_00 <- st_read(dsn = "./data/gadm/drc", layer = "gadm40_COD_0", 
                  stringsAsFactors = F)
drc_01 <- st_read(dsn = "./data/gadm/drc", layer = "gadm40_COD_1", 
                  stringsAsFactors = F)

# combine UGA and DRC so we can shade in with country names
uga_drc = rbind(uga_00, drc_00)




st_crs(b.join.0) = st_crs(uga_00)
st_crs(b.join.1) = st_crs(uga_00)
st_crs(b.join.2) = st_crs(uga_00)


# below code is useful if plotting all of DRC and Uganda w/ surrounding countries on plot
######### 

# read in neighbor shapefiles
# ago_00 <- st_read(dsn = "./data/gadm/angola", layer = "gadm40_AGO_0", 
#                   stringsAsFactors = F)
# bdi_00 <- st_read(dsn = "./data/gadm/burundi", layer = "gadm40_BDI_0", 
#                   stringsAsFactors = F)
# caf_00 <- st_read(dsn = "./data/gadm/caf", layer = "gadm40_CAF_0", 
#                   stringsAsFactors = F)
# cog_00 <- st_read(dsn = "./data/gadm/congo", layer = "gadm40_COG_0", 
#                   stringsAsFactors = F)
# rwa_00 <- st_read(dsn = "./data/gadm/rwanda", layer = "gadm40_RWA_0", 
#                   stringsAsFactors = F)
# ssd_00 <- st_read(dsn = "./data/gadm/s_sudan", layer = "gadm40_SSD_0", 
#                   stringsAsFactors = F)
# tza_00 <- st_read(dsn = "./data/gadm/tanzania", layer = "gadm40_TZA_0", 
#                   stringsAsFactors = F)
# zmb_00 <- st_read(dsn = "./data/gadm/zambia", layer = "gadm40_ZMB_0", 
#                   stringsAsFactors = F)
# try using tm_shape instead
# bbox_uga <- st_bbox(uga_00) # current bounding box
# bbox_drc <- st_bbox(drc_00) # current bounding box
# bbox_new = bbox_drc
# bbox_new[1] = bbox_drc[1]
# bbox_new[2] = bbox_drc[2]
# bbox_new[3] = bbox_uga[3]
# bbox_new[4] = bbox_drc[4]


# tm_shape(shp = uga_drc, bbox= bbox_new) + tm_borders(col = "black", lwd = 3) +
#   tm_fill(col="COUNTRY") + tm_layout(legend.width = 2, legend.frame = "black", legend.bg.alpha = 0.1) + 
# #  tm_shape(shp = uga_drc) + tm_borders(col = "red", lwd = 3) +
#   tm_shape(shp = uga_01) + tm_borders(col = "black", lty = "dashed", alpha = 0.7, lwd = 0.5) +
#   tm_shape(shp = drc_01) + tm_borders(col = "black", lty = "dashed", alpha = 0.7, lwd = 0.5) +
#   tm_shape(shp = ago_00) + tm_borders(lty = "solid", alpha = 0.3) +
#   tm_shape(shp = bdi_00) + tm_borders(lty = "solid", alpha = 0.3) +
#   tm_shape(shp = caf_00) + tm_borders(lty = "solid", alpha = 0.3) +
#   tm_shape(shp = cog_00) + tm_borders(lty = "solid", alpha = 0.3) +
#   tm_shape(shp = rwa_00) + tm_borders(lty = "solid", alpha = 0.3) +
#   tm_shape(shp = ssd_00) + tm_borders(lty = "solid", alpha = 0.3) +
#   tm_shape(shp = tza_00) + tm_borders(lty = "solid", alpha = 0.3) +
#   tm_shape(shp = zmb_00) + tm_borders(lty = "solid", alpha = 0.3)


# plot_1 = ggplot() + geom_sf(aes(fill = b.join.0$fatalities, geometry = b.join.0$prio_geometry)) +
#   scale_fill_viridis_c(option = "plasma", limits=c(0,2050)) +
#   geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
#   geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
#   geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
#   geom_sf_label(data = uga_drc, aes(label = COUNTRY), label.padding = unit(1, "mm")) +
#   theme_void()
# plot_1

# above plot can be used if we want to have a box over the grids with a country level focus


################

# Plot of moving violence after PK entrance
###############

plot_1 = 
  ggplot() + geom_sf(aes(fill = b.join.0$fatalities, geometry = b.join.0$prio_geometry)) +
  scale_fill_viridis_c(option = "plasma", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  theme(plot.margin = unit(c(1,1,1,0.15), "cm"))

#plot_2 = 
  ggplot() + geom_sf(aes(fill = b.join.1$fatalities, geometry = b.join.1$prio_geometry)) +
  scale_fill_viridis_c(option = "plasma", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  theme(plot.margin = unit(c(1,1,1,0.15), "cm")) +
  labs(color ="Fatalities")

plot_3 = 
  ggplot() + geom_sf(aes(fill = b.join.2$fatalities, geometry = b.join.2$prio_geometry)) +
  scale_fill_viridis_c(option = "plasma", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  theme(plot.margin = unit(c(1,1,1,0.15), "cm"))

# try extracting legend and attaching in Latex? first put legend on bottom of plot, then extract


# see here: https://stackoverflow.com/questions/59162865/how-to-edit-common-legend-title-in-ggarrange
# pdf("./results/violence_over_time.pdf")
ggarrange(plot_1, plot_2, plot_3, 
          ncol = 3, nrow = 1, 
          labels = c("DRC", "Uganda"), label.x = c(1.0, 0.55), vjust = c(7.3, 7.3),
          common.legend = TRUE, legend = "bottom", font.label = list(size = 13))
dev.off()

rm(list = ls())




### Descriptive Statistics Plots and Graphs ###
library(ggplot2)
library(tidyverse)
library(sf)
library(janitor)
library(lubridate)
library(viridis)

a = readRDS("./data/Kunkel-Atkinson-Warner-final.rds") %>%
  as.data.frame()

df = a %>%
  group_by(gid) %>%
  summarize(pko_deployed = sum(radpko_units_deployed), 
            violence = sum(acled_fatalities_battles, acled_fatalities_violence_against_civilians,
                           acled_fatalities_protests, acled_fatalities_strategic_developments,
                           acled_fatalities_explosions_remote_violence, acled_fatalities_riots))

df$pko_deployed[df$pko_deployed == 0] <- NA
df$violence[df$violence == 0] <- NA

# restructure the data so grids can be duplicated and pko/violence is on the same scale
df.pk = subset(df, pko_deployed > 0) %>%
  select(-c("violence")) %>%
  rename(count = pko_deployed)
df.pk$ct.type = "Peacekeepers Deployed"
df.vo = subset(df, violence > 0) %>%
  select(-c("pko_deployed")) %>%
  rename(count = violence)
df.vo$ct.type = "Violent Deaths"

# rejoin to same column
dd = rbind(df.pk, df.vo)

rm(a)
gc()

#### MERGE ACLED DATA WITH PRIO GRID IDS #####
prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", # get prio shapefiles
                    stringsAsFactors = F)
afr_shp <- st_read(dsn = "./data/gadm/africa", layer = "afr_g2014_2013_0", # get Africa shapefiles
                   stringsAsFactors = F)

### save the CRS
proj_crs <- st_crs(prio_shp)

# convert, get rid of useless data
df.prio = left_join(df, prio_shp, by = "gid") %>%
  select(-c(2:7))
df = left_join(df, prio_shp, by = "gid") %>%
  as.data.frame() %>%
  select(-c("geometry", "col", "row"))
dd_ac = left_join(dd, prio_shp, by = "gid") %>%
  as.data.frame() %>%
  select(-c("geometry", "col", "row"))
df_ac= df %>%
  drop_na(violence) # drop NAs
df_pk = df %>%
  drop_na(pko_deployed)


ggplot(data = dd_ac, aes(x=xcoord, y=ycoord, size=count, color=ct.type)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Count") +
  scale_fill_viridis_c(option="E") +
  xlab("latitude") +
  ylab("longitude")

ggplot() + geom_sf(aes(geometry = afr_shp$geometry), fill = NA) + 
  geom_point(data = dd_ac, aes(x=xcoord, y=ycoord, size=count, color=ct.type)) +
  geom_point(alpha=0.01, shape = 21) +
  scale_size(range = c(.1, 24), name="Count") +
  xlab("latitude") +
  ylab("longitude")

ggplot() + geom_point(data = dd_ac, aes(x = xcoord, y = ycoord, size=count, color=ct.type)) +
  scale_size(range = c(.1, 24), name="Count") +
  geom_point(alpha=0.5, shape = 19)




# this theme from here: https://r-graph-gallery.com/320-the-basis-of-bubble-plot.html
# isnt plotting correctly because I don't have arial narrow installed, try different theme

ggplot(df.prio) + geom_sf(aes(geometry = geometry), fill = "blue") +
  geom_point(data = df_ac, mapping = aes(x = xcoord, y = ycoord, 
                                         size = violence), colour = "dark green") +
  geom_point(data = df_pk, mapping = aes(x = xcoord, y = ycoord, 
                                         size = pko_deployed), colour = "dark red")


plot_bat = ggplot(data = df) + geom_sf(aes(fill = fatalities_battles, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events")
plot_bat

plot_pko = ggplot(data = df) + geom_sf(aes(fill = pko_deployed, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeeper deployment")
plot_pko





