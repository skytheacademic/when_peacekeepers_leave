# When Peacekeepers Leave: Figures and Plots #
# Sky Kunkel #
# reading in cleaned data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../")
library(did)
library(sf)
library(tidyverse)
library(lubridate)
library(ggtext)
library(xtable)

### Make summary tables - main paper ###
rm(list = ls())
df_tab_1 = read_rds("./results/main_models.RDS") %>%
  slice(1:8)

# Add asterisks for statistical significance
# if ATT divided by SE is greater than 1.96 (for a 95% confidence interval), mark with an asterisk
df_tab_1$ATT <- ifelse(abs(df_tab_1$att / df_tab_1$se) > 1.96, paste0(formatC(df_tab_1$att, format = "f", digits = 4), "*"), formatC(df_tab_1$att, format = "f", digits = 4))
df_tab_1$SE <- formatC(df_tab_1$se, format = "f", digits = 4)  # Format SE to 4 decimal places
df_tab_1$att = NULL
df_tab_1$se = NULL
df_tab_1$dv = NULL
df_tab_1$dv_type = NULL

# Adjust column names to reflect the table
colnames(df_tab_1) <- c("Time", "Cell", "Actor", "ATT", "SE")

# Create the xtable object
xtable_obj <- xtable(
  df_tab_1,
  caption = "Table for our main results testing the temporal effects of peacekeeper entrance 
  and exit on the total events of violence against civilians. * $=$ p-value lower than 0.05; 
  bootstrapped standard errors are used to calculate the confidence intervals.",
  label = "tab:main_results_total",
  digits = 4  # Set decimal places
)

# Adjust alignments to include vertical lines between columns
align(xtable_obj) <- "|l|l|l|l|r|r|"

# Print the LaTex table
print(
  xtable_obj,
  include.rownames = FALSE,  # No row numbers
  caption.placement = "bottom",  # Caption at the bottom
  sanitize.text.function = identity  # Retains LaTex text formatting
)

rm(list = ls())
df_tab_2 = read_rds("./results/main_models.RDS") %>%
  slice(9:16)

# Add asterisks for statistical significance
# if ATT divided by SE is greater than 1.96 (for a 95% confidence interval), mark with an asterisk
df_tab_2$ATT <- ifelse(abs(df_tab_2$att / df_tab_2$se) > 1.96, paste0(formatC(df_tab_2$att, format = "f", digits = 4), "*"), formatC(df_tab_2$att, format = "f", digits = 4))
df_tab_2$SE <- formatC(df_tab_2$se, format = "f", digits = 4)  # Format SE to 4 decimal places
df_tab_2$att = NULL
df_tab_2$se = NULL
df_tab_2$dv = NULL
df_tab_2$dv_type = NULL
# Adjust column names to reflect the table
colnames(df_tab_2) <- c("Time", "Cell", "Actor", "ATT", "SE")

# Create the xtable object
xtable_obj <- xtable(
  df_tab_2,
  caption = "Table for our main results testing the temporal effects of peacekeeper entrance 
  and exit on the probability of violence against civilians. * $=$ p-value lower than 0.05; 
  bootstrapped standard errors are used to calculate the confidence intervals.",
  label = "tab:main_results_pr",
  digits = 4  # Set decimal places
)

# Adjust alignments to include vertical lines between columns
align(xtable_obj) <- "|l|l|l|l|r|r|"

# Print the LaTex table
print(
  xtable_obj,
  include.rownames = FALSE,  # No row numbers
  caption.placement = "bottom",  # Caption at the bottom
  sanitize.text.function = identity  # Retains LaTex text formatting
)

### Make summary tables - APPENDIX ###
rm(list = ls())
df = read_rds("./results/main_models.RDS")

# Calculate upper and lower bounds for confidence intervals
df$lower_bound <- df$att - 1.96 * df$se  # 95% confidence interval lower bound
df$upper_bound <- df$att + 1.96 * df$se  # 95% confidence interval upper bound

# Adjust column names to include the new bounds
colnames(df) <- c("Time", "Cell", "Actor", "DV", "DV Type", "ATT", "SE", "CI Lower", "CI Upper")

df_event = df %>%
  filter(`DV Type` == "Event")
df_death = df %>%
  filter(`DV Type` == "Death")

# Create a LaTex table with vertical lines, without row numbers
xtable_obj <- xtable(
  df_event,
  caption = "Models with violent events as the outcome. Bootstrapped standard errors are used to calculate the confidence intervals.",
  label = "tab:appendix_main_results_events",
  digits = 5  # Set decimal places
)

# Modify 'align' to add vertical lines between columns
align(xtable_obj) <- "|c|c|l|c|c|c|r|r|r|r|"  # Vertical lines between columns

# Print the LaTex table without row numbers
print(
  xtable_obj,
  include.rownames = FALSE,  # Remove row numbers
  caption.placement = "top",  # Placement of the caption
  sanitize.text.function = identity  # Retains original text for LaTex
)

# Create a LaTex table with vertical lines, without row numbers
xtable_obj <- xtable(
  df_death,
  caption = "Models with deaths as the outcome. Bootstrapped standard errors are used to calculate the confidence intervals.",
  label = "tab:appendix_main_results_deaths",
  digits = 5  # Set decimal places
)

align(xtable_obj) <- "|c|c|l|c|c|c|r|r|r|r|"  # Vertical lines between columns

# Print the LaTex table without row numbers
print(
  xtable_obj,
  include.rownames = FALSE,  # Remove row numbers
  caption.placement = "top",  # Placement of the caption
  sanitize.text.function = identity  # Retains original text for LaTex
)

##### PLOT MAIN RESULTS #####
rm(list = ls())
df = read_rds("./results/main_models.RDS")

##### Entrance - same - total #####
d_enter_same_total = df %>%
  filter(time == "Enter" & cell == "Same" & dv == "Total" & dv_type == "Event")

pdf("./results/enter_same_total.pdf", width = 6, height = 4)
ggplot(d_enter_same_total, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.7, 0.7), breaks = seq(-0.7, 0.7, 0.2)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

##### Exit - same - total #####
d_leave_same_total = df %>%
  filter(time == "Leave" & cell == "Same" & dv == "Total" & dv_type == "Event")

pdf("./results/leave_same_total.pdf", width = 6, height = 4)
ggplot(d_leave_same_total, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.7, 0.7), breaks = seq(-0.7, 0.7, 0.2)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()



##### Entrance - neighbor - total #####
d_enter_neighbor_total = df %>%
  filter(time == "Enter" & cell == "Neighbor" & dv == "Total" & dv_type == "Event")

pdf("./results/enter_neighbor_total.pdf", width = 6, height = 4)
ggplot(d_enter_neighbor_total, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.7, 0.7), breaks = seq(-0.7, 0.7, 0.2)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()


##### Exit - neighbor - total #####
d_leave_neighbor_total = df %>%
  filter(time == "Leave" & cell == "Neighbor" & dv == "Total" & dv_type == "Event")

pdf("./results/leave_neighbor_total.pdf", width = 6, height = 4)
ggplot(d_leave_neighbor_total, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.7, 0.7), breaks = seq(-0.7, 0.7, 0.2)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()



# Pr()
d_enter_same_pr = df %>%
  filter(time == "Enter" & cell == "Same" & dv == "Binary" & dv_type == "Event")

pdf("./results/enter_same_pr.pdf", width = 6, height = 4)
ggplot(d_enter_same_pr, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) sprintf("%.1f", x)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

d_enter_neighbor_pr = df %>%
  filter(time == "Enter" & cell == "Neighbor" & dv == "Binary" & dv_type == "Event")

pdf("./results/enter_neighbor_pr.pdf", width = 6, height = 4)
ggplot(d_enter_neighbor_pr, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) sprintf("%.1f", x)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

d_leave_same_pr = df %>%
  filter(time == "Leave" & cell == "Same" & dv == "Binary" & dv_type == "Event")

pdf("./results/leave_same_pr.pdf", width = 6, height = 4)
ggplot(d_leave_same_pr, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) sprintf("%.1f", x)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()

d_leave_neighbor_pr = df %>%
  filter(time == "Leave" & cell == "Neighbor" & dv == "Binary" & dv_type == "Event")

pdf("./results/leave_neighbor_pr.pdf", width = 6, height = 4)
ggplot(d_leave_neighbor_pr, aes(x = att, y = factor(actor), color = actor)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbarh(aes(xmin = att - se, xmax = att + se),
                 height = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~dv + time + cell, scales = "free") +
  scale_color_manual(values = c("GOV" = "#4D858E", "REB" = "#E38030")) +
  theme_minimal() +
  labs(title = "", x = "ATT", y = "") +
  theme(strip.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y.right = element_blank(),
        legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1),
                     labels = function(x) sprintf("%.1f", x)) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5)
dev.off()









######### Descriptives Plots ######### 
library(ggpubr)
rm(list = ls())
df = read_rds("./data/Kunkel-Atkinson-Dudley-Warner-final.rds") %>% 
  as.data.frame() %>%
  select(gid, year, month, time, first_treated, treated, post_treatment, first_treated_leave, treated_leave, 
         post_treatment_leave)
gc()

## 
# Calculate the frequency of peacekeeping entrances over time
entrances_over_time <- df %>%
  distinct(gid, first_treated) %>%
  filter(first_treated > 0) %>%
  count(first_treated)

pdf("./test_entrance.pdf")
ggplot(entrances_over_time, aes(x = first_treated, y = n)) +
  geom_line(color = "#5b92e5") +
  geom_point(shape = 16, color = "#5b92e5", size = 2) + # Shape 16 corresponds to a solid circle
  labs(title = "Frequency of Peacekeeping Entrances Over Time",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubr()

ggplot(entrances_over_time, aes(x = first_treated, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Peacekeeping Entrances Over Time",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean()

ggplot(entrances_over_time, aes(x = first_treated, y = n)) +
  geom_point(color = "#5b92e5", size = 2) + # Adjusted point color and size
  geom_smooth(method = "loess", color = "black") + # LOESS smooth line
  labs(title = "Frequency of Peacekeeping Entrances Over Time",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean()
dev.off()

# Calculate the frequency of peacekeeping entrances over time
exits_over_time <- df %>%
  distinct(gid, first_treated_leave) %>%
  filter(first_treated_leave > 0) %>%
  count(first_treated_leave)

pdf("./test_exit.pdf")
ggplot(exits_over_time, aes(x = first_treated_leave, y = n)) +
  geom_line(color = "#5b92e5") +
  geom_point(shape = 16, color = "#5b92e5", size = 2) + # Shape 16 corresponds to a solid circle
  labs(title = "Frequency of Peacekeeping Exits Over Time",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubr()

ggplot(exits_over_time, aes(x = first_treated_leave, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Peacekeeping Exits Over Time",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean()

ggplot(exits_over_time, aes(x = first_treated_leave, y = n)) +
  geom_point(color = "#5b92e5", size = 2) + # Adjusted point color and size
  geom_smooth(method = "loess", color = "black") + # LOESS smooth line
  labs(title = "Frequency of Peacekeeping Exits Over Time",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean()
dev.off()

## by mission ##
# mission was dropped earlier to solve different coding issue, so we'll read RADPKO back in and join it to main data
radpko <- read_csv("./data/radpko/radpko_grid.csv") %>% 
  # make the date variable a date type
  mutate(date = ymd(date),
         month = month(date),
         year = year(date)) %>% 
  filter(year > 1999 & year < 2018) %>% # until data is updated, need to filter out incomplete years
  # rename variable for ease of merging
  rename(gid = prio.grid) %>%
  distinct(mission, gid)

df = left_join(df, radpko, relationship = "many-to-many")

entrances_over_time_mission <- df %>%
  distinct(gid, first_treated, mission) %>%
  filter(first_treated > 0, !is.na(mission)) %>%
  count(first_treated, mission)

pdf("./test_entrance_mission.pdf")
ggplot(entrances_over_time_mission, aes(x = first_treated, y = n, group = mission)) +
  geom_line(color = "#5b92e5") +
  geom_point(shape = 16, color = "#5b92e5", size = 2) +
  labs(title = "Frequency of Peacekeeping Entrances Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubr() +
  facet_wrap(~mission, scales = "free_y")

ggplot(entrances_over_time_mission, aes(x = first_treated, y = n, fill = mission)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Peacekeeping Entrances Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean()

ggplot(entrances_over_time_mission, aes(x = first_treated, y = n, color = mission)) +
  geom_line() +
  geom_point(shape = 16, size = 2) +
  labs(title = "Frequency of Peacekeeping Entrances Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean()

ggplot(entrances_over_time_mission, aes(x = first_treated, y = n, fill = mission)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Peacekeeping Entrances Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean() +
  facet_wrap(~mission, scales = "free_y") +
  theme(legend.position = "none")

ggplot(entrances_over_time_mission, aes(x = first_treated, y = n, color = mission)) +
  geom_point(shape = 16, size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Frequency of Peacekeeping Entrances Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Entrances") +
  theme_pubclean() +
  facet_wrap(~ mission, scales = "free_y") +
  theme(legend.position = "none")
dev.off()


exits_over_time_mission <- df %>%
  distinct(gid, first_treated_leave, mission) %>%
  filter(first_treated_leave > 0, !is.na(mission)) %>%
  count(first_treated_leave, mission)

pdf("./test_exit_mission.pdf")
ggplot(exits_over_time_mission, aes(x = first_treated_leave, y = n, group = mission)) +
  geom_line(color = "#5b92e5") +
  geom_point(shape = 16, color = "#5b92e5", size = 2) +
  labs(title = "Frequency of Peacekeeping exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of exits") +
  theme_pubr() +
  facet_wrap(~mission, scales = "free_y")

ggplot(exits_over_time_mission, aes(x = first_treated_leave, y = n, fill = mission)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Peacekeeping exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of exits") +
  theme_pubclean()

ggplot(exits_over_time_mission, aes(x = first_treated_leave, y = n, color = mission)) +
  geom_line() +
  geom_point(shape = 16, size = 2) +
  labs(title = "Frequency of Peacekeeping exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of exits") +
  theme_pubclean()

ggplot(exits_over_time_mission, aes(x = first_treated_leave, y = n, fill = mission)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Peacekeeping exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of exits") +
  theme_pubclean() +
  facet_wrap(~mission, scales = "free_y") +
  theme(legend.position = "none")

ggplot(exits_over_time_mission, aes(x = first_treated_leave, y = n, color = mission)) +
  geom_point(shape = 16, size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Frequency of Peacekeeping exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of exits") +
  theme_pubclean() +
  facet_wrap(~ mission, scales = "free_y") +
  theme(legend.position = "none")
dev.off()

## combined? ##
# Combine entrances and exits data
entrances_over_time_mission <- df %>%
  filter(first_treated > 0, !is.na(mission)) %>%
  count(first_treated, mission) %>%
  rename(time = first_treated, n_entrances = n)

# Prepare data for exits
exits_over_time_mission <- df %>%
  filter(first_treated_leave > 0, !is.na(mission)) %>%
  count(first_treated_leave, mission) %>%
  rename(time = first_treated_leave, n_exits = n)
combined_data <- full_join(entrances_over_time_mission, exits_over_time_mission, by = c("time", "mission")) %>%
  replace_na(list(n_entrances = 0, n_exits = 0))


pdf("./test_combined_mission.pdf")

ggplot(combined_data, aes(x = time, group = mission)) +
  geom_line(aes(y = n_entrances, color = "Entrances")) +
  geom_line(aes(y = n_exits, color = "Exits")) +
  geom_point(aes(y = n_entrances, color = "Entrances"), shape = 16, size = 2) +
  geom_point(aes(y = n_exits, color = "Exits"), shape = 16, size = 2) +
  labs(title = "Frequency of Peacekeeping Entrances and Exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Events") +
  theme_pubr() +
  facet_wrap(~mission, scales = "free_y") +
  scale_color_manual(values = c("Entrances" = "#5b92e5", "Exits" = "red"))

#### 2. Bar Plot
ggplot(combined_data, aes(x = time)) +
  geom_bar(aes(y = n_entrances, fill = "Entrances"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = n_exits, fill = "Exits"), stat = "identity", position = "dodge") +
  labs(title = "Frequency of Peacekeeping Entrances and Exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Events") +
  theme_pubclean() +
  facet_wrap(~mission, scales = "free_y") +
  scale_fill_manual(values = c("Entrances" = "#5b92e5", "Exits" = "red"))

#### 4. Faceted Bar Plot
ggplot(combined_data, aes(x = time)) +
  geom_bar(aes(y = n_entrances, fill = "Entrances"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = n_exits, fill = "Exits"), stat = "identity", position = "dodge") +
  labs(title = "Frequency of Peacekeeping Entrances and Exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Events") +
  theme_pubclean() +
  facet_wrap(~mission, scales = "free_y") +
  scale_fill_manual(values = c("Entrances" = "#5b92e5", "Exits" = "red")) +
  theme(legend.position = "none")

#### 5. Scatter Plot with LOESS Smoothing
ggplot(combined_data, aes(x = time)) +
  geom_point(aes(y = n_entrances, color = "Entrances"), shape = 16, size = 2) +
  geom_smooth(aes(y = n_entrances, color = "Entrances"), method = "loess", se = FALSE) +
  geom_point(aes(y = n_exits, color = "Exits"), shape = 16, size = 2) +
  geom_smooth(aes(y = n_exits, color = "Exits"), method = "loess", se = FALSE, color = "black") +
  labs(title = "Frequency of Peacekeeping Entrances and Exits Over Time by Mission",
       x = "Time (months since January 2000)",
       y = "Number of Events") +
  theme_pubclean() +
  facet_wrap(~ mission, scales = "free_y") +
  scale_color_manual(values = c("Entrances" = "#5b92e5", "Exits" = "red")) +
  theme(legend.position = "none")

dev.off()


### average length of stay ###

df <- df %>%
  mutate(first_treated_leave = ifelse(first_treated_leave == 0, 216, first_treated_leave),
         length_of_stay = first_treated_leave - first_treated) 
dd = df %>%
  filter(first_treated > 0, !is.na(mission)) %>%
  distinct(gid, mission, length_of_stay)

## length of stay by mission ##
# Calculate descriptives for length of stay by mission
length_of_stay_by_mission <- dd %>%
  mutate(mission = ifelse(mission == "MONUC" | mission == "MONUSCO", "MONUC/MONUSCO", mission)) %>%
  group_by(mission) %>%
  summarize(mean_stay = mean(length_of_stay, na.rm = TRUE),
            median_stay = median(length_of_stay, na.rm = TRUE),
            sd_stay = sd(length_of_stay, na.rm = TRUE),
            min_stay = min(length_of_stay, na.rm = TRUE),
            max_stay = max(length_of_stay, na.rm = TRUE))

# Load necessary library for table rendering
library(knitr)

# Display the descriptive statistics by mission as a table
colnames(length_of_stay_by_mission) <- c("Mission", "Mean", "Median", "SD", "Min", "Max")
length_of_stay_by_mission$`Mean Stay` <- round(length_of_stay_by_mission$`Mean Stay`, 2)
length_of_stay_by_mission$`SD Stay` <- round(length_of_stay_by_mission$`SD Stay`, 2)
kable(length_of_stay_by_mission, caption = "Descriptive Statistics for Length of Stay by Mission")

kable(length_of_stay_by_mission, format = "latex", booktabs = TRUE, 
      caption = "Descriptive Statistics for Length of Stay by Mission", label = "tab:desc",
    linesep = "")


### anecdotes ###
z = df %>% 
  filter(length_of_stay == 0 | length_of_stay == 1) %>%
  select(gid, year, month, time, first_treated, treated, post_treatment, first_treated_leave, treated_leave, post_treatment_leave, 
         length_of_stay, mission, country, radpko_units_deployed)



######### make parallel trends plots ######### 
rm(list = ls())
df = read_rds("./data/Kunkel-Atkinson-Warner-final.rds")

##################################### VIOLENT EVENTS #####################################
###### TOTAL #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "dynamic", na.rm = T) # extract for parallel trends plot
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

pdf("./results/event_study_pt/1.pdf")
ggplot(data = es1_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
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
dev.off()
rm(es1, es1_plot, out1)


set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "dynamic", na.rm = T)
es2_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es2$egt, ")"),
  event.time= es2$egt,
  estimate  = es2$att.egt,
  std.error = es2$se.egt,
  conf.low  = es2$att.egt - es2$crit.val.egt * es2$se.egt,
  conf.high = es2$att.egt + es2$crit.val.egt  * es2$se.egt,
  point.conf.low  = es2$att.egt - stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt,
  point.conf.high = es2$att.egt + stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/2.pdf")
ggplot(data = es2_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es2_plot$event.time), max(es2_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es2, es2_plot, out2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "dynamic", na.rm = T)
es3_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es3$egt, ")"),
  event.time= es3$egt,
  estimate  = es3$att.egt,
  std.error = es3$se.egt,
  conf.low  = es3$att.egt - es3$crit.val.egt * es3$se.egt,
  conf.high = es3$att.egt + es3$crit.val.egt  * es3$se.egt,
  point.conf.low  = es3$att.egt - stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt,
  point.conf.high = es3$att.egt + stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/3.pdf")
ggplot(data = es3_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es3_plot$event.time), max(es3_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es3, es3_plot, out3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "dynamic", na.rm = T)
es4_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es4$egt, ")"),
  event.time= es4$egt,
  estimate  = es4$att.egt,
  std.error = es4$se.egt,
  conf.low  = es4$att.egt - es4$crit.val.egt * es4$se.egt,
  conf.high = es4$att.egt + es4$crit.val.egt  * es4$se.egt,
  point.conf.low  = es4$att.egt - stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt,
  point.conf.high = es4$att.egt + stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/4.pdf")
ggplot(data = es4_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es4_plot$event.time), max(es4_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es4, es4_plot, out4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "dynamic", na.rm = T)
es5_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es5$egt, ")"),
  event.time= es5$egt,
  estimate  = es5$att.egt,
  std.error = es5$se.egt,
  conf.low  = es5$att.egt - es5$crit.val.egt * es5$se.egt,
  conf.high = es5$att.egt + es5$crit.val.egt  * es5$se.egt,
  point.conf.low  = es5$att.egt - stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt,
  point.conf.high = es5$att.egt + stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/5.pdf")
ggplot(data = es5_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es5_plot$event.time), max(es5_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es5, es5_plot, out5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "dynamic", na.rm = T)
es6_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es6$egt, ")"),
  event.time= es6$egt,
  estimate  = es6$att.egt,
  std.error = es6$se.egt,
  conf.low  = es6$att.egt - es6$crit.val.egt * es6$se.egt,
  conf.high = es6$att.egt + es6$crit.val.egt  * es6$se.egt,
  point.conf.low  = es6$att.egt - stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt,
  point.conf.high = es6$att.egt + stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/6.pdf")
ggplot(data = es6_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es6_plot$event.time), max(es6_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es6, es6_plot, out6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "dynamic", na.rm = T)
es7_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es7$egt, ")"),
  event.time= es7$egt,
  estimate  = es7$att.egt,
  std.error = es7$se.egt,
  conf.low  = es7$att.egt - es7$crit.val.egt * es7$se.egt,
  conf.high = es7$att.egt + es7$crit.val.egt  * es7$se.egt,
  point.conf.low  = es7$att.egt - stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt,
  point.conf.high = es7$att.egt + stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/7.pdf")
ggplot(data = es7_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es7_plot$event.time), max(es7_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es7, es7_plot, out7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_event_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "dynamic", na.rm = T)
es8_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es8$egt, ")"),
  event.time= es8$egt,
  estimate  = es8$att.egt,
  std.error = es8$se.egt,
  conf.low  = es8$att.egt - es8$crit.val.egt * es8$se.egt,
  conf.high = es8$att.egt + es8$crit.val.egt  * es8$se.egt,
  point.conf.low  = es8$att.egt - stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt,
  point.conf.high = es8$att.egt + stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/8.pdf")
ggplot(data = es8_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es8_plot$event.time), max(es8_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es8, es8_plot, out8)

###### Pr() #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "dynamic", na.rm = T)
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

pdf("./results/event_study_pt/9.pdf")
ggplot(data = es1_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
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
dev.off()
rm(es1, es1_plot, out1)

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "dynamic", na.rm = T)
es2_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es2$egt, ")"),
  event.time= es2$egt,
  estimate  = es2$att.egt,
  std.error = es2$se.egt,
  conf.low  = es2$att.egt - es2$crit.val.egt * es2$se.egt,
  conf.high = es2$att.egt + es2$crit.val.egt  * es2$se.egt,
  point.conf.low  = es2$att.egt - stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt,
  point.conf.high = es2$att.egt + stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/10.pdf")
ggplot(data = es2_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es2_plot$event.time), max(es2_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es2, es2_plot, out2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "dynamic", na.rm = T)
es3_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es3$egt, ")"),
  event.time= es3$egt,
  estimate  = es3$att.egt,
  std.error = es3$se.egt,
  conf.low  = es3$att.egt - es3$crit.val.egt * es3$se.egt,
  conf.high = es3$att.egt + es3$crit.val.egt  * es3$se.egt,
  point.conf.low  = es3$att.egt - stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt,
  point.conf.high = es3$att.egt + stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/11.pdf")
ggplot(data = es3_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es3_plot$event.time), max(es3_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es3, es3_plot, out3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "dynamic", na.rm = T)
es4_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es4$egt, ")"),
  event.time= es4$egt,
  estimate  = es4$att.egt,
  std.error = es4$se.egt,
  conf.low  = es4$att.egt - es4$crit.val.egt * es4$se.egt,
  conf.high = es4$att.egt + es4$crit.val.egt  * es4$se.egt,
  point.conf.low  = es4$att.egt - stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt,
  point.conf.high = es4$att.egt + stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/12.pdf")
ggplot(data = es4_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es4_plot$event.time), max(es4_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es4, es4_plot, out4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "dynamic", na.rm = T)
es5_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es5$egt, ")"),
  event.time= es5$egt,
  estimate  = es5$att.egt,
  std.error = es5$se.egt,
  conf.low  = es5$att.egt - es5$crit.val.egt * es5$se.egt,
  conf.high = es5$att.egt + es5$crit.val.egt  * es5$se.egt,
  point.conf.low  = es5$att.egt - stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt,
  point.conf.high = es5$att.egt + stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/13.pdf")
ggplot(data = es5_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es5_plot$event.time), max(es5_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es5, es5_plot, out5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "dynamic", na.rm = T)
es6_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es6$egt, ")"),
  event.time= es6$egt,
  estimate  = es6$att.egt,
  std.error = es6$se.egt,
  conf.low  = es6$att.egt - es6$crit.val.egt * es6$se.egt,
  conf.high = es6$att.egt + es6$crit.val.egt  * es6$se.egt,
  point.conf.low  = es6$att.egt - stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt,
  point.conf.high = es6$att.egt + stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/14.pdf")
ggplot(data = es6_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es6_plot$event.time), max(es6_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es6, es6_plot, out6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "dynamic", na.rm = T)
es7_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es7$egt, ")"),
  event.time= es7$egt,
  estimate  = es7$att.egt,
  std.error = es7$se.egt,
  conf.low  = es7$att.egt - es7$crit.val.egt * es7$se.egt,
  conf.high = es7$att.egt + es7$crit.val.egt  * es7$se.egt,
  point.conf.low  = es7$att.egt - stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt,
  point.conf.high = es7$att.egt + stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/15.pdf")
ggplot(data = es7_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es7_plot$event.time), max(es7_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es7, es7_plot, out7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_event_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "dynamic", na.rm = T)
es8_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es8$egt, ")"),
  event.time= es8$egt,
  estimate  = es8$att.egt,
  std.error = es8$se.egt,
  conf.low  = es8$att.egt - es8$crit.val.egt * es8$se.egt,
  conf.high = es8$att.egt + es8$crit.val.egt  * es8$se.egt,
  point.conf.low  = es8$att.egt - stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt,
  point.conf.high = es8$att.egt + stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/16.pdf")
ggplot(data = es8_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es8_plot$event.time), max(es8_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es8, es8_plot, out8)

##################################### FATALITIES ##################################### 
###### TOTAL #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 1, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "dynamic", na.rm = T) # extract for parallel trends plot
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

pdf("./results/event_study_pt/17.pdf")
ggplot(data = es1_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
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
dev.off()
rm(es1, es1_plot, out1)


set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "dynamic", na.rm = T)
es2_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es2$egt, ")"),
  event.time= es2$egt,
  estimate  = es2$att.egt,
  std.error = es2$se.egt,
  conf.low  = es2$att.egt - es2$crit.val.egt * es2$se.egt,
  conf.high = es2$att.egt + es2$crit.val.egt  * es2$se.egt,
  point.conf.low  = es2$att.egt - stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt,
  point.conf.high = es2$att.egt + stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/18.pdf")
ggplot(data = es2_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es2_plot$event.time), max(es2_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es2, es2_plot, out2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "dynamic", na.rm = T)
es3_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es3$egt, ")"),
  event.time= es3$egt,
  estimate  = es3$att.egt,
  std.error = es3$se.egt,
  conf.low  = es3$att.egt - es3$crit.val.egt * es3$se.egt,
  conf.high = es3$att.egt + es3$crit.val.egt  * es3$se.egt,
  point.conf.low  = es3$att.egt - stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt,
  point.conf.high = es3$att.egt + stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/19.pdf")
ggplot(data = es3_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es3_plot$event.time), max(es3_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es3, es3_plot, out3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "dynamic", na.rm = T)
es4_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es4$egt, ")"),
  event.time= es4$egt,
  estimate  = es4$att.egt,
  std.error = es4$se.egt,
  conf.low  = es4$att.egt - es4$crit.val.egt * es4$se.egt,
  conf.high = es4$att.egt + es4$crit.val.egt  * es4$se.egt,
  point.conf.low  = es4$att.egt - stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt,
  point.conf.high = es4$att.egt + stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/20.pdf")
ggplot(data = es4_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es4_plot$event.time), max(es4_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es4, es4_plot, out4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "dynamic", na.rm = T)
es5_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es5$egt, ")"),
  event.time= es5$egt,
  estimate  = es5$att.egt,
  std.error = es5$se.egt,
  conf.low  = es5$att.egt - es5$crit.val.egt * es5$se.egt,
  conf.high = es5$att.egt + es5$crit.val.egt  * es5$se.egt,
  point.conf.low  = es5$att.egt - stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt,
  point.conf.high = es5$att.egt + stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/21.pdf")
ggplot(data = es5_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es5_plot$event.time), max(es5_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es5, es5_plot, out5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "dynamic", na.rm = T)
es6_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es6$egt, ")"),
  event.time= es6$egt,
  estimate  = es6$att.egt,
  std.error = es6$se.egt,
  conf.low  = es6$att.egt - es6$crit.val.egt * es6$se.egt,
  conf.high = es6$att.egt + es6$crit.val.egt  * es6$se.egt,
  point.conf.low  = es6$att.egt - stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt,
  point.conf.high = es6$att.egt + stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/22.pdf")
ggplot(data = es6_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es6_plot$event.time), max(es6_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es6, es6_plot, out6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "dynamic", na.rm = T)
es7_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es7$egt, ")"),
  event.time= es7$egt,
  estimate  = es7$att.egt,
  std.error = es7$se.egt,
  conf.low  = es7$att.egt - es7$crit.val.egt * es7$se.egt,
  conf.high = es7$att.egt + es7$crit.val.egt  * es7$se.egt,
  point.conf.low  = es7$att.egt - stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt,
  point.conf.high = es7$att.egt + stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/23.pdf")
ggplot(data = es7_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es7_plot$event.time), max(es7_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es7, es7_plot, out7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_death_all", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "dynamic", na.rm = T)
es8_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es8$egt, ")"),
  event.time= es8$egt,
  estimate  = es8$att.egt,
  std.error = es8$se.egt,
  conf.low  = es8$att.egt - es8$crit.val.egt * es8$se.egt,
  conf.high = es8$att.egt + es8$crit.val.egt  * es8$se.egt,
  point.conf.low  = es8$att.egt - stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt,
  point.conf.high = es8$att.egt + stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/24.pdf")
ggplot(data = es8_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es8_plot$event.time), max(es8_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es8, es8_plot, out8)

###### Pr() #######
## Same cell, enter ##
set.seed(8675309) # hey jenny
out1 <- att_gt(yname = "acled_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es1 <- aggte(out1, type = "dynamic", na.rm = T)
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

pdf("./results/event_study_pt/25.pdf")
ggplot(data = es1_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
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
dev.off()
rm(es1, es1_plot, out1)

set.seed(8675309) # hey jenny
out2 <- att_gt(yname = "acled_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es2 <- aggte(out2, type = "dynamic", na.rm = T)
es2_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es2$egt, ")"),
  event.time= es2$egt,
  estimate  = es2$att.egt,
  std.error = es2$se.egt,
  conf.low  = es2$att.egt - es2$crit.val.egt * es2$se.egt,
  conf.high = es2$att.egt + es2$crit.val.egt  * es2$se.egt,
  point.conf.low  = es2$att.egt - stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt,
  point.conf.high = es2$att.egt + stats::qnorm(1 - es2$DIDparams$alp/2) * es2$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/26.pdf")
ggplot(data = es2_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es2_plot$event.time), max(es2_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es2, es2_plot, out2)

## Neighbor cell, enter ##
set.seed(8675309) # hey jenny
out3 <- att_gt(yname = "neighbor_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es3 <- aggte(out3, type = "dynamic", na.rm = T)
es3_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es3$egt, ")"),
  event.time= es3$egt,
  estimate  = es3$att.egt,
  std.error = es3$se.egt,
  conf.low  = es3$att.egt - es3$crit.val.egt * es3$se.egt,
  conf.high = es3$att.egt + es3$crit.val.egt  * es3$se.egt,
  point.conf.low  = es3$att.egt - stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt,
  point.conf.high = es3$att.egt + stats::qnorm(1 - es3$DIDparams$alp/2) * es3$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/27.pdf")
ggplot(data = es3_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es3_plot$event.time), max(es3_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es3, es3_plot, out3)

set.seed(8675309) # hey jenny
out4 <- att_gt(yname = "neighbor_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es4 <- aggte(out4, type = "dynamic", na.rm = T)
es4_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es4$egt, ")"),
  event.time= es4$egt,
  estimate  = es4$att.egt,
  std.error = es4$se.egt,
  conf.low  = es4$att.egt - es4$crit.val.egt * es4$se.egt,
  conf.high = es4$att.egt + es4$crit.val.egt  * es4$se.egt,
  point.conf.low  = es4$att.egt - stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt,
  point.conf.high = es4$att.egt + stats::qnorm(1 - es4$DIDparams$alp/2) * es4$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/28.pdf")
ggplot(data = es4_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es4_plot$event.time), max(es4_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es4, es4_plot, out4)

## Same cell, leave ##
set.seed(8675309) # hey jenny
out5 <- att_gt(yname = "acled_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es5 <- aggte(out5, type = "dynamic", na.rm = T)
es5_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es5$egt, ")"),
  event.time= es5$egt,
  estimate  = es5$att.egt,
  std.error = es5$se.egt,
  conf.low  = es5$att.egt - es5$crit.val.egt * es5$se.egt,
  conf.high = es5$att.egt + es5$crit.val.egt  * es5$se.egt,
  point.conf.low  = es5$att.egt - stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt,
  point.conf.high = es5$att.egt + stats::qnorm(1 - es5$DIDparams$alp/2) * es5$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/29.pdf")
ggplot(data = es5_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es5_plot$event.time), max(es5_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es5, es5_plot, out5)

set.seed(8675309) # hey jenny
out6 <- att_gt(yname = "acled_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es6 <- aggte(out6, type = "dynamic", na.rm = T)
es6_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es6$egt, ")"),
  event.time= es6$egt,
  estimate  = es6$att.egt,
  std.error = es6$se.egt,
  conf.low  = es6$att.egt - es6$crit.val.egt * es6$se.egt,
  conf.high = es6$att.egt + es6$crit.val.egt  * es6$se.egt,
  point.conf.low  = es6$att.egt - stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt,
  point.conf.high = es6$att.egt + stats::qnorm(1 - es6$DIDparams$alp/2) * es6$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/30.pdf")
ggplot(data = es6_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es6_plot$event.time), max(es6_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es6, es6_plot, out6)

## Neighbor cell, leave ##
set.seed(8675309) # hey jenny
out7 <- att_gt(yname = "neighbor_vac_gov_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es7 <- aggte(out7, type = "dynamic", na.rm = T)
es7_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es7$egt, ")"),
  event.time= es7$egt,
  estimate  = es7$att.egt,
  std.error = es7$se.egt,
  conf.low  = es7$att.egt - es7$crit.val.egt * es7$se.egt,
  conf.high = es7$att.egt + es7$crit.val.egt  * es7$se.egt,
  point.conf.low  = es7$att.egt - stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt,
  point.conf.high = es7$att.egt + stats::qnorm(1 - es7$DIDparams$alp/2) * es7$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/31.pdf")
ggplot(data = es7_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es7_plot$event.time), max(es7_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es7, es7_plot, out7)

set.seed(8675309) # hey jenny
out8 <- att_gt(yname = "neighbor_vac_reb_death_any", tname = "time", idname = "gid", 
               gname = "first_treated_leave",data = df, pl = T, cores = 6, allow_unbalanced_panel = T)
es8 <- aggte(out8, type = "dynamic", na.rm = T)
es8_plot <-   data.frame(
  type          = "dynamic",
  term = paste0('ATT(', es8$egt, ")"),
  event.time= es8$egt,
  estimate  = es8$att.egt,
  std.error = es8$se.egt,
  conf.low  = es8$att.egt - es8$crit.val.egt * es8$se.egt,
  conf.high = es8$att.egt + es8$crit.val.egt  * es8$se.egt,
  point.conf.low  = es8$att.egt - stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt,
  point.conf.high = es8$att.egt + stats::qnorm(1 - es8$DIDparams$alp/2) * es8$se.egt
) %>%
  filter(event.time < 1)

pdf("./results/event_study_pt/32.pdf")
ggplot(data = es8_plot, mapping = aes(x = event.time, y = estimate)) +
  geom_vline(xintercept = 0-0.05, color = 'grey', linewidth = 1.2, linetype = "dotted") + 
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
  geom_line(mapping = aes(x = event.time, y=estimate), colour = "black", linewidth = 0.6, linetype = "dashed") +
  geom_line(size = 1.2, alpha = 2, colour = "darkblue") +
  geom_hline(yintercept = 0, colour="black", size = 0.25, linetype = "dotted") +
  xlab('Event time') +
  ylab("Event-Study Estimate") +
  scale_x_continuous(breaks = seq(min(es8_plot$event.time), max(es8_plot$event.time), by = 30)) +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color="black",  size = 12))+
  theme(plot.title=ggtext::element_markdown(size = 12, color="black", hjust=0, lineheight=1.2))
dev.off()
rm(es8, es8_plot, out8)

### plotting event study ###
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
  geom_ribbon(aes(ymin= point.conf.low, ymax=  point.conf.high), alpha = 0.5, linewidth = 1, fill = "steelblue")+
  geom_ribbon(aes(ymin=  conf.low, ymax =  conf.high), alpha =  0.3, linewidth = 1, fill = "steelblue")+
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
### end plotting
# Search for violence data

sort(tapply(a$acled_fatalities_violence_against_civilians, a$gid, max))

##### Check highest fatality grids #####
### Grid 139387 ###
# a.139387 = subset(a, gid == 139387     | gid == 139387-1   | gid == 139387+1 | 
#                     gid == 139387+720 | gid == 139387+719 | gid == 139387+721 | 
#                     gid == 139387-720 | gid == 139387-719 | gid == 139387-721) #%>%
# relocate(131:137, .after = 12)
# a.139387 = a.139387[order(a.139387$year, a.139387$month, a.139387$gid),]
# 
# 
# # search for pk presence
# sort(tapply(a$radpko_pko_deployed, a$gid, max))
# 
# ### Grid 143697 ###
# a.143697 = subset(a, gid == 143697     | gid == 143697-1   | gid == 143697+1 | 
#                     gid == 143697+720 | gid == 143697+719 | gid == 143697+721 | 
#                     gid == 143697-720 | gid == 143697-719 | gid == 143697-721) #%>%
# relocate(131:137, .after = 12)
# a.143697 = a.143697[order(a.143697$year, a.143697$month, a.143697$gid),]
# 
# ### Grid 141454 ### (not enough violence)
# a.141454 = subset(a, gid == 141454     | gid == 141454-1   | gid == 141454+1 | 
#                     gid == 141454+720 | gid == 141454+719 | gid == 141454+721 | 
#                     gid == 141454-720 | gid == 141454-719 | gid == 141454-721) #%>%
# a.141454 = a.141454[order(a.141454$year, a.141454$month, a.141454$gid),]
# 
# ### Grid 127139 ### (not enough violence)
# a.127139 = subset(a, gid == 127139     | gid == 127139-1   | gid == 127139+1 | 
#                     gid == 127139+720 | gid == 127139+719 | gid == 127139+721 | 
#                     gid == 127139-720 | gid == 127139-719 | gid == 127139-721) #%>%
# a.127139 = a.127139[order(a.127139$gid, a.127139$year, a.127139$month),]
# 
### Grid 138579 ### (not enough violence)
# a.138579 = subset(a, gid == 138579     | gid == 138579-1   | gid == 138579+1 |
#                     gid == 138579+720 | gid == 138579+719 | gid == 138579+721 |
#                     gid == 138579-720 | gid == 138579-719 | gid == 138579-721) #%>%
# a.138579 = a.138579[order(a.138579$gid, a.138579$year, a.138579$month),]
# 
# ### Grid 149451 ### (not enough violence)
# a.149451 = subset(a, gid == 149451     | gid == 149451-1   | gid == 149451+1 | 
#                     gid == 149451+720 | gid == 149451+719 | gid == 149451+721 | 
#                     gid == 149451-720 | gid == 149451-719 | gid == 149451-721 &
#                     time < 228) #%>%
# a.149451 = a.149451[order(a.149451$gid, a.149451$year, a.149451$month),]

# potential grids for plotting: 
# 142978, 2014-5
# 127139, 2018-1


##### Grid 132181 subset and cleaning ####
a.132181 = subset(a, gid == 132181     | gid == 132181-1   | gid == 132181+1 | gid == 132181+2 | gid == 132181-2| 
                    gid == 132181+720 | gid == 132181+719 | gid == 132181+721 | gid == 132181+718 | gid == 132181+722|
                    gid == 132181+1438| gid == 132181+1439| gid == 132181+1440| gid == 132181+1441| gid == 132181+1442|
                    gid == 132181-720 | gid == 132181-719 | gid == 132181-721 | gid == 132181-722 | gid == 132181-718 |
                    gid == 132181-1442| gid == 132181-1441| gid == 132181-1440| gid == 132181-1439| gid == 132181-1438) #%>%
a.132181 = a.132181[order(a.132181$gid, a.132181$year, a.132181$month),]
rm(a)
gc()

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
b.join$radpko_pko_deployed_any[b.join$radpko_pko_deployed_any == 0] <- NA
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

#### useful if plotting all of DRC and Uganda w/ surrounding countries on plot ####

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


##### Plot of moving violence after PK entrance #####
plot_1 = 
  ggplot() + geom_sf(aes(fill = b.join.0$fatalities, geometry = b.join.0$prio_geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  geom_point(data = b.join.0, aes(x = prio_xcoord, y = prio_ycoord, size=radpko_pko_deployed_any), alpha=0.4, shape = 19, colour = "#5b92e5") +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position="none")

plot_2 = 
  ggplot() + geom_sf(aes(fill = b.join.1$fatalities, geometry = b.join.1$prio_geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  geom_point(data = b.join.1, aes(x = prio_xcoord, y = prio_ycoord, size=radpko_pko_deployed_any), alpha=0.4, shape = 19, colour = "#5b92e5") +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position="none") + 
  geom_label(aes(x=31.2, y=2.88), label = "Uganda", label.padding = unit(0.55, "lines"),
             label.size = 0.35, color = alpha("black", 1), fill="#3bff9d") +
  geom_label(aes(x=29.8, y=2.88), label = "Democratic Republic of the Congo", 
             label.padding = unit(0.55, "lines"), label.size = 0.35, color = alpha("black", 1), fill="#3b9dff")


legend = 
  ggplot() + geom_sf(aes(fill = b.join.1$fatalities, geometry = b.join.1$prio_geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  geom_label(aes(x=31.2, y=2.88), label = "Uganda", label.padding = unit(0.55, "lines"),
                   label.size = 0.5, color = alpha("black", 1), fill="#3bff9d") +
  geom_label(aes(x=29.8, y=2.88), label = "Democratic Republic of the Congo", 
             label.padding = unit(1, "lines"), label.size = 0.35, color = alpha("black", 1), fill="#3b9dff") + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.background = element_rect(color = "black"), 
        legend.position = "bottom", legend.key.size = unit(1.75, 'cm'), legend.margin=margin(c(5,5,5,5)))
plot_2.1 = legend + labs(fill = "Fatalities")  

gg_legend = as_ggplot(get_legend(plot_2.1))

plot_3 = 
  ggplot() + geom_sf(aes(fill = b.join.2$fatalities, geometry = b.join.2$prio_geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  geom_point(data = b.join.2, aes(x = prio_xcoord, y = prio_ycoord, size=radpko_pko_deployed_any), alpha=0.4, shape = 19, colour = "#5b92e5") +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm", labs(fill = "Fatalities")), legend.position="none")

##### plot 2 vertical ####
# p_2 = 
#   ggplot() + geom_sf(aes(fill = b.join.1$fatalities, geometry = b.join.1$prio_geometry)) +
#   scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
#                       guide = "colourbar", aesthetics = "fill", limits=c(0,2050)) +
#   geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
#   geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
#   geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
#   xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
#   geom_label(aes(x=31.2, y=2.88), label = "Uganda", label.padding = unit(0.55, "lines"),
#              label.size = 0.35, color = alpha("black", 1), fill="#3bff9d") +
#   geom_label(aes(x=29.8, y=2.88), label = "Democratic Republic of the Congo", 
#              label.padding = unit(0.55, "lines"), label.size = 0.35, color = alpha("black", 1), fill="#3b9dff") +
#   theme(plot.margin = unit(c(0,5,0,0.15), "cm"), legend.background = element_rect(color = "black"), 
#         legend.position = c(1.25, 0.5), legend.key.size = unit(1.75, 'cm'))
# 
# plot_2 = p_2 + labs(fill = "Fatalities") 

##### Export Results #####
pdf("./results/violence_before.pdf")
plot_1
dev.off()
pdf("./results/violence_during.pdf")
plot_2
dev.off()
pdf("./results/violence_legend.pdf")
gg_legend
dev.off()
pdf("./results/violence_after.pdf")
plot_3
dev.off()


dev.off()

rm(list = ls())

svg("./drc_ug.svg", height = 10, width = 15)
ggarrange(plot_1, NULL, plot_2, NULL, plot_3, nrow = 1,
          labels = c("6 Months Before", "", "3 Years of PK Presence", "", "6 Months After"),
          legend = "none", label.y = 0.2, label.x = 0.15, widths = c(1, 0.05, 1, 0.05, 1))
dev.off()
#### Search for violence displacement grid ####

# Search for displacement grids
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(ggpubr)
library(tmap)
library(tmaptools)

a = readRDS("./data/plot.RDS") %>%
  relocate(c(16,29), .after = 4)
a1 = subset(a, a$radpko_pko_deployed > 0 & a$neighbor_fatalities_all > 0)
sort(decreasing = TRUE, tapply(a1$neighbor_fatalities_all, a1$gid, max))
# a1 grids
sort(decreasing = TRUE, tapply(a1$neighbor_fatalities_all, a1$gid, max))

# Potential grids
# Tested unsuccessfully: 111995 150914 151634
# Not tested:            152354 150915 151636
rm(list = setdiff(ls(), "a"))
#### Search for grids w/ violence displacement ####

# 127857
a.127857 = subset(a, gid == 127857     | gid == 127857-1   | gid == 127857+1 | gid == 127857+2 | gid == 127857-2| 
                    gid == 127857+720 | gid == 127857+719 | gid == 127857+721 | gid == 127857+718 | gid == 127857+722|
                    gid == 127857+1438| gid == 127857+1439| gid == 127857+1440| gid == 127857+1441| gid == 127857+1442|
                    gid == 127857-720 | gid == 127857-719 | gid == 127857-721 | gid == 127857-722 | gid == 127857-718 |
                    gid == 127857-1442| gid == 127857-1441| gid == 127857-1440| gid == 127857-1439| gid == 127857-1438) #%>%
a.127857 = a.127857[order(a.127857$gid, a.127857$year, a.127857$month),]
rm(list = setdiff(ls(), "a.127857"))
gc()


#### Prepare data for grid 127857 plotting ####
# PKs enter at time 2008-11 (119)
# PKs exit at time 2009-6 (126) [AKA, there were 0 PKs at this time in the data]

b = subset(a.127857, time < 130 & time > 114)
b = as.data.frame(b)
b$t_ind = 0
b$t_ind[b$time > 118 & b$time < 126] = 1
b$t_ind[b$time > 125] = 2


b.ag = b %>%
  group_by(t_ind, gid) %>%
  summarize(fatalities = sum(neighbor_fatalities_all))

b.ag$fatalities[b.ag$fatalities == 0] <- NA

# now join geographic data so we can plot it
prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", # get prio shapefiles
                    stringsAsFactors = F)
b.join = left_join(b.ag, b)
b.join = left_join(b.join, prio_shp, by = "gid")
b.join$radpko_pko_deployed_any[b.join$radpko_pko_deployed_any == 0] <- NA
b.join.0 = subset(b.join, t_ind == 0) %>%
  st_as_sf()
b.join.1 = subset(b.join, t_ind == 1)%>%
  st_as_sf()
b.join.2 = subset(b.join, t_ind == 2)%>%
  st_as_sf()

# read in DRC shapefiles
drc_00 <- st_read(dsn = "./data/gadm/drc", layer = "gadm40_COD_0", 
                  stringsAsFactors = F)
drc_01 <- st_read(dsn = "./data/gadm/drc", layer = "gadm40_COD_1", 
                  stringsAsFactors = F)

# read in Rwanda shapefiles
rwa_00 <- st_read(dsn = "./data/gadm/rwanda", layer = "gadm40_RWA_0", 
                  stringsAsFactors = F)
rwa_01 <- st_read(dsn = "./data/gadm/rwanda", layer = "gadm40_RWA_1", 
                  stringsAsFactors = F)

st_crs(b.join.0) = st_crs(drc_00)
st_crs(b.join.1) = st_crs(drc_00)
st_crs(b.join.2) = st_crs(drc_00)

#### grid 127857 plotting ####

plot_4 = 
  ggplot() + geom_sf(aes(fill = b.join.0$fatalities, geometry = b.join.0$geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,1200)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = rwa_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = rwa_00$geometry), size = 2, fill = alpha("red",0)) +
  xlim(27.112,29.388) + ylim(-2.388,-0.115) + theme_void() +
  geom_point(data = b.join.0, aes(x = xcoord, y = ycoord, size=radpko_pko_deployed_any), alpha=0.4, shape = 19, colour = "#5b92e5") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position="none")

plot_5 = 
  ggplot() + geom_sf(aes(fill = b.join.1$fatalities, geometry = b.join.1$geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,1200)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = rwa_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = rwa_00$geometry), size = 2, fill = alpha("red",0)) +
  xlim(27.112,29.388) + ylim(-2.388,-0.115) + theme_void() +
  geom_point(data = b.join.1, aes(x = xcoord, y = ycoord, size=radpko_pko_deployed_any), alpha=0.4, shape = 19, colour = "#5b92e5") +
  geom_label(aes(x=29.33, y=-2.3), label = "Uganda", label.padding = unit(0.55, "lines"),
             label.size = 0.5, color = alpha("black", 1), fill="#ff9d3b") +
  geom_label(aes(x=27.93, y=-2.3), label = "Democratic Republic of the Congo", 
             label.padding = unit(1, "lines"), label.size = 0.35, color = alpha("black", 1), fill="#3b9dff") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position="none")
  

legend_1 = 
  ggplot() + geom_sf(aes(fill = b.join.1$fatalities, geometry = b.join.1$geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,1200)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = rwa_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = rwa_00$geometry), size = 2, fill = alpha("red",0)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.background = element_rect(color = "black"), 
        legend.position = "bottom", legend.key.size = unit(1.75, 'cm'), legend.margin=margin(c(5,10,5,5)))
plot_5.1 = legend_1 + labs(fill = "Fatalities")  

gg_legend_1 = as_ggplot(get_legend(plot_5.1))

plot_6 = 
  ggplot() + geom_sf(aes(fill = b.join.2$fatalities, geometry = b.join.2$geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,1200)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = rwa_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = rwa_00$geometry), size = 2, fill = alpha("red",0)) +
  geom_point(data = b.join.2, aes(x = xcoord, y = ycoord, size=radpko_pko_deployed_any), alpha=0.4, shape = 19, colour = "#5b92e5") +
  xlim(27.112,29.388) + ylim(-2.388,-0.115) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position="none")

##### Export Results #####
pdf("./results/neighbor_violence_before.pdf")
plot_4
dev.off()
pdf("./results/neighbor_violence_during.pdf")
plot_5
dev.off()
pdf("./results/neighbor_violence_legend.pdf")
gg_legend_1
dev.off()
pdf("./results/neighbor_violence_after.pdf")
plot_6
dev.off()


dev.off()

rm(list = ls())

##### Descriptive Statistics Plots and Graphs #####
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

### MERGE ACLED DATA WITH PRIO GRID IDS ###
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

# plot of variables as different colors and different shape

dsc.1 = 
  ggplot(afr_shp) + geom_sf(aes(geometry = geometry), alpha = 0.3,fill = NA) +
  geom_point(data = df, aes(x = xcoord, y = ycoord, size=violence, colour = "#e5695b"), alpha=0.4, shape = 19) +
  geom_point(data = df, aes(x = xcoord, y = ycoord, size=pko_deployed, colour = "#5b92e5"), alpha=0.5, shape = 19) +
#  geom_sf(data = df.prio, aes(geometry = geometry), alpha = 0.1, fill = NA, size = 0.001) +
  scale_fill_viridis_c(option="E") +
  scale_size(range = c(.1, 24), name="Count", labels = c("20,000", "40,000", "60,000"), breaks = c(20000, 40000,60000)) +
  theme_void()

dsc = 
  dsc.1 + labs(colour = "Variable") + 
  scale_color_manual(labels = c("PKs Deployed", "Violence"), values = c("#5b92e5", "#e5695b")) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.25, 0.3),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)), 
        legend.key.size = unit(0.2, 'cm')) + 
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2), legend.direction="vertical")

pdf("./results/desc_plot.pdf")
dsc
dev.off()


# plot w/ variables as different colors but same shape

dsc = ggplot(afr_shp) + geom_sf(aes(geometry = geometry), fill = NA) +
  geom_point(data = dd_ac, aes(x = xcoord, y = ycoord, size=count, color=ct.type), alpha=0.4, shape = 19) +
  scale_size(range = c(.1, 24), name="Count") +
  scale_fill_viridis_c(option="E") +
  xlab("latitude") +
  ylab("longitude")

dsc + labs(color = "Variables of Interest")

# end of plot






##### Results Plot #####
# Clean data, subset to data size that won't crash my machine, then figure out DiD plot #

library(ggplot2)
library(tidyverse)
library(sf)
library(janitor)
library(lubridate)
library(viridis)
library(did)
library(ggthemes)
setwd("~/GitHub/when_peacekeepers_leave")
#### Plot data, add treated variables ####
a = readRDS("./data/Kunkel-Atkinson-Warner-final.rds") %>%
  as.data.frame() %>%
  select(-c(7,8,19:130)) %>%
  rename(first_treated = first_treated.x, post_treatment = post_treatment.x)
gc()

dd <- a %>% as.data.frame() %>% select(gid, time, radpko_pko_deployed_any)
dd <- split(dd, f = dd$gid)
dd <- lapply(dd, FUN = function(x){
  x$l_pko <- lag(x$radpko_pko_deployed_any, 1)
  y <- x[which(x$radpko_pko_deployed_any == 0 & x$l_pko == 1),]
  # create a "first treated" variable. needs to be 0 for untreated
  x$first_treated_leave <- ifelse(nrow(y) == 0, 0, min(y$time))
  # create a "post treated" variable. needs to be 0 until treatment then 1
  x$post_treatment_leave <- ifelse(x$first_treated != 0 & 
                                     x$time >= x$first_treated, 
                                   1, 0)
  # create a "treated" variable. needs to be 0 if control and 1 if treated
  x$treated_leave <- ifelse(sum(x$l_pko, na.rm = T) > 0, 1, 0)
  x
})
dd <- do.call(rbind, dd)
dd <- dd[,c("gid", "time", "first_treated_leave", "treated_leave", 
            "post_treatment_leave")]
a <- left_join(a, dd, by = c("gid", "time"))
dd = NULL
gc()
b = a %>%
  group_by(gid, time) %>%
  summarise(neighbor_fatalities_all = sum(neighbor_fatalities_battles, neighbor_fatalities_protests, neighbor_fatalities_strategic_developments,
                neighbor_fatalities_riots, neighbor_fatalities_explosions_remote_violence, 
                neighbor_fatalities_violence_against_civilians))
a = left_join(a, b, by = c("gid", "time"))
b = NULL
a = a %>%
  select(-c(18:20, 32:34))
gc()
saveRDS(a, "./data/plot.RDS")

#### Read in plotable data ####
library(ggplot2)
library(tidyverse)
library(sf)
library(janitor)
library(lubridate)
library(viridis)
library(did)
library(ggthemes)

a = readRDS("./data/plot.RDS")


# same grid
#### Build ES2 ####
# PK arrival/presence
out2 <- att_gt(yname = "acled_fatalities_any", 
               tname = "time", idname = "gid", 
               gname = "first_treated", data = a, pl = T, cores = 6)
es2 <- aggte(out2, type = "group")
saveRDS(es2, "./results/es2.RDS")

#### Plot ES2 ####
es2 = readRDS("./results/es2.RDS")
pdf("./results/es2.pdf")
ggdid(es2,theming = FALSE, title = " ", ylim = c(-4,4)) + geom_errorbarh(color = "white") +
  geom_point(shape = 18, colour = "#e5695b") +
  theme_few() + theme(legend.position = "none") + scale_colour_few("Light") +
  geom_errorbarh(color = "black", alpha = 0.3) +
  coord_cartesian(xlim = c(-2,2)) + 
  scale_y_discrete(breaks = seq(21, 231, by = 8))
dev.off()

# here we see an increase in violence when PKs arrive; because data is agg. at month level,
# this could mean that PKs arrive, hurt rebels, who then have to hurt civilians more to 
# extract resources from population, esp. since they don't have the usual mechanisms of a state
# plot can also be explained as, we don't see any v

# If your confidence interval for a difference between groups includes zero, that means that 
# if you run your experiment again you have a good chance of finding no difference between 
# groups.

#### Build ES4 ####
# PK Leaving
out4 <- att_gt(yname = "acled_fatalities_any", 
               tname = "time", idname = "gid", 
               gname = "first_treated_leave.x", data = a, pl = T, cores = 6)
es4 <- aggte(out4, type = "group")
saveRDS(es4, "./results/es4.RDS")

#### Plot ES4 ####
es4 = readRDS("./results/es4.RDS")
pdf("./results/es4.pdf")
ggdid(es4,theming = FALSE, title = " ", ylim = c(-4,4)) + geom_errorbarh(color = "white") +
  geom_point(shape = 18, colour = "#5b92e5") +
  theme_few() + theme(legend.position = "none") + scale_colour_few("Light") +
  geom_errorbarh(color = "black", alpha = 0.3) +
  coord_cartesian(xlim = c(-2,2)) + 
  scale_y_discrete(breaks = seq(21, 231, by = 8))

dev.off()

# Neighboring grids:

#### Build ES7 ####
# Cells: neighbor cells. IV: PKO presence. DV: Pr(fatalities). ATT = 0.07, significant
out7 <- att_gt(yname = "neighbor_fatalities_any", tname = "time", idname = "gid", 
               gname = "first_treated", data = df, pl = T, cores = 6)
es7 <- aggte(out7, type = "group")
summary(es7)
rm(out7, es7)
saveRDS(es7, "./results/es7.RDS")

#### Plot ES7 ####
es7 = readRDS("./results/es7.RDS")
pdf("./results/es7.pdf")
ggdid(es7,theming = FALSE, title = " ", ylim = c(-4,4)) + geom_errorbarh(color = "white") +
  geom_point(shape = 18, colour = "#e5695b") +
  theme_few() + theme(legend.position = "none") + scale_colour_few("Light") +
  geom_errorbarh(color = "black", alpha = 0.3) +
  coord_cartesian(xlim = c(-2,2)) + 
  scale_y_discrete(breaks = seq(21, 231, by = 8))
dev.off()

# ggdid(es7,theming = FALSE, title = " ", ylim = c(-4.5,4.5)) +
#   geom_point(shape = 18, colour = "dark green") + geom_errorbarh(color = "red") +
#   theme_few() + theme(legend.position = "none") + scale_colour_few("Medium")
dev.off()
# scale color manual? https://www.rdocumentation.org/packages/ggthemes/versions/3.5.0/topics/scale_colour_few

#### Build ES11 ####
# Cells: neighbor cells. IV: PKO leaving.  DV: fatalities.     ATT = -6.50, significant
out11 <- att_gt(yname = "neighbor_fatalities_all", 
                tname = "time", idname = "gid", 
                gname = "first_treated_leave", data = df, pl = T, cores = 6)
es11 <- aggte(out11, type = "group")
summary(es11)
saveRDS(es11, "./results/es11.RDS")
rm(es11)

#### Plot ES11 ####
es11 = readRDS("./results/es11.RDS")
gc()


pdf("./results/es11.pdf")
ggdid(es11,theming = FALSE, title = " ", ylim = c(-4,4)) + geom_errorbarh(color = "white") +
  geom_point(shape = 18, colour = "#5b92e5") +
  theme_few() + theme(legend.position = "none") + scale_colour_few("Light") +
  geom_errorbarh(color = "black", alpha = 0.3) +
  coord_cartesian(xlim = c(-2,2)) + 
  scale_y_discrete(breaks = seq(21, 231, by = 8))
dev.off()









#### Plot Parallel Trends ####







df = a
rm(a)
df$prio_geometry = NULL
gc()
library(ggplot2)
# first treatment (PKs enter, same violence)
sort(table(df$first_treated), decreasing = T)
df$treat_period <- ifelse(df$time < 34, 0, 1)
df$treat_period2 <- ifelse(df$time < 68, 0, 1) # 68 is the largest set of observations
df$treat_period3 <- ifelse(df$time < 81, 0, 1) # 81 is also the media of non-0 first_treated observations
df$treat_period4 <- ifelse(df$time < 112, 0, 1)
df$treat_period5 <- ifelse(df$time < 167, 0, 1)

data <- df %>% dplyr::group_by(gid) %>% 
  mutate(
    density_scaled = scale(acled_fatalities_any)
  )

data$ever_tl_text <- ifelse(data$first_treated > 0, "Treated", "Not Treated")

## Graph:
require("ggjoy")

p <- ggplot(data = data, aes(time, density_scaled))
pdf("./results/pt_pks_enter_same.pdf", 10, 8)
p + #geom_point(aes(color = factor(ever_tl_text)), alpha = .1, shape = 16) +
  scale_color_manual(values = c("black", "gray44")) +
  stat_smooth(data = subset(data, treat_period2 == 0), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  stat_smooth(data = subset(data, treat_period2 == 1), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  geom_vline(xintercept = 68) +
  #  theme_safeskies +
  ggtitle("Examining Parallel Trends Assumption, Treatment in t-68") + xlab("") + ylab("Pr(Fatalities)") +
  theme(plot.title = element_text(face="bold"), axis.text=element_text(size=11), axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank())
dev.off()


# first treatment (PKs enter, neighbor violence)
rm(data, p)
data <- df %>% dplyr::group_by(gid) %>% 
  mutate(
    density_scaled = scale(neighbor_fatalities_any)
  )

data$ever_tl_text <- ifelse(data$first_treated > 0, "Treated", "Not Treated")

## Graph:
require("ggjoy")

p <- ggplot(data = data, aes(time, density_scaled))
pdf("./results/pt_pks_enter_neighbor.pdf", 10, 8)
p + #geom_point(aes(color = factor(ever_tl_text)), alpha = .1, shape = 16) +
  scale_color_manual(values = c("black", "gray44")) +
  stat_smooth(data = subset(data, treat_period2 == 0), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  stat_smooth(data = subset(data, treat_period2 == 1), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  geom_vline(xintercept = 68) +
  #  theme_safeskies +
  ggtitle("Examining Parallel Trends Assumption, Treatment in t-68") + xlab("") + ylab("Pr(Fatalities)") +
  theme(plot.title = element_text(face="bold"), axis.text=element_text(size=11), axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank())
dev.off()


rm(data, p)
# first treatment (when pks leave)
sort(table(df$first_treated_leave), decreasing = T)
# 229      38     188     231      84 
median(df$first_treated_leave[df$first_treated_leave>0])
df$treat_period <- ifelse(df$time < 38, 0, 1)
df$treat_period2 <- ifelse(df$time < 84, 0, 1)
df$treat_period3 <- ifelse(df$time < 188, 0, 1) 
df$treat_period4 <- ifelse(df$time < 229, 0, 1)
df$treat_period5 <- ifelse(df$time < 231, 0, 1)

data <- df %>% dplyr::group_by(gid) %>% 
  mutate(
    density_scaled = scale(acled_fatalities_any)
  )

data$ever_tl_text <- ifelse(data$first_treated_leave > 0, "Treated", "Not Treated")

## Graph:
require("ggjoy")

p <- ggplot(data = data, aes(time, density_scaled))
pdf("./results/pt_pks_leave_same.pdf", 10, 8)
p + #geom_point(aes(color = factor(ever_tl_text)), alpha = .1, shape = 16) +
  scale_color_manual(values = c("black", "gray44")) +
  stat_smooth(data = subset(data, treat_period3 == 0), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  stat_smooth(data = subset(data, treat_period3 == 1), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  geom_vline(xintercept = 188) +
  #  theme_safeskies +
  ggtitle("Examining Parallel Trends Assumption, Treatment in t-188") + xlab("") + ylab("Pr(Fatalities)") +
  theme(plot.title = element_text(face="bold"), axis.text=element_text(size=11), axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank())
dev.off()


# first treatment (when pks leave, neighbor violence)
rm(data, p)
data <- df %>% dplyr::group_by(gid) %>% 
  mutate(
    density_scaled = scale(neighbor_fatalities_any)
  )

data$ever_tl_text <- ifelse(data$first_treated_leave > 0, "Treated", "Not Treated")

## Graph:
require("ggjoy")

p <- ggplot(data = data, aes(time, density_scaled))
pdf("./results/pt_pks_leave_neighbor.pdf", 10, 8)
p + #geom_point(aes(color = factor(ever_tl_text)), alpha = .1, shape = 16) +
  scale_color_manual(values = c("black", "gray44")) +
  stat_smooth(data = subset(data, treat_period3 == 0), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  stat_smooth(data = subset(data, treat_period3 == 1), aes(color = factor(ever_tl_text), linetype = factor(ever_tl_text)), method = "lm", fill = "lightgray") +
  geom_vline(xintercept = 188) +
  #  theme_safeskies +
  ggtitle("Examining Parallel Trends Assumption, Treatment in t-188") + xlab("") + ylab("Pr(Fatalities)") +
  theme(plot.title = element_text(face="bold"), axis.text=element_text(size=11), axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank())
dev.off()



# try group_by time at first treatment and time (and treatment?)
# then plot avg violence for groups before treatment that were eventually treated and those that weren't
# first need to drop obs after first treatment
# df = df[order(df$time, decreasing=FALSE), ] 
# df = df[order(df$gid, decreasing=FALSE), ]
# 
# df1 = df %>%
#   group_by(gid, group = with(rle(treated), 
#                             rep(seq_along(values), lengths))) %>%
#   slice(if (first(treated) == 0) seq_len(n()) else  1L) %>%
#   ungroup() %>%
#   select(-group)
# 
# 
# df1 = df1 %>%
#   group_by(treated, time) %>%
#   summarize(fatalities = sum(acled_fatalities_any))
# 
# 
# ggplot(df1, aes(time, fatalities, color = treated)) +
#   stat_summary(geom = 'line') +
#   geom_vline(xintercept = 0) +
#   theme_minimal()
# 
# 
# # use str()
# 
# 
# my_countries<-c('usa',"canada")
# test_results<-c(0,0,0)
# for(my_country in my_countries){
#   temp_violence<-NULL
#   temp_lags<-NULL
#   temp_dat<-dat[dat$country==my_country,]
#   good_year<-unique(temp_dat[temp_dat$intervention==1&temp_dat$year==min(temp_dat$year),'year'])
#   my_years<-unique(temp_dat$year)
#   my_lags<-my_years-good_year
#   for(my_lag in my_lags){
#     if(0>my_lag){
#       temp_violence<-c(temp_violence,temp_dat[temp_dat$year==my_years[my_lags==my_lag],'violence'])
#       temp_lags<-c(temp_lags,my_lag)
#     }
#   }
#   temp_results<-cbind(my_country,temp_lags,temp_violence)
#   test_results<-rbind(test_results,temp_results)
# }
# test_results<-test_results[-1,]
# colnames(test_results)<-c('country','lag','violence')
# 
# corr_dat<-NULL
# for(my_country in my_countries){
#   temp_dat<-test_results[test_results$country==my_country,]
#   temp_dat<-temp_dat[order(temp_dat$lag),]
#   other_countries<-my_countries[my_countries!=my_country]
#   for(my_other in other_countries){
#     temp_dat2<-test_results[test_results$country==my_other,]
#     temp_dat2<-temp_dat2[order(temp_dat2$lag),]
#    corr_dat<-c(corr_dat,ccf(temp_dat$violence,temp_dat2$violence)$statistic)
#   }
# }
# plot(density(corr_dat))