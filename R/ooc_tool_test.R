#OOC TOOL TEST

#If running on a new machine: uncomment the following package installations...
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("reshape2")


### Pretty sure when the euclidian distance is taken I'll have to scale and center the min_state_vol as well
### might want to center the scores as well so it's, (-1,1)
### need to update the final steps to provide state combos instead of manually picking


#load necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(reshape2)
library(stringi)

#import data from excel
data <- read.csv("sample_geo_data.csv")

#turn data into long format and rename column headers
data_ooc <- melt(data, id = "Week") %>%
  dplyr::rename(week = 1, state= 2, leads = 3)

data <- melt(data, id = "Week") %>%
  dplyr::rename(time = 1, geography= 2, value = 3)

#remove excel_data object
rm(data)

#create a reference table that stores the total lead volume for each state
volume_reference_table <- data_ooc %>%
  group_by(state) %>%
  dplyr::summarize(lead_volume = sum(leads))

#create a reference table that contains a vector of all states
testing_final_table <- data_ooc %>%
  distinct(state) %>%
  droplevels()

#alter table above by creating a second column such that the table contains a deduped list of all state-state pair combinations
testing_final_table <- crossing(testing_final_table$state, testing_final_table$state) %>%
  rename(state_a = 1) %>%
  rename(state_b = 2) %>%
  filter(state_a != state_b) %>%
  mutate(state_a = as.character(state_a), state_b = as.character(state_b)) %>%
  group_by(grp = paste(pmax(state_a, state_b), pmin(state_a, state_b), sep = "_")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-grp)

#further alter the table above by adding a 3rd and 4th column that, respectively, generates 'min_state_vol' = the minimum state volume between the two states; and 'pair_size' = the ratio between the total lead volume between each state (state a / state b)
testing_final_table <- testing_final_table %>%
  left_join(select(volume_reference_table, lead_volume, state), by = c("state_a" = "state")) %>%
  rename(state_a_lead_volume = lead_volume) %>%
  left_join(select(volume_reference_table, lead_volume, state), by = c("state_b" = "state")) %>%
  rename(state_b_lead_volume = lead_volume) %>%
  group_by(state_a, state_b) %>%
  mutate(min_state_vol = min(state_a_lead_volume, state_b_lead_volume),
         pair_size = state_a_lead_volume / state_b_lead_volume)

#create a new table object that has 2 columns: 'state' = the concatenation of each state pair combination; and 'vol_diff_metric' = my metric for volume difference score between states...
# 1) calculate the lead volume difference between each state at each week squared (^2) and then
# 2) scale the data by dividing by sqrt(of all values) and then
# 3) multiply the result by -1 so the best scores are as close to 1 as possible
test_volume_differences_metric_table <- data_ooc %>%
  left_join(data_ooc,
            suffix = c("1", "2"),
            by = c("week")) %>%
  filter(state1 != state2) %>%
  mutate(vol_diff = (leads1 - leads2)^2) %>%
  unite("state", c(state1, state2), sep = "-") %>%
  select(-c(leads1, leads2)) %>%
  group_by(state) %>%
  dplyr::summarize(vol_diff_metric = sum(vol_diff)) %>%
  mutate(vol_diff_metric = scale(vol_diff_metric)*-1)

#create a new table object that calculates the 'lag' value for each state at each week -- 'lag' is the difference between volume at week X minus volume at week (X-1). As such, there is no week 1 value.
#python -- df.shift() ??
parallelism_data <- data_ooc %>%
  group_by(state) %>%
  dplyr::mutate(lag = lag(leads, n = 1, default = NA),
                lag = leads - lag,
                lag = if_else(is.na(lag), 0, lag))%>%
  as.data.frame() %>%
  filter(week != 1) %>%
  select(-leads)

#create a new table object that has 2 columns: 'state' = the concatenation of each state pair combination; and 'parallelism_metric' = my metric for parallelism score between states...
# 1) calculate the 'lag' (see above) value for each state and its pair state and then
# 2) calculate the lag difference between each state at each week squared (^2) and then
# 3) scale the data by dividing by sqrt(of all values) and then
# 4) multiply the result by -1 so the best scores are as close to 1 as possible
parallelism_metric_table <- data_ooc %>%
  group_by(state) %>%
  dplyr::mutate(lag = lag(leads, n = 1, default = NA),
                lag = leads - lag,
                lag = if_else(is.na(lag), 0, lag)) %>%
  as.data.frame() %>%
  filter(week != 1) %>%
  select(-leads) %>%
  left_join(parallelism_data,
            suffix = c("1", "2"),
            by = c("week")) %>%
  filter(state1 != state2) %>%
  mutate(para_metr = (lag1 - lag2)^2) %>%
  unite("state", c(state1, state2), sep = "-") %>%
  select(-c(lag1, lag2)) %>%
  group_by(state) %>%
  dplyr::summarize(parallelism_metric = sum(para_metr)) %>%
  mutate(parallelism_metric = scale(parallelism_metric)*-1)

#update the final table by adding a 5th column in the first position to concatenate the state pair combo for each row
testing_final_table <- testing_final_table %>%
  mutate(state_combo = paste(state_a, state_b, sep = "-")) %>%
  relocate(state_combo)

#further update the final table by adding in the corresponding 'vol_diff_metric' and 'parallelism_metric' scores for each state pair
testing_final_table <- testing_final_table %>%
  left_join(select(volume_differences_metric_table, vol_diff_metric, state), by = c("state_combo" = "state")) %>%
  left_join(select(parallelism_metric_table, parallelism_metric, state), by = c("state_combo" = "state"))

#plot each unique state pair combination in a 3d plane with the following axis parameters:
# x) parallelism score -- (-1, 1); The larger the x-axis value the stronger the parallelism for the state pair
# y) pair volume ratio score -- (-1, 1); The larger the y-axis value the stronger the parallelism for the state pair
# z) [represented by color range] state volume of the smaller state in the pair -- The larger the z-value [deeper the color] the larger the smaller state volume in the pair
testing_final_table %>%
  filter(min_state_vol > 100) %>%
  ggplot(aes(x = parallelism_metric, y = vol_diff_metric, color = min_state_vol, size = min_state_vol)) +
  geom_point(alpha = .6) +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  theme_classic() +
  labs(title = "State Pair Scores", x = "Parallelism Score", y = "Pair Volume Ratio Score") +
  geom_text(aes(label=ifelse(parallelism_metric > 0,as.character(state_combo),'')),hjust=-0.05,vjust=0.2) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "coral2") +
  geom_vline(xintercept = 0, linetype = 'dashed', color = "coral2")

#make user interface
#produce plots of states
#can also just use 2d and change size or color for volume metric

#plot the top state pair
data_ooc %>%
  filter(state %in% c("Colorado", "Georgia")) %>%
  ggplot(aes(x = week, y = leads, color = state)) +
  geom_line() +
  theme_classic()

#plot the top control states for each state in the top pair
testing_final_table %>%
  filter(state_a %in% c("Virginia", "Illinois")) %>%
  ggplot(aes(x = parallelism_metric, y = vol_diff_metric, color = min_state_vol, size = min_state_vol)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "coral2") +
  geom_vline(xintercept = 0, linetype = 'dashed', color = "coral2") +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  facet_wrap(~state_a) +
  theme_classic()

#also give the percentile for how good the volume difference and parallelism is
#cluster and highlight states for controls -- automate for user
#euclidian distance to pick control states -- on 3d plane; volume diff, total volume, parallelism
#and euclidian distance from a point 1,1,600 or max(min_state_vol) to pick the best state
