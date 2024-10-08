#testing multiple rounds of grouping

#create a reference table that contains a vector of all geographies
test_testing_final_table <- data %>%
  distinct(geography) %>%
  droplevels()

crossing_test <- crossing(test_testing_final_table$geography, test_testing_final_table$geography) %>%
  rename(geography_a = 1, geography_b = 2)

crossing_test_3_states <- crossing(test_testing_final_table$geography, test_testing_final_table$geography, test_testing_final_table$geography) %>%
  rename(geography_a = 1, geography_b = 2, geography_c = 3)

# ding ding ding!

crossing_test_filt <- crossing_test %>% filter(geo_a != geo_b)


crossing_test_3_states_filt <- crossing_test_3_states %>% filter(geography_a != geography_b & geography_a != geography_c & geography_b != geography_c)

#applying functions.R code

crossing_result_b <- crossing(test_testing_final_table$geography, test_testing_final_table$geography, test_testing_final_table$geography) %>%
  rename(geography_a = 1, geography_b = 2, geography_c = 3) %>%
  filter(geography_a != geography_b & geography_a != geography_c & geography_b != geography_c) %>%
  mutate(geography_a = as.character(geography_a),
         geography_b = as.character(geography_b),
         geography_c = as.character(geography_c)) %>%
  mutate(geography = paste(geography_a, geography_b, geography_c, sep = "|"),
         geography_sorted = str_sort(c("geography_a", "geography_b", "geography_c"))) %>%
  distinct(geography_sorted, .keep_all = T)


#### add weekly volume ####

crossing_weekly <- crossing_result_b %>%
  left_join(data, by = c("geography_a" = "geography")) %>%
  left_join(data, by = c("geography_b" = "geography", "time" = "time"), suffix = c("_a", "_b")) %>%
  left_join(data, by = c("geography_c" = "geography", "time" = "time")) %>%
  mutate(geography = paste(geography_a, geography_b, geography_c, sep = "|")) %>%
  mutate(value = value_a + value_b + value) %>%
  select(-value_a, - value_b)

# lags

crossing_weekly_lags <- crossing_weekly %>%
  group_by(geography) %>%
  mutate(lag = lag(value, n = 1, default = NA),
         lag = value - lag,
         lag = if_else(is.na(lag), 0, lag)) #%>%
  # as.data.frame() %>%
  # filter(week != 1) %>%
  # select(-leads)

cross1_for_join <- crossing_weekly_lags %>%
  select(1,2,3,6) %>%
  distinct(geography_a, geography_b, geography_c, geography)



#create a new table object that has 2 columns: 'state' = the concatenation of each state pair combination; and 'parallelism_metric' = my metric for parallelism score between states...
# 1) calculate the 'lag' (see above) value for each state and its pair state and then
# 2) calculate the lag difference between each state at each week squared (^2) and then
# 3) scale the data by dividing by sqrt(of all values) and then
# 4) multiply the result by -1 so the best scores are as close to 1 as possible
para_cross <- crossing_weekly_lags %>%
  left_join(crossing_weekly_lags,
            suffix = c("1", "2"),
            by = c("time")) %>%
  filter(state1 != state2) %>%
  mutate(para_metr = (lag1 - lag2)^2) %>%
  unite("state", c(state1, state2), sep = "-") %>%
  select(-c(lag1, lag2)) %>%
  group_by(state) %>%
  dplyr::summarize(parallelism_metric = sum(para_metr)) %>%
  mutate(parallelism_metric = scale(parallelism_metric)*-1)



