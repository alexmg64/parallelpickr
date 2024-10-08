#' Get Parallel Geographies
#'
#' Produces dataframe of parallelism metrics
#'
#' @param data the dataset
#' @param time the name of the column containing time period
#' @param geography the name of the column containing geographies
#' @param value the name of the column containing the "value" of interest to be compared across geographies over time: conversions, occurrences, etc
#'
#' @return df -- a dataframe of parallelism metrics for each geography pair
#' @export
#'
#'
getParallelGeographies <- function(data){

  #create a reference table that stores the total lead volume for each geography
  test_volume_reference_table <- data %>%
    group_by(geography) %>%
    dplyr::summarize(volume = sum(value))

  #create a reference table that contains a vector of all geographies
  test_testing_final_table <- data %>%
    distinct(geography) %>%
    droplevels()

  #alter table above by creating a second column such that the table contains a deduped list of all geography-geography pair combinations
  test_testing_final_table <- crossing(test_testing_final_table$geography, test_testing_final_table$geography) %>%
    rename(geography_a = 1) %>%
    rename(geography_b = 2) %>%
    filter(geography_a != geography_b) %>%
    mutate(geography_a = as.character(geography_a), geography_b = as.character(geography_b)) %>%
    group_by(grp = paste(pmax(geography_a, geography_b), pmin(geography_a, geography_b), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-grp)

  #further alter the table above by adding a 3rd and 4th column that, respectively, generates 'min_geography_vol' = the minimum geography volume between the two geographys; and 'pair_size' = the ratio between the total lead volume between each geography (geography a / geography b)
  test_testing_final_table <- test_testing_final_table %>%
    left_join(select(test_volume_reference_table, volume, geography), by = c("geography_a" = "geography")) %>%
    rename(geography_a_volume = volume) %>%
    left_join(select(test_volume_reference_table, volume, geography), by = c("geography_b" = "geography")) %>%
    rename(geography_b_volume = volume) %>%
    group_by(geography_a, geography_b) %>%
    mutate(min_geography_vol = min(geography_a_volume, geography_b_volume),
           pair_size = geography_a_volume / geography_b_volume)

  #create a new table object that has 2 columns: 'state' = the concatenation of each state pair combination; and 'vol_diff_metric' = my metric for volume difference score between states...
  # 1) calculate the lead volume difference between each state at each week squared (^2) and then
  # 2) scale the data by dividing by sqrt(of all values) and then
  # 3) multiply the result by -1 so the best scores are as close to 1 as possible
  test_volume_differences_metric_table <- data %>%
    left_join(data,
              suffix = c("1", "2"),
              by = c("time")) %>%
    filter(geography1 != geography2) %>%
    mutate(vol_diff = (value1 - value2)^2) %>%
    unite("geography", c(geography1, geography2), sep = "-") %>%
    select(-c(value1, value2)) %>%
    group_by(geography) %>%
    dplyr::summarize(vol_diff_metric = sum(vol_diff)) %>%
    mutate(vol_diff_metric = scale(vol_diff_metric)*-1)

  #create a new table object that calculates the 'lag' value for each state at each week -- 'lag' is the difference between volume at week X minus volume at week (X-1). As such, there is no week 1 value.
  #python -- df.shift() ??
  parallelism_data <- data %>%
    group_by(geography) %>%
    dplyr::mutate(lag = lag(value, n = 1, default = NA),
                  lag = value - lag,
                  lag = if_else(is.na(lag), 0, lag))%>%
    as.data.frame() %>%
    filter(time != 1) %>%
    select(-value)

  #create a new table object that has 2 columns: 'state' = the concatenation of each state pair combination; and 'parallelism_metric' = my metric for parallelism score between states...
  # 1) calculate the 'lag' (see above) value for each state and its pair state and then
  # 2) calculate the lag difference between each state at each week squared (^2) and then
  # 3) scale the data by dividing by sqrt(of all values) and then
  # 4) multiply the result by -1 so the best scores are as close to 1 as possible
  parallelism_metric_table <- data %>%
    group_by(geography) %>%
    dplyr::mutate(lag = lag(value, n = 1, default = NA),
                  lag = value - lag,
                  lag = if_else(is.na(lag), 0, lag)) %>%
    as.data.frame() %>%
    filter(time != 1) %>%
    select(-value) %>%
    left_join(parallelism_data,
              suffix = c("1", "2"),
              by = c("time")) %>%
    filter(geography1 != geography2) %>%
    mutate(para_metr = (lag1 - lag2)^2) %>%
    unite("geography", c(geography1, geography2), sep = "-") %>%
    select(-c(lag1, lag2)) %>%
    group_by(geography) %>%
    dplyr::summarize(parallelism_metric = sum(para_metr)) %>%
    mutate(parallelism_metric = scale(parallelism_metric)*-1)

  #update the final table by adding a 5th column in the first position to concatenate the state pair combo for each row
  test_testing_final_table <- test_testing_final_table %>%
    mutate(geography_combo = paste(geography_a, geography_b, sep = "-")) %>%
    relocate(geography_combo)

  #further update the final table by adding in the corresponding 'vol_diff_metric' and 'parallelism_metric' scores for each state pair
  test_testing_final_table <- test_testing_final_table %>%
    left_join(select(test_volume_differences_metric_table, vol_diff_metric, geography), by = c("geography_combo" = "geography")) %>%
    left_join(select(parallelism_metric_table, parallelism_metric, geography), by = c("geography_combo" = "geography")) %>%
    ungroup()

  #plot each unique state pair combination in a 3d plane with the following axis parameters:
  # x) parallelism score -- (-1, 1); The larger the x-axis value the stronger the parallelism for the state pair
  # y) pair volume ratio score -- (-1, 1); The larger the y-axis value the stronger the parallelism for the state pair
  # z) [represented by color range] state volume of the smaller state in the pair -- The larger the z-value [deeper the color] the larger the smaller state volume in the pair
  test_testing_final_table %>%
    filter(min_geography_vol > 100) %>%
    ggplot(aes(x = parallelism_metric, y = vol_diff_metric, color = min_geography_vol, size = min_geography_vol)) +
    geom_point(alpha = .6) +
    scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
    theme_classic() +
    labs(title = "State Pair Scores", x = "Parallelism Score", y = "Pair Volume Ratio Score") +
    geom_text(aes(label=ifelse(parallelism_metric > 0,as.character(geography_combo),'')),hjust=-0.05,vjust=0.2) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = "coral2") +
    geom_vline(xintercept = 0, linetype = 'dashed', color = "coral2")

  #make user interface
  #produce plots of states
  #can also just use 2d and change size or color for volume metric

  #plot the top state pair
  data %>%
    filter(geography %in% c("Colorado", "Texas")) %>%
    ggplot(aes(x = time, y = value, color = geography)) +
    geom_line() +
    theme_classic()

  #plot the top control states for each state in the top pair
  test_testing_final_table %>%
    filter(geography_a %in% c("Virginia", "Illinois")) %>%
    ggplot(aes(x = parallelism_metric, y = vol_diff_metric, color = min_geography_vol, size = min_geography_vol)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed', color = "coral2") +
    geom_vline(xintercept = 0, linetype = 'dashed', color = "coral2") +
    scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
    facet_wrap(~geography_a) +
    theme_classic()

  test_testing_final_table %>%
    filter(stringr::str_detect(geography_combo, "Illinois")) %>%
    ggplot(aes(x = parallelism_metric, y = vol_diff_metric, color = min_geography_vol, size = min_geography_vol)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed', color = "coral2") +
    geom_vline(xintercept = 0, linetype = 'dashed', color = "coral2") +
    scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
    facet_wrap(~geography_combo) +
    theme_classic()

  #also give the percentile for how good the volume difference and parallelism is
  #cluster and highlight states for controls -- automate for user
  #euclidian distance to pick control states -- on 3d plane; volume diff, total volume, parallelism
  #and euclidian distance from a point 1,1,600 or max(min_geography_vol) to pick the best state

  }
