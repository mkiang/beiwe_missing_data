library(tidyverse)
library(metabeiwe)

summarize_rds_file <- function(rds_path) {
    ## Read in the data
    temp_x <- readRDS(rds_path)
    
    ## Clean up
    temp_x <- temp_x %>% 
        rename_time() %>% 
        metabeiwe:::create_time_cols()
    
    ## Summarize
    sum_x <- temp_x %>% 
        group_by(year, month, day) %>% 
        summarize(min_ts = min(utc_orig), 
                  max_ts = max(utc_orig), 
                  obs = n()) %>% 
        ungroup() %>% 
        summarize(first_obs      = min(min_ts), 
                  last_obs       = max(max_ts), 
                  max_daily_obs  = max(obs), 
                  min_daily_obs  = min(obs), 
                  mean_daily_obs = mean(obs), 
                  total_obs      = sum(obs)) %>% 
        create_metadata_cols(., rds_path) %>% 
        mutate(hours_of_obs = as.numeric(
            difftime(last_obs, first_obs, units = "hours")
        )) %>% 
        select(study_name, user_id, data_type, first_obs, last_obs, 
               hours_of_obs, mean_daily_obs, max_daily_obs, min_daily_obs, 
               total_obs)
    
    return(sum_x)
}

summarize_data_df <- function(df, df_path) {
    ## Clean up
    temp_x <- df %>% 
        rename_time() %>% 
        metabeiwe:::create_time_cols()
    
    ## Summarize
    sum_x <- temp_x %>% 
        group_by(year, month, day) %>% 
        summarize(min_ts = min(utc_orig), 
                  max_ts = max(utc_orig), 
                  obs = n()) %>% 
        ungroup() %>% 
        summarize(first_obs      = min(min_ts), 
                  last_obs       = max(max_ts), 
                  max_daily_obs  = max(obs), 
                  min_daily_obs  = min(obs), 
                  mean_daily_obs = mean(obs), 
                  total_obs      = sum(obs)) %>% 
        create_metadata_cols(., df_path) %>% 
        mutate(hours_of_obs = as.numeric(
            difftime(last_obs, first_obs, units = "hours")
        )) %>% 
        select(study_name, user_id, data_type, first_obs, last_obs, 
               hours_of_obs, mean_daily_obs, max_daily_obs, min_daily_obs, 
               total_obs)
    
    return(sum_x)
}

return_buffer <- function(data_type, user_id, user_studies_df, 
                          prop_time = .2) {
    x <- user_studies_df %>% 
        filter(beiwe_user_id == user_id)
    
    if (length(x) == 0) {
        stop("Cannot find user.")
    } else if (data_type == "accelerometer") {
        min(x$accel_off * prop_time, x$accel_on)
    } else if (data_type == "bluetooth") {
        min(x$bluetooth_on, 
            (x$bluetooth_total - x$bluetooth_on) * prop_time)
    } else if (data_type == "gps") {
        min(x$gps_off * prop_time, x$gps_on)
    } else if (data_type == "gyro") {
        min(x$gyro_off * prop_time, x$gyro_on)
    } else if (data_type == "magnetometer") {
        min(x$magnet_off * prop_time, x$magnet_on)
    } else if (data_type == "devicemotion") {
        min(x$devicemotion_off * prop_time, x$devicemotion_on)
    } else if (data_type == "wifi") {
        x$wifi_freq * prop_time
    } else {
        stop("Data type not found.")
    }
}

return_exp_grp <- function(data_type, user_id, user_studies_df, dig = 0) {
    ## Returns the expected number of groupings PER HOUR
    x <- user_studies_df %>% 
        filter(beiwe_user_id == user_id)
    
    if (length(x) == 0) {
        stop("Cannot find user.")
    } else if (data_type == "accelerometer") {
        r <- (x$accel_off + x$accel_on)
    } else if (data_type == "bluetooth") {
        r <- x$bluetooth_total
    } else if (data_type == "gps") {
        r <- (x$gps_off + x$gps_on)
    } else if (data_type == "gyro") {
        r <- (x$gyro_off + x$gyro_on)
    } else if (data_type == "magnetometer") {
        r <- (x$magnet_off + x$magnet_on)
    } else if (data_type == "devicemotion") {
        r <- (x$devicemotion_off + x$devicemotion_on)
    } else if (data_type == "wifi") {
        r <- x$wifi_freq
    } else {
        stop("Data type not found.")
    }
    
    return( round((60 * 60) / r, digits = dig) )
}
