library(tidyverse)
library(lubridate)
source('./code/helpers/file_manipulation.R')

return_grouped_summary <- function(file_path, buffer = 30) {
    ## Given a csv file with time stamps, will return a summarized version
    ## grouped by observations with less than `buffer` seconds between them.
    
    ## Read in and rename time stamp column
    df <- readr::read_csv(file_path) %>%
        dplyr::rename(utc_orig = `UTC time`)

    ## Add in metadata 
    metadata <- parse_file_name(file_path)
    df <- df %>% 
        mutate(study_name = metadata$study_name, 
               data_type  = metadata$data_type, 
               user_id    = metadata$user_id)
    
    ## Add in time components
    df <- df %>% 
        mutate(year   = lubridate::year(utc_orig), 
               month  = lubridate::month(utc_orig),
               day    = lubridate::day(utc_orig),
               hour   = lubridate::hour(utc_orig),
               minute = lubridate::minute(utc_orig))
    
    ## Add time deltas
    df <- df %>% 
        dplyr::group_by(study_name, data_type, user_id, 
                        year, month, day, hour) %>% 
        dplyr::arrange(study_name, data_type, user_id, utc_orig) %>% 
        dplyr::mutate(t_delta  = utc_orig - lag(utc_orig), 
                      time_gap = ifelse(is.na(t_delta), 1, t_delta > buffer))
    
    
    ## Add grouping column
    df <- df %>% 
        dplyr::mutate(grouping = cumsum(time_gap))
    
    ## Summarize over the date/hour and group
    df <- df %>% 
        dplyr::group_by(study_name, data_type, user_id, grouping, 
                        year, month, day, hour) %>% 
        dplyr::summarize(t_min   = min(utc_orig), 
                         t_max   = max(utc_orig), 
                         t_delta = difftime(t_max, t_min, units = "secs"), 
                         dur_sec = round(t_delta, 0), 
                         n_obs   = n()) %>% 
        dplyr::arrange(study_name, data_type, user_id, t_min) %>% 
        dplyr::ungroup()
    
    return(df)
}

return_renamed_raw_file <- function(file_path) {
    df <- readr::read_csv(file_path, col_types = cols()) %>% 
        dplyr::rename(utc_orig = `UTC time`)
    return(df)
}


dedup_ids_df <- function(identifier_df) {
    ## For idents, if there's a duplicate user_id, just take the first row ----
    ids <- identifier_df %>% 
        dplyr::group_by(patient_id) %>% 
        dplyr::summarize(user_id   = first(patient_id), 
                         device_os = first(device_os))
    
    return(ids)
}


make_col_of_fake_ids <- function(identifier_df, pref = "User", seed = 1) {
    ## Takes an identifier dataframe and makes random numbers and returns
    ## a character vector of fake user ids.
    set.seed(seed)
    
    ids <- identifier_df %>% 
        dplyr::mutate(r_num = runif(n())) %>% 
        dplyr::arrange(r_num) %>% 
        dplyr::mutate(fake_id_int = 1:n(), 
                      new_user_id = sprintf("%s %02.i", pref, fake_id_int))
    
    return(ids %>% dplyr::pull(new_user_id))
}
