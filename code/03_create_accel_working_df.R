## Imports
library(here)
library(fs)
library(furrr)
library(future)
library(tidyverse)
source(here("code", "utils.R"))

## Get data
user_studies <- readRDS(here("data_working", "list_of_users_and_studies.RDS"))
user_df <- readRDS("./data_working/user_data_with_demographics.RDS")

all_groupings <- dir_ls("./data_stripped", recurse = TRUE,
                        type = "file", regexp = "groupings_summary.RDS")

## Now for accelerometer, let's get all the grouping files, summarize them
## to number of groupings per day, and then combine that into one df.
if (!file.exists("./data_working/accel_groupings.RDS")) {
    accel_files <- all_groupings[grepl("accel", all_groupings)]
    
    future::plan("multiprocess")
    accel_groupings <- furrr::future_map_dfr(
        .x = accel_files,
        .f = ~ readRDS(.x)  %>%
            mutate(uid = str_replace_all(user_id, multi_id_replace))
        ) %>%
        group_by(study_name, uid, year, month, day) %>%
        summarize(daily_groupings = n()) %>%
        ungroup()
    
    ## Now we need the expected number of groupings (based on study settings)
    accel_groupings <- accel_groupings %>% 
        rowwise() %>% 
        mutate(exp_grps = return_exp_grp("accelerometer", 
                                         uid, user_studies) * 24) %>% 
        ungroup()
    
    ## We want to model rate of missingness so substract from expected and in 
    ## some rare cases, we can actually get more groupings than expected so 
    ## truncate those at 0. (E.g., GPS pings or games that use accelerometer).
    accel_groupings <- accel_groupings %>% 
        mutate(missing_grps = exp_grps - daily_groupings, 
               missing_grps = ifelse(missing_grps < 0, 0, missing_grps))
    
    ## Join with demographics
    accel_groupings <- accel_groupings %>% 
        left_join(
            user_df %>% 
                select(uid = user_id_merged, female, 
                       age, race, education, iphone) %>% 
                filter(uid %in% accel_groupings$uid) %>% 
                distinct()
        )
    
    ## Save
    saveRDS(accel_groupings, "./data_working/accel_groupings.RDS")
}
