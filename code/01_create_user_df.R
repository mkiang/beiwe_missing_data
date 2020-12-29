## Imports
library(here)
library(tidyverse)
library(fs)

## Get list of all users and studies
## Here, `list_of_users_and_studies.RDS` is just a mapping of user ID to
## study ID as well as information about user sensor parameters.
user_studies <- readRDS(here("data_working", "list_of_users_and_studies.RDS"))

## Here, `cleaned_demographics.csv` are just files containing user ID (to
## be matched) as well as any additiona demographic data. In our case,
## age, sex, race/ethnicity, and education. 
user_demog <- bind_rows(read_csv(here(
    "data_raw", "study_a",
    "cleaned_demographics.csv"
)),
read_csv(here(
    "data_raw", "study_b",
    "cleaned_demographics.csv"
)))

## Get a list of test user accounts that need to be dropped
## Most studies create "test users" to get make sure everything is running
## before they go live. This file just contains a list of those user names
## to make sure we do not include them in the analysis. 
drop_users <- read_csv(here("data_working", "test_users_to_drop.csv"))

## Get phone types
id_files <- dir_ls(
    "./data_stripped",
    recursive = TRUE,
    type = "file",
    regexp = "identifiers"
)

holder <- NULL
for (f in id_files) {
    f_info <- metabeiwe:::.parse_file_name(f)
    dev_os <-
        suppressMessages(suppressWarnings(read_csv(f)$device_os))
    
    holder <- bind_rows(holder,
                        tibble(beiwe_user_id = f_info$user_id,
                               dev_os = dev_os))
}

phone_types <- holder %>%
    distinct() %>%
    mutate(iphone  = ifelse(grepl("iPhone|iOS", dev_os), 1, 0),
           android = ifelse(grepl("Android", dev_os), 1, 0)
    )

## Merge them and remove the test cases
user_df <- user_studies %>%
    left_join(user_demog) %>%
    left_join(phone_types %>% select(-dev_os)) %>%
    filter(!(beiwe_user_id %in% drop_users$beiwe_user_id)) %>%
    mutate(user_id_merged =
               str_replace_all(beiwe_user_id, multi_id_replace)) %>%
    distinct()

## Save
saveRDS(user_df, "./data_working/user_data_with_demographics.RDS")
