## Imports
library(here)
library(fs)
library(tidyverse)
source(here("code", "utils.R"))

## Get data
grouping_details <- readRDS("./data_working/grouping_details.RDS")
data_df  <- readRDS(here("data_working" , "analytical_df.RDS"))

table2 <- bind_rows(
    grouping_details %>%
        group_by(study_code, sensor) %>%
        summarize(person_days = sum(hours_of_obs) / 24,
                  obs = sum(total_obs) / 1000000) %>%
        left_join(
            data_df %>%
                group_by(study_code, sensor) %>%
                summarize(
                    measurement_groupings = sum(daily_groupings) / 1000000,
                    avg_missingness = mean(missing_grps / exp_grps, na.rm = TRUE) * 100
                )
        ) %>% 
        arrange(sensor, study_code),
    grouping_details %>%
        mutate(study_code = "TOTAL") %>% 
        group_by(study_code, sensor) %>%
        summarize(person_days = sum(hours_of_obs) / 24,
                  obs = sum(total_obs) / 1000000) %>%
        left_join(
            data_df %>%
                mutate(study_code = "TOTAL") %>% 
                group_by(study_code, sensor) %>%
                summarize(
                    measurement_groupings = sum(daily_groupings) / 1000000,
                    avg_missingness = mean(missing_grps / exp_grps, na.rm = TRUE) * 100
                )
        ) 
)%>% 
    arrange(sensor, study_code)

write_csv(table2, "./output/tableS1_study_summary_stats.csv")
